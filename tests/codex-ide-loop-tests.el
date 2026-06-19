;;; codex-ide-loop-tests.el --- Tests for Codex loop buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-loop'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide)
(require 'codex-ide-loop)
(require 'codex-ide-test-fixtures)

(defmacro codex-ide-loop-test-with-loop (&rest body)
  "Run BODY with an isolated loop and session."
  (declare (indent 0) (debug t))
  `(let* ((buffers-before (buffer-list))
          (codex-ide-loop--loops-by-session (make-hash-table :test 'eq))
          (session-buffer (generate-new-buffer "*Codex[test-loop]*"))
          (process (codex-ide-test-process-create :live t))
          (session (make-codex-ide-session
                    :directory temporary-file-directory
                    :buffer session-buffer
                    :process process
                    :thread-id "thread-1"
                    :status "idle"))
          loop)
     (unwind-protect
         (codex-ide-test-with-fake-processes
           (cl-letf (((symbol-function 'codex-ide-transcript-update-header-line)
                      (lambda (&rest _) nil)))
             (setq loop (codex-ide-loop--create session 60))
             ,@body))
       (when (and (boundp 'loop)
                  loop
                  (codex-ide-loop-p loop))
         (setf (codex-ide-loop-state loop) 'stopped)
         (codex-ide-loop--cancel-timer loop))
       (dolist (buffer (buffer-list))
         (when (and (not (memq buffer buffers-before))
                    (buffer-live-p buffer))
           (with-current-buffer buffer
             (when (and (boundp 'codex-ide-loop--loop)
                        (codex-ide-loop-p codex-ide-loop--loop))
               (setf (codex-ide-loop-state codex-ide-loop--loop) 'stopped)
               (codex-ide-loop--cancel-timer codex-ide-loop--loop)))))
       (codex-ide-test--cleanup-buffers buffers-before))))

(ert-deftest codex-ide-loop-parse-intervals ()
  (should (= (codex-ide-loop--parse-interval "30s") 30))
  (should (= (codex-ide-loop--parse-interval "15m") 900))
  (should (= (codex-ide-loop--parse-interval "0.5h") 1800))
  (should (= (codex-ide-loop--parse-interval "2h") 7200))
  (should (= (codex-ide-loop--parse-interval "1d") 86400))
  (should (= (codex-ide-loop--parse-interval "5") 300))
  (should (equal (codex-ide-loop--format-duration 1800.0) "30m")))

(ert-deftest codex-ide-loop-buffer-name-follows-session-buffer-name ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-loop]<2>*"))
         (session (make-codex-ide-session
                   :directory temporary-file-directory
                   :name-suffix 2
                   :buffer session-buffer)))
    (unwind-protect
        (should (equal (codex-ide-loop--loop-buffer-name session)
                       "*codex[test-loop]<2>*-loop"))
      (kill-buffer session-buffer))))

(ert-deftest codex-ide-loop-current-prompt-reads-editable-region ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "  Review the changes.  \n")
      (should (equal (codex-ide-loop--current-prompt loop)
                     "Review the changes.")))))

(ert-deftest codex-ide-loop-render-preserves-prompt-text ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "Keep this prompt.")
      (codex-ide-loop--render-buffer loop)
      (should (equal (codex-ide-loop--current-prompt loop)
                     "Keep this prompt.")))))

(ert-deftest codex-ide-loop-create-places-point-at-prompt-start ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (should (= (point)
                 (marker-position
                  (codex-ide-loop-prompt-start-marker loop)))))))

(ert-deftest codex-ide-loop-mode-uses-conservative-session-like-keys ()
  (should (eq (lookup-key codex-ide-loop-mode-map (kbd "C-c RET"))
              #'codex-ide-loop-start))
  (should (eq (lookup-key codex-ide-loop-mode-map (kbd "C-c C-c"))
              #'codex-ide-loop-pause))
  (dolist (key '("C-c C-p" "C-c C-k" "C-c C-s" "C-c C-j"))
    (should-not (lookup-key codex-ide-loop-mode-map (kbd key)))))

(ert-deftest codex-ide-loop-jump-or-create-opens-existing-loop-without-prompt ()
  (codex-ide-loop-test-with-loop
    (let (displayed-buffer prompted)
      (cl-letf (((symbol-function 'codex-ide-loop--read-interval)
                 (lambda ()
                   (setq prompted t)
                   "30s"))
                ((symbol-function 'codex-ide-display-buffer)
                 (lambda (buffer &rest _)
                   (setq displayed-buffer buffer)
                   nil)))
        (with-current-buffer session-buffer
          (setq-local codex-ide--session session)
          (should (eq (codex-ide-loop-jump-or-create) loop))))
      (should (eq displayed-buffer (codex-ide-loop-buffer loop)))
      (should-not prompted)
      (with-current-buffer (codex-ide-loop-buffer loop)
        (should (= (point)
                   (marker-position
                    (codex-ide-loop-prompt-start-marker loop))))))))

(ert-deftest codex-ide-loop-jump-or-create-creates-loop-for-session-buffer ()
  (codex-ide-loop-test-with-loop
    (let ((old-buffer (codex-ide-loop-buffer loop))
          created-loop
          displayed-buffer)
      (kill-buffer old-buffer)
      (puthash session loop codex-ide-loop--loops-by-session)
      (cl-letf (((symbol-function 'codex-ide-display-buffer)
                 (lambda (buffer &rest _)
                   (setq displayed-buffer buffer)
                   nil)))
        (with-current-buffer session-buffer
          (setq-local codex-ide--session session)
          (setq created-loop (codex-ide-loop-jump-or-create "30s"))))
      (should (codex-ide-loop-p created-loop))
      (should-not (eq created-loop loop))
      (should (eq (gethash session codex-ide-loop--loops-by-session)
                  created-loop))
      (should (= (codex-ide-loop-interval-seconds created-loop) 30))
      (should (eq displayed-buffer (codex-ide-loop-buffer created-loop)))
      (with-current-buffer (codex-ide-loop-buffer created-loop)
        (should (= (point)
                   (marker-position
                    (codex-ide-loop-prompt-start-marker created-loop))))))))

(ert-deftest codex-ide-loop-rendered-prompt-text-uses-user-face ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "Keep this prompt.")
      (codex-ide-loop--render-buffer loop)
      (let ((start (marker-position (codex-ide-loop-prompt-start-marker loop))))
        (should (equal (buffer-substring-no-properties start (+ start 4))
                       "Keep"))
        (should (eq (get-text-property start 'face)
                    'codex-ide-user-prompt-face))
        (should-not (memq 'codex-ide-prompt-prefix-face
                          (ensure-list
                           (get-char-property start 'face))))))))

(ert-deftest codex-ide-loop-header-region-is-read-only ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (point-min))
      (should-error (insert "x") :type 'text-read-only)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "editable")
      (should (equal (codex-ide-loop--current-prompt loop) "editable")))))

(ert-deftest codex-ide-loop-prompt-padding-is-read-only ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "editable")
      (goto-char codex-ide-loop--input-end-marker)
      (should-error (delete-char 1) :type 'text-read-only)
      (should (equal (codex-ide-loop--current-prompt loop) "editable")))))

(ert-deftest codex-ide-loop-prompt-sync-clamps-point-out-of-padding ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "editable")
      (goto-char (codex-ide-loop--prompt-display-start-position))
      (codex-ide-loop--sync-prompt-point)
      (should (= (point)
                 (marker-position
                  (codex-ide-loop-prompt-start-marker loop))))
      (goto-char (point-max))
      (codex-ide-loop--sync-prompt-point)
      (should (= (point)
                 (marker-position codex-ide-loop--input-end-marker))))))

(ert-deftest codex-ide-loop-prompt-uses-session-prompt-styling ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (should-not (string-match-p "--- Prompt ---" (buffer-string)))
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (let ((line-start
             (save-excursion
               (goto-char (1- (marker-position
                               (codex-ide-loop-prompt-start-marker loop))))
               (line-beginning-position))))
        (should (equal (buffer-substring-no-properties
                        line-start
                        (codex-ide-loop-prompt-start-marker loop))
                       "> ")))
      (should (memq 'codex-ide-user-prompt-face
                    (ensure-list
                     (get-text-property
                      (marker-position
                       (codex-ide-loop-prompt-start-marker loop))
                      'face)))))))

(ert-deftest codex-ide-loop-header-uses-styled-title-and-metadata ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'face)
                  'codex-ide-loop-title-face))
      (should (search-forward "* Session:" nil t))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'codex-ide-loop-metadata-label-face))
      (should (search-forward "* State:" nil t))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'codex-ide-loop-metadata-label-face))
      (should (search-forward "* Actions:" nil t))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'codex-ide-loop-metadata-label-face)))))

(ert-deftest codex-ide-loop-session-name-is-jump-button ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (should-not (string-match-p "\\[jump\\]" (buffer-string)))
      (goto-char (point-min))
      (search-forward (buffer-name session-buffer))
      (let* ((pos (match-beginning 0))
             (button (button-at pos)))
        (should button)
        (should (equal (substring-no-properties (button-label button))
                       (buffer-name session-buffer)))
        (should (eq (get-text-property pos 'face)
                    'codex-ide-loop-session-link-face))))))

(ert-deftest codex-ide-loop-actions-omit-stop-button ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (point-min))
      (let (labels)
        (while (re-search-forward "start\\|pause\\|send now\\|stop" nil t)
          (when (button-at (match-beginning 0))
            (push (substring-no-properties
                   (button-label (button-at (match-beginning 0))))
                  labels)))
        (should (equal (nreverse labels)
                       '("start" "pause" "send now")))))))

(ert-deftest codex-ide-loop-typed-prompt-text-uses-user-face ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "Typed prompt.")
      (let ((start (marker-position (codex-ide-loop-prompt-start-marker loop))))
        (should (memq 'codex-ide-user-prompt-face
                      (ensure-list (get-char-property start 'face))))
        (should-not (memq 'codex-ide-prompt-prefix-face
                          (ensure-list
                           (get-char-property start 'face))))))))

(ert-deftest codex-ide-loop-prompt-keeps-unstyled-spacer-line ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (forward-line -2)
      (should (= (line-beginning-position) (line-end-position)))
      (should-not (get-text-property (point) 'face)))))

(ert-deftest codex-ide-loop-rerender-keeps-prompt-face-local ()
  (codex-ide-loop-test-with-loop
    (setf (codex-ide-loop-state loop) 'running)
    (codex-ide-loop--render-buffer loop)
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (forward-line -2)
      (should-not (text-property-any
                   (point-min)
                   (line-end-position)
                   'face
                   'codex-ide-user-prompt-face)))))

(ert-deftest codex-ide-loop-empty-prompt-shows-placeholder ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (codex-ide-loop--refresh-placeholder)
      (should (overlayp codex-ide-loop--placeholder-overlay))
      (should (equal (substring-no-properties
                      (overlay-get codex-ide-loop--placeholder-overlay
                                   'after-string))
                     codex-ide-loop-prompt-placeholder-text))
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "not empty")
      (codex-ide-loop--refresh-placeholder)
      (should-not (overlayp codex-ide-loop--placeholder-overlay)))))

(ert-deftest codex-ide-loop-tab-navigates-buttons ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (point-min))
      (codex-ide-loop-nav-forward)
      (should (equal (substring-no-properties
                      (button-label (button-at (point))))
                     (buffer-name session-buffer)))
      (codex-ide-loop-nav-forward)
      (should (equal (button-label (button-at (point))) "start"))
      (should (eq (get-text-property (point) 'face)
                  'codex-ide-loop-action-button-face))
      (codex-ide-loop-nav-backward)
      (should (equal (substring-no-properties
                      (button-label (button-at (point))))
                     (buffer-name session-buffer))))))

(ert-deftest codex-ide-loop-empty-prompt-skips-submit ()
  (codex-ide-loop-test-with-loop
    (let (submitted)
      (cl-letf (((symbol-function 'codex-ide-transcript-submit-prompt-to-session)
                 (lambda (&rest _)
                   (setq submitted t))))
        (should-not (codex-ide-loop--submit-now loop)))
      (should-not submitted)
      (should (equal (codex-ide-loop-last-skip-reason loop)
                     "Prompt is empty")))))

(ert-deftest codex-ide-loop-busy-session-skips-submit ()
  (codex-ide-loop-test-with-loop
    (setf (codex-ide-session-current-turn-id session) "turn-1")
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "Continue."))
    (let (submitted)
      (cl-letf (((symbol-function 'codex-ide-transcript-submit-prompt-to-session)
                 (lambda (&rest _)
                   (setq submitted t))))
        (should-not (codex-ide-loop--submit-now loop)))
      (should-not submitted)
      (should (equal (codex-ide-loop-last-skip-reason loop)
                     "Session is busy")))))

(ert-deftest codex-ide-loop-submit-now-reads-current-prompt ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (goto-char (codex-ide-loop-prompt-start-marker loop))
      (insert "Initial prompt.")
      (delete-region (codex-ide-loop-prompt-start-marker loop)
                     codex-ide-loop--input-end-marker)
      (insert "Changed prompt."))
    (let (submitted-session submitted-prompt)
      (cl-letf (((symbol-function 'codex-ide-transcript-submit-prompt-to-session)
                 (lambda (session prompt &rest plist)
                   (setq submitted-session session)
                   (setq submitted-prompt prompt)
                   (should (plist-get plist :metadata-line))
                   (should (plist-get plist :suppress-context)))))
        (should (codex-ide-loop--submit-now loop)))
      (should (eq submitted-session session))
      (should (equal submitted-prompt "Changed prompt."))
      (should (= (codex-ide-loop-run-count loop) 1)))))

(ert-deftest codex-ide-loop-start-schedules-timer ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (codex-ide-loop-start))
    (should (eq (codex-ide-loop-state loop) 'active))
    (should (timerp (codex-ide-loop-timer loop)))
    (should (codex-ide-loop-next-run-at loop))
    (setf (codex-ide-loop-state loop) 'stopped)
    (codex-ide-loop--cancel-timer loop)))

(ert-deftest codex-ide-loop-killing-buffer-detaches-loop ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (codex-ide-loop-start))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) t)))
      (kill-buffer (codex-ide-loop-buffer loop)))
    (should (eq (codex-ide-loop-state loop) 'stopped))
    (should-not (codex-ide-loop-timer loop))
    (should-not (gethash session codex-ide-loop--loops-by-session))))

(ert-deftest codex-ide-loop-session-destroyed-stops-loop ()
  (codex-ide-loop-test-with-loop
    (with-current-buffer (codex-ide-loop-buffer loop)
      (codex-ide-loop-start))
    (codex-ide-loop--handle-session-event 'destroyed session)
    (should (eq (codex-ide-loop-state loop) 'stopped))
    (should-not (codex-ide-loop-timer loop))
    (should-not (gethash session codex-ide-loop--loops-by-session))
    (should (equal (codex-ide-loop-last-error loop) "Session destroyed"))))

(ert-deftest codex-ide-loop-header-summary-shows-attached-loop ()
  (codex-ide-loop-test-with-loop
    (let ((summary (codex-ide-loop--header-summary session)))
      (should (string-match-p "Loop: paused" (substring-no-properties summary)))
      (should (get-text-property 0 'local-map summary)))))

(ert-deftest codex-ide-loop-placeholder-reflects-attached-loop ()
  (codex-ide-loop-test-with-loop
    (should (equal (codex-ide-loop--session-placeholder session)
                   "Loop paused"))
    (setf (codex-ide-loop-state loop) 'active)
    (should (equal (codex-ide-loop--session-placeholder session)
                   "Loop active: waiting for next scheduled prompt..."))))

(ert-deftest codex-ide-transcript-submit-prompt-to-session-uses-exact-session ()
  (let* ((session-buffer (generate-new-buffer "*Codex[test-submit]*"))
         (other-buffer (generate-new-buffer "*other*"))
         (process (codex-ide-test-process-create :live t))
         (session (make-codex-ide-session
                   :directory temporary-file-directory
                   :buffer session-buffer
                   :process process
                   :thread-id "thread-1"
                   :status "idle"))
         submitted-session
         submitted-prompt
         submitted-buffer
         submitted-origin)
    (unwind-protect
        (codex-ide-test-with-fake-processes
          (cl-letf (((symbol-function 'codex-ide--submit-prompt-to-session)
                     (lambda (session prompt &rest _)
                       (setq submitted-session session)
                       (setq submitted-prompt prompt)
                       (setq submitted-buffer (current-buffer))
                       (setq submitted-origin codex-ide--prompt-origin-buffer))))
            (with-current-buffer other-buffer
              (codex-ide-transcript-submit-prompt-to-session
               session
               "Loop prompt."))))
      (kill-buffer session-buffer)
      (kill-buffer other-buffer))
    (should (eq submitted-session session))
    (should (equal submitted-prompt "Loop prompt."))
    (should (eq submitted-buffer session-buffer))
    (should (eq submitted-origin session-buffer))))

(ert-deftest codex-ide-transcript-loop-submission-renders-prompt-and-metadata ()
  (let* ((session-buffer (generate-new-buffer "*Codex[test-loop-submit]*"))
         (process (codex-ide-test-process-create :live t))
         (session (make-codex-ide-session
                   :directory temporary-file-directory
                   :buffer session-buffer
                   :process process
                   :thread-id "thread-1"
                   :status "idle"))
         sent-payload)
    (unwind-protect
        (codex-ide-test-with-fake-processes
          (with-current-buffer session-buffer
            (codex-ide-session-mode)
            (setq-local codex-ide--session session)
            (codex-ide--insert-input-prompt session nil))
          (cl-letf (((symbol-function 'codex-ide--send-turn-start)
                     (lambda (_session _thread-id payload)
                       (setq sent-payload payload)))
                    ((symbol-function 'codex-ide--after-turn-start-submitted)
                     (lambda (&rest _) nil))
                    ((symbol-function 'codex-ide--update-header-line)
                     (lambda (&rest _) nil)))
            (codex-ide-transcript-submit-prompt-to-session
             session
             "Loop visible prompt"
             :metadata-line "Loop: from *Codex Loop[test]* at 2026-06-19 15:00:00"
             :suppress-context t))
          (with-current-buffer session-buffer
            (should (string-match-p "> Loop visible prompt" (buffer-string)))
            (should (string-match-p
                     "Loop: from \\*Codex Loop\\[test\\]\\* at 2026-06-19 15:00:00"
                     (buffer-string)))))
      (kill-buffer session-buffer))
    (let ((text (alist-get 'text (aref (alist-get 'input sent-payload) 0))))
      (should (equal text "Loop visible prompt"))
      (should-not (string-match-p "\\[Emacs prompt context\\]" text))
      (should-not (string-match-p "\\[Emacs session context\\]" text)))))

(provide 'codex-ide-loop-tests)

;;; codex-ide-loop-tests.el ends here
