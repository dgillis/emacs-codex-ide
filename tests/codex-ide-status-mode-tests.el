;;; codex-ide-status-mode-tests.el --- Tests for codex-ide status mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-status-mode'.

;;; Code:

(require 'ert)
(require 'button)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-status-mode)

(defun codex-ide-status-mode-test--set-session-buffer-prompts (session prompts &optional active-prompt)
  "Populate SESSION buffer with PROMPTS and optional ACTIVE-PROMPT."
  (with-current-buffer (codex-ide-session-buffer session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (prompt prompts)
        (let ((start (point)))
          (insert "> " prompt)
          (codex-ide--style-user-prompt-region start (point))
          (insert "\n\nassistant\n\n"))))
    (codex-ide--insert-input-prompt session (or active-prompt ""))))

(defun codex-ide-status-mode-test--set-session-buffer-with-response
    (session prompts response-lines &optional active-prompt)
  "Populate SESSION with PROMPTS followed by RESPONSE-LINES and ACTIVE-PROMPT."
  (with-current-buffer (codex-ide-session-buffer session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (prompt prompts)
        (let ((start (point)))
          (insert "> " prompt)
          (codex-ide--style-user-prompt-region start (point))
          (insert "\n\n")
          (let ((codex-ide--current-agent-item-type "agentMessage"))
            (codex-ide--append-agent-text
             (current-buffer)
             (concat (mapconcat (lambda (line) (or line "")) response-lines "\n") "\n"))))))
    (codex-ide--insert-input-prompt session (or active-prompt ""))))

(defun codex-ide-status-mode-test--preview-text (text max-lines)
  "Return transcript preview text for TEXT using MAX-LINES."
  (with-temp-buffer
    (insert text)
    (let ((codex-ide-status-mode-transcript-preview-max-lines max-lines))
      (when-let ((range (codex-ide-status-mode--transcript-preview-range
                         (point-min)
                         (point-max))))
        (buffer-substring-no-properties (car range) (cdr range))))))

(defun codex-ide-status-mode-test--transcript-text (&rest blocks)
  "Return transcript text containing BLOCKS separated like session output."
  (mapconcat
   (lambda (block-lines)
     (concat
      (codex-ide--output-separator-string)
      "\n"
      (mapconcat #'identity block-lines "\n")
      "\n"))
   blocks
   ""))

(defun codex-ide-status-mode-test--face-includes-p (value face)
  "Return non-nil when face VALUE includes FACE."
  (if (listp value)
      (memq face value)
    (eq value face)))

(defun codex-ide-status-mode-test--header-line-string ()
  "Return the current buffer header line as plain text."
  (if (stringp header-line-format)
      (substring-no-properties header-line-format)
    (substring-no-properties (format-mode-line header-line-format))))

(ert-deftest codex-ide-status-transcript-preview-expands-to-full-current-block ()
  (let* ((text (codex-ide-status-mode-test--transcript-text
                '("alpha 1" "alpha 2")
                '("beta 1" "beta 2" "beta 3")))
         (preview (codex-ide-status-mode-test--preview-text text 1)))
    (should (equal preview
                   (concat "beta 1\nbeta 2\nbeta 3")))))

(ert-deftest codex-ide-status-transcript-preview-starts-at-containing-block ()
  (let* ((text (codex-ide-status-mode-test--transcript-text
                '("alpha 1" "alpha 2")
                '("beta 1" "beta 2" "beta 3")
                '("gamma 1" "gamma 2")))
         (preview (codex-ide-status-mode-test--preview-text text 5)))
    (should (equal preview
                   (concat
                    "beta 1\nbeta 2\nbeta 3\n"
                    "------------------------------------------------------------------------\n\n"
                    "gamma 1\ngamma 2")))))

(ert-deftest codex-ide-status-mode-disables-line-wrapping ()
  (with-temp-buffer
    (visual-line-mode 1)
    (codex-ide-status-mode)
    (should truncate-lines)
    (should-not word-wrap)
    (should-not line-move-visual)
    (should-not visual-line-mode)))

(ert-deftest codex-ide-status-renders-project-sections-and-collapsed-entries ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (other-dir (expand-file-name "beta" root-dir))
         (threads `(((id . "thread-alpha")
                     (name . "Summarize\nsession")
                     (createdAt . 10)
                     (updatedAt . 20))
                    ((id . "thread-stored")
                     (preview . "Stored preview")
                     (createdAt . 30)
                     (updatedAt . 40)))))
    (make-directory project-dir t)
    (make-directory other-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (other-session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (let ((default-directory other-dir))
            (setq other-session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-with-response
           session
           '("Earlier prompt" "Explain\nfailure")
           '("Assistant reply"))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "running"
                (codex-ide-session-status other-session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (should (derived-mode-p 'codex-ide-status-mode))
              (should (equal (codex-ide-status-mode-test--header-line-string)
                             "Project: alpha | 2 sessions"))
              (goto-char (point-min))
              (should-not (looking-at-p "^$"))
              (should (or (looking-at-p "Stored  .*  Stored preview")
                          (looking-at-p "Running  .*  Earlier prompt")))
              (should (string-match-p "Running  .*  Earlier prompt" (buffer-string)))
              (should (string-match-p "Stored  .*  Stored preview" (buffer-string)))
              (should-not (string-match-p (regexp-quote (buffer-name (codex-ide-session-buffer other-session)))
                                          (buffer-string)))
              (goto-char (point-min))
              (search-forward "Earlier prompt")
              (beginning-of-line)
              (forward-line 1)
              (should (invisible-p (point)))
              (goto-char (point-min))
              (search-forward "Stored preview")
              (beginning-of-line)
              (forward-line 1)
              (should (invisible-p (point))))))))))

(ert-deftest codex-ide-status-cold-start-creates-query-session-buffer-header-and-prompt ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (requests nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                   (lambda () nil))
                  ((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (push method requests)
                     (pcase method
                       ("initialize" '((ok . t)))
                       (_ (ert-fail (format "Unexpected method %s" method))))))
                  ((symbol-function 'codex-ide--thread-list-data)
                   (lambda (&optional _session _omit-thread-id) nil)))
          (let ((default-directory project-dir))
            (codex-ide-status))
          (should (equal (seq-remove (lambda (method)
                                       (equal method "config/read"))
                                     (nreverse requests))
                         '("initialize")))
          (should (= (length codex-ide--sessions) 1))
          (let ((session (car codex-ide--sessions)))
            (should (codex-ide--input-prompt-active-p session))
            (with-current-buffer (codex-ide-session-buffer session)
              (should (string-match-p
                       (concat "^Codex session for "
                               (regexp-quote
                                 (abbreviate-file-name
                                 (codex-ide--normalize-directory project-dir))))
                       (buffer-string)))
              (should (markerp
                       (codex-ide-session-input-prompt-start-marker session)))
              (should (markerp
                       (codex-ide-session-input-start-marker session))))))))))

(ert-deftest codex-ide-status-keeps-all-sibling-headings-visible-when-entries-are-collapsed ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20))
                    ((id . "thread-beta")
                     (preview . "Beta thread")
                     (createdAt . 30)
                     (updatedAt . 40)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session-a nil)
              (session-b nil))
          (let ((default-directory project-dir))
            (setq session-a (codex-ide--create-process-session))
            (setq session-b (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-prompts
           session-a
           '("First prompt"))
          (codex-ide-status-mode-test--set-session-buffer-prompts
           session-b
           '("Second prompt"))
          (setf (codex-ide-session-thread-id session-a) "thread-alpha"
                (codex-ide-session-thread-id session-b) "thread-beta"
                (codex-ide-session-status session-a) "idle"
                (codex-ide-session-status session-b) "running")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session-a))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (should-not (invisible-p (point)))
              (search-forward "Second prompt")
              (beginning-of-line)
              (should-not (invisible-p (point))))))))))

(ert-deftest codex-ide-status-heading-previews-are-single-line-and-dimmed ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (long-preview "This is a deliberately long preview line that should be truncated in the heading only")
         (threads `(((id . "thread-alpha")
                     (preview . ,long-preview)
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-prompts
           session
           (list long-preview))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (let ((preview (codex-ide-status-mode--preview-line long-preview)))
                (goto-char (point-min))
                (search-forward preview)
                (should-not (eq (get-text-property (match-beginning 0) 'face)
                                'font-lock-doc-face))
                (should-not (string-match-p "\n" preview))
                (should (string-match-p (regexp-quote long-preview)
                                        (buffer-string)))))))))))

(ert-deftest codex-ide-status-buffer-heading-uses-first-submitted-prompt-text ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-prompts
           session
           '("First prompt" "Submitted prompt")
           "I am the real active prompt")
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "First prompt")
              (let ((line-end (line-end-position)))
                (goto-char (line-beginning-position))
                (should-not (search-forward "Submitted prompt" line-end t))
                (goto-char (line-beginning-position))
                (should-not (search-forward "I am the real active prompt" line-end t))))))))))

(ert-deftest codex-ide-status-buffer-expanded-view-shows-last-prompt-and-last-response ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-with-response
           session
           '("First prompt"
             "Explain\nfailure")
           '("Assistant reply"))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "First prompt")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((buffer-section-end (point-max)))
                (should (search-forward "* Buffer:" buffer-section-end t))
                (should (search-forward (buffer-name (codex-ide-session-buffer session))
                                        buffer-section-end
                                        t))
                (should (button-at (match-beginning 0)))
                (should (search-forward "* Number of Prompts: 2" buffer-section-end t))
                (should (search-forward "* Last Prompt: Explain↵failure" buffer-section-end t))
                (should (codex-ide-status-mode-test--face-includes-p
                         (get-text-property (match-beginning 0) 'face)
                         'codex-ide-status-expanded-content-face))
                (should (search-forward "* Last Response: Assistant reply" buffer-section-end t))
                (should (codex-ide-status-mode-test--face-includes-p
                         (get-text-property (match-beginning 0) 'face)
                         'codex-ide-status-expanded-content-face))))))))))

(ert-deftest codex-ide-status-thread-expanded-view-shows-buffer-details-when-linked ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Stored preview")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-with-response
           session
           '("First prompt"
             "Explain\nfailure")
           '("Assistant reply"))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (let ((line-end (line-end-position)))
                (should (search-forward "First prompt" line-end t)))
              (codex-ide-section-toggle-at-point)
              (forward-line 1)
              (should (looking-at-p (regexp-quote "* Thread ID: thread-alpha")))
              (should (search-forward
                       (format "* Created: %s"
                               (codex-ide--format-thread-updated-at 10))
                       nil t))
              (should (search-forward
                       (format "* Updated: %s"
                               (codex-ide--format-thread-updated-at 20))
                       nil t))
              (should (search-forward "* Buffer:" nil t))
              (should (search-forward (buffer-name (codex-ide-session-buffer session)) nil t))
              (should (button-at (match-beginning 0)))
              (should (search-forward "* Number of Prompts: 2" nil t))
              (should (search-forward "* Last Prompt: Explain↵failure" nil t))
              (should (search-forward "* Last Response: Assistant reply" nil t)))))))))

(ert-deftest codex-ide-status-thread-expanded-view-omits-buffer-details-when-unlinked ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-stored")
                     (preview . "Stored preview")
                     (createdAt . 30)
                     (updatedAt . 40)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "Stored preview")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((thread-text
                     (buffer-substring-no-properties
                      (line-beginning-position 2)
                      (save-excursion
                        (forward-line 4)
                        (line-beginning-position)))))
                (should (string-match-p "\\* Thread ID: thread-stored" thread-text))
                (should (string-match-p
                         (regexp-quote
                          (format "* Created: %s"
                                  (codex-ide--format-thread-updated-at 30)))
                         thread-text))
                (should (string-match-p
                         (regexp-quote
                          (format "* Updated: %s"
                                  (codex-ide--format-thread-updated-at 40)))
                         thread-text))
                (should-not (string-match-p "Number of Prompts:" thread-text))
                (should-not (string-match-p "Buffer:" thread-text))
                (should-not (string-match-p "Last Prompt:" thread-text))
                (should-not (string-match-p "Last Response:" thread-text))))))))))

(ert-deftest codex-ide-status-tab-toggles-session-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "Alpha thread")
              (beginning-of-line)
              (should-not (invisible-p (point)))
              (forward-line 1)
              (should (invisible-p (point)))
              (forward-line -1)
              (codex-ide-section-toggle-at-point)
              (forward-line 1)
              (should-not (invisible-p (point)))
              (forward-line -1)
              (codex-ide-section-toggle-at-point)
              (forward-line 1)
              (should (invisible-p (point))))))))))

(ert-deftest codex-ide-status-plus-is-bound-to-start-a-new-session ()
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "+"))
              #'codex-ide)))

(ert-deftest codex-ide-status-mode-exposes-focal-point-navigation-commands ()
  (should (commandp #'codex-ide-status-mode-nav-forward))
  (should (commandp #'codex-ide-status-mode-nav-backward)))

(ert-deftest codex-ide-status-refresh-preserves-expanded-sections ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-with-response
           session
           '("Submitted prompt")
           '("Assistant reply"))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "Submitted prompt")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (codex-ide-status-mode-refresh)
              (goto-char (point-min))
              (search-forward "Submitted prompt")
              (beginning-of-line)
              (forward-line 1)
              (should-not (invisible-p (point))))))))))

(ert-deftest codex-ide-status-refresh-preserves-point-relative-to-section ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (expected-offset nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-with-response
           session
           '("Submitted prompt")
           '("first line" "second line"))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "Submitted prompt")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (search-forward "* Last Response: first line↵second line")
              (goto-char (match-beginning 0))
              (setq expected-offset
                    (- (point)
                       (codex-ide-section-heading-start
                        (codex-ide-status-mode--section-containing-point))))
              (codex-ide-status-mode-refresh)
              (should (equal (codex-ide-section-value
                              (codex-ide-status-mode--section-containing-point))
                             (car threads)))
              (should (= (- (point)
                            (codex-ide-section-heading-start
                             (codex-ide-status-mode--section-containing-point)))
                         expected-offset))
              (should (string-match-p
                       "Last Response: first line↵second line"
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))))))))

(ert-deftest codex-ide-status-ret-visits-thread-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (visited-thread-id nil)
              (visited-directory nil)
              (display-options nil)
              (prepare-count 0))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda ()
                       (setq prepare-count (1+ prepare-count))))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads))
                    ((symbol-function 'codex-ide--show-or-resume-thread)
                     (lambda (thread-id directory)
                       (setq visited-thread-id thread-id
                             visited-directory directory
                             display-options codex-ide-display-buffer-options))))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "thread-a")
              (beginning-of-line)
              (call-interactively #'codex-ide-status-mode-visit-thing-at-point)
              (should (equal visited-thread-id "thread-alpha"))
              (should (equal visited-directory
                             (codex-ide--normalize-directory project-dir)))
              (should (= prepare-count 2))
              (should (equal display-options '(:reuse-buffer-window
                                              :reuse-mode-window))))))))))

(ert-deftest codex-ide-status-delete-removes-thread-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (deleted-thread-id nil)
              (skip-confirmation nil)
              (prepare-count 0))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda ()
                       (setq prepare-count (1+ prepare-count))))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       (if deleted-thread-id nil threads)))
                    ((symbol-function 'codex-ide-delete-session-thread)
                     (lambda (thread-id &optional skip-confirm)
                       (setq deleted-thread-id thread-id
                             skip-confirmation skip-confirm)))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (ert-fail "Unexpected y-or-n-p prompt")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (should (equal prompt
                                      (format "Permanently remove 1 Codex thread from %s? "
                                              (abbreviate-file-name
                                               (codex-ide--codex-home)))))
                       t)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "thread-a")
              (beginning-of-line)
              (call-interactively #'codex-ide-status-mode-delete-thing-at-point)
              (should (equal deleted-thread-id "thread-alpha"))
              (should skip-confirmation)
              (should (= prepare-count 3))
              (should (equal (codex-ide-status-mode-test--header-line-string)
                             "Project: alpha | 0 sessions")))))))))

(ert-deftest codex-ide-status-delete-removes-thread-sections-in-region ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20))
                    ((id . "thread-beta")
                     (preview . "Beta thread")
                     (createdAt . 30)
                     (updatedAt . 40))
                    ((id . "thread-gamma")
                     (preview . "Gamma thread")
                     (createdAt . 50)
                     (updatedAt . 60)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (deleted-thread-ids nil)
              (skip-confirmations nil)
              (prepare-count 0)
              (confirmation-count 0))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda ()
                       (setq prepare-count (1+ prepare-count))))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       (seq-remove
                        (lambda (thread)
                          (member (alist-get 'id thread) deleted-thread-ids))
                        threads)))
                    ((symbol-function 'codex-ide-delete-session-thread)
                     (lambda (thread-id &optional skip-confirm)
                       (setq deleted-thread-ids
                             (append deleted-thread-ids (list thread-id)))
                       (setq skip-confirmations
                             (append skip-confirmations (list skip-confirm)))))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (ert-fail "Unexpected y-or-n-p prompt")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (prompt)
                       (setq confirmation-count (1+ confirmation-count))
                       (should (equal prompt
                                      (format "Permanently remove 2 Codex threads from %s? "
                                              (abbreviate-file-name
                                               (codex-ide--codex-home)))))
                       t)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "Beta thread")
              (beginning-of-line)
              (push-mark (save-excursion
                           (search-forward "Alpha thread")
                           (line-end-position))
                         t
                         t)
              (activate-mark)
              (call-interactively #'codex-ide-status-mode-delete-thing-at-point)
              (should (= confirmation-count 1))
              (should (equal (sort (copy-sequence deleted-thread-ids) #'string-lessp)
                             '("thread-alpha" "thread-beta")))
              (should (equal skip-confirmations '(t t)))
              (should (= prepare-count 3))
              (should (equal (codex-ide-status-mode-test--header-line-string)
                             "Project: alpha | 1 session"))
              (should-not (string-match-p "thread-a" (buffer-string)))
              (should-not (string-match-p "thread-b" (buffer-string)))
              (should (string-match-p "thread-g" (buffer-string))))))))))

(ert-deftest codex-ide-status-actions-reject-parent-sections ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads nil))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (should (equal (codex-ide-status-mode-test--header-line-string)
                             "Project: alpha | 0 sessions"))
              (should-error
               (call-interactively #'codex-ide-status-mode-visit-thing-at-point)
               :type 'user-error)
              (should-error
               (call-interactively #'codex-ide-status-mode-delete-thing-at-point)
               :type 'user-error))))))))

(ert-deftest codex-ide-status-thread-section-hides-context-preview-and-shows-compact-metadata ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (thread-preview (concat "[Emacs prompt context]\n"
                                 "Buffer: sample.el\n"
                                 "[/Emacs prompt context]\n\n"
                                 "Thread preview first line\n"
                                 "Thread preview second line"))
         (threads `(((id . "thread-alpha")
                     (preview . ,thread-preview)
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "idle")
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward
               (codex-ide-status-mode--preview-line
                (codex-ide--thread-choice-preview thread-preview)))
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (should (search-forward "* Thread ID: thread-alpha" nil t))
              (should (codex-ide-status-mode-test--face-includes-p
                       (get-text-property (match-beginning 0) 'face)
                       'codex-ide-status-expanded-content-face))
              (should (search-forward
                       (format "* Created: %s"
                               (codex-ide--format-thread-updated-at 10))
                       nil t))
              (should (codex-ide-status-mode-test--face-includes-p
                       (get-text-property (match-beginning 0) 'face)
                       'codex-ide-status-expanded-content-face))
              (should (search-forward
                       (format "* Updated: %s"
                               (codex-ide--format-thread-updated-at 20))
                       nil t))
              (should (codex-ide-status-mode-test--face-includes-p
                       (get-text-property (match-beginning 0) 'face)
                       'codex-ide-status-expanded-content-face))
              (should-not (search-forward "Last Prompt:" nil t))
              (should-not (search-forward "Last Response:" nil t))
              (should-not (search-forward "Thread preview first line" nil t))
              (should-not (search-forward "Thread preview second line" nil t))
              (should-not (search-forward "Buffer: sample.el" nil t)))))))))

(ert-deftest codex-ide-status-auto-refresh-subscribes-and-filters-session-events ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (other-dir (expand-file-name "beta" root-dir))
         (scheduled-callback nil)
         (scheduled-args nil)
         (refresh-count 0))
    (make-directory project-dir t)
    (make-directory other-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (other-session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (let ((default-directory other-dir))
            (setq other-session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       nil))
                    ((symbol-function 'run-with-idle-timer)
                     (lambda (_delay _repeat function &rest args)
                       (setq scheduled-callback function
                             scheduled-args args)
                       'codex-ide-status-test-timer))
                    ((symbol-function 'timerp)
                     (lambda (object)
                       (eq object 'codex-ide-status-test-timer)))
                    ((symbol-function 'cancel-timer)
                     (lambda (_timer)
                       nil))
                    ((symbol-function 'codex-ide-status-mode-refresh)
                     (lambda (&optional _ignore-auto _noconfirm)
                       (setq refresh-count (1+ refresh-count)))))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (run-hook-with-args 'codex-ide-session-event-hook
                                  'status-changed
                                  other-session
                                  '(:status "running"))
              (should-not scheduled-callback)
              (run-hook-with-args 'codex-ide-session-event-hook
                                  'status-changed
                                  session
                                  '(:status "running"))
              (should (eq scheduled-callback
                          #'codex-ide-status-mode--run-scheduled-refresh))
              (should (equal scheduled-args (list (current-buffer))))
              (run-hook-with-args 'codex-ide-session-event-hook
                                  'status-changed
                                  session
                                  '(:status "idle"))
              (should (= refresh-count 0))
              (apply scheduled-callback scheduled-args)
              (should (= refresh-count 1))
              (should-not codex-ide-status-mode--refresh-timer)
              (let ((listener codex-ide-status-mode--event-listener))
                (kill-buffer (current-buffer))
                (should-not (member listener codex-ide-session-event-hook))))))))))

(provide 'codex-ide-status-mode-tests)

;;; codex-ide-status-mode-tests.el ends here
