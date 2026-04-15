;;; codex-ide-status-mode-tests.el --- Tests for codex-ide status mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-status-mode'.

;;; Code:

(require 'ert)
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
            (let ((default-directory project-dir)
                  (codex-ide-status-mode-preview-max-width 120))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (should (derived-mode-p 'codex-ide-status-mode))
              (should (string-match-p
                       (concat "^Project: "
                               (regexp-quote
                                (abbreviate-file-name
                                 (codex-ide--normalize-directory project-dir)))
                               "$")
                       (buffer-string)))
              (should (string-match-p "Buffers (1)" (buffer-string)))
              (should (string-match-p "Threads (2)" (buffer-string)))
              (should (string-match-p
                       (regexp-quote
                        (format "Running  %s  Explain↵failure"
                                (buffer-name (codex-ide-session-buffer session))))
                       (buffer-string)))
              (should (string-match-p "Stored  thread-s  Stored preview" (buffer-string)))
              (should-not (string-match-p (regexp-quote (buffer-name (codex-ide-session-buffer other-session)))
                                          (buffer-string)))
              (goto-char (point-min))
              (search-forward
               (format "Running  %s  Explain↵failure"
                       (buffer-name (codex-ide-session-buffer session))))
              (beginning-of-line)
              (forward-line 1)
              (should (invisible-p (point)))
              (goto-char (point-min))
              (search-forward "Stored  thread-s  Stored preview")
              (beginning-of-line)
              (forward-line 1)
              (should (invisible-p (point))))))))))

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
            (let ((default-directory project-dir)
                  (codex-ide-status-mode-preview-max-width 120))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "Buffers (2)")
              (forward-line 1)
              (should-not (invisible-p (point)))
              (search-forward "Second prompt")
              (beginning-of-line)
              (should-not (invisible-p (point)))
              (search-forward "Threads (2)")
              (beginning-of-line)
              (should-not (invisible-p (point)))
              (forward-line 1)
              (should-not (invisible-p (point)))
              (search-forward "Beta thread")
              (beginning-of-line)
              (should-not (invisible-p (point))))))))))

(ert-deftest codex-ide-status-heading-previews-are-truncated-and-dimmed ()
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
            (let ((default-directory project-dir)
                  (codex-ide-status-mode-preview-max-width 24))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (let ((truncated-preview
                     (let ((codex-ide-status-mode-preview-max-width 24))
                       (codex-ide-status-mode--truncate-preview long-preview))))
                (goto-char (point-min))
                (search-forward (buffer-name (codex-ide-session-buffer session)))
                (let ((line-end (line-end-position)))
                  (search-forward truncated-preview line-end t)
                  (search-backward (buffer-name (codex-ide-session-buffer session)) (line-beginning-position) t)
                  (should (eq (get-text-property (point) 'face) 'shadow)))
                (goto-char (point-min))
                (search-forward "thread-a")
                (let ((line-end (line-end-position)))
                  (search-forward truncated-preview line-end t)
                  (search-backward "thread-a" (line-beginning-position) t)
                  (should (not (null (get-text-property (point) 'face)))))
                (should-not (string-match-p (regexp-quote long-preview)
                                            (buffer-string)))))))))))

(ert-deftest codex-ide-status-buffer-heading-uses-last-submitted-prompt-text ()
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
           '("Submitted prompt")
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
            (let ((default-directory project-dir)
                  (codex-ide-status-mode-preview-max-width 120))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward (buffer-name (codex-ide-session-buffer session)))
              (let ((line-end (line-end-position)))
                (should (search-forward "Submitted prompt" line-end t))
                (goto-char (line-beginning-position))
                (should-not (search-forward "I am the real active prompt" line-end t))))))))))

(ert-deftest codex-ide-status-tab-toggles-section-at-point ()
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
              (search-forward "Buffers (1)")
              (beginning-of-line)
              (forward-line 1)
              (should-not (invisible-p (point)))
              (goto-char (line-beginning-position 0))
              (codex-ide-section-toggle-at-point)
              (forward-line 1)
              (should (invisible-p (point)))
              (goto-char (line-beginning-position 0))
              (codex-ide-section-toggle-at-point)
              (forward-line 1)
              (should-not (invisible-p (point))))))))))

(ert-deftest codex-ide-status-ret-visits-buffer-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads nil))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (visited nil)
              (display-options nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-prompts
           session
           '("Submitted prompt"))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads))
                    ((symbol-function 'codex-ide--show-session-buffer)
                     (lambda (arg)
                       (setq visited arg
                             display-options codex-ide-display-buffer-options))))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward (buffer-name (codex-ide-session-buffer session)))
              (beginning-of-line)
              (call-interactively #'codex-ide-status-mode-visit-thing-at-point)
              (should (eq visited session))
              (should (equal display-options '(:reuse-buffer-window
                                              :reuse-mode-window))))))))))

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

(ert-deftest codex-ide-status-delete-kills-buffer-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads nil))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-status-mode-test--set-session-buffer-prompts
           session
           '("Submitted prompt"))
          (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                     (lambda () nil))
                    ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
                     (lambda (_directory) session))
                    ((symbol-function 'codex-ide--thread-list-data)
                     (lambda (&optional _session _omit-thread-id)
                       threads))
                    ((symbol-function 'y-or-n-p)
                     (lambda (_prompt) t)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward (buffer-name (codex-ide-session-buffer session)))
              (beginning-of-line)
              (call-interactively #'codex-ide-status-mode-delete-thing-at-point)
              (should-not (buffer-live-p (codex-ide-session-buffer session)))
              (should-not (string-match-p
                           (regexp-quote "Buffers (1)")
                           (buffer-string)))
              (should (string-match-p "Buffers (0)" (buffer-string))))))))))

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
                     (lambda (thread-id)
                       (setq deleted-thread-id thread-id)))
                    ((symbol-function 'y-or-n-p)
                     (lambda (_prompt) t)))
            (let ((default-directory project-dir))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "thread-a")
              (beginning-of-line)
              (call-interactively #'codex-ide-status-mode-delete-thing-at-point)
              (should (equal deleted-thread-id "thread-alpha"))
              (should (= prepare-count 3))
              (should-not (string-match-p "Threads (1)" (buffer-string)))
              (should (string-match-p "Threads (0)" (buffer-string))))))))))

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
              (search-forward "Buffers (1)")
              (beginning-of-line)
              (should-error
               (call-interactively #'codex-ide-status-mode-visit-thing-at-point)
               :type 'user-error)
              (should-error
               (call-interactively #'codex-ide-status-mode-delete-thing-at-point)
               :type 'user-error))))))))

(ert-deftest codex-ide-status-buffer-section-renders-last-agent-output-block-only ()
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
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (let ((first-start (point)))
                (insert "> First prompt")
                (codex-ide--style-user-prompt-region first-start (point))
                (insert "\n\n")
                (let ((codex-ide--current-agent-item-type "agentMessage"))
                  (codex-ide--append-output-separator (current-buffer))
                  (codex-ide--append-agent-text (current-buffer) "\nold response\n")))
              (let ((second-start (point)))
                (insert "> Latest prompt")
                (codex-ide--style-user-prompt-region second-start (point))
                (insert "\n\n")
                (let ((codex-ide--current-agent-item-type "agentMessage"))
                  (codex-ide--append-output-separator (current-buffer))
                  (codex-ide--append-agent-text (current-buffer) "\nolder block in latest reply\n")
                  (codex-ide--append-output-separator (current-buffer))
                  (codex-ide--append-agent-text (current-buffer) "\nlatest response\n")))))
          (codex-ide--insert-input-prompt session "draft follow-up")
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
              (search-forward "Idle  *codex[alpha]*")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((buffer-section-end (save-excursion
                                          (search-forward "Threads (1)")
                                          (match-beginning 0))))
                (should (search-forward "  latest response" buffer-section-end t))
                (should (eq (get-text-property (match-beginning 0) 'face) 'shadow))
                (goto-char (line-beginning-position))
                (should-not (search-forward
                             (string-trim-right (codex-ide--output-separator-string))
                             buffer-section-end t))
                (should-not (search-forward "Latest prompt" buffer-section-end t))
                (should-not (search-forward "draft follow-up" buffer-section-end t))
                (should-not (search-forward "First prompt" buffer-section-end t))
                (should-not (search-forward "old response" buffer-section-end t))
                (should-not (search-forward "older block in latest reply" buffer-section-end t))
                (goto-char (line-beginning-position))
                (should-not (search-forward "Buffer:" buffer-section-end t))))))))))

(ert-deftest codex-ide-status-buffer-section-renders-in-progress-agent-output ()
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
           '("Earlier prompt")
           '("completed response")
           "Current prompt")
          (codex-ide--begin-turn-display session)
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((codex-ide--current-agent-item-type "agentMessage"))
              (codex-ide--append-agent-text
               (current-buffer)
               "in-progress preface\n")
              (codex-ide--append-output-separator (current-buffer))
              (codex-ide--append-agent-text
               (current-buffer)
               "\nin-progress response\n")))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "running")
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
              (search-forward "Running  *codex[alpha]*")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((buffer-section-end (save-excursion
                                          (search-forward "Threads (1)")
                                          (match-beginning 0))))
                (should (search-forward "  in-progress response" buffer-section-end t))
                (goto-char (line-beginning-position))
                (should-not (search-forward "in-progress preface" buffer-section-end t))
                (goto-char (line-beginning-position))
                (should-not (search-forward "completed response" buffer-section-end t))
                (should-not (search-forward "Current prompt" buffer-section-end t))))))))))

(ert-deftest codex-ide-status-buffer-section-renders-first-in-progress-agent-output ()
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
           nil
           "First prompt")
          (codex-ide--begin-turn-display session)
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((codex-ide--current-agent-item-type "agentMessage"))
              (codex-ide--append-output-separator (current-buffer))
              (codex-ide--append-agent-text
               (current-buffer)
               "\nfirst live response\n")))
          (setf (codex-ide-session-thread-id session) "thread-alpha"
                (codex-ide-session-status session) "running")
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
              (search-forward "Running  *codex[alpha]*")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((buffer-section-end (save-excursion
                                          (search-forward "Threads (1)")
                                          (match-beginning 0))))
                (should (search-forward "  first live response" buffer-section-end t))
                (goto-char (line-beginning-position))
                (should-not (search-forward "First prompt" buffer-section-end t))))))))))

(ert-deftest codex-ide-status-buffer-section-renders-approval-block ()
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
           nil
           "Approval prompt")
          (codex-ide--begin-turn-display session)
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((codex-ide--current-agent-item-type "agentMessage"))
              (codex-ide--append-agent-text
               (current-buffer)
               "Need approval before I continue.\n")))
          (codex-ide--render-item-start
           session
           '((id . "cmd-1")
             (type . "commandExecution")
             (callId . "call-1")
             (command . "/bin/zsh -lc \"printf foo\"")
             (cwd . "/tmp")))
          (codex-ide--render-buffer-approval
           session
           42
           'command
           "[Approval required]"
           '((:kind command :text "/bin/zsh -lc \"printf foo\"")
             (:label "Reason" :text "Need to write a file"))
           '(("accept" . "accept")
             ("decline" . "decline"))
           '((command . "/bin/zsh -lc \"printf foo\"")))
          (setf (codex-ide-session-thread-id session) "thread-alpha")
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
              (search-forward "Approval  *codex[alpha]*")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((buffer-section-end (save-excursion
                                          (search-forward "Threads (1)")
                                          (match-beginning 0))))
                (should (search-forward "  [Approval required]" buffer-section-end t))
                (should (search-forward "  Run the following command?" buffer-section-end t))
                (should (search-forward "      /bin/zsh -lc \"printf foo\"" buffer-section-end t))
                (should (search-forward "  Reason: Need to write a file" buffer-section-end t))
                (should (search-forward "  [accept]" buffer-section-end t))
                (goto-char (line-beginning-position))
                (should-not (search-forward "Need approval before I continue." buffer-section-end t))
                (should-not (search-forward "* Ran /bin/zsh -lc \"printf foo\"" buffer-section-end t))
                (should-not (search-forward "cwd: /tmp" buffer-section-end t))))))))))

(ert-deftest codex-ide-status-thread-section-renders-dimmed-metadata-and-full-preview ()
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
            (let ((default-directory project-dir)
                  (codex-ide-status-mode-preview-max-width 24))
              (codex-ide-status))
            (with-current-buffer "codex-ide: alpha"
              (goto-char (point-min))
              (search-forward "Threads (1)")
              (search-forward "thread-a")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (should (search-forward "└ Thread: thread-alpha" nil t))
              (should (eq (get-text-property (match-beginning 0) 'face) 'shadow))
              (should (search-forward "└ Buffer: *codex[alpha]*" nil t))
              (should (eq (get-text-property (match-beginning 0) 'face) 'shadow))
              (should (search-forward
                       (format "└ Created: %s"
                               (codex-ide--format-thread-updated-at 10))
                       nil t))
              (should (eq (get-text-property (match-beginning 0) 'face) 'shadow))
              (should (search-forward
                       (format "└ Updated: %s"
                               (codex-ide--format-thread-updated-at 20))
                       nil t))
              (should (eq (get-text-property (match-beginning 0) 'face) 'shadow))
              (should-not (search-forward "Preview:" nil t))
              (should-not (search-forward "Thread preview first line" nil t))
              (should-not (search-forward "Thread preview second line" nil t))
              (should-not (search-forward "Buffer: sample.el" nil t)))))))))

(ert-deftest codex-ide-status-buffer-section-preserves-source-faces ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (response-lines (list (propertize "highlighted line"
                                           'face 'font-lock-keyword-face)))
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
           '("Prompt")
           response-lines)
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
              (search-forward "Idle  *codex[alpha]*")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (search-forward "  highlighted line")
              (should (eq (get-text-property (match-beginning 0) 'face) 'shadow))
              (should (eq (get-text-property (+ (match-beginning 0) 2) 'face)
                          'font-lock-keyword-face)))))))))

(ert-deftest codex-ide-status-buffer-section-renders-nothing-without-agent-reply ()
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
           nil
           "Later prompt")
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
              (search-forward "Idle  *codex[alpha]*")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((buffer-section-end (save-excursion
                                          (search-forward "Threads (1)")
                                          (match-beginning 0))))
                (should-not (search-forward "└ Preview:" buffer-section-end t))))))))))

(ert-deftest codex-ide-status-buffer-section-renders-nothing-when-empty ()
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
          (codex-ide-status-mode-test--set-session-buffer-prompts session nil "")
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
              (search-forward "Idle  *codex[alpha]*")
              (beginning-of-line)
              (codex-ide-section-toggle-at-point)
              (let ((buffer-section-end (save-excursion
                                          (search-forward "Threads (1)")
                                          (match-beginning 0))))
                (should-not (search-forward "└ Last Prompt:" buffer-section-end t))
                (should-not (search-forward "└ Preview:" buffer-section-end t))))))))))

(provide 'codex-ide-status-mode-tests)

;;; codex-ide-status-mode-tests.el ends here
