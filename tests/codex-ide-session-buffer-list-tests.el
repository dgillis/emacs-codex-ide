;;; codex-ide-session-buffer-list-tests.el --- Tests for session buffer list -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-session-buffer-list'.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-session-buffer-list)

(defun codex-ide-test--set-session-buffer-prompts (session prompts &optional active-prompt)
  "Populate SESSION buffer with PROMPTS and optional ACTIVE-PROMPT."
  (with-current-buffer (codex-ide-session-buffer session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (prompt prompts)
        (let ((start (point)))
          (insert "> " prompt)
          (codex-ide--style-user-prompt-region start (point))
          (insert "\n\nassistant\n\n")))
      (let ((start (point)))
        (insert "> " (or active-prompt ""))
        (codex-ide--style-user-prompt-region start (point))))))

(defmacro codex-ide-test-with-empty-thread-list (&rest body)
  "Run BODY with thread metadata lookups stubbed to return no threads."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'codex-ide--thread-list-data)
              (lambda (&optional _session _omit-thread-id)
                nil)))
     ,@body))

(ert-deftest codex-ide-session-buffer-list-renders-live-sessions-across-workspaces ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-a (expand-file-name "alpha" root-dir))
         (project-b (expand-file-name "beta" root-dir)))
    (make-directory project-a t)
    (make-directory project-b t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session-a nil)
              (session-b nil))
          (let ((default-directory project-a))
            (setq session-a (codex-ide--create-process-session)))
          (let ((default-directory project-b))
            (setq session-b (codex-ide--create-process-session)))
          (codex-ide-test--set-session-buffer-prompts
           session-a
           '("Earlier prompt" "Investigate\nfailure")
           "")
          (codex-ide-test--set-session-buffer-prompts
           session-b
           '("Summarize session state"))
          (setf (codex-ide-session-thread-id session-a) "thread-alpha"
                (codex-ide-session-thread-id session-b) "thread-beta"
                (codex-ide-session-status session-a) "idle"
                (codex-ide-session-status session-b) "running")
          (cl-letf (((symbol-function 'codex-ide--thread-list-data)
                     (lambda (session &optional _omit-thread-id)
                       (cond
                        ((equal (codex-ide-session-directory session) project-a)
                         '(((id . "thread-alpha")
                            (updatedAt . "stamp-alpha"))))
                        ((equal (codex-ide-session-directory session) project-b)
                         '(((id . "thread-beta")
                            (updatedAt . "stamp-beta"))))
                        (t nil))))
                    ((symbol-function 'codex-ide--format-thread-updated-at)
                     (lambda (timestamp)
                       (pcase timestamp
                         ("stamp-alpha" "2 minutes ago")
                         ("stamp-beta" "1 hour ago")
                         (_ "")))))
            (codex-ide-session-buffer-list))
          (with-current-buffer "*Codex Session Buffers*"
            (should (derived-mode-p 'codex-ide-session-buffer-list-mode))
            (should hl-line-mode)
            (should (eq hl-line-face 'codex-ide-session-list-current-row-face))
            (should (equal (mapcar #'car (append tabulated-list-format nil))
                           '("Buffer" "Status" "Updated" "Preview")))
            (let* ((entries (codex-ide-test-with-empty-thread-list
                              (funcall tabulated-list-entries)))
                   (first-row (cadr (car entries))))
              (should (eq (get-text-property 0 'face (aref first-row 0))
                          'codex-ide-session-list-primary-face))
              (should (eq (get-text-property 0 'face (aref first-row 1))
                          'codex-ide-session-list-status-face))
              (should (eq (get-text-property 0 'face (aref first-row 3))
                          'codex-ide-session-list-primary-face)))
            (should (string-match-p (regexp-quote (buffer-name (codex-ide-session-buffer session-a)))
                                    (buffer-string)))
            (should (string-match-p "Investigate↵failure" (buffer-string)))
            (should (string-match-p (regexp-quote (buffer-name (codex-ide-session-buffer session-b)))
                                    (buffer-string)))
            (should (string-match-p "Summarize session state" (buffer-string)))
            (should (string-match-p "Idle" (buffer-string)))
            (should (string-match-p "Running" (buffer-string)))))))))

(ert-deftest codex-ide-session-buffer-list-delete-buffer-kills-session-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-a (expand-file-name "alpha" root-dir))
         (project-b (expand-file-name "beta" root-dir)))
    (make-directory project-a t)
    (make-directory project-b t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session-a nil)
              (session-b nil)
              (deleted-session nil)
              (deleted-buffer nil)
              (deleted-buffer-name nil))
          (let ((default-directory project-a))
            (setq session-a (codex-ide--create-process-session)))
          (let ((default-directory project-b))
            (setq session-b (codex-ide--create-process-session)))
          (codex-ide-test-with-empty-thread-list
            (codex-ide-session-buffer-list))
          (with-current-buffer "*Codex Session Buffers*"
            (goto-char (point-min))
            (forward-line 1)
            (setq deleted-session (tabulated-list-get-id)
                  deleted-buffer (codex-ide-session-buffer deleted-session)
                  deleted-buffer-name (buffer-name deleted-buffer))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt)
                         (should (string-match-p
                                  (regexp-quote deleted-buffer-name)
                                  prompt))
                         t)))
              (codex-ide-test-with-empty-thread-list
                (codex-ide-session-buffer-list-delete-buffer)))
            (should-not (buffer-live-p deleted-buffer))
            (should (= (length codex-ide--sessions) 1))
            (should-not (memq deleted-session codex-ide--sessions))
            (should (or (memq session-a codex-ide--sessions)
                        (memq session-b codex-ide--sessions)))
            (should-not (string-match-p (regexp-quote deleted-buffer-name)
                                        (buffer-string)))))))))

(ert-deftest codex-ide-session-buffer-list-delete-buffer-aborts-when-not-confirmed ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir)))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil)
              (buffer nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (setq buffer (codex-ide-session-buffer session))
          (codex-ide-test-with-empty-thread-list
            (codex-ide-session-buffer-list))
          (with-current-buffer "*Codex Session Buffers*"
            (goto-char (point-min))
            (search-forward (buffer-name buffer))
            (beginning-of-line)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (_prompt) nil)))
              (codex-ide-test-with-empty-thread-list
                (codex-ide-session-buffer-list-delete-buffer)))
            (should (buffer-live-p buffer))
            (should (memq session codex-ide--sessions))
            (should (string-match-p (regexp-quote (buffer-name buffer))
                                    (buffer-string)))))))))

(ert-deftest codex-ide-session-buffer-list-delete-buffer-deletes-region-with-one-confirmation ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-a (expand-file-name "alpha" root-dir))
         (project-b (expand-file-name "beta" root-dir))
         (project-c (expand-file-name "gamma" root-dir))
         (confirmation-count 0))
    (make-directory project-a t)
    (make-directory project-b t)
    (make-directory project-c t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session-a nil)
              (session-b nil)
              (session-c nil)
              (selected-sessions nil)
              (selected-buffer-names nil)
              (buffer-a-name nil)
              (buffer-b-name nil)
              (buffer-c-name nil))
          (let ((default-directory project-a))
            (setq session-a (codex-ide--create-process-session)))
          (let ((default-directory project-b))
            (setq session-b (codex-ide--create-process-session)))
          (let ((default-directory project-c))
            (setq session-c (codex-ide--create-process-session)))
          (setq buffer-a-name (buffer-name (codex-ide-session-buffer session-a))
                buffer-b-name (buffer-name (codex-ide-session-buffer session-b))
                buffer-c-name (buffer-name (codex-ide-session-buffer session-c)))
          (codex-ide-test-with-empty-thread-list
            (codex-ide-session-buffer-list))
          (with-current-buffer "*Codex Session Buffers*"
            (goto-char (point-min))
            (forward-line 1)
            (push-mark (line-beginning-position 3) t t)
            (activate-mark)
            (setq selected-sessions
                  (codex-ide-session-list-selected-ids))
            (setq selected-buffer-names
                  (mapcar (lambda (session)
                            (buffer-name (codex-ide-session-buffer session)))
                          selected-sessions))
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt)
                         (setq confirmation-count (1+ confirmation-count))
                         (should (equal prompt "Kill 2 Codex session buffers? "))
                         t)))
              (codex-ide-test-with-empty-thread-list
                (codex-ide-session-buffer-list-delete-buffer)))
            (should (= confirmation-count 1))
            (dolist (session selected-sessions)
              (should-not (buffer-live-p (codex-ide-session-buffer session)))
              (should-not (memq session codex-ide--sessions)))
            (should (= (length codex-ide--sessions) 1))
            (should (buffer-live-p (codex-ide-session-buffer (car codex-ide--sessions))))
            (dolist (buffer-name selected-buffer-names)
              (should-not
               (string-match-p (regexp-quote buffer-name)
                               (buffer-string))))
            (should (or (string-match-p (regexp-quote buffer-a-name) (buffer-string))
                        (string-match-p (regexp-quote buffer-b-name) (buffer-string))
                        (string-match-p (regexp-quote buffer-c-name) (buffer-string))))))))))

(ert-deftest codex-ide-session-buffer-list-redisplay-refreshes-visible-rows ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-a (expand-file-name "alpha" root-dir))
         (project-b (expand-file-name "beta" root-dir))
         (buffer-a-name nil)
         (buffer-b-name nil))
    (make-directory project-a t)
    (make-directory project-b t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session-a nil)
              (session-b nil))
          (let ((default-directory project-a))
            (setq session-a (codex-ide--create-process-session)))
          (setq buffer-a-name (buffer-name (codex-ide-session-buffer session-a)))
          (codex-ide-test--set-session-buffer-prompts session-a '("First prompt"))
          (codex-ide-test-with-empty-thread-list
            (codex-ide-session-buffer-list))
          (let ((default-directory project-b))
            (setq session-b (codex-ide--create-process-session)))
          (setq buffer-b-name (buffer-name (codex-ide-session-buffer session-b)))
          (codex-ide-test--set-session-buffer-prompts session-b '("Second prompt"))
          (with-current-buffer "*Codex Session Buffers*"
            (should (eq (lookup-key codex-ide-session-buffer-list-mode-map (kbd "l"))
                        #'codex-ide-session-buffer-list-redisplay))
            (should-not (string-match-p (regexp-quote buffer-b-name)
                                        (buffer-string)))
            (should-not (string-match-p "Second prompt" (buffer-string)))
            (codex-ide-test-with-empty-thread-list
              (call-interactively #'codex-ide-session-buffer-list-redisplay))
            (should (string-match-p (regexp-quote buffer-a-name)
                                    (buffer-string)))
            (should (string-match-p (regexp-quote buffer-b-name)
                                    (buffer-string)))
            (should (string-match-p "First prompt" (buffer-string)))
            (should (string-match-p "Second prompt" (buffer-string)))))))))

(ert-deftest codex-ide-session-buffer-list-last-prompt-uses-last-non-empty-faced-region ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir)))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-test--set-session-buffer-prompts
           session
           '("First prompt"
             "Multiline\nprompt body")
           "")
          (should (equal (codex-ide-session-buffer-list--last-prompt-text session)
                         "Multiline\nprompt body"))
          (should (equal (codex-ide-session-buffer-list--preview session)
                         "Multiline↵prompt body")))))))

(ert-deftest codex-ide-session-buffer-list-preview-uses-last-submitted-prompt ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir)))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
      (codex-ide-test-with-fake-processes
        (let ((session nil))
          (let ((default-directory project-dir))
            (setq session (codex-ide--create-process-session)))
          (codex-ide-test--set-session-buffer-prompts
           session
           '("First prompt"
             "Later\nprompt body")
           "")
          (should (equal (codex-ide-session-buffer-list--preview session)
                         "Later↵prompt body")))))))

(ert-deftest codex-ide-session-buffer-list-inherits-session-display-bindings ()
  (should (eq (lookup-key codex-ide-session-buffer-list-mode-map (kbd "K"))
              #'codex-ide-session-buffer-list-delete-buffer))
  (should-not (lookup-key codex-ide-session-buffer-list-mode-map (kbd "D")))
  (should (eq (lookup-key codex-ide-session-buffer-list-mode-map (kbd "RET"))
              #'codex-ide-session-list-display-session-at-point))
  (should (eq (lookup-key codex-ide-session-buffer-list-mode-map (kbd "M-<return>"))
              #'codex-ide-session-list-display-session-at-point-other-window))
  (should (eq (lookup-key codex-ide-session-buffer-list-mode-map (kbd "C-M-j"))
              #'codex-ide-session-list-display-session-at-point-other-window)))

(ert-deftest codex-ide-session-list-display-session-at-point-other-window-forwards-pop-up-action ()
  (let ((captured-action nil)
        (session-buffer (generate-new-buffer " *codex-ide-session-list-test*")))
    (unwind-protect
        (let ((session (make-codex-ide-session :buffer session-buffer)))
          (with-temp-buffer
            (codex-ide-session-list-mode)
            (setq-local codex-ide-session-list--visit-function
                        (lambda (_id)
                          (codex-ide--show-session-buffer session)))
            (cl-letf (((symbol-function 'tabulated-list-get-id)
                       (lambda () "session-1"))
                      ((symbol-function 'codex-ide-display-buffer)
                       (lambda (_buffer &optional action)
                         (setq captured-action action)
                         (selected-window)))
                      ((symbol-function 'codex-ide--ensure-input-prompt)
                       (lambda (_session) nil)))
              (call-interactively
               #'codex-ide-session-list-display-session-at-point-other-window))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))
    (should (equal captured-action
                   '((display-buffer-reuse-window
                      display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t))))))

(provide 'codex-ide-session-buffer-list-tests)

;;; codex-ide-session-buffer-list-tests.el ends here
