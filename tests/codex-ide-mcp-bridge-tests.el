;;; codex-ide-mcp-bridge-tests.el --- Tests for codex-ide-mcp-bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge-specific tests for codex-ide.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-mcp-bridge)

(ert-deftest codex-ide-mcp-bridge-enabled-p-respects-want-setting ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-enable-emacs-tool-bridge t)
            (codex-ide-want-mcp-bridge nil))
        (should-not (codex-ide-mcp-bridge-enabled-p)))
      (let ((codex-ide-enable-emacs-tool-bridge nil)
            (codex-ide-want-mcp-bridge t))
        (should (codex-ide-mcp-bridge-enabled-p)))
      (let ((codex-ide-enable-emacs-tool-bridge nil)
            (codex-ide-want-mcp-bridge 'prompt))
        (should-not (codex-ide-mcp-bridge-enabled-p)))
      (let ((codex-ide-enable-emacs-tool-bridge t)
            (codex-ide-want-mcp-bridge 'prompt))
        (should (codex-ide-mcp-bridge-enabled-p))))))

(ert-deftest codex-ide-mcp-bridge-prompt-to-enable-respects-want-setting ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((prompted nil)
            (ensured nil)
            (codex-ide-enable-emacs-tool-bridge t)
            (codex-ide-want-mcp-bridge nil))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (setq prompted t)
                     t))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda ()
                     (setq ensured t))))
          (codex-ide-mcp-bridge-prompt-to-enable)
          (should-not prompted)
          (should-not ensured)))
      (let ((prompted nil)
            (ensured nil)
            (codex-ide-enable-emacs-tool-bridge nil)
            (codex-ide-want-mcp-bridge t))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (setq prompted t)
                     t))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda ()
                     (setq ensured t))))
          (codex-ide-mcp-bridge-prompt-to-enable)
          (should-not prompted)
          (should ensured)
          (should codex-ide-enable-emacs-tool-bridge)))
      (let ((prompted nil)
            (ensured nil)
            (codex-ide-enable-emacs-tool-bridge nil)
            (codex-ide-want-mcp-bridge 'prompt))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (setq prompted t)
                     t))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda ()
                     (setq ensured t))))
          (codex-ide-mcp-bridge-prompt-to-enable)
          (should prompted)
          (should ensured)
          (should codex-ide-enable-emacs-tool-bridge)))
      (let ((prompted nil)
            (ensured nil)
            (codex-ide-enable-emacs-tool-bridge nil)
            (codex-ide-want-mcp-bridge 'prompt))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (setq prompted t)
                     nil))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda ()
                     (setq ensured t))))
          (codex-ide-mcp-bridge-prompt-to-enable)
          (should prompted)
          (should-not ensured)
          (should-not codex-ide-enable-emacs-tool-bridge))))))

(ert-deftest codex-ide-mcp-bridge-mcp-config-args-reflect-enabled-settings ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-enable-emacs-tool-bridge t)
            (codex-ide-emacs-tool-bridge-name "editor")
            (codex-ide-emacs-bridge-python-command "python3")
            (codex-ide-emacs-bridge-emacsclient-command "emacsclient")
            (codex-ide-emacs-bridge-script-path "/tmp/codex-ide-mcp-server.py")
            (codex-ide-emacs-bridge-server-name "testsrv")
            (codex-ide-emacs-bridge-startup-timeout 15)
            (codex-ide-emacs-bridge-tool-timeout 45))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (command)
                     (pcase command
                       ("python3" "/usr/bin/python3")
                       ("emacsclient" "/usr/bin/emacsclient")
                       (_ nil)))))
          (should
           (equal (codex-ide-mcp-bridge-mcp-config-args)
                  '("-c" "mcp_servers.editor.command=\"/usr/bin/python3\""
                    "-c" "mcp_servers.editor.args=[\"/tmp/codex-ide-mcp-server.py\",\"--emacsclient\",\"/usr/bin/emacsclient\",\"--server-name\",\"testsrv\"]"
                    "-c" "mcp_servers.editor.startup_timeout_sec=15"
                    "-c" "mcp_servers.editor.tool_timeout_sec=45"))))))))

(ert-deftest codex-ide-mcp-bridge-mcp-config-args-omit-default-server-name ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-enable-emacs-tool-bridge t)
            (codex-ide-emacs-tool-bridge-name "editor")
            (codex-ide-emacs-bridge-python-command "python3")
            (codex-ide-emacs-bridge-emacsclient-command "emacsclient")
            (codex-ide-emacs-bridge-script-path "/tmp/codex-ide-mcp-server.py")
            (codex-ide-emacs-bridge-server-name nil)
            (codex-ide-emacs-bridge-startup-timeout 15)
            (codex-ide-emacs-bridge-tool-timeout 45))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (command)
                     (pcase command
                       ("python3" "/usr/bin/python3")
                       ("emacsclient" "/usr/bin/emacsclient")
                       (_ nil)))))
          (should
           (equal (codex-ide-mcp-bridge-mcp-config-args)
                  '("-c" "mcp_servers.editor.command=\"/usr/bin/python3\""
                    "-c" "mcp_servers.editor.args=[\"/tmp/codex-ide-mcp-server.py\",\"--emacsclient\",\"/usr/bin/emacsclient\"]"
                    "-c" "mcp_servers.editor.startup_timeout_sec=15"
                    "-c" "mcp_servers.editor.tool_timeout_sec=45"))))))))

(ert-deftest codex-ide-mcp-bridge-request-exempt-from-approval-matches-bridge-tool-payload ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-emacs-tool-bridge-name "editor")
            (codex-ide-emacs-bridge-require-approval nil))
        (should
         (codex-ide-mcp-bridge-request-exempt-from-approval-p
          '((serverName . "editor")
            (message . "Allow the editor MCP server to run tool \"get_diagnostics\"?"))))
        (should-not
         (codex-ide-mcp-bridge-request-exempt-from-approval-p
          '((serverName . "another-server")
            (message . "Allow another server to run search_web"))))))))

(ert-deftest codex-ide-mcp-bridge-request-exempt-from-approval-respects-require-approval ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-emacs-tool-bridge-name "editor")
            (codex-ide-emacs-bridge-require-approval t))
        (should-not
         (codex-ide-mcp-bridge-request-exempt-from-approval-p
          '((serverName . "editor")
            (message . "Allow the editor MCP server to run tool \"get_diagnostics\"?"))))))))

(ert-deftest codex-ide-mcp-bridge-request-exempt-from-approval-ignores-shell-requests-from-emacs-paths ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-emacs-tool-bridge-name "emacs")
            (codex-ide-emacs-bridge-require-approval nil))
        (should-not
         (codex-ide-mcp-bridge-request-exempt-from-approval-p
          '((threadId . "019d93c2-1e36-7e00-a71d-760a9b2fdba5")
            (turnId . "019d93c3-5bc0-7910-b092-50b4a99fec66")
            (itemId . "call_KZFC1pPstKLWXaXy0AiDcVvZ")
            (reason . "Do you want me to overwrite your ~/.zshrc with random text as requested?")
            (command . "/bin/zsh -lc \"printf '%s\n' 'J8vQ2mLp7Xc9rNt4Hb6K' > ~/.zshrc\"")
            (cwd . "/Users/dgillis/.emacs.d/lib/local/codex-ide")
            (commandActions ((type . "unknown")
                             (command . "printf '%s\n' 'J8vQ2mLp7Xc9rNt4Hb6K' > ~/.zshrc")))
            (proposedExecpolicyAmendment "/bin/zsh"
                                         "-lc"
                                         "printf '%s\n' 'J8vQ2mLp7Xc9rNt4Hb6K' > ~/.zshrc")
            (availableDecisions "accept"
                                ((acceptWithExecpolicyAmendment
                                  (execpolicy_amendment "/bin/zsh"
                                                        "-lc"
                                                        "printf '%s\n' 'J8vQ2mLp7Xc9rNt4Hb6K' > ~/.zshrc")))
                                "cancel"))))))))

(ert-deftest codex-ide-mcp-bridge-permissions-approval-still-prompts-for-bridge-requests ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (message-text nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((codex-ide-emacs-tool-bridge-name "editor")
              (codex-ide-emacs-bridge-require-approval nil)
              (session (codex-ide--create-process-session)))
          (setf (codex-ide-session-current-turn-id session) "turn-bridge-permissions"
                (codex-ide-session-status session) "running")
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_time _repeat function)
                       (funcall function)))
                    ((symbol-function 'codex-ide-log-message)
                     (lambda (&rest _args) nil))
                    ((symbol-function 'codex-ide-display-buffer)
                     (lambda (_buffer &optional _action) (selected-window)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-text (apply #'format format-string args)))))
            (codex-ide--handle-permissions-approval
             session
             17
             '((serverName . "editor")
               (reason . "Allow MCP server editor to run get_diagnostics")
               (permissions . (((tool . "get_diagnostics"))
                               ((server . "editor")))))))
          (should (string= (codex-ide-session-status session) "approval"))
          (should (= (hash-table-count (codex-ide--pending-approvals session)) 1))
          (should (equal message-text
                         (format "Codex approval required in %s"
                                 (buffer-name (codex-ide-session-buffer session)))))
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((text (buffer-string)))
              (should (string-match-p "\\[Approval required\\]" text))
              (should (string-match-p "Reason: Allow MCP server editor to run get_diagnostics"
                                      text))
              (should (string-match-p "Permissions: (((tool \\. \"get_diagnostics\")) ((server \\. \"editor\")))"
                                      text)))))))))

(ert-deftest codex-ide-mcp-bridge-elicitation-auto-accepts-bridge-approval-prompts ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (response nil)
        (handler-called nil))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-emacs-tool-bridge-name "editor")
            (codex-ide-emacs-bridge-require-approval nil))
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_time _repeat function)
                     (funcall function)))
                  ((symbol-function 'codex-ide-log-message)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'codex-ide--jsonrpc-send-response)
                   (lambda (_session id payload)
                     (setq response (list id payload))))
                  ((symbol-function 'codex-ide-mcp-elicitation-handle-request)
                   (lambda (_params)
                     (setq handler-called t)
                     '((action . "decline")))))
          (codex-ide--handle-elicitation-request
           nil
           18
           '((serverName . "editor")
             (message . "Allow the editor MCP server to run tool \"get_diagnostics\"?")
             (mode . "form")))
          (should-not handler-called)
          (should (equal response '(18 ((action . "accept"))))))))))

(ert-deftest codex-ide-mcp-bridge-get-all-open-file-buffers-lists-file-backed-buffers ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-a (codex-ide-test--make-project-file project-dir "a.el" "(message \"a\")\n"))
         (file-b (codex-ide-test--make-project-file project-dir "b.el" "(message \"b\")\n")))
    (codex-ide-test-with-fixture project-dir
      (let ((buffer-a (find-file-noselect file-a))
            (buffer-b (find-file-noselect file-b)))
        (with-current-buffer buffer-b
          (set-buffer-modified-p t))
        (with-temp-buffer
          (let ((files (alist-get 'files
                                  (codex-ide-mcp-bridge--tool-call--get_all_open_file_buffers nil))))
            (should (equal (alist-get 'major-mode
                                      (seq-find (lambda (item)
                                                  (equal (alist-get 'file item) file-a))
                                                files))
                           "emacs-lisp-mode"))
            (should (member file-a (mapcar (lambda (item) (alist-get 'file item)) files)))
            (should (member file-b (mapcar (lambda (item) (alist-get 'file item)) files)))
            (should (alist-get 'modified
                               (seq-find (lambda (item)
                                           (equal (alist-get 'file item) file-b))
                                         files)))))))))

(ert-deftest codex-ide-mcp-bridge-ensure-file-buffer-open-does-not-display-buffer ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file project-dir "ensure.el" "(message \"ensure\")\n")))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (let* ((starting-buffer (window-buffer (selected-window)))
               (buffer (find-buffer-visiting file-path)))
          (when buffer
            (kill-buffer buffer))
          (let ((result (codex-ide-mcp-bridge--tool-call--ensure_file_buffer_open
                         `((path . ,file-path)))))
            (should-not (alist-get 'already-open result))
            (should (equal (alist-get 'path result) file-path))
            (should (find-buffer-visiting file-path))
            (should (eq (window-buffer (selected-window)) starting-buffer))))))))

(ert-deftest codex-ide-mcp-bridge-view-file-buffer-uses-non-selected-window ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-a (codex-ide-test--make-project-file project-dir "one.el" "(message \"one\")\n"))
         (file-b (codex-ide-test--make-project-file project-dir "two.el" "(message \"two\")\n")))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) (find-file-noselect file-a))
        (let* ((origin (selected-window))
               (split-width-threshold 0)
               (split-height-threshold 0)
               (result (codex-ide-mcp-bridge--tool-call--view_file_buffer
                        `((path . ,file-b)
                          (line . 1)
                          (column . 2))))
               (other-windows (seq-remove (lambda (window)
                                            (eq window origin))
                                          (window-list (selected-frame) 'no-minibuf origin)))
               (target (car other-windows)))
          (should (eq (selected-window) origin))
          (should (= (length other-windows) 1))
          (should target)
          (should (equal (alist-get 'window-id result) (format "%s" target)))
          (should (equal (buffer-file-name (window-buffer target)) file-b))
          (should (= (with-selected-window target (current-column)) 1)))))))

(ert-deftest codex-ide-mcp-bridge-kill-file-buffer-kills-visiting-buffer ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file project-dir "kill.el" "(message \"kill\")\n")))
    (codex-ide-test-with-fixture project-dir
      (let* ((buffer (find-file-noselect file-path))
             (killed-buffer nil)
             (result nil))
        (cl-letf (((symbol-function 'kill-buffer)
                   (lambda (target)
                     (setq killed-buffer target)
                     t)))
          (setq result (codex-ide-mcp-bridge--tool-call--kill_file_buffer
                        `((path . ,file-path)))))
        (should (eq killed-buffer buffer))
        (should (equal (alist-get 'buffer result) (buffer-name buffer)))
        (should (alist-get 'killed result))))))

(ert-deftest codex-ide-mcp-bridge-lisp-check-parens-returns-success-when-balanced ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir
                     "balanced.el"
                     "(defun balanced ()\n  (list 1 2 3))\n")))
    (codex-ide-test-with-fixture project-dir
      (let ((result (codex-ide-mcp-bridge--tool-call--lisp_check_parens
                     `((path . ,file-path)))))
        (should (equal (alist-get 'path result) file-path))
        (should (alist-get 'balanced result))
        (should-not (eq (alist-get 'mismatch result) t))
        (should-not (alist-get 'line result))
        (should-not (alist-get 'column result))))))

(ert-deftest codex-ide-mcp-bridge-lisp-check-parens-reports-mismatch-location ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (contents "(defun broken ()\n  (list 1 2 3]\n")
         (file-path (codex-ide-test--make-project-file project-dir "broken.el" contents)))
    (codex-ide-test-with-fixture project-dir
      (let ((result (codex-ide-mcp-bridge--tool-call--lisp_check_parens
                     `((path . ,file-path)))))
        (should-not (eq (alist-get 'balanced result) t))
        (should (alist-get 'mismatch result))
        (should (= (alist-get 'line result) 1))
        (should (= (alist-get 'column result) 1))
        (should (= (alist-get 'point result) 1))
        (should (equal (alist-get 'message result) "Unmatched bracket or quote"))))))

(ert-deftest codex-ide-mcp-bridge-lisp-check-parens-uses-live-buffer-contents ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir
                     "live.el"
                     "(defun live ()\n  (list 1 2 3))\n")))
    (codex-ide-test-with-fixture project-dir
      (let ((buffer (find-file-noselect file-path)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (delete-char -2)
          (set-buffer-modified-p t))
        (let ((result (codex-ide-mcp-bridge--tool-call--lisp_check_parens
                       `((path . ,file-path)))))
          (should-not (eq (alist-get 'balanced result) t))
          (should (alist-get 'mismatch result))
          (should (= (alist-get 'line result) 1))
          (should (= (alist-get 'column result) 1))
          (should (= (alist-get 'point result) 1)))))))

(ert-deftest codex-ide-mcp-bridge-get-diagnostics-returns-empty-when-disabled ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (buffer (get-buffer-create " *codex-ide-diagnostics-none*")))
    (codex-ide-test-with-fixture project-dir
      (with-current-buffer buffer
        (setq-local flymake-mode nil)
        (setq-local flycheck-mode nil)
        (let ((result (codex-ide-mcp-bridge--tool-call--get_diagnostics
                       `((buffer . ,(buffer-name buffer))))))
          (should (equal (alist-get 'buffer result) (buffer-name buffer)))
          (should (equal (alist-get 'diagnostics result) '())))))))

(ert-deftest codex-ide-mcp-bridge-get-diagnostics-prefers-flymake ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (buffer (get-buffer-create " *codex-ide-diagnostics-flymake*")))
    (codex-ide-test-with-fixture project-dir
      (with-current-buffer buffer
        (erase-buffer)
        (insert "hello world\n")
        (let ((fake-diag 'fake-flymake))
          (setq-local flymake-mode t)
          (cl-letf (((symbol-function 'flymake-diagnostics)
                     (lambda () (list fake-diag)))
                    ((symbol-function 'flymake-diagnostic-text)
                     (lambda (_diag) "Example flymake error"))
                    ((symbol-function 'flymake-diagnostic-type)
                     (lambda (_diag) 'warning))
                    ((symbol-function 'flymake-diagnostic-beg)
                     (lambda (_diag) 1))
                    ((symbol-function 'flymake-diagnostic-end)
                     (lambda (_diag) 6)))
            (let* ((result (codex-ide-mcp-bridge--tool-call--get_diagnostics
                            `((buffer . ,(buffer-name buffer)))))
                   (diagnostics (alist-get 'diagnostics result))
                   (diag (car diagnostics)))
              (should (= (length diagnostics) 1))
              (should (equal (alist-get 'source diag) "flymake"))
              (should (equal (alist-get 'message diag) "Example flymake error"))
              (should (equal (alist-get 'severity diag) "warning"))
              (should (= (alist-get 'line diag) 1))
              (should (= (alist-get 'column diag) 1)))))))))

(ert-deftest codex-ide-mcp-bridge-get-window-list-describes-visible-windows ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-a (codex-ide-test--make-project-file project-dir "one.el" "(message \"one\")\n"))
         (file-b (codex-ide-test--make-project-file project-dir "two.el" "(message \"two\")\n")))
    (codex-ide-test-with-fixture project-dir
      (let ((buffer-a (find-file-noselect file-a))
            (buffer-b (find-file-noselect file-b)))
        (delete-other-windows)
        (set-window-buffer (selected-window) buffer-a)
        (let ((other-window (split-window-right)))
          (set-window-buffer other-window buffer-b)
          (let ((windows (alist-get 'windows
                                    (codex-ide-mcp-bridge--tool-call--get_window_list nil))))
            (should (= (length windows) 2))
            (should (equal (alist-get 'buffer
                                      (alist-get 'buffer-info (car windows)))
                           (buffer-name buffer-a)))
            (should (equal (alist-get 'file
                                      (alist-get 'buffer-info (cadr windows)))
                           file-b))
            (should (equal (alist-get 'major-mode
                                      (alist-get 'buffer-info (car windows)))
                           "emacs-lisp-mode"))
            (should (listp (alist-get 'edges (car windows))))))))))

(provide 'codex-ide-mcp-bridge-tests)

;;; codex-ide-mcp-bridge-tests.el ends here
