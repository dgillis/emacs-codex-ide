;;; codex-ide-bridge-tests.el --- Tests for codex-ide-bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge-specific tests for codex-ide.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-bridge)

(ert-deftest codex-ide-bridge-mcp-config-args-reflect-enabled-settings ()
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
           (equal (codex-ide-bridge-mcp-config-args)
                  '("-c" "mcp_servers.editor.command=\"/usr/bin/python3\""
                    "-c" "mcp_servers.editor.args=[\"/tmp/codex-ide-mcp-server.py\",\"--emacsclient\",\"/usr/bin/emacsclient\",\"--server-name\",\"testsrv\"]"
                    "-c" "mcp_servers.editor.startup_timeout_sec=15"
                    "-c" "mcp_servers.editor.tool_timeout_sec=45"))))))))

(ert-deftest codex-ide-bridge-mcp-config-args-omit-default-server-name ()
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
           (equal (codex-ide-bridge-mcp-config-args)
                  '("-c" "mcp_servers.editor.command=\"/usr/bin/python3\""
                    "-c" "mcp_servers.editor.args=[\"/tmp/codex-ide-mcp-server.py\",\"--emacsclient\",\"/usr/bin/emacsclient\"]"
                    "-c" "mcp_servers.editor.startup_timeout_sec=15"
                    "-c" "mcp_servers.editor.tool_timeout_sec=45"))))))))

(ert-deftest codex-ide-bridge-emacs-all-open-files-lists-file-backed-buffers ()
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
                                  (codex-ide-bridge--tool-call--emacs_all_open_files nil))))
            (should (= (length files) 2))
            (should (equal (mapcar (lambda (item) (alist-get 'file item)) files)
                           (list file-a file-b)))
            (should (equal (alist-get 'major-mode (car files)) "emacs-lisp-mode"))
            (should (alist-get 'modified (cadr files)))))))))

(ert-deftest codex-ide-bridge-emacs-get-diagnostics-returns-empty-when-disabled ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (buffer (get-buffer-create " *codex-ide-diagnostics-none*")))
    (codex-ide-test-with-fixture project-dir
      (with-current-buffer buffer
        (setq-local flymake-mode nil)
        (setq-local flycheck-mode nil)
        (let ((result (codex-ide-bridge--tool-call--emacs_get_diagnostics
                       `((buffer . ,(buffer-name buffer))))))
          (should (equal (alist-get 'buffer result) (buffer-name buffer)))
          (should (equal (alist-get 'diagnostics result) '())))))))

(ert-deftest codex-ide-bridge-emacs-get-diagnostics-prefers-flymake ()
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
            (let* ((result (codex-ide-bridge--tool-call--emacs_get_diagnostics
                            `((buffer . ,(buffer-name buffer)))))
                   (diagnostics (alist-get 'diagnostics result))
                   (diag (car diagnostics)))
              (should (= (length diagnostics) 1))
              (should (equal (alist-get 'source diag) "flymake"))
              (should (equal (alist-get 'message diag) "Example flymake error"))
              (should (equal (alist-get 'severity diag) "warning"))
              (should (= (alist-get 'line diag) 1))
              (should (= (alist-get 'column diag) 1)))))))))

(ert-deftest codex-ide-bridge-emacs-window-list-describes-visible-windows ()
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
                                    (codex-ide-bridge--tool-call--emacs_window_list nil))))
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

(provide 'codex-ide-bridge-tests)

;;; codex-ide-bridge-tests.el ends here
