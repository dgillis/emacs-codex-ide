;;; codex-ide-bridge-tests.el --- Tests for codex-ide-bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge-specific tests for codex-ide.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide-bridge)

(ert-deftest codex-ide-bridge-mcp-config-args-reflect-enabled-settings ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-enable-emacs-tool-bridge t)
            (codex-ide-emacs-tool-bridge-name "editor")
            (codex-ide-emacs-bridge-python-command "python3")
            (codex-ide-emacs-bridge-emacsclient-command "emacsclient")
            (codex-ide-emacs-bridge-script-path "/tmp/codex-ide-mcp.py")
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
                    "-c" "mcp_servers.editor.args=[\"/tmp/codex-ide-mcp.py\",\"--emacsclient\",\"/usr/bin/emacsclient\",\"--server-name\",\"testsrv\"]"
                    "-c" "mcp_servers.editor.startup_timeout_sec=15"
                    "-c" "mcp_servers.editor.tool_timeout_sec=45"))))))))

(ert-deftest codex-ide-bridge-mcp-config-args-omit-default-server-name ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-enable-emacs-tool-bridge t)
            (codex-ide-emacs-tool-bridge-name "editor")
            (codex-ide-emacs-bridge-python-command "python3")
            (codex-ide-emacs-bridge-emacsclient-command "emacsclient")
            (codex-ide-emacs-bridge-script-path "/tmp/codex-ide-mcp.py")
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
                    "-c" "mcp_servers.editor.args=[\"/tmp/codex-ide-mcp.py\",\"--emacsclient\",\"/usr/bin/emacsclient\"]"
                    "-c" "mcp_servers.editor.startup_timeout_sec=15"
                    "-c" "mcp_servers.editor.tool_timeout_sec=45"))))))))

(provide 'codex-ide-bridge-tests)

;;; codex-ide-bridge-tests.el ends here
