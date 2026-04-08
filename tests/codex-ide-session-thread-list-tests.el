;;; codex-ide-session-thread-list-tests.el --- Tests for session thread list -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-session-thread-list'.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-session-thread-list)

(ert-deftest codex-ide-session-thread-list-creates-background-query-session-and-renders-threads ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (requests nil)
        (buffer-name nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                   (lambda () t))
                  ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
                   (lambda () nil))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda () nil))
                  ((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (push method requests)
                     (pcase method
                       ("initialize" '((ok . t)))
                       ("thread/list"
                        '((data . [((id . "thread-12345678")
                                    (createdAt . 1744038896)
                                    (updatedAt . 1744039999)
                                    (preview . "Investigate\nfailure"))])))
                       (_ (ert-fail (format "Unexpected method %s" method)))))))
          (setq buffer-name
                (format "*Codex Threads: %s*"
                        (file-name-nondirectory
                         (directory-file-name project-dir))))
          (codex-ide-session-thread-list)
          (should (= (length codex-ide--sessions) 1))
          (should (equal (nreverse requests) '("initialize" "thread/list")))
          (with-current-buffer buffer-name
            (should (derived-mode-p 'codex-ide-session-thread-list-mode))
            (should (string-match-p "Investigate↵failure" (buffer-string)))
            (should (string-match-p "thread-1" (buffer-string)))))))))

(provide 'codex-ide-session-thread-list-tests)

;;; codex-ide-session-thread-list-tests.el ends here
