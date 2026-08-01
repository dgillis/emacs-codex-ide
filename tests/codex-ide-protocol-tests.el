;;; codex-ide-protocol-tests.el --- Tests for protocol helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused ERT coverage for JSON-RPC request/response helpers.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)

(ert-deftest codex-ide-request-sync-preserves-jsonrpc-error-code ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let* ((session (codex-ide--create-process-session))
               (pending (codex-ide-session-pending-requests session)))
          (cl-letf (((symbol-function 'codex-ide--jsonrpc-send)
                     (lambda (_session payload)
                       (funcall
                        (gethash (alist-get 'id payload) pending)
                        nil
                        '((code . -32601)
                          (message . "Invalid request"))))))
            (let ((err (should-error
                        (codex-ide--request-sync session "thread/delete" nil)
                        :type 'error)))
              (should (string-match-p
                       (regexp-quote "-32601")
                       (error-message-string err))))))))))

(provide 'codex-ide-protocol-tests)

;;; codex-ide-protocol-tests.el ends here
