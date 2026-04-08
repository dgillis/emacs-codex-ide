;;; codex-ide-session-buffer-list-tests.el --- Tests for session buffer list -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-session-buffer-list'.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-session-buffer-list)

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
          (setf (codex-ide-session-thread-id session-a) "thread-alpha"
                (codex-ide-session-thread-id session-b) "thread-beta"
                (codex-ide-session-status session-a) "idle"
                (codex-ide-session-status session-b) "running")
          (codex-ide-session-buffer-list)
          (with-current-buffer "*Codex Session Buffers*"
            (should (derived-mode-p 'codex-ide-session-buffer-list-mode))
            (should (string-match-p (regexp-quote (buffer-name (codex-ide-session-buffer session-a)))
                                    (buffer-string)))
            (should (string-match-p (regexp-quote (abbreviate-file-name project-a))
                                    (buffer-string)))
            (should (string-match-p (regexp-quote (buffer-name (codex-ide-session-buffer session-b)))
                                    (buffer-string)))
            (should (string-match-p (regexp-quote (abbreviate-file-name project-b))
                                    (buffer-string)))))))))

(provide 'codex-ide-session-buffer-list-tests)

;;; codex-ide-session-buffer-list-tests.el ends here
