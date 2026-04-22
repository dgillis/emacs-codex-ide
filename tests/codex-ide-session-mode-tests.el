;;; codex-ide-session-mode-tests.el --- Tests for codex-ide session mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-session-mode'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide)
(require 'codex-ide-session-mode)

(ert-deftest codex-ide-session-mode-theme-refresh-subscribes-and-tears-down-hooks ()
  (let ((refresh-count 0)
        (codex-ide-session-mode--theme-refresh-buffers nil)
        (buffer (generate-new-buffer " *codex-ide-session-theme-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'codex-ide-renderer-refresh-theme-faces)
                     (lambda ()
                       (setq refresh-count (1+ refresh-count)))))
            (codex-ide-session-mode)
            (should (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          disable-theme-functions))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (run-hook-with-args 'disable-theme-functions 'test-theme)
            (should (= refresh-count 2))
            (fundamental-mode)
            (should-not (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              disable-theme-functions))
            (codex-ide-session-mode)
            (should (memq (current-buffer) codex-ide-session-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-session-mode--handle-theme-change
                          disable-theme-functions))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (should (= refresh-count 3))
            (kill-buffer buffer)
            (should-not (memq buffer codex-ide-session-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-session-mode--handle-theme-change
                              disable-theme-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'codex-ide-session-mode-tests)

;;; codex-ide-session-mode-tests.el ends here
