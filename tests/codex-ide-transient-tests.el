;;; codex-ide-transient-tests.el --- Tests for codex-ide-transient -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for transient menu behavior and context-sensitive actions.

;;; Code:

(require 'ert)
(require 'codex-ide-transient)

(ert-deftest codex-ide-menu-exposes-navigation-and-view-suffixes ()
  (should (transient-get-suffix 'codex-ide-menu "b"))
  (should (transient-get-suffix 'codex-ide-menu "p"))
  (should (transient-get-suffix 'codex-ide-menu "l"))
  (should (transient-get-suffix 'codex-ide-menu "t")))

(ert-deftest codex-ide-config-menu-exposes-reasoning-effort-suffix ()
  (should (transient-get-suffix 'codex-ide-config-menu "R")))

(ert-deftest codex-ide-menu-session-suffixes-use-current-commands ()
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "s")) :command)
              #'codex-ide))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "c")) :command)
              #'codex-ide-continue))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "r")) :command)
              #'codex-ide-reset-current-session))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "p")) :command)
              #'codex-ide-prompt)))

(ert-deftest codex-ide-save-config-persists-reasoning-effort ()
  (let ((codex-ide-reasoning-effort "high")
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol value)
                 (push (cons symbol value) saved))))
      (codex-ide--save-config))
    (should (equal (alist-get 'codex-ide-reasoning-effort saved)
                   "high"))))

(provide 'codex-ide-transient-tests)

;;; codex-ide-transient-tests.el ends here
