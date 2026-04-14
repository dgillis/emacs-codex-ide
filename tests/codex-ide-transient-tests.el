;;; codex-ide-transient-tests.el --- Tests for codex-ide-transient -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for transient menu behavior and context-sensitive actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide-transient)

(ert-deftest codex-ide-menu-exposes-navigation-and-view-suffixes ()
  (should (transient-get-suffix 'codex-ide-menu "b"))
  (should (transient-get-suffix 'codex-ide-menu "p"))
  (should (transient-get-suffix 'codex-ide-menu "l"))
  (should (transient-get-suffix 'codex-ide-menu "t")))

(ert-deftest codex-ide-config-menu-exposes-reasoning-effort-suffix ()
  (should (transient-get-suffix 'codex-ide-config-menu "R")))

(ert-deftest codex-ide-config-menu-exposes-new-session-split-suffix ()
  (should (transient-get-suffix 'codex-ide-config-menu "w")))

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
        (codex-ide-new-session-split 'vertical)
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol value)
                 (push (cons symbol value) saved))))
      (codex-ide--save-config))
    (should (equal (alist-get 'codex-ide-reasoning-effort saved)
                   "high"))
    (should (eq (alist-get 'codex-ide-new-session-split saved)
                'vertical))))

(ert-deftest codex-ide-set-new-session-split-updates-global-default ()
  (let ((codex-ide-new-session-split nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide--set-new-session-split 'horizontal))
    (should (eq codex-ide-new-session-split 'horizontal))))

(ert-deftest codex-ide-read-model-uses-server-provided-choices ()
  (let ((called nil))
    (cl-letf (((symbol-function 'codex-ide--available-model-names)
               (lambda ()
                 '("gpt-5.4" "gpt-5.4-mini")))
              ((symbol-function 'completing-read)
               (lambda (prompt collection predicate require-match
                        &optional initial-input hist def inherit-input-method)
                 (setq called (list prompt collection predicate require-match
                                    initial-input hist def inherit-input-method))
                 "gpt-5.4")))
      (should (equal (codex-ide--read-model) "gpt-5.4")))
    (should (equal (nth 0 called)
                   "Model (choose or use Other...; empty clears): "))
    (should (equal (nth 1 called)
                   '("gpt-5.4" "gpt-5.4-mini" "<empty>" "Other...")))
    (should-not (nth 3 called))))

(ert-deftest codex-ide-read-model-empty-choice-clears-value ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda ()
               '("gpt-5.4" "gpt-5.4-mini")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "<empty>")))
    (should (equal (codex-ide--read-model) ""))))

(ert-deftest codex-ide-read-model-other-choice-prompts-for-custom-value ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda ()
               '("gpt-5.4" "gpt-5.4-mini")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "Other..."))
            ((symbol-function 'read-string)
             (lambda (prompt initial-input)
               (should (equal prompt "Custom model (leave empty to clear): "))
               (should (equal initial-input ""))
               "my-custom-model")))
    (should (equal (codex-ide--read-model) "my-custom-model"))))

(ert-deftest codex-ide-read-model-falls-back-to-freeform-when-server-list-unavailable ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda () nil))
            ((symbol-function 'read-string)
             (lambda (prompt initial-input)
               (should (equal prompt "Model (leave empty to clear): "))
               (should (equal initial-input ""))
               "manual-model")))
    (should (equal (codex-ide--read-model) "manual-model"))))

(ert-deftest codex-ide-set-model-updates-global-default ()
  (let ((codex-ide-model nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide--set-model "gpt-5.4"))
    (should (equal codex-ide-model "gpt-5.4"))))
(ert-deftest codex-ide-debug-menu-exposes-show-debug-info ()
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-debug-menu "i")) :command)
              #'codex-ide-show-debug-info)))

(provide 'codex-ide-transient-tests)

;;; codex-ide-transient-tests.el ends here
