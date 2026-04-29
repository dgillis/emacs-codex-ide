;;; codex-ide-transient-tests.el --- Tests for codex-ide-transient -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for transient menu behavior and context-sensitive actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide)
(require 'codex-ide-test-fixtures)
(require 'codex-ide-transient)

(ert-deftest codex-ide-menu-exposes-navigation-and-view-suffixes ()
  (should (transient-get-suffix 'codex-ide-menu "b"))
  (should (transient-get-suffix 'codex-ide-menu "p"))
  (should (transient-get-suffix 'codex-ide-menu "l"))
  (should-error (transient-get-suffix 'codex-ide-menu "t")))

(ert-deftest codex-ide-config-menu-exposes-reasoning-effort-suffix ()
  (should (transient-get-suffix 'codex-ide-config-menu "r")))

(ert-deftest codex-ide-config-menu-exposes-agent-setting-suffixes-with-lowercase-mnemonics ()
  (should (transient-get-suffix 'codex-ide-config-menu "p"))
  (should (transient-get-suffix 'codex-ide-config-menu "s")))

(ert-deftest codex-ide-config-menu-exposes-new-session-split-suffix ()
  (should (transient-get-suffix 'codex-ide-config-menu "w")))

(ert-deftest codex-ide-config-menu-does-not-expose-focus-on-open-suffix ()
  (should-error (transient-get-suffix 'codex-ide-config-menu "f")))

(ert-deftest codex-ide-config-menu-exposes-running-submit-action-suffix ()
  (should (transient-get-suffix 'codex-ide-config-menu "u")))

(ert-deftest codex-ide-config-menu-exposes-save-suffix ()
  (should (transient-get-suffix 'codex-ide-config-menu "S")))

(ert-deftest codex-ide-config-menu-groups-codex-ide-settings-under-one-column ()
  (let* ((layout (plist-get (symbol-plist 'codex-ide-config-menu) 'transient--layout))
         (columns (aref (car layout) 3))
         (descriptions (mapcar (lambda (column)
                                 (plist-get (aref column 2) :description))
                               columns))
         (codex-ide-column (seq-find (lambda (column)
                                       (equal (plist-get (aref column 2) :description)
                                              "Codex-IDE"))
                                     columns))
         (keys (mapcar (lambda (suffix)
                         (plist-get (nth 2 suffix) :key))
                       (aref codex-ide-column 3))))
    (should (equal descriptions '("Agent" "Codex-IDE" "Save")))
    (should (equal keys '("c" "x" "e" "A" "u" "w")))))

(ert-deftest codex-ide-config-menu-setting-suffixes-exit-after-applying ()
  (dolist (command '(codex-ide--set-cli-path
                     codex-ide--set-model
                     codex-ide--set-reasoning-effort
                     codex-ide--set-running-submit-action
                     codex-ide--set-cli-extra-flags
                     codex-ide--set-approval-policy
                     codex-ide--set-personality
                     codex-ide--set-sandbox-mode
                     codex-ide--set-new-session-split
                     codex-ide--toggle-emacs-tool-bridge
                     codex-ide--toggle-emacs-bridge-approval))
    (let ((obj (transient-suffix-object command)))
      (should obj)
      (should (slot-boundp obj 'transient))
      (should-not (oref obj transient)))))

(ert-deftest codex-ide-config-menu-save-suffix-exits-after-applying ()
  (should-not (plist-get (nth 2 (transient-get-suffix 'codex-ide-config-menu "S"))
                         :transient)))

(ert-deftest codex-ide-menu-session-suffixes-use-current-commands ()
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "s")) :command)
              #'codex-ide))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "c")) :command)
              #'codex-ide-continue))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "r")) :command)
              #'codex-ide-reset-current-session))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "p")) :command)
              #'codex-ide-prompt))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "S")) :command)
              #'codex-ide-steer))
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-menu "Q")) :command)
              #'codex-ide-queue)))

(ert-deftest codex-ide-save-config-persists-reasoning-effort ()
  (let ((codex-ide-reasoning-effort "high")
        (codex-ide-new-session-split 'vertical)
        (codex-ide-running-submit-action 'queue)
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol value)
                 (push (cons symbol value) saved))))
      (codex-ide--save-config))
    (should (equal (alist-get 'codex-ide-reasoning-effort saved)
                   "high"))
    (should (eq (alist-get 'codex-ide-new-session-split saved)
                'vertical))
    (should (eq (alist-get 'codex-ide-running-submit-action saved)
                'queue))))

(ert-deftest codex-ide-set-new-session-split-updates-global-default ()
  (let ((codex-ide-new-session-split nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide--set-new-session-split 'horizontal))
    (should (eq codex-ide-new-session-split 'horizontal))))

(ert-deftest codex-ide-set-running-submit-action-updates-global-default ()
  (let ((codex-ide-running-submit-action 'steer))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide--set-running-submit-action 'queue))
    (should (eq codex-ide-running-submit-action 'queue))))

(ert-deftest codex-ide-set-model-updates-global-default ()
  (let ((codex-ide-model nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide--set-model "gpt-5.4"))
    (should (equal codex-ide-model "gpt-5.4"))))

(ert-deftest codex-ide-set-sandbox-mode-can-target-current-session ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-sandbox-mode "workspace-write"))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (cl-letf (((symbol-function 'completing-read)
						 (lambda (prompt collection &rest _)
						   (cond
						    ((equal prompt "Apply to: ") "This session")
						    ((equal prompt "Sandbox mode: ") "read-only")
						    (t (car collection)))))
						((symbol-function 'message)
						 (lambda (&rest _) nil)))
					(call-interactively #'codex-ide--set-sandbox-mode)))
				    (should (equal codex-ide-sandbox-mode "workspace-write"))
				    (should (equal (codex-ide-config-effective-value 'sandbox-mode session)
						   "read-only")))))))

(ert-deftest codex-ide-set-approval-policy-signals-quit-when-called-directly ()
  (let ((applied nil))
    (cl-letf (((symbol-function 'codex-ide-config-read-value)
               (lambda (&rest _)
                 (signal 'quit nil)))
              ((symbol-function 'codex-ide-config-apply-interactively)
               (lambda (&rest _)
                 (setq applied t))))
      (should
       (eq (condition-case nil
               (progn
                 (call-interactively #'codex-ide--set-approval-policy)
                 :no-quit)
             (quit :quit))
           :quit)))
    (should-not applied)))

(ert-deftest codex-ide-set-model-signals-quit-when-called-directly ()
  (let ((applied nil))
    (cl-letf (((symbol-function 'codex-ide-config-read-value)
               (lambda (&rest _)
                 (signal 'quit nil)))
              ((symbol-function 'codex-ide-config-apply-interactively)
               (lambda (&rest _)
                 (setq applied t))))
      (should
       (eq (condition-case nil
               (progn
                 (call-interactively #'codex-ide--set-model)
                 :no-quit)
             (quit :quit))
           :quit)))
    (should-not applied)))

(ert-deftest codex-ide-debug-menu-exposes-show-debug-info ()
  (should (eq (plist-get (nth 2 (transient-get-suffix 'codex-ide-debug-menu "i")) :command)
              #'codex-ide-show-debug-info)))

(provide 'codex-ide-transient-tests)

;;; codex-ide-transient-tests.el ends here
