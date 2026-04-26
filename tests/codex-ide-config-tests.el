;;; codex-ide-config-tests.el --- Tests for codex-ide-config -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for session-aware Codex IDE configuration behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide)
(require 'codex-ide-test-fixtures)

(ert-deftest codex-ide-config-effective-value-prefers-session-overrides ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-sandbox-mode "workspace-write"))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (should (equal (codex-ide-config-effective-value 'sandbox-mode session)
                         "workspace-write"))
          (codex-ide-config-set-session-value 'sandbox-mode "read-only" session)
          (should (equal (codex-ide-config-session-value 'sandbox-mode session)
                         "read-only"))
          (should (equal (codex-ide-config-effective-value 'sandbox-mode session)
                         "read-only"))
          (codex-ide-config-clear-session-value 'sandbox-mode session)
          (should-not (codex-ide-config-session-value 'sandbox-mode session))
          (should (equal (codex-ide-config-effective-value 'sandbox-mode session)
                         "workspace-write")))))))

(ert-deftest codex-ide-config-apply-to-session-keeps-global-defaults ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-model "gpt-5.4")
        (updated nil)
        (events nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--update-header-line)
                     (lambda (arg)
                       (push arg updated))))
            (let ((codex-ide-session-event-hook
                   (list (lambda (event target payload)
                           (push (list event target payload) events)))))
              (should (= (codex-ide-config-apply 'model "gpt-5.4-mini" 'this-session session)
                         1))
              (should (equal codex-ide-model "gpt-5.4"))
              (should (equal (codex-ide-config-effective-value 'model session)
                             "gpt-5.4-mini"))
              (should (equal updated (list session)))
              (should (equal (caar events) 'config-changed))
              (should (eq (cadar events) session)))))))))

(ert-deftest codex-ide-config-apply-to-all-sessions-updates-live-and-future-sessions ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-approval-policy "on-request")
        (updated nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session-a (codex-ide--create-process-session))
              (session-b (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--update-header-line)
                     (lambda (session)
                       (push session updated))))
            (should (= (codex-ide-config-apply 'approval-policy "never" 'all-sessions)
                       2))
            (should (equal codex-ide-approval-policy "never"))
            (should (equal (codex-ide-config-effective-value 'approval-policy session-a)
                           "never"))
            (should (equal (codex-ide-config-effective-value 'approval-policy session-b)
                           "never"))
            (should (= (length updated) 2))))))))

(ert-deftest codex-ide-config-read-scope-uses-future-sessions-when-no-live-sessions ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (should (eq (codex-ide-config-read-scope nil) 'future-sessions)))))

(ert-deftest codex-ide-config-read-scope-prompts-in-session-buffers ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (choice nil)
        (recorded-extra-properties nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (with-current-buffer (codex-ide-session-buffer session)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq recorded-extra-properties completion-extra-properties)
                         (setq choice "This session"))))
              (should (eq (codex-ide-config-read-scope session) 'this-session))
              (should (equal choice "This session"))
              (should (eq (plist-get recorded-extra-properties :display-sort-function)
                          'identity))
              (should (eq (plist-get recorded-extra-properties :cycle-sort-function)
                          'identity)))))))))

(ert-deftest codex-ide-config-read-value-shows-effective-and-default-in-session ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-approval-policy "untrusted")
        (captured-prompt nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (codex-ide-config-set-session-value 'approval-policy "on-request" session)
          (with-current-buffer (codex-ide-session-buffer session)
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt &rest _)
                         (setq captured-prompt prompt)
                         "never")))
              (should (equal (codex-ide-config-read-value 'approval-policy)
                             "never"))))
          (should (equal captured-prompt
                         "Approval policy (effective = on-request, default = untrusted): ")))))))

(ert-deftest codex-ide-config-read-value-requires-listed-choices ()
  (let ((collection nil)
        (require-match nil)
        (prompt nil)
        (default :unset)
        (recorded-extra-properties nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (captured-prompt choices _predicate match
                        &optional _initial-input _hist def)
                 (setq prompt captured-prompt
                       collection choices
                       require-match match
                       default def
                       recorded-extra-properties completion-extra-properties)
                 "")))
      (should (equal (codex-ide-config-read-value 'reasoning-effort) "")))
    (should (equal prompt
                   "Reasoning effort (default = nil): "))
    (should (equal collection
                   '("<empty>" "none" "minimal" "low" "medium" "high" "xhigh")))
    (should require-match)
    (should-not default)
    (should (eq (plist-get recorded-extra-properties :display-sort-function)
                'identity))
    (should (eq (plist-get recorded-extra-properties :cycle-sort-function)
                'identity))))

(ert-deftest codex-ide-config-read-value-uses-dynamic-choices-for-model ()
  (let ((called nil))
    (cl-letf (((symbol-function 'codex-ide--available-model-names)
               (lambda ()
                 '("gpt-5.4" "gpt-5.4-mini")))
              ((symbol-function 'completing-read)
               (lambda (prompt collection predicate require-match
                        &optional initial-input hist def inherit-input-method)
                 (setq called (list prompt collection predicate require-match
                                    initial-input hist def inherit-input-method
                                    completion-extra-properties))
                 "gpt-5.4")))
      (should (equal (codex-ide-config-read-value 'model) "gpt-5.4")))
    (should (equal (nth 0 called)
                   "Model (default = nil): "))
    (should (equal (nth 1 called)
                   '("<empty>" "gpt-5.4" "gpt-5.4-mini" "Other...")))
    (should (nth 3 called))
    (should-not (nth 6 called))
    (should (eq (plist-get (nth 8 called) :display-sort-function)
                'identity))
    (should (eq (plist-get (nth 8 called) :cycle-sort-function)
                'identity))))

(ert-deftest codex-ide-config-read-value-allows-clearing-model ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda ()
               '("gpt-5.4" "gpt-5.4-mini")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "<empty>")))
    (should (equal (codex-ide-config-read-value 'model) ""))))

(ert-deftest codex-ide-config-read-value-allows-custom-model-entry ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda ()
               '("gpt-5.4" "gpt-5.4-mini")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "Other..."))
            ((symbol-function 'read-string)
             (lambda (prompt initial-input)
               (should (equal prompt "Custom model: "))
               (should (equal initial-input ""))
               "my-custom-model")))
    (should (equal (codex-ide-config-read-value 'model) "my-custom-model"))))

(ert-deftest codex-ide-config-read-value-falls-back-to-freeform-when-model-list-unavailable ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda () nil))
            ((symbol-function 'read-string)
             (lambda (prompt initial-input)
               (should (equal prompt "Custom model: "))
               (should (equal initial-input ""))
               "manual-model")))
    (should (equal (codex-ide-config-read-value 'model) "manual-model"))))

(ert-deftest codex-ide-config-applies-to-live-session-p-flags-turn-scoped-settings ()
  (should (codex-ide-config-applies-to-live-session-p 'approval-policy))
  (should (codex-ide-config-applies-to-live-session-p 'sandbox-mode))
  (should (codex-ide-config-applies-to-live-session-p 'reasoning-effort))
  (should (codex-ide-config-applies-to-live-session-p 'personality)))

(ert-deftest codex-ide-config-format-apply-message-omits-live-session-restart-note-for-turn-scoped-settings ()
  (should
   (equal
    (codex-ide-config-format-apply-message 'approval-policy "never" 'this-session 1)
    "Codex Approval Policy set to never for this session."))
  (should
   (equal
    (codex-ide-config-format-apply-message 'sandbox-mode "read-only" 'all-sessions 2)
    "Codex Sandbox Mode set to read-only for 2 live sessions and future sessions."))
  (should
   (equal
    (codex-ide-config-format-apply-message 'personality "friendly" 'this-session 1)
    "Codex Personality set to friendly for this session."))
  (should
   (equal
    (codex-ide-config-format-apply-message 'personality "friendly" 'all-sessions 2)
    "Codex Personality set to friendly for 2 live sessions and future sessions."))
  (should
   (equal
    (codex-ide-config-format-apply-message 'reasoning-effort "high" 'this-session 1)
    "Codex Reasoning Effort set to high for this session.")))

(ert-deftest codex-ide-thread-start-params-use-session-aware-config ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-model "gpt-5.4")
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-personality "pragmatic"))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (codex-ide-config-set-session-value 'model "gpt-5.4-mini" session)
          (codex-ide-config-set-session-value 'approval-policy "never" session)
          (codex-ide-config-set-session-value 'sandbox-mode "read-only" session)
          (codex-ide-config-set-session-value 'personality "friendly" session)
          (should (equal (codex-ide--thread-start-params session)
                         `((cwd . ,(codex-ide--get-working-directory))
                           (approvalPolicy . "never")
                           (sandbox . "read-only")
                           (personality . "friendly")
                           (model . "gpt-5.4-mini")))))))))

(ert-deftest codex-ide-submit-uses-session-aware-turn-config ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-model "gpt-5.4")
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-reasoning-effort "medium")
        (codex-ide-personality "pragmatic")
        (submitted nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (setf (codex-ide-session-thread-id session) "thread-config-1")
          (codex-ide-config-set-session-value 'approval-policy "never" session)
          (codex-ide-config-set-session-value 'sandbox-mode "read-only" session)
          (codex-ide-config-set-session-value 'model "gpt-5.4-mini" session)
          (codex-ide-config-set-session-value 'reasoning-effort "high" session)
          (codex-ide-config-set-session-value 'personality "friendly" session)
          (with-current-buffer (codex-ide-session-buffer session)
            (codex-ide--insert-input-prompt session "Explain this")
            (cl-letf (((symbol-function 'codex-ide--request-sync)
                       (lambda (_session _method params)
                         (setq submitted params)
                         nil)))
              (codex-ide--submit-prompt)))
          (should (equal (alist-get 'approvalPolicy submitted) "never"))
          (should (equal (alist-get 'sandboxPolicy submitted)
                         '((type . "readOnly"))))
          (should (equal (alist-get 'model submitted) "gpt-5.4-mini"))
          (should (equal (alist-get 'effort submitted) "high"))
          (should (equal (alist-get 'personality submitted) "friendly")))))))

(provide 'codex-ide-config-tests)

;;; codex-ide-config-tests.el ends here
