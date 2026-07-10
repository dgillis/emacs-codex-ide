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

(ert-deftest codex-ide-config-effective-value-uses-session-buffer-dir-locals ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (class (gensym "codex-ide-test-dir-locals")))
    (dir-locals-set-class-variables
     class
     '((nil . ((codex-ide-model . "gpt-5.4-mini")
               (codex-ide-fast . "on")
               (codex-ide-reasoning-effort . "medium")))))
    (setq project-dir (file-name-as-directory (file-truename project-dir)))
    (dir-locals-set-directory-class project-dir class)
    (should (eq (cadr (dir-locals-find-file project-dir))
                class))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (local-variable-p 'codex-ide-model))
				      (should (local-variable-p 'codex-ide-fast))
				      (should (local-variable-p 'codex-ide-reasoning-effort)))
				    (should (equal (codex-ide-config-effective-value 'model session)
						   "gpt-5.4-mini"))
				    (should (equal (codex-ide-config-effective-value
                                                    'fast
                                                    session)
						   "on"))
				    (should (equal (codex-ide-config-effective-value
                                                    'reasoning-effort
                                                    session)
						   "medium")))))))

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

(ert-deftest codex-ide-config-apply-records-restorable-session-history ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-config-history nil)
        (codex-ide-model "gpt-5.4"))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (codex-ide-config-set-session-value
				     'reasoning-effort "medium" session)
				    (codex-ide-config-apply
				     'reasoning-effort "high" 'this-session session)
				    (should (= (length codex-ide-config-history) 1))
				    (should (equal (codex-ide-config-effective-value
						    'reasoning-effort session)
						   "high"))
				    (codex-ide-config-restore-last)
				    (should-not codex-ide-config-history)
				    (should (equal (codex-ide-config-effective-value
						    'reasoning-effort session)
						   "medium"))
				    (should (equal codex-ide-model "gpt-5.4")))))))

(ert-deftest codex-ide-config-restore-last-restores-all-session-overrides ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-config-history nil)
        (codex-ide-approval-policy "on-request"))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session-a (codex-ide--create-process-session))
					(session-b (codex-ide--create-process-session)))
				    (codex-ide-config-set-session-value
				     'approval-policy "untrusted" session-a)
				    (codex-ide-config-apply
				     'approval-policy "never" 'all-sessions)
				    (should (equal codex-ide-approval-policy "never"))
				    (should (equal (codex-ide-config-effective-value
						    'approval-policy session-a)
						   "never"))
				    (codex-ide-config-restore-last)
				    (should (equal codex-ide-approval-policy "on-request"))
				    (should (equal (codex-ide-config-effective-value
						    'approval-policy session-a)
						   "untrusted"))
				    (should (equal (codex-ide-config-effective-value
						    'approval-policy session-b)
						   "on-request")))))))

(ert-deftest codex-ide-config-apply-preset-records-one-restorable-history-entry ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-config-history nil)
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-approval-policy "on-request"))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session))
					(preset '("Test" . (approval-policy "never"
							    sandbox-mode "read-only"))))
				    (codex-ide-config-apply-preset
				     preset 'this-session session)
				    (should (= (length codex-ide-config-history) 1))
				    (should (equal (codex-ide-config-effective-value
						    'approval-policy session)
						   "never"))
				    (should (equal (codex-ide-config-effective-value
						    'sandbox-mode session)
						   "read-only"))
				    (let ((formatted
					   (codex-ide-config-format-history-entry
					    (car codex-ide-config-history))))
				      (should (string-match-p
					       (regexp-quote "approval policy=never")
					       formatted))
				      (should (string-match-p
					       (regexp-quote "sandbox mode=read-only")
					       formatted))
				      (should-not (string-match-p
						   (regexp-quote "model=")
						   formatted))
				      (should-not (string-match-p
						   (regexp-quote "fast=")
						   formatted))
				      (should-not (string-match-p
						   (regexp-quote "reasoning effort=")
						   formatted))
				      (should-not (string-match-p
						   (regexp-quote "personality=")
						   formatted)))
				    (codex-ide-config-restore-last)
				    (should (equal (codex-ide-config-effective-value
						    'approval-policy session)
						   "on-request")))))))

(ert-deftest codex-ide-config-format-preset-shows-only-explicit-values ()
  (let ((formatted
         (codex-ide-config-format-preset
          '("Safe Review" . (approval-policy "on-request"
                             sandbox-mode "read-only")))))
    (should (equal formatted
                   "Safe Review  approval policy=on-request; sandbox mode=read-only"))
    (should-not (string-match-p (regexp-quote "model=") formatted))
    (should-not (string-match-p (regexp-quote "fast=") formatted))
    (should-not (string-match-p (regexp-quote "reasoning effort=") formatted))
    (should-not (string-match-p (regexp-quote "personality=") formatted))
    (should-not (string-match-p (regexp-quote "=-") formatted))))

(ert-deftest codex-ide-config-format-preset-omits-empty-summary ()
  (should (equal (codex-ide-config-format-preset '("No-op" . nil))
                 "No-op")))

(ert-deftest codex-ide-config-history-group-records-one-entry-for-multiple-applies ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-config-history nil)
        (codex-ide-config--history-group nil)
        (codex-ide-model "gpt-5.4")
        (codex-ide-reasoning-effort "medium")
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-personality "pragmatic"))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
                                         (interaction-id
                                          (encode-time 0 0 12 4 6 2026)))
                                    (should
                                     (equal
                                      (codex-ide-config-begin-history-group
                                       session
                                       interaction-id)
                                      interaction-id))
				    (codex-ide-config-apply
				     'approval-policy "never" 'this-session session)
				    (codex-ide-config-apply
				     'sandbox-mode "read-only" 'this-session session)
				    (should-not codex-ide-config-history)
				    (codex-ide-config-commit-history-group)
				    (should (= (length codex-ide-config-history) 1))
                                    (let* ((entry (car codex-ide-config-history))
                                           (formatted
                                            (codex-ide-config-format-history-entry
                                             entry)))
                                      (should (equal (plist-get entry :time)
                                                     interaction-id))
                                      (should (equal
                                               (plist-get entry :interaction-id)
                                               interaction-id))
                                      (should (eq (plist-get entry :scope)
                                                  'this-session))
                                      (should (eq (plist-get entry :origin)
                                                  'menu-interaction))
                                      (should (equal (plist-get entry :scopes)
                                                     '(this-session)))
				      (should (string-match-p
					       (regexp-quote "this session")
					       formatted))
                                      (should-not (string-match-p
						   (regexp-quote "agent menu")
						   formatted))
				      (should (string-match-p
					       (regexp-quote "approval policy=never")
					       formatted))
				      (should (string-match-p
					       (regexp-quote "sandbox mode=read-only")
					       formatted))
				      (should-not (string-match-p
						   (regexp-quote "model=")
						   formatted))
				      (should-not (string-match-p
						   (regexp-quote "fast=")
						   formatted))
				      (should-not (string-match-p
						   (regexp-quote "reasoning effort=")
						   formatted))
				      (should-not (string-match-p
						   (regexp-quote "personality=")
						   formatted)))
				    (codex-ide-config-restore-last)
				    (should (equal (codex-ide-config-effective-value
						    'approval-policy session)
						   "on-request"))
				    (should (equal (codex-ide-config-effective-value
						    'sandbox-mode session)
						   "workspace-write")))))))

(ert-deftest codex-ide-config-history-group-shows-mixed-scope-for-mixed-menu-applies ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-config-history nil)
        (codex-ide-config--history-group nil)
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write"))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
                                    (codex-ide-config-begin-history-group session)
				    (codex-ide-config-apply
				     'approval-policy "never" 'this-session session)
				    (codex-ide-config-apply
				     'sandbox-mode "read-only" 'all-sessions session)
				    (codex-ide-config-commit-history-group)
                                    (let* ((entry (car codex-ide-config-history))
                                           (formatted
                                            (codex-ide-config-format-history-entry
                                             entry)))
                                      (should (eq (plist-get entry :scope)
                                                  'mixed-scope))
                                      (should (equal (plist-get entry :scopes)
                                                     '(this-session
                                                       all-sessions)))
				      (should (string-match-p
					       (regexp-quote "mixed scope")
					       formatted))
                                      (should-not (string-match-p
						   (regexp-quote "agent menu")
						   formatted))))))))

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

(ert-deftest codex-ide-config-format-value-prompt-preserves-config-default ()
  (let ((codex-ide-reasoning-effort "high"))
    (should
     (equal
      (codex-ide-config-format-value-prompt
       'reasoning-effort "Reasoning effort")
      "Reasoning effort (default = high): "))))

(ert-deftest codex-ide-config-read-value-falls-back-to-known-reasoning-efforts ()
  (let ((collection nil)
        (require-match nil)
        (prompt nil)
        (default :unset)
        (recorded-extra-properties nil))
    (cl-letf (((symbol-function 'codex-ide--reasoning-effort-options)
               (lambda (&rest _) nil))
              ((symbol-function 'completing-read)
               (lambda (captured-prompt choices _predicate match
					&optional _initial-input _hist def)
                 (setq prompt captured-prompt
                       collection choices
                       require-match match
                       default def
                       recorded-extra-properties completion-extra-properties)
                 "medium")))
      (should (equal (codex-ide-config-read-value 'reasoning-effort) "medium")))
    (should (equal prompt
                   "Reasoning effort (default = medium): "))
    (should (equal collection
                   '("none" "minimal" "low" "medium" "high" "xhigh")))
    (should require-match)
    (should-not default)
    (should (eq (plist-get recorded-extra-properties :display-sort-function)
                'identity))
    (should (eq (plist-get recorded-extra-properties :cycle-sort-function)
                'identity))))

(ert-deftest codex-ide-config-read-value-uses-model-reasoning-efforts-and-default ()
  (let ((called nil))
    (cl-letf (((symbol-function 'codex-ide--reasoning-effort-options)
               (lambda (model &optional session)
                 (should (equal model "gpt-5.6-sol"))
                 (should-not session)
                 '(:choices ("low" "medium" "high" "xhigh" "max" "ultra")
			    :default "low")))
              ((symbol-function 'completing-read)
               (lambda (prompt collection predicate require-match
                               &optional initial-input hist def inherit-input-method)
                 (setq called (list prompt collection predicate require-match
                                    initial-input hist def inherit-input-method
                                    completion-extra-properties))
                 def)))
      (should (equal (codex-ide-config-read-value
                      'reasoning-effort nil "gpt-5.6-sol")
                     "low")))
    (should (equal (nth 0 called)
                   "Reasoning effort (default = low): "))
    (should (equal (nth 1 called)
                   '("low" "medium" "high" "xhigh" "max" "ultra")))
    (should (nth 3 called))
    (should (equal (nth 6 called) "low"))
    (should (eq (plist-get (nth 8 called) :display-sort-function)
                'identity))
    (should (eq (plist-get (nth 8 called) :cycle-sort-function)
                'identity))))

(ert-deftest codex-ide-config-read-value-uses-dynamic-choices-for-model ()
  (let ((called nil))
    (cl-letf (((symbol-function 'codex-ide--available-model-names)
               (lambda (&rest _)
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

(ert-deftest codex-ide-config-read-value-uses-static-choices-for-fast ()
  (let ((called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection predicate require-match
                               &optional initial-input hist def inherit-input-method)
                 (setq called (list prompt collection predicate require-match
                                    initial-input hist def inherit-input-method
                                    completion-extra-properties))
                 "on")))
      (should (equal (codex-ide-config-read-value 'fast) "on")))
    (should (equal (nth 0 called)
                   "Fast (default = off): "))
    (should (equal (nth 1 called)
                   '("off" "on")))
    (should (nth 3 called))
    (should-not (nth 6 called))
    (should (eq (plist-get (nth 8 called) :display-sort-function)
                'identity))
    (should (eq (plist-get (nth 8 called) :cycle-sort-function)
                'identity))))

(ert-deftest codex-ide-config-read-value-allows-clearing-model ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda (&rest _)
               '("gpt-5.4" "gpt-5.4-mini")))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "<empty>")))
    (should (equal (codex-ide-config-read-value 'model) ""))))

(ert-deftest codex-ide-config-read-value-allows-custom-model-entry ()
  (cl-letf (((symbol-function 'codex-ide--available-model-names)
             (lambda (&rest _)
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
             (lambda (&rest _) nil))
            ((symbol-function 'read-string)
             (lambda (prompt initial-input)
               (should (equal prompt "Custom model: "))
               (should (equal initial-input ""))
               "manual-model")))
    (should (equal (codex-ide-config-read-value 'model) "manual-model"))))

(ert-deftest codex-ide-set-model-and-reasoning-effort-prompts-in-order-and-applies ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-config-history nil)
        (codex-ide-model "gpt-5.4")
        (codex-ide-reasoning-effort "medium")
        (reads nil)
        (scope-session nil)
        (message-text nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
                                    (with-current-buffer
                                        (codex-ide-session-buffer session)
                                      (cl-letf (((symbol-function
                                                  'codex-ide-config-read-value)
						 (lambda (key &optional _session context)
                                                   (push (list key context) reads)
                                                   (pcase key
                                                     ('model "gpt-5.4-mini")
                                                     ('reasoning-effort "high"))))
                                                ((symbol-function
                                                  'codex-ide-config-read-scope)
						 (lambda (&optional session)
                                                   (setq scope-session session)
                                                   'this-session))
                                                ((symbol-function 'message)
                                                 (lambda (format-string &rest args)
                                                   (setq message-text
                                                         (apply #'format
                                                                format-string
                                                                args)))))
					(codex-ide-set-model-and-reasoning-effort)))
				    (should (equal (nreverse reads)
						   '((model nil)
						     (reasoning-effort "gpt-5.4-mini"))))
                                    (should (eq scope-session session))
                                    (should (equal codex-ide-model "gpt-5.4"))
                                    (should (equal codex-ide-reasoning-effort
                                                   "medium"))
                                    (should (equal
                                             (codex-ide-config-effective-value
                                              'model session)
                                             "gpt-5.4-mini"))
                                    (should (equal
                                             (codex-ide-config-effective-value
                                              'reasoning-effort session)
                                             "high"))
                                    (should (= (length codex-ide-config-history)
                                               1))
                                    (should (string-match-p
                                             (regexp-quote
                                              "model set to gpt-5.4-mini")
                                             message-text))
					    (should (string-match-p
						     (regexp-quote
						      "reasoning effort set to high")
						     message-text)))))))

(ert-deftest codex-ide-set-model-and-reasoning-effort-reuses-model-list ()
  (let ((codex-ide-model "gpt-5.4")
        (codex-ide-reasoning-effort "medium")
        (fetch-count 0))
    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
               (lambda () t))
              ((symbol-function 'codex-ide--cleanup-dead-sessions)
               (lambda () nil))
              ((symbol-function 'codex-ide--ensure-active-buffer-tracking)
               (lambda () nil))
              ((symbol-function 'codex-ide--get-working-directory)
               (lambda () "/tmp/codex-ide-model-picker/"))
              ((symbol-function 'codex-ide--query-session-for-thread-selection)
               (lambda (&optional _directory) 'query-session))
              ((symbol-function 'codex-ide--list-models)
               (lambda (&optional _session)
                 (setq fetch-count (1+ fetch-count))
                 '(((id . "gpt-5.6-sol")
                    (model . "gpt-5.6-sol")
                    (supportedReasoningEfforts
                     . (((reasoningEffort . "low"))
                        ((reasoningEffort . "medium"))
                        ((reasoningEffort . "high"))
                        ((reasoningEffort . "xhigh"))
                        ((reasoningEffort . "max"))
                        ((reasoningEffort . "ultra"))))
                    (defaultReasoningEffort . "low")
                    (isDefault . t)))))
              ((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (if (string-prefix-p "Model " prompt)
                     "gpt-5.6-sol"
                   "low")))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide-set-model-and-reasoning-effort
       nil nil 'future-sessions))
    (should (= fetch-count 1))
    (should (equal codex-ide-model "gpt-5.6-sol"))
    (should (equal codex-ide-reasoning-effort "low"))))

(ert-deftest codex-ide-set-model-and-reasoning-effort-can-clear-model ()
  (let ((codex-ide-model "gpt-5.4")
        (codex-ide-reasoning-effort "medium"))
    (codex-ide-set-model-and-reasoning-effort
     "" "low" 'future-sessions)
    (should-not codex-ide-model)
    (should (equal codex-ide-reasoning-effort "low"))))

(ert-deftest codex-ide-set-model-and-reasoning-effort-can-use-default-model-this-session ()
  (let ((codex-ide-model "gpt-5.4")
        (codex-ide-reasoning-effort "medium")
        (session (make-codex-ide-session)))
    (cl-letf (((symbol-function 'codex-ide-config--refresh-session)
               (lambda (&rest _) nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide-set-model-and-reasoning-effort
       "" "low" 'this-session session))
    (should (codex-ide-config--session-value-bound-p 'model session))
    (should-not (codex-ide-config-effective-value 'model session))
    (should (equal (codex-ide-config-effective-value
                    'reasoning-effort session)
                   "low"))
    (should (equal codex-ide-model "gpt-5.4"))))

(ert-deftest codex-ide-config-clear-session-value-preserves-default-model-override ()
  (let ((codex-ide-model "gpt-5.4")
        (session (make-codex-ide-session)))
    (codex-ide-config-set-session-value 'model nil session)
    (codex-ide-config-set-session-value 'reasoning-effort "high" session)
    (codex-ide-config-clear-session-value 'reasoning-effort session)
    (should (codex-ide-config--session-value-bound-p 'model session))
    (should-not (codex-ide-config-effective-value 'model session))
    (should-not (codex-ide-config--session-value-bound-p
                 'reasoning-effort session))))

(ert-deftest codex-ide-set-model-and-reasoning-effort-uses-default-model-metadata-when-clearing ()
  (let ((codex-ide-model "gpt-5.4")
        (codex-ide-reasoning-effort "medium")
        (reads nil))
    (cl-letf (((symbol-function 'codex-ide-config-read-value)
               (lambda (key &optional _session context)
                 (push (list key context) reads)
                 (pcase key
                   ('model "")
                   ('reasoning-effort "low"))))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide-set-model-and-reasoning-effort
       nil nil 'future-sessions))
    (should (equal (nreverse reads)
                   '((model nil) (reasoning-effort ""))))
    (should-not codex-ide-model)
    (should (equal codex-ide-reasoning-effort "low"))))

(ert-deftest codex-ide-config-applies-to-live-session-p-flags-turn-scoped-settings ()
  (should (codex-ide-config-applies-to-live-session-p 'approval-policy))
  (should (codex-ide-config-applies-to-live-session-p 'sandbox-mode))
  (should (codex-ide-config-applies-to-live-session-p 'fast))
  (should (codex-ide-config-applies-to-live-session-p 'reasoning-effort))
  (should (codex-ide-config-applies-to-live-session-p 'personality)))

(ert-deftest codex-ide-reasoning-effort-safe-local-values-include-max-and-ultra ()
  (let ((predicate (get 'codex-ide-reasoning-effort 'safe-local-variable)))
    (should (funcall predicate "max"))
    (should (funcall predicate "ultra"))))

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
    (codex-ide-config-format-apply-message 'fast "on" 'this-session 1)
    "Codex Fast set to on for this session."))
  (should
   (equal
    (codex-ide-config-format-apply-message 'reasoning-effort "high" 'this-session 1)
    "Codex Reasoning Effort set to high for this session.")))

(ert-deftest codex-ide-thread-start-params-use-session-aware-config ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-model "gpt-5.4")
        (codex-ide-fast "off")
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-personality "pragmatic"))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (codex-ide-config-set-session-value 'model "gpt-5.4-mini" session)
				    (codex-ide-config-set-session-value 'fast "on" session)
				    (codex-ide-config-set-session-value 'approval-policy "never" session)
				    (codex-ide-config-set-session-value 'sandbox-mode "read-only" session)
				    (codex-ide-config-set-session-value 'personality "friendly" session)
				    (should (equal (codex-ide--thread-start-params session)
						   `((cwd . ,(codex-ide--get-working-directory))
						     (approvalPolicy . "never")
						     (sandbox . "read-only")
						     (personality . "friendly")
						     (config
						      (model_reasoning_effort
						       . "medium"))
						     (model . "gpt-5.4-mini")
						     (serviceTier . "priority")))))))))

(ert-deftest codex-ide-submit-uses-session-aware-turn-config ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-model "gpt-5.4")
        (codex-ide-fast "off")
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
				    (codex-ide-config-set-session-value 'fast "on" session)
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
				    (should (equal (alist-get 'serviceTier submitted) "priority"))
				    (should (equal (alist-get 'effort submitted) "high"))
				    (should (equal (alist-get 'personality submitted) "friendly")))))))

(provide 'codex-ide-config-tests)

;;; codex-ide-config-tests.el ends here
