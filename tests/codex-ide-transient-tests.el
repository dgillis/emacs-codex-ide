;;; codex-ide-transient-tests.el --- Tests for codex-ide-transient -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for transient menu behavior and context-sensitive actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide)
(require 'codex-ide-test-fixtures)
(require 'codex-ide-transient)

(defun codex-ide-test--transient-suffix-prop (suffix prop)
  "Return PROP from SUFFIX across transient's serialized forms."
  (cond
   ((and (consp suffix) (eq (car suffix) 'transient-suffix))
    (plist-get (cdr suffix) prop))
   ((and (consp suffix) (listp (nth 2 suffix)))
    (plist-get (nth 2 suffix) prop))))

(defun codex-ide-test--transient-layout-node-p (value)
  "Return non-nil when VALUE is a transient layout node."
  (codex-ide-test--transient-node-type value))

(defun codex-ide-test--transient-node-type (value)
  "Return VALUE's transient layout node type, if any."
  (seq-some (lambda (part)
              (when (memq part '(transient-columns transient-column transient-suffix))
                part))
            (cond
             ((vectorp value) (append value nil))
             ((consp value) value))))

(defun codex-ide-test--plist-p (value)
  "Return non-nil when VALUE looks like a plist."
  (and (listp value) (keywordp (car-safe value))))

(defun codex-ide-test--transient-node-props (node)
  "Return NODE's property list across transient layout shapes."
  (seq-find #'codex-ide-test--plist-p
            (if (vectorp node) (append node nil) node)))

(defun codex-ide-test--transient-node-children (node)
  "Return NODE's child layout nodes across transient layout shapes."
  (seq-find (lambda (value)
              (and (listp value)
                   (seq-some #'codex-ide-test--transient-layout-node-p value)))
            (if (vectorp node) (append node nil) node)))

(defun codex-ide-test--transient-layout-root (symbol)
  "Return SYMBOL's root transient layout node."
  (let ((layout (plist-get (symbol-plist symbol) 'transient--layout)))
    (if (vectorp layout)
        layout
      (car layout))))

(defun codex-ide-test--transient-suffix-object (prefix key)
  "Return PREFIX's generated transient suffix object for KEY."
  (seq-find (lambda (obj)
              (equal (oref obj key) key))
            (transient-suffixes prefix)))

(ert-deftest codex-ide-menu-exposes-navigation-and-view-suffixes ()
  (should (transient-get-suffix 'codex-ide-menu "b"))
  (should (transient-get-suffix 'codex-ide-menu "p"))
  (should (transient-get-suffix 'codex-ide-menu "i"))
  (should (transient-get-suffix 'codex-ide-menu "I"))
  (should (transient-get-suffix 'codex-ide-menu "l"))
  (should (equal (codex-ide-test--transient-suffix-prop
                  (transient-get-suffix 'codex-ide-menu "D")
                  :description)
                 "Session diff (live/transcript/pinned)"))
  (should (eq (codex-ide-test--transient-suffix-prop
               (transient-get-suffix 'codex-ide-menu "C-g")
               :command)
              #'transient-quit-all))
  (should (equal (codex-ide-test--transient-suffix-prop
                  (transient-get-suffix 'codex-ide-menu "C-g")
                  :description)
                 "Exit"))
  (should-error (transient-get-suffix 'codex-ide-menu "q"))
  (should-error (transient-get-suffix 'codex-ide-menu "t")))

(ert-deftest codex-ide-nested-menus-expose-back-and-exit-suffixes ()
  (dolist (menu '(codex-ide-agent-config-menu
                  codex-ide-debug-menu))
    (should (eq (codex-ide-test--transient-suffix-prop
                 (transient-get-suffix menu "DEL")
                 :command)
                #'transient-quit-one))
    (should (equal (codex-ide-test--transient-suffix-prop
                    (transient-get-suffix menu "DEL")
                    :description)
                   "Back"))
    (should (eq (codex-ide-test--transient-suffix-prop
                 (transient-get-suffix menu "C-g")
                 :command)
                #'transient-quit-all))
    (should (equal (codex-ide-test--transient-suffix-prop
                    (transient-get-suffix menu "C-g")
                    :description)
                   "Exit"))))

(ert-deftest codex-ide-agent-config-menu-exposes-reasoning-effort-suffix ()
  (should (transient-get-suffix 'codex-ide-agent-config-menu "r")))

(ert-deftest codex-ide-agent-config-menu-exposes-agent-setting-suffixes-with-lowercase-mnemonics ()
  (should (transient-get-suffix 'codex-ide-agent-config-menu "p"))
  (should (transient-get-suffix 'codex-ide-agent-config-menu "f"))
  (should (transient-get-suffix 'codex-ide-agent-config-menu "s")))

(ert-deftest codex-ide-agent-config-menu-omits-non-agent-suffixes ()
  (dolist (key '("c" "t" "x" "e" "A" "u" "U" "R" "w" "S"))
    (should-error (transient-get-suffix 'codex-ide-agent-config-menu key))))

(ert-deftest codex-ide-agent-config-menu-does-not-expose-uppercase-focus-suffix ()
  (should-error (transient-get-suffix 'codex-ide-agent-config-menu "F")))

(ert-deftest codex-ide-agent-config-menu-exposes-history-suffixes ()
  (should (transient-get-suffix 'codex-ide-agent-config-menu "h"))
  (should (equal (codex-ide-test--transient-suffix-prop
                  (transient-get-suffix 'codex-ide-agent-config-menu "h")
                  :description)
                 "Config history")))

(ert-deftest codex-ide-agent-config-menu-exposes-direct-preset-suffixes ()
  (let ((codex-ide-config-presets
         '(("Max" . (reasoning-effort "xhigh"))
           ("Budget" . (reasoning-effort "medium"))
           ("Read-only" . (sandbox-mode "read-only"))
           ("Danger" . (sandbox-mode "danger-full-access")))))
    (should (codex-ide-test--transient-suffix-object
             'codex-ide-agent-config-menu "1"))
    (should (codex-ide-test--transient-suffix-object
             'codex-ide-agent-config-menu "2"))
    (should (codex-ide-test--transient-suffix-object
             'codex-ide-agent-config-menu "3"))
    (should (codex-ide-test--transient-suffix-object
             'codex-ide-agent-config-menu "4"))
    (should-not (codex-ide-test--transient-suffix-object
                 'codex-ide-agent-config-menu "5"))
    (should-error (transient-get-suffix 'codex-ide-agent-config-menu "P"))))

(ert-deftest codex-ide-agent-config-menu-shows-at-most-nine-preset-suffixes ()
  (let ((codex-ide-config-presets
         (cl-loop for index from 1 to 10
                  collect (cons (format "Preset %d" index)
                                '(reasoning-effort "medium")))))
    (should (codex-ide-test--transient-suffix-object
             'codex-ide-agent-config-menu "9"))
    (should-not (codex-ide-test--transient-suffix-object
                 'codex-ide-agent-config-menu "10"))))

(ert-deftest codex-ide-agent-config-menu-exposes-sticky-scope-suffix ()
  (should (transient-get-suffix 'codex-ide-agent-config-menu "o")))

(ert-deftest codex-ide-agent-config-menu-groups-agent-settings-only ()
  (let* ((root (codex-ide-test--transient-layout-root 'codex-ide-agent-config-menu))
         (columns-group (if (eq (codex-ide-test--transient-node-type root)
                                'transient-columns)
                            root
                          (car (codex-ide-test--transient-node-children root))))
         (columns (codex-ide-test--transient-node-children columns-group))
         (descriptions (mapcar (lambda (column)
                                 (plist-get (codex-ide-test--transient-node-props column)
                                            :description))
                               columns))
         (agent-column (car columns))
         (agent-suffixes (codex-ide-test--transient-node-children agent-column))
         (actions-column (caddr columns))
         (actions-suffixes (codex-ide-test--transient-node-children
                            actions-column))
         (navigation-column (cadddr columns))
         (navigation-suffixes (codex-ide-test--transient-node-children
                               navigation-column))
         (keys (mapcar (lambda (suffix)
                         (codex-ide-test--transient-suffix-prop suffix :key))
                       agent-suffixes))
         (actions-keys (mapcar (lambda (suffix)
                                 (codex-ide-test--transient-suffix-prop
                                  suffix :key))
                               actions-suffixes))
         (navigation-keys (mapcar (lambda (suffix)
                                    (codex-ide-test--transient-suffix-prop
                                     suffix :key))
                                  navigation-suffixes))
         (agent-descriptions (mapcar (lambda (suffix)
                                       (codex-ide-test--transient-suffix-prop
                                        suffix :description))
                                     agent-suffixes)))
    (should (equal descriptions '("Agent Config" "Presets" "Actions" "Navigation")))
    (should (equal keys '("m" "f" "r" "a" "s" "p")))
    (should (equal actions-keys '("o" "h")))
    (should (equal navigation-keys '("DEL" "C-g")))
    (should (member "o" actions-keys))
    (should (member "h" actions-keys))
    (should-error (transient-get-suffix 'codex-ide-agent-config-menu "q"))
    (should (equal agent-descriptions '(nil nil nil nil nil nil)))))

(ert-deftest codex-ide-agent-config-menu-agent-labels-show-current-values ()
  (let ((codex-ide-model "gpt-5.4")
        (codex-ide-fast "on")
        (codex-ide-reasoning-effort "high"))
    (let ((model-label (codex-ide--config-menu-agent-value-label 'model))
          (fast-label (codex-ide--config-menu-agent-value-label
                       'fast))
          (effort-label (codex-ide--config-menu-agent-value-label
                         'reasoning-effort)))
      (should (equal (substring-no-properties model-label)
                     "Model gpt-5.4"))
      (should (eq (get-text-property 6 'face model-label)
                  'transient-inactive-value))
      (should (equal (substring-no-properties fast-label)
                     "Fast on"))
      (should (eq (get-text-property 5 'face fast-label)
                  'transient-inactive-value))
      (should (equal (substring-no-properties effort-label)
                     "Reasoning effort high"))
      (should (eq (get-text-property 17 'face effort-label)
                  'transient-inactive-value)))))

(ert-deftest codex-ide-agent-config-menu-preset-labels-show-current-preset-names ()
  (let ((codex-ide-config-presets
         '(("Deep Review" . (reasoning-effort "high"))
           ("Fast" . (reasoning-effort "minimal"))
           ("Safe Read" . (sandbox-mode "read-only")))))
    (should (equal (codex-ide--config-menu-preset-label 0)
                   "Deep Review"))
    (should (equal (codex-ide--config-menu-preset-label 1)
                   "Fast"))
    (should (equal (codex-ide--config-menu-preset-label 2)
                   "Safe Read"))))

(ert-deftest codex-ide-agent-config-menu-scope-label-shows-muted-current-value ()
  (let ((codex-ide-agent-config-menu-scope 'future-sessions))
    (let ((label (funcall
                  (oref (transient-suffix-object
                         'codex-ide--config-menu-cycle-scope)
                        description))))
      (should (equal (substring-no-properties label)
                     "Scope future sessions"))
      (should (eq (get-text-property 6 'face label)
                  'transient-inactive-value)))))

(ert-deftest codex-ide-agent-config-menu-resets-default-scope-in-session-buffers ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-agent-config-menu-scope 'all-sessions))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (with-current-buffer (codex-ide-session-buffer session)
            (codex-ide--config-menu-reset-default-scope)
            (should (eq codex-ide-agent-config-menu-scope
                        'this-session))))))))

(ert-deftest codex-ide-agent-config-menu-groups-history-for-one-menu-interaction ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-agent-config-menu-scope 'all-sessions)
        (codex-ide-agent-config-menu--history-interaction-id nil)
        (codex-ide-config-history nil)
        (codex-ide-config--history-group nil)
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write"))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (with-current-buffer (codex-ide-session-buffer session)
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest _) nil)))
              (codex-ide--config-menu-enter)
              (should (eq codex-ide-agent-config-menu-scope
                          'this-session))
              (should codex-ide-agent-config-menu--history-interaction-id)
              (codex-ide--set-approval-policy "never")
              (codex-ide--set-sandbox-mode "read-only")
              (should-not codex-ide-config-history)
              (codex-ide--config-menu-commit-history-group)
              (should-not
               codex-ide-agent-config-menu--history-interaction-id)
              (should (= (length codex-ide-config-history) 1))
              (let ((formatted
                     (codex-ide-config-format-history-entry
                      (car codex-ide-config-history))))
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
                         formatted))))))))))

(ert-deftest codex-ide-agent-config-menu-numbered-preset-applies-configured-preset ()
  (let ((codex-ide-config-presets
         '(("Review" . (approval-policy "never"))
           ("Quick" . (sandbox-mode "read-only"))
           ("Read-only" . (reasoning-effort "minimal"))))
        (codex-ide-agent-config-menu-scope 'future-sessions)
        (codex-ide-config-history nil)
        (codex-ide-config--history-group nil)
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-reasoning-effort "medium"))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) nil)))
      (call-interactively
       (oref (codex-ide-test--transient-suffix-object
              'codex-ide-agent-config-menu "2")
             command)))
    (should (equal codex-ide-sandbox-mode "read-only"))
    (should (equal codex-ide-approval-policy "on-request"))
    (should (equal codex-ide-reasoning-effort "medium"))))

(ert-deftest codex-ide-apply-config-preset-prompts-and-applies-preset ()
  (let ((codex-ide-config-presets
         '(("Review" . (approval-policy "never"))
           ("Quick" . (sandbox-mode "read-only"))))
        (codex-ide-agent-config-menu-scope 'future-sessions)
        (codex-ide-config-history nil)
        (codex-ide-config--history-group nil)
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (codex-ide-config-format-preset
                  (cadr codex-ide-config-presets))))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (call-interactively #'codex-ide-apply-config-preset))
    (should (equal codex-ide-sandbox-mode "read-only"))
    (should (equal codex-ide-approval-policy "on-request"))))

(ert-deftest codex-ide-agent-config-menu-agent-setting-suffixes-stay-open-after-applying ()
  (let ((codex-ide-model "gpt-5.4")
        (codex-ide-fast "on")
        (codex-ide-reasoning-effort "medium")
        (codex-ide-approval-policy "on-request")
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-personality "pragmatic"))
    (dolist (command '(codex-ide--set-model
                       codex-ide--set-fast
                       codex-ide--set-reasoning-effort
                       codex-ide--set-approval-policy
                       codex-ide--set-personality
                       codex-ide--set-sandbox-mode))
      (let ((obj (transient-suffix-object command)))
        (should obj)
        (should (slot-boundp obj 'transient))
        (should (oref obj transient))))))

(ert-deftest codex-ide-agent-config-menu-preset-suffixes-stay-open-after-applying ()
  (let ((codex-ide-config-presets
         '(("Max" . (reasoning-effort "xhigh"))
           ("Budget" . (reasoning-effort "medium"))
           ("Read-only" . (sandbox-mode "read-only")))))
    (dolist (key '("1" "2" "3"))
      (let ((obj (codex-ide-test--transient-suffix-object
                  'codex-ide-agent-config-menu key)))
        (should obj)
        (should (slot-boundp obj 'transient))
        (should (oref obj transient))))))

(ert-deftest codex-ide-non-agent-setting-suffixes-exit-after-applying ()
  (dolist (command '(codex-ide--set-cli-path
                     codex-ide--set-running-submit-action
                     codex-ide--set-cli-extra-flags
                     codex-ide--set-new-session-split
                     codex-ide--toggle-emacs-tool-bridge
                     codex-ide--toggle-emacs-bridge-approval))
    (let ((obj (transient-suffix-object command)))
      (should obj)
      (should (slot-boundp obj 'transient))
      (should-not (oref obj transient)))))

(ert-deftest codex-ide-menu-session-suffixes-use-current-commands ()
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "s")
                                                     :command)
              #'codex-ide))
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "c")
                                                     :command)
              #'codex-ide-continue))
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "r")
                                                     :command)
              #'codex-ide-reset-current-session))
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "p")
                                                     :command)
              #'codex-ide-prompt))
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "i")
                                                     :command)
              #'codex-ide-submit-image))
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "I")
                                                     :command)
              #'codex-ide-submit-clipboard-image))
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "S")
                                                     :command)
              #'codex-ide-steer))
  (should (eq (codex-ide-test--transient-suffix-prop (transient-get-suffix 'codex-ide-menu "Q")
                                                     :command)
              #'codex-ide-queue)))

(ert-deftest codex-ide-save-config-persists-reasoning-effort ()
  (let ((codex-ide-reasoning-effort "high")
        (codex-ide-fast "on")
        (codex-ide-image-detail "high")
        (codex-ide-new-session-split 'vertical)
        (codex-ide-running-submit-action 'queue)
        (saved nil))
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (symbol value)
                 (push (cons symbol value) saved))))
      (codex-ide--save-config))
    (should (equal (alist-get 'codex-ide-reasoning-effort saved)
                   "high"))
    (should (equal (alist-get 'codex-ide-fast saved)
                   "on"))
    (should (equal (alist-get 'codex-ide-image-detail saved)
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

(ert-deftest codex-ide-set-fast-updates-global-default ()
  (let ((codex-ide-fast "off"))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest _) nil)))
      (codex-ide--set-fast "on"))
    (should (equal codex-ide-fast "on"))))

(ert-deftest codex-ide-set-sandbox-mode-can-target-current-session ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-sandbox-mode "workspace-write")
        (codex-ide-agent-config-menu-scope nil))
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
  (should (eq (codex-ide-test--transient-suffix-prop
               (transient-get-suffix 'codex-ide-debug-menu "i")
               :command)
              #'codex-ide-show-debug-info)))

(provide 'codex-ide-transient-tests)

;;; codex-ide-transient-tests.el ends here
