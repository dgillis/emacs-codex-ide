;;; codex-ide-transient.el --- Transient menus for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Transient entry points for the Codex CLI wrapper.

;;; Code:

(require 'subr-x)
(require 'transient)
(require 'codex-ide-config)

(declare-function codex-ide-mcp-bridge-enable "codex-ide-mcp-bridge" ())
(declare-function codex-ide-mcp-bridge-disable "codex-ide-mcp-bridge" ())
(declare-function codex-ide "codex-ide" ())
(declare-function codex-ide-continue "codex-ide" ())
(declare-function codex-ide-prompt "codex-ide" ())
(declare-function codex-ide-queue "codex-ide" ())
(declare-function codex-ide-reset-current-session "codex-ide" ())
(declare-function codex-ide-submit-clipboard-image "codex-ide-images" ())
(declare-function codex-ide-submit-image "codex-ide-images" (path))
(declare-function codex-ide-steer "codex-ide" ())
(declare-function codex-ide-switch-to-buffer "codex-ide" ())
(declare-function codex-ide-show-cli-info "codex-ide" ())
(autoload 'codex-ide-show-debug-info "codex-ide-debug-info"
  "Show a minibuffer summary of live Codex IDE session state." t)
(declare-function codex-ide--get-working-directory "codex-ide-core" ())
(declare-function codex-ide--get-process "codex-ide-core" ())
(declare-function codex-ide--session-for-current-buffer "codex-ide-core" ())
(declare-function codex-ide-config-read-history-entry "codex-ide-config" ())
(declare-function codex-ide-config-read-preset "codex-ide-config" ())
(declare-function codex-ide-config-apply-preset "codex-ide-config" (preset scope &optional session))
(declare-function codex-ide-config-begin-history-group "codex-ide-config" (&optional session interaction-id))
(declare-function codex-ide-config-commit-history-group "codex-ide-config" ())
(declare-function codex-ide-config-restore-history-entry "codex-ide-config" (entry))
(declare-function codex-ide-config-restore-last "codex-ide-config" ())

(autoload 'codex-ide-session-buffer-list "codex-ide-session-buffer-list"
  "Show a tabulated list of live Codex session buffers." t)
(autoload 'codex-ide-status "codex-ide-status-mode"
  "Show the Codex status buffer for the current project." t)
(autoload 'codex-ide-session-diff-open "codex-ide-diff-view"
  "Open or reuse the canonical session diff buffer for the current project." t)

(defvar codex-ide-cli-path)
(defvar codex-ide-cli-extra-flags)
(defvar codex-ide-model)
(defvar codex-ide-fast)
(defvar codex-ide-image-detail)
(defvar codex-ide-reasoning-effort)
(defvar codex-ide-running-submit-action)
(defvar codex-ide-approval-policy)
(defvar codex-ide-sandbox-mode)
(defvar codex-ide-personality)
(defvar codex-ide-new-session-split)
(defvar codex-ide-enable-emacs-tool-bridge)
(defvar codex-ide-want-mcp-bridge)
(defvar codex-ide-emacs-bridge-require-approval)

(defvar codex-ide-agent-config-menu-scope nil
  "Sticky scope used by `codex-ide-agent-config-menu' setting suffixes.")

(defvar codex-ide-agent-config-menu--history-interaction-id nil
  "History interaction id for the active agent config menu.")

(defconst codex-ide--config-menu-preset-limit 9
  "Maximum number of config presets shown in the agent config menu.")

(defconst codex-ide--new-session-split-choices
  '(("default display" . nil)
    ("vertical split" . vertical)
    ("horizontal split" . horizontal))
  "Completion choices for `codex-ide-new-session-split'.")

(defconst codex-ide--running-submit-action-choices
  '(("steer active turn" . steer)
    ("queue next turn" . queue))
  "Completion choices for `codex-ide-running-submit-action'.")

(defun codex-ide--in-session-buffer-p ()
  "Return non-nil when the current buffer is a Codex session buffer."
  (derived-mode-p 'codex-ide-session-mode))

(defun codex-ide--has-active-session-p ()
  "Return non-nil if the current project has an active Codex session."
  (when-let* ((process (codex-ide--get-process)))
    (process-live-p process)))

(defun codex-ide--session-status ()
  "Return a transient-ready status line."
  (if (codex-ide--has-active-session-p)
      (propertize
       (format "Active session in [%s]"
               (file-name-nondirectory
		(directory-file-name (codex-ide--get-working-directory))))
       'face 'success)
    (propertize "No active session" 'face 'transient-inactive-value)))

(defun codex-ide--config-menu-available-scopes ()
  "Return valid config scopes for the current context."
  (let ((session (codex-ide--session-for-current-buffer)))
    (cond
     (session
      '(this-session all-sessions future-sessions))
     ((codex-ide-config--live-sessions)
      '(all-sessions future-sessions))
     (t
      '(future-sessions)))))

(defun codex-ide--config-menu-scope ()
  "Return the current sticky config scope, normalized for this context."
  (let ((available (codex-ide--config-menu-available-scopes)))
    (unless (memq codex-ide-agent-config-menu-scope available)
      (setq codex-ide-agent-config-menu-scope (car available)))
    codex-ide-agent-config-menu-scope))

(defun codex-ide--config-menu-scope-label (&optional scope)
  "Return a display label for config SCOPE."
  (pcase (or scope (codex-ide--config-menu-scope))
    ('this-session "this session")
    ('all-sessions "all live + future")
    ('future-sessions "future sessions")
    (_ "unknown scope")))

(defun codex-ide--config-menu-reset-default-scope (&rest _)
  "Reset the agent config menu scope default for the current context."
  (when (codex-ide--session-for-current-buffer)
    (setq codex-ide-agent-config-menu-scope 'this-session)))

(defun codex-ide--config-menu-enter (&rest _)
  "Prepare transient-local state for entering the agent config menu."
  (codex-ide--config-menu-reset-default-scope)
  (setq codex-ide-agent-config-menu--history-interaction-id
        (codex-ide-config-begin-history-group
         (codex-ide--session-for-current-buffer)
         (current-time))))

(defun codex-ide--config-menu-commit-history-group ()
  "Commit the active agent config menu history group, if any."
  (when codex-ide-agent-config-menu--history-interaction-id
    (codex-ide-config-commit-history-group)
    (setq codex-ide-agent-config-menu--history-interaction-id nil)))

(defun codex-ide--config-menu-restore-history-entry ()
  "Open config history after committing the current menu interaction."
  (interactive)
  (codex-ide--config-menu-commit-history-group)
  (call-interactively #'codex-ide-config-restore-history-entry))

(defun codex-ide--config-menu-agent-value-label (key)
  "Return a config menu display label for agent setting KEY."
  (let* ((session (codex-ide--session-for-current-buffer))
         (label (codex-ide-config--label key))
         (value (codex-ide-config-format-value
                 (codex-ide-config-effective-value key session))))
    (concat (upcase (substring label 0 1))
            (substring label 1)
            " "
            (propertize value 'face 'transient-inactive-value))))

(transient-define-suffix codex-ide--config-menu-cycle-scope ()
			 "Cycle the scope used by agent setting suffixes."
			 :transient t
			 :description (lambda ()
					(concat
					 "Scope "
					 (propertize
					  (codex-ide--config-menu-scope-label)
					  'face
					  'transient-inactive-value)))
			 (interactive)
			   (let* ((available (codex-ide--config-menu-available-scopes))
				(current (codex-ide--config-menu-scope))
				(rest (cdr (memq current available))))
			   (setq codex-ide-agent-config-menu-scope
				 (or (car rest) (car available))))
			 (message "Codex config scope: %s"
				  (codex-ide--config-menu-scope-label)))

(defun codex-ide--config-menu-apply-agent-setting (key value &optional session)
  "Apply agent config KEY VALUE using the config menu's sticky scope."
  (let* ((session (or session (codex-ide--session-for-current-buffer)))
         (scope (codex-ide--config-menu-scope))
         (count (codex-ide-config-apply key value scope session)))
    (message "%s"
             (codex-ide-config-format-apply-message key value scope count))))

(transient-define-suffix codex-ide--config-menu-apply-preset (&optional preset)
			 "Apply a named Codex config preset."
			 :description "Apply preset"
			 :transient t
			 (interactive)
			 (let* ((preset (or preset (codex-ide-config-read-preset)))
				(scope (codex-ide--config-menu-scope))
				(count (codex-ide-config-apply-preset
					preset
					scope
					(codex-ide--session-for-current-buffer))))
			   (message "Applied Codex config preset %s to %s."
				    (car preset)
				    (pcase scope
				      ('this-session "this session")
				      ('all-sessions
				       (format "%d live session%s and future sessions"
					       count
					       (if (= count 1) "" "s")))
				      ('future-sessions "future sessions")
				      (_ "the selected scope")))))

;;;###autoload
(defun codex-ide-apply-config-preset ()
  "Prompt for and apply a named Codex config preset."
  (interactive)
  (codex-ide--config-menu-apply-preset))

(defun codex-ide--config-menu-preset (index)
  "Return the configured preset at zero-based INDEX."
  (nth index codex-ide-config-presets))

(defun codex-ide--config-menu-preset-label (index)
  "Return the menu label for the configured preset at zero-based INDEX."
  (if-let* ((preset (codex-ide--config-menu-preset index)))
      (car preset)
    (format "Preset %d" (1+ index))))

(defun codex-ide--config-menu-preset-key (index)
  "Return the menu key for the preset at zero-based INDEX."
  (number-to-string (1+ index)))

(defun codex-ide--config-menu-preset-suffix-specs ()
  "Return transient suffix specs for visible config presets."
  (let ((index 0)
        (presets codex-ide-config-presets)
        specs)
    (while (and presets (< index codex-ide--config-menu-preset-limit))
      (let ((index index)
            (preset (car presets)))
        (push (list (codex-ide--config-menu-preset-key index)
                    (codex-ide--config-menu-preset-label index)
                    (lambda ()
                      (interactive)
                      (codex-ide--config-menu-apply-preset preset))
                    :transient t)
              specs))
      (setq index (1+ index)
            presets (cdr presets)))
    (nreverse specs)))

(defun codex-ide--config-menu-preset-suffixes (_children)
  "Return transient suffixes for visible config presets."
  (transient-parse-suffixes
   'codex-ide-agent-config-menu
   (codex-ide--config-menu-preset-suffix-specs)))

(transient-define-suffix codex-ide--set-cli-path (&optional path)
			 "Set the Codex CLI path."
			 :description "Set CLI path"
			 :transient nil
			 (interactive)
			 (let ((path (or path
					 (read-file-name "Codex CLI path: " nil codex-ide-cli-path t))))
			   (setq codex-ide-cli-path path)
			   (message "Codex CLI path set to %s" path)))

(transient-define-suffix codex-ide--set-cli-extra-flags (&optional flags)
			 "Set additional Codex CLI flags."
			 :description "Set extra flags"
			 :transient nil
			 (interactive)
			 (let ((flags (or flags
					  (read-string "Additional CLI flags: " codex-ide-cli-extra-flags))))
			   (setq codex-ide-cli-extra-flags flags)
			   (message "Codex extra flags set to %s" flags)))

(transient-define-suffix codex-ide--set-approval-policy (&optional value)
			 "Set `codex-ide-approval-policy'."
			 :description (lambda ()
					(codex-ide--config-menu-agent-value-label
					 'approval-policy))
			 :transient t
			 (interactive)
			 (codex-ide--config-menu-apply-agent-setting
			  'approval-policy
			  (or value
			      (codex-ide-config-read-value 'approval-policy))))

(transient-define-suffix codex-ide--set-sandbox-mode (&optional value)
			 "Set `codex-ide-sandbox-mode'."
			 :description (lambda ()
					(codex-ide--config-menu-agent-value-label
					 'sandbox-mode))
			 :transient t
			 (interactive)
			 (codex-ide--config-menu-apply-agent-setting
			  'sandbox-mode
			  (or value
			      (codex-ide-config-read-value 'sandbox-mode))))

(transient-define-suffix codex-ide--set-personality (&optional value)
			 "Set `codex-ide-personality'."
			 :description (lambda ()
					(codex-ide--config-menu-agent-value-label
					 'personality))
			 :transient t
			 (interactive)
			 (codex-ide--config-menu-apply-agent-setting
			  'personality
			  (or value
			      (codex-ide-config-read-value 'personality))))

(transient-define-suffix codex-ide--set-model (&optional model)
			 "Set the Codex model."
			 :description (lambda ()
					(codex-ide--config-menu-agent-value-label
					 'model))
			 :transient t
			 (interactive)
			 (let ((model (or model
					  (codex-ide-config-read-value 'model))))
			   (codex-ide--config-menu-apply-agent-setting
			    'model
			    (unless (string-empty-p model) model))))

(transient-define-suffix codex-ide--set-fast (&optional value)
			 "Set `codex-ide-fast'."
			 :description (lambda ()
					(codex-ide--config-menu-agent-value-label
					 'fast))
			 :transient t
			 (interactive)
			 (let ((value
				(or value
				    (codex-ide-config-read-value 'fast))))
			   (codex-ide--config-menu-apply-agent-setting
			    'fast
			    value)))

(transient-define-suffix codex-ide--set-reasoning-effort (&optional value)
			 "Set `codex-ide-reasoning-effort'."
			 :description (lambda ()
					(codex-ide--config-menu-agent-value-label
					 'reasoning-effort))
			 :transient t
			 (interactive)
			 (let ((value
				(or value
				    (codex-ide-config-read-value 'reasoning-effort))))
			   (codex-ide--config-menu-apply-agent-setting
			    'reasoning-effort
			    value)))

(defun codex-ide--running-submit-action-label ()
  "Return a short label for `codex-ide-running-submit-action'."
  (or (car (rassoc codex-ide-running-submit-action
                   codex-ide--running-submit-action-choices))
      (format "%S" codex-ide-running-submit-action)))

(transient-define-suffix codex-ide--set-running-submit-action (&optional action)
			 "Set `codex-ide-running-submit-action'."
			 :description "Set running submit action"
			 :transient nil
			 (interactive)
			 (setq codex-ide-running-submit-action
			       (or action
				   (cdr
				    (assoc
				     (completing-read
				      "Running submit action: "
				      codex-ide--running-submit-action-choices
				      nil t nil nil
				      (codex-ide--running-submit-action-label))
				     codex-ide--running-submit-action-choices))))
			 (message "Running submit action set to %s"
				  (codex-ide--running-submit-action-label)))

(defun codex-ide--new-session-split-label ()
  "Return a short label for `codex-ide-new-session-split'."
  (or (car (rassoc codex-ide-new-session-split
                   codex-ide--new-session-split-choices))
      (format "%S" codex-ide-new-session-split)))

(transient-define-suffix codex-ide--set-new-session-split (&optional split)
			 "Set `codex-ide-new-session-split'."
			 :description "Set new session split"
			 :transient nil
			 (interactive)
			 (setq codex-ide-new-session-split
			       (or split
				   (cdr
				    (assoc
				     (completing-read
				      "New session split: "
				      codex-ide--new-session-split-choices
				      nil t nil nil
				      (codex-ide--new-session-split-label))
				     codex-ide--new-session-split-choices))))
			 (message "New session split set to %s"
				  (codex-ide--new-session-split-label)))

(transient-define-suffix codex-ide--toggle-emacs-tool-bridge ()
			 "Toggle `codex-ide-want-mcp-bridge'."
			 :transient nil
			 (interactive)
			 (if (eq codex-ide-want-mcp-bridge t)
			     (progn
			       (setq codex-ide-want-mcp-bridge nil)
			       (codex-ide-mcp-bridge-disable))
			   (setq codex-ide-want-mcp-bridge t)
			   (codex-ide-mcp-bridge-enable))
			 (message "Emacs callback bridge %s"
				  (if (eq codex-ide-want-mcp-bridge t) "enabled" "disabled")))

(transient-define-suffix codex-ide--toggle-emacs-bridge-approval ()
			 "Toggle `codex-ide-emacs-bridge-require-approval'."
			 :transient nil
			 (interactive)
			 (setq codex-ide-emacs-bridge-require-approval
			       (not codex-ide-emacs-bridge-require-approval))
			 (message "Emacs bridge approvals %s"
				  (if codex-ide-emacs-bridge-require-approval
				      "enabled"
				    "disabled")))

(defun codex-ide--save-config ()
  "Persist current Codex settings with Customize."
  (interactive)
  (customize-save-variable 'codex-ide-cli-path codex-ide-cli-path)
  (customize-save-variable 'codex-ide-cli-extra-flags codex-ide-cli-extra-flags)
  (customize-save-variable 'codex-ide-model codex-ide-model)
  (customize-save-variable 'codex-ide-fast codex-ide-fast)
  (customize-save-variable 'codex-ide-image-detail codex-ide-image-detail)
  (customize-save-variable 'codex-ide-reasoning-effort codex-ide-reasoning-effort)
  (customize-save-variable 'codex-ide-running-submit-action
                           codex-ide-running-submit-action)
  (customize-save-variable 'codex-ide-approval-policy codex-ide-approval-policy)
  (customize-save-variable 'codex-ide-sandbox-mode codex-ide-sandbox-mode)
  (customize-save-variable 'codex-ide-personality codex-ide-personality)
  (customize-save-variable 'codex-ide-new-session-split
                           codex-ide-new-session-split)
  (customize-save-variable 'codex-ide-want-mcp-bridge
                           codex-ide-want-mcp-bridge)
  (customize-save-variable 'codex-ide-enable-emacs-tool-bridge
                           codex-ide-enable-emacs-tool-bridge)
  (customize-save-variable 'codex-ide-emacs-bridge-require-approval
                           codex-ide-emacs-bridge-require-approval)
  (message "Codex IDE configuration saved"))

;;;###autoload
(transient-define-prefix codex-ide-menu ()
			 "Open the main Codex IDE menu."
			 [:description codex-ide--session-status]
			 ["Codex IDE"
			  ["Session"
			   ("b" "Switch to session buffer" codex-ide-switch-to-buffer)
			   ("p" "Send prompt from minibuffer" codex-ide-prompt)
			   ("i" "Attach image file" codex-ide-submit-image)
			   ("I" "Attach clipboard image" codex-ide-submit-clipboard-image)
			   ("S" "Steer active turn" codex-ide-steer
			    :if codex-ide--in-session-buffer-p)
			   ("Q" "Queue next turn" codex-ide-queue
			    :if codex-ide--in-session-buffer-p)
			   ("c" "Continue most recent" codex-ide-continue)
			   ("s" "Start new" codex-ide)
			   ("r" "Reset current session" codex-ide-reset-current-session
			    :if codex-ide--in-session-buffer-p)]
			  ["Manage"
			   ("m" "Manage sessions" codex-ide-status)
			   ("l" "Live session buffers" codex-ide-session-buffer-list)
			   ("D" "Session diff (live/transcript/pinned)" codex-ide-session-diff-open)]
			  ["Submenus"
			   ("C" "Agent configuration" codex-ide-agent-config-menu)
			   ("d" "Debug" codex-ide-debug-menu)]
			  ["Navigation"
			   ("C-g" "Exit" transient-quit-all)]])

;;;###autoload
(transient-define-prefix codex-ide-agent-config-menu ()
			 "Open the Codex IDE agent configuration menu."
			 [["Agent Config"
			   ("m" codex-ide--set-model)
			   ("f" codex-ide--set-fast)
			   ("r" codex-ide--set-reasoning-effort)
			   ("a" codex-ide--set-approval-policy)
			   ("s" codex-ide--set-sandbox-mode)
			   ("p" codex-ide--set-personality)]
			  ["Presets"
			   :class transient-column
			   :setup-children codex-ide--config-menu-preset-suffixes]
			  ["Actions"
			   ("o" codex-ide--config-menu-cycle-scope)
			   ("h" "Config history" codex-ide--config-menu-restore-history-entry
			    :transient nil)]
			  ["Navigation"
			   ("DEL" "Back" transient-quit-one)
			   ("C-g" "Exit" transient-quit-all)]])

(advice-remove 'codex-ide-agent-config-menu
	       #'codex-ide--config-menu-reset-default-scope)
(advice-remove 'codex-ide-agent-config-menu
	       #'codex-ide--config-menu-enter)
(advice-add 'codex-ide-agent-config-menu :before
	    #'codex-ide--config-menu-enter)

(remove-hook 'transient-post-exit-hook
             #'codex-ide--config-menu-commit-history-group)
(add-hook 'transient-post-exit-hook
          #'codex-ide--config-menu-commit-history-group)

;;;###autoload
(transient-define-prefix codex-ide-debug-menu ()
			 "Open a small debug/status menu for Codex IDE."
			 ["Codex IDE Debug"
			  ["Status"
			   ("s" "Check CLI status" codex-ide-show-cli-info)
			   ("i" "Show debug info" codex-ide-show-debug-info)]
			  ["Navigation"
			   ("DEL" "Back" transient-quit-one)
			   ("C-g" "Exit" transient-quit-all)]])

(provide 'codex-ide-transient)

;;; codex-ide-transient.el ends here
