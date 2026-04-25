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
(declare-function codex-ide-steer "codex-ide" ())
(declare-function codex-ide-stop "codex-ide" ())
(declare-function codex-ide-switch-to-buffer "codex-ide" ())
(declare-function codex-ide-show-cli-info "codex-ide" ())
(autoload 'codex-ide-show-debug-info "codex-ide-debug-info"
  "Show a minibuffer summary of live Codex IDE session state." t)
(declare-function codex-ide--get-working-directory "codex-ide-core" ())
(declare-function codex-ide--get-process "codex-ide-core" ())

(autoload 'codex-ide-session-buffer-list "codex-ide-session-buffer-list"
  "Show a tabulated list of live Codex session buffers." t)
(autoload 'codex-ide-status "codex-ide-status-mode"
  "Show the Codex status buffer for the current project." t)

(defvar codex-ide-cli-path)
(defvar codex-ide-cli-extra-flags)
(defvar codex-ide-model)
(defvar codex-ide-reasoning-effort)
(defvar codex-ide-running-submit-action)
(defvar codex-ide-approval-policy)
(defvar codex-ide-sandbox-mode)
(defvar codex-ide-personality)
(defvar codex-ide-new-session-split)
(defvar codex-ide-enable-emacs-tool-bridge)
(defvar codex-ide-want-mcp-bridge)
(defvar codex-ide-emacs-bridge-require-approval)

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
  (when-let ((process (codex-ide--get-process)))
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
  :description "Set approval policy"
  :transient nil
  (interactive)
  (codex-ide-config-apply-interactively
   'approval-policy
   (or value
       (codex-ide-config-read-value 'approval-policy))))

(transient-define-suffix codex-ide--set-sandbox-mode (&optional value)
  "Set `codex-ide-sandbox-mode'."
  :description "Set sandbox mode"
  :transient nil
  (interactive)
  (codex-ide-config-apply-interactively
   'sandbox-mode
   (or value
       (codex-ide-config-read-value 'sandbox-mode))))

(transient-define-suffix codex-ide--set-personality (&optional value)
  "Set `codex-ide-personality'."
  :description "Set personality"
  :transient nil
  (interactive)
  (codex-ide-config-apply-interactively
   'personality
   (or value
       (codex-ide-config-read-value 'personality))))

(transient-define-suffix codex-ide--set-model (&optional model)
  "Set the Codex model."
  :description "Set model"
  :transient nil
  (interactive)
  (let ((model (or model
                   (codex-ide-config-read-value 'model))))
    (codex-ide-config-apply-interactively
     'model
     (unless (string-empty-p model) model))))

(transient-define-suffix codex-ide--set-reasoning-effort (&optional value)
  "Set `codex-ide-reasoning-effort'."
  :description "Set reasoning effort"
  :transient nil
  (interactive)
  (let ((value
         (or value
             (codex-ide-config-read-value 'reasoning-effort))))
    (codex-ide-config-apply-interactively
     'reasoning-effort
     (unless (string-empty-p value) value))))

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
    ("S" "Steer active turn" codex-ide-steer
     :if codex-ide--in-session-buffer-p)
    ("Q" "Queue next turn" codex-ide-queue
     :if codex-ide--in-session-buffer-p)
    ("c" "Continue most recent" codex-ide-continue)
    ("s" "Start new" codex-ide)
    ("r" "Reset current session" codex-ide-reset-current-session
     :if codex-ide--in-session-buffer-p)
    ("q" "Stop current" codex-ide-stop
     :if codex-ide--in-session-buffer-p)]
   ["Manage"
    ("m" "Manage sessions" codex-ide-status)
    ("l" "Live session buffers" codex-ide-session-buffer-list)]
   ["Submenus"
    ("C" "Configuration" codex-ide-config-menu)
    ("d" "Debug" codex-ide-debug-menu)]])

;;;###autoload
(transient-define-prefix codex-ide-config-menu ()
  "Open the Codex IDE configuration menu."
  [["Agent"
    ("m" "Set model" codex-ide--set-model)
    ("r" "Set reasoning effort" codex-ide--set-reasoning-effort)
    ("a" "Set approval policy" codex-ide--set-approval-policy)
    ("s" "Set sandbox mode" codex-ide--set-sandbox-mode)
    ("p" "Set personality" codex-ide--set-personality)]
   ["Codex-IDE"
    ("c" "Set CLI path" codex-ide--set-cli-path)
    ("x" "Set extra flags" codex-ide--set-cli-extra-flags)
    ("e" "Toggle Emacs callback bridge" codex-ide--toggle-emacs-tool-bridge
     :description (lambda ()
                     (format "Emacs callback bridge (%s)"
                             (pcase codex-ide-want-mcp-bridge
                               ('t "ON")
                               ('prompt "PROMPT")
                               (_ "OFF")))))
    ("A" "Toggle bridge approvals" codex-ide--toggle-emacs-bridge-approval
     :description (lambda ()
                     (format "Bridge approvals (%s)"
                             (if codex-ide-emacs-bridge-require-approval
                                 "ON"
                               "OFF"))))
	    ("u" "Set running submit action" codex-ide--set-running-submit-action
	     :description (lambda ()
	                     (format "Running submit action (%s)"
	                             (codex-ide--running-submit-action-label))))
	    ("w" "Set new session split" codex-ide--set-new-session-split
	     :description (lambda ()
	                     (format "New session split (%s)"
	                             (codex-ide--new-session-split-label))))]
   ["Save"
    ("S" "Save configuration" codex-ide--save-config :transient nil)]])

;;;###autoload
(transient-define-prefix codex-ide-debug-menu ()
  "Open a small debug/status menu for Codex IDE."
  ["Codex IDE Debug"
   ["Status"
    ("s" "Check CLI status" codex-ide-show-cli-info)
    ("i" "Show debug info" codex-ide-show-debug-info)]])

(provide 'codex-ide-transient)

;;; codex-ide-transient.el ends here
