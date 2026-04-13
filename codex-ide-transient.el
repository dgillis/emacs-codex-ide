;;; codex-ide-transient.el --- Transient menus for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Transient entry points for the Codex CLI wrapper.

;;; Code:

(require 'subr-x)
(require 'transient)

(declare-function codex-ide-mcp-bridge-enable "codex-ide-mcp-bridge" ())
(declare-function codex-ide-mcp-bridge-disable "codex-ide-mcp-bridge" ())
(declare-function codex-ide--available-model-names "codex-ide" ())
(declare-function codex-ide "codex-ide" ())
(declare-function codex-ide-continue "codex-ide" ())
(declare-function codex-ide-prompt "codex-ide" ())
(declare-function codex-ide-reset-current-session "codex-ide" ())
(declare-function codex-ide-stop "codex-ide" ())
(declare-function codex-ide-switch-to-buffer "codex-ide" ())
(declare-function codex-ide-check-status "codex-ide" ())
(declare-function codex-ide--get-working-directory "codex-ide-core" ())
(declare-function codex-ide--get-process "codex-ide-core" ())

(autoload 'codex-ide-session-buffer-list "codex-ide-session-buffer-list"
  "Show a tabulated list of live Codex session buffers." t)
(autoload 'codex-ide-session-thread-list "codex-ide-session-thread-list"
  "Show a tabulated list of stored Codex threads for the current workspace." t)

(defvar codex-ide-cli-path)
(defvar codex-ide-cli-extra-flags)
(defvar codex-ide-model)
(defvar codex-ide-reasoning-effort)
(defvar codex-ide-approval-policy)
(defvar codex-ide-sandbox-mode)
(defvar codex-ide-personality)
(defvar codex-ide-focus-on-open)
(defvar codex-ide-new-session-split)
(defvar codex-ide-enable-emacs-tool-bridge)
(defvar codex-ide-emacs-bridge-require-approval)

(defconst codex-ide--other-model-choice "Other..."
  "Sentinel choice used to enter a custom model name.")

(defconst codex-ide--empty-model-choice "<empty>"
  "Sentinel choice used to clear the configured model.")

(defconst codex-ide--new-session-split-choices
  '(("default display" . nil)
    ("vertical split" . vertical)
    ("horizontal split" . horizontal))
  "Completion choices for `codex-ide-new-session-split'.")

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

(defun codex-ide--read-model ()
  "Prompt for a model, preferring server-provided choices when available."
  (let* ((default (or codex-ide-model ""))
         (models (codex-ide--available-model-names)))
    (if models
        (let ((choice (completing-read
                       "Model (choose or use Other...; empty clears): "
                       (append models
                               (list codex-ide--empty-model-choice
                                     codex-ide--other-model-choice))
                       nil nil nil nil default)))
          (cond
           ((equal choice codex-ide--empty-model-choice)
            "")
           ((equal choice codex-ide--other-model-choice)
            (read-string "Custom model (leave empty to clear): " default))
           (t
            choice)))
      (read-string "Model (leave empty to clear): " default))))

(transient-define-suffix codex-ide--set-cli-path (path)
  "Set the Codex CLI path."
  :description "Set CLI path"
  (interactive (list (read-file-name "Codex CLI path: " nil codex-ide-cli-path t)))
  (setq codex-ide-cli-path path)
  (message "Codex CLI path set to %s" path))

(transient-define-suffix codex-ide--set-cli-extra-flags (flags)
  "Set additional Codex CLI flags."
  :description "Set extra flags"
  (interactive (list (read-string "Additional CLI flags: " codex-ide-cli-extra-flags)))
  (setq codex-ide-cli-extra-flags flags)
  (message "Codex extra flags set to %s" flags))

(transient-define-suffix codex-ide--set-approval-policy (value)
  "Set `codex-ide-approval-policy'."
  :description "Set approval policy"
  (interactive (list (completing-read "Approval policy: "
                                      '("untrusted" "on-failure" "on-request" "never")
                                      nil t nil nil codex-ide-approval-policy)))
  (setq codex-ide-approval-policy value)
  (message "Codex approval policy set to %s" value))

(transient-define-suffix codex-ide--set-sandbox-mode (value)
  "Set `codex-ide-sandbox-mode'."
  :description "Set sandbox mode"
  (interactive (list (completing-read "Sandbox mode: "
                                      '("read-only" "workspace-write" "danger-full-access")
                                      nil t nil nil codex-ide-sandbox-mode)))
  (setq codex-ide-sandbox-mode value)
  (message "Codex sandbox mode set to %s" value))

(transient-define-suffix codex-ide--set-personality (value)
  "Set `codex-ide-personality'."
  :description "Set personality"
  (interactive (list (completing-read "Personality: "
                                      '("none" "friendly" "pragmatic")
                                      nil t nil nil codex-ide-personality)))
  (setq codex-ide-personality value)
  (message "Codex personality set to %s" value))

(transient-define-suffix codex-ide--set-model (model)
  "Set the Codex model."
  :description "Set model"
  (interactive (list (codex-ide--read-model)))
  (setq codex-ide-model (unless (string-empty-p model) model))
  (message "Codex model %s"
           (if codex-ide-model
               (format "set to %s" codex-ide-model)
             "cleared")))

(transient-define-suffix codex-ide--set-reasoning-effort (value)
  "Set `codex-ide-reasoning-effort'."
  :description "Set reasoning effort"
  (interactive (list (completing-read "Reasoning effort (leave empty to clear): "
                                      '("none" "minimal" "low" "medium" "high" "xhigh")
                                      nil nil nil nil codex-ide-reasoning-effort)))
  (setq codex-ide-reasoning-effort (unless (string-empty-p value) value))
  (message "Codex reasoning effort %s"
           (if codex-ide-reasoning-effort
               (format "set to %s" codex-ide-reasoning-effort)
             "cleared")))

(transient-define-suffix codex-ide--toggle-focus-on-open ()
  "Toggle `codex-ide-focus-on-open'."
  (interactive)
  (setq codex-ide-focus-on-open (not codex-ide-focus-on-open))
  (message "Focus on open %s" (if codex-ide-focus-on-open "enabled" "disabled")))

(defun codex-ide--new-session-split-label ()
  "Return a short label for `codex-ide-new-session-split'."
  (or (car (rassoc codex-ide-new-session-split
                   codex-ide--new-session-split-choices))
      (format "%S" codex-ide-new-session-split)))

(transient-define-suffix codex-ide--set-new-session-split (split)
  "Set `codex-ide-new-session-split'."
  :description "Set new session split"
  (interactive
   (list
    (cdr
     (assoc
      (completing-read
       "New session split: "
       codex-ide--new-session-split-choices
       nil t nil nil
       (codex-ide--new-session-split-label))
      codex-ide--new-session-split-choices))))
  (setq codex-ide-new-session-split split)
  (message "New session split set to %s"
           (codex-ide--new-session-split-label)))

(transient-define-suffix codex-ide--toggle-emacs-tool-bridge ()
  "Toggle `codex-ide-enable-emacs-tool-bridge'."
  (interactive)
  (if codex-ide-enable-emacs-tool-bridge
      (codex-ide-mcp-bridge-disable)
    (codex-ide-mcp-bridge-enable))
  (message "Emacs callback bridge %s"
           (if codex-ide-enable-emacs-tool-bridge "enabled" "disabled")))

(transient-define-suffix codex-ide--toggle-emacs-bridge-approval ()
  "Toggle `codex-ide-emacs-bridge-require-approval'."
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
  (customize-save-variable 'codex-ide-approval-policy codex-ide-approval-policy)
  (customize-save-variable 'codex-ide-sandbox-mode codex-ide-sandbox-mode)
  (customize-save-variable 'codex-ide-personality codex-ide-personality)
  (customize-save-variable 'codex-ide-focus-on-open codex-ide-focus-on-open)
  (customize-save-variable 'codex-ide-new-session-split
                           codex-ide-new-session-split)
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
    ("c" "Continue most recent" codex-ide-continue)
    ("s" "Start new" codex-ide)
    ("r" "Reset current session" codex-ide-reset-current-session
     :if codex-ide--in-session-buffer-p)
    ("q" "Stop current" codex-ide-stop
     :if codex-ide--in-session-buffer-p)]
   ["View"
    ("t" "Previous sessions" codex-ide-session-thread-list)
    ("l" "Live session buffers" codex-ide-session-buffer-list)]
   ["Submenus"
    ("C" "Configuration" codex-ide-config-menu)
    ("d" "Debug" codex-ide-debug-menu)]])

;;;###autoload
(transient-define-prefix codex-ide-config-menu ()
  "Open the Codex IDE configuration menu."
  ["Codex IDE Configuration"
   ["CLI"
    ("p" "Set CLI path" codex-ide--set-cli-path)
    ("m" "Set model" codex-ide--set-model)
    ("R" "Set reasoning effort" codex-ide--set-reasoning-effort)
    ("x" "Set extra flags" codex-ide--set-cli-extra-flags)
   ("a" "Set approval policy" codex-ide--set-approval-policy)
   ("P" "Set personality" codex-ide--set-personality)
   ("S" "Set sandbox mode" codex-ide--set-sandbox-mode)]
   ["Window"
    ("f" "Toggle focus on open" codex-ide--toggle-focus-on-open
     :description (lambda ()
                     (format "Focus on open (%s)"
                             (if codex-ide-focus-on-open "ON" "OFF"))))
    ("w" "Set new session split" codex-ide--set-new-session-split
     :description (lambda ()
                     (format "New session split (%s)"
                             (codex-ide--new-session-split-label))))]
   ["Bridge"
    ("e" "Toggle Emacs callback bridge" codex-ide--toggle-emacs-tool-bridge
     :description (lambda ()
                     (format "Emacs callback bridge (%s)"
                             (if codex-ide-enable-emacs-tool-bridge "ON" "OFF"))))
    ("A" "Toggle bridge approvals" codex-ide--toggle-emacs-bridge-approval
     :description (lambda ()
                     (format "Bridge approvals (%s)"
                             (if codex-ide-emacs-bridge-require-approval
                                 "ON"
                               "OFF"))))]]
  ["Save"
   ("V" "Save configuration" codex-ide--save-config)])

;;;###autoload
(transient-define-prefix codex-ide-debug-menu ()
  "Open a small debug/status menu for Codex IDE."
  ["Codex IDE Debug"
   ["Status"
    ("s" "Check CLI status" codex-ide-check-status)]])

(provide 'codex-ide-transient)

;;; codex-ide-transient.el ends here
