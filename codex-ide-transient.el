;;; codex-ide-transient.el --- Transient menus for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Transient entry points for the Codex CLI wrapper.

;;; Code:

(require 'subr-x)
(require 'transient)

(declare-function codex-ide-mcp-bridge-enable "codex-ide-mcp-bridge" ())
(declare-function codex-ide-mcp-bridge-disable "codex-ide-mcp-bridge" ())
(declare-function codex-ide "codex-ide" ())
(declare-function codex-ide-resume "codex-ide" ())
(declare-function codex-ide-continue "codex-ide" ())
(declare-function codex-ide-prompt "codex-ide" ())
(declare-function codex-ide-stop "codex-ide" ())
(declare-function codex-ide-list-session-buffers "codex-ide" ())
(declare-function codex-ide-switch-to-buffer "codex-ide" ())
(declare-function codex-ide-send-active-buffer-context "codex-ide" ())
(declare-function codex-ide-interrupt "codex-ide" ())
(declare-function codex-ide-insert-newline "codex-ide" ())
(declare-function codex-ide-toggle "codex-ide" ())
(declare-function codex-ide-toggle-recent "codex-ide" ())
(declare-function codex-ide-check-status "codex-ide" ())
(declare-function codex-ide--get-working-directory "codex-ide" ())
(declare-function codex-ide--get-process "codex-ide" ())
(declare-function codex-ide--ensure-cli "codex-ide" ())

(defvar codex-ide-cli-path)
(defvar codex-ide-cli-extra-flags)
(defvar codex-ide-model)
(defvar codex-ide-approval-policy)
(defvar codex-ide-sandbox-mode)
(defvar codex-ide-personality)
(defvar codex-ide-focus-on-open)
(defvar codex-ide-use-side-window)
(defvar codex-ide-window-side)
(defvar codex-ide-window-width)
(defvar codex-ide-window-height)
(defvar codex-ide-enable-emacs-tool-bridge)
(defvar codex-ide-emacs-bridge-require-approval)

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

(defun codex-ide--start-description ()
  "Return the dynamic description for the start action."
  (if (codex-ide--has-active-session-p)
      (propertize "Start new session (session already running)"
                  'face 'transient-inactive-value)
    "Start new Codex session"))

(defun codex-ide--resume-description ()
  "Return the dynamic description for the resume action."
  (if (codex-ide--has-active-session-p)
      "Resume with picker (replaces current session)"
    "Resume with picker"))

(defun codex-ide--continue-description ()
  "Return the dynamic description for the continue action."
  (if (codex-ide--has-active-session-p)
      (propertize "Continue most recent (session already running)"
                  'face 'transient-inactive-value)
    "Continue most recent"))

(defun codex-ide--start-from-menu ()
  "Start Codex, optionally replacing an active session."
  (interactive)
  (if (codex-ide--has-active-session-p)
      (when (y-or-n-p
             (format "Quit existing Codex session in %s and start a new one? "
                     (abbreviate-file-name (codex-ide--get-working-directory))))
        (codex-ide-stop)
        (codex-ide))
    (codex-ide)))

(defun codex-ide--resume-from-menu ()
  "Resume Codex using the thread picker."
  (interactive)
  (codex-ide-resume))

(defun codex-ide--continue-if-no-session ()
  "Continue the most recent Codex session when there is no active session."
  (interactive)
  (if (codex-ide--has-active-session-p)
      (message "Codex session already running in %s"
               (abbreviate-file-name (codex-ide--get-working-directory)))
    (codex-ide-continue)))

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
  (interactive (list (read-string "Model (leave empty to clear): "
                                  (or codex-ide-model ""))))
  (setq codex-ide-model (unless (string-empty-p model) model))
  (message "Codex model %s"
           (if codex-ide-model
               (format "set to %s" codex-ide-model)
             "cleared")))

(transient-define-suffix codex-ide--set-window-side (side)
  "Set `codex-ide-window-side' to SIDE."
  :description "Set window side"
  (interactive (list (intern (completing-read "Window side: "
                                              '("left" "right" "top" "bottom")
                                              nil t nil nil
                                              (symbol-name codex-ide-window-side)))))
  (setq codex-ide-window-side side)
  (message "Codex window side set to %s" side))

(transient-define-suffix codex-ide--set-window-width (width)
  "Set `codex-ide-window-width' to WIDTH."
  :description "Set window width"
  (interactive (list (read-number "Window width: " codex-ide-window-width)))
  (setq codex-ide-window-width width)
  (message "Codex window width set to %d" width))

(transient-define-suffix codex-ide--set-window-height (height)
  "Set `codex-ide-window-height' to HEIGHT."
  :description "Set window height"
  (interactive (list (read-number "Window height: " codex-ide-window-height)))
  (setq codex-ide-window-height height)
  (message "Codex window height set to %d" height))

(transient-define-suffix codex-ide--toggle-focus-on-open ()
  "Toggle `codex-ide-focus-on-open'."
  (interactive)
  (setq codex-ide-focus-on-open (not codex-ide-focus-on-open))
  (message "Focus on open %s" (if codex-ide-focus-on-open "enabled" "disabled")))

(transient-define-suffix codex-ide--toggle-use-side-window ()
  "Toggle `codex-ide-use-side-window'."
  (interactive)
  (setq codex-ide-use-side-window (not codex-ide-use-side-window))
  (message "Use side window %s" (if codex-ide-use-side-window "enabled" "disabled")))

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
  (customize-save-variable 'codex-ide-approval-policy codex-ide-approval-policy)
  (customize-save-variable 'codex-ide-sandbox-mode codex-ide-sandbox-mode)
  (customize-save-variable 'codex-ide-personality codex-ide-personality)
  (customize-save-variable 'codex-ide-focus-on-open codex-ide-focus-on-open)
  (customize-save-variable 'codex-ide-use-side-window codex-ide-use-side-window)
  (customize-save-variable 'codex-ide-window-side codex-ide-window-side)
  (customize-save-variable 'codex-ide-window-width codex-ide-window-width)
  (customize-save-variable 'codex-ide-window-height codex-ide-window-height)
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
    ("s" codex-ide--start-from-menu :description codex-ide--start-description)
    ("c" codex-ide--continue-if-no-session :description codex-ide--continue-description)
    ("r" codex-ide--resume-from-menu :description codex-ide--resume-description)
    ("q" "Stop current session" codex-ide-stop)
    ("l" "List session buffers" codex-ide-list-session-buffers)]
   ["Navigation"
   ("b" "Switch to Codex buffer" codex-ide-switch-to-buffer)
    ("w" "Toggle current window" codex-ide-toggle)
    ("W" "Toggle recent window" codex-ide-toggle-recent)]
   ["Interaction"
    ("p" "Prompt from minibuffer" codex-ide-prompt)
    ("i" "Send active buffer context" codex-ide-send-active-buffer-context)
    ("e" "Interrupt turn" codex-ide-interrupt)
    ("n" "Insert newline" codex-ide-insert-newline)]
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
    ("x" "Set extra flags" codex-ide--set-cli-extra-flags)
    ("a" "Set approval policy" codex-ide--set-approval-policy)
    ("P" "Set personality" codex-ide--set-personality)
    ("S" "Set sandbox mode" codex-ide--set-sandbox-mode)]
   ["Window"
    ("u" "Toggle side window" codex-ide--toggle-use-side-window
     :description (lambda ()
                     (format "Use side window (%s)"
                             (if codex-ide-use-side-window "ON" "OFF"))))
    ("s" "Set window side" codex-ide--set-window-side)
    ("w" "Set window width" codex-ide--set-window-width)
    ("h" "Set window height" codex-ide--set-window-height)
    ("f" "Toggle focus on open" codex-ide--toggle-focus-on-open
     :description (lambda ()
                     (format "Focus on open (%s)"
                             (if codex-ide-focus-on-open "ON" "OFF"))))]
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
