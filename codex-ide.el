;;; codex-ide.el --- Codex app-server integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (transient "0.9.0"))
;; Keywords: ai, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Lightweight Emacs integration for Codex built on `codex app-server`.
;; Each project gets its own app-server session buffer in Emacs.  Prompts are
;; sent with JSON-RPC requests and assistant output is streamed back into the
;; buffer via notifications.
;;
;; OpenAI app-server docs: https://developers.openai.com/codex/app-server#api-overview

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 'project)
(require 'subr-x)
(require 'codex-ide-mcp-elicitation)
(require 'codex-ide-transient)

;;;###autoload
(defgroup codex-ide nil
  "Codex app-server integration for Emacs."
  :group 'tools
  :prefix "codex-ide-")

(require 'codex-ide-mcp-bridge)

(defface codex-ide-user-prompt-face
  '((((class color) (background light))
     :background "#f4f1e8")
    (((class color) (background dark))
     :background "#2d2a24"))
  "Face used to distinguish submitted and active user prompts."
  :group 'codex-ide)

(defface codex-ide-output-separator-face
  '((((class color) (background light))
     :foreground "#c7c1b4")
    (((class color) (background dark))
     :foreground "#5a554b"))
  "Face used for transcript separator rules."
  :group 'codex-ide)

(defface codex-ide-item-summary-face
  '((t :inherit font-lock-function-name-face))
  "Face used for item summary lines."
  :group 'codex-ide)

(defface codex-ide-item-detail-face
  '((t :inherit shadow))
  "Face used for item detail lines."
  :group 'codex-ide)

(defface codex-ide-file-diff-header-face
  '((t :inherit font-lock-keyword-face))
  "Face used for file-change diff headers."
  :group 'codex-ide)

(defface codex-ide-file-diff-hunk-face
  '((t :inherit diff-hunk-header))
  "Face used for file-change diff hunk lines."
  :group 'codex-ide)

(defface codex-ide-file-diff-added-face
  '((t :inherit diff-added))
  "Face used for added lines in file-change diffs."
  :group 'codex-ide)

(defface codex-ide-file-diff-removed-face
  '((t :inherit diff-removed))
  "Face used for removed lines in file-change diffs."
  :group 'codex-ide)

(defface codex-ide-file-diff-context-face
  '((t :inherit fixed-pitch))
  "Face used for context lines in file-change diffs."
  :group 'codex-ide)

(defface codex-ide-header-line-face
  '((t :inherit (header-line font-lock-comment-face) :weight light :height 0.9))
  "Face used for the Codex session header line."
  :group 'codex-ide)

(defface codex-ide-status-running-face
  '((t :inherit mode-line-emphasis :weight bold))
  "Face used for a running Codex session in the mode line."
  :group 'codex-ide)

(defface codex-ide-status-idle-face
  '((t :inherit success :weight semibold))
  "Face used for an idle Codex session in the mode line."
  :group 'codex-ide)

(defface codex-ide-status-busy-face
  '((t :inherit warning :weight bold))
  "Face used for transitional Codex session states in the mode line."
  :group 'codex-ide)

(defface codex-ide-status-error-face
  '((t :inherit error :weight bold))
  "Face used for failed or disconnected Codex session states in the mode line."
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-cli-path "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-buffer-name-function #'codex-ide--default-buffer-name
  "Function used to derive the Codex session buffer name."
  :type 'function
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-cli-extra-flags ""
  "Additional flags appended to the `codex app-server` command."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-model nil
  "Optional model name for new or resumed threads."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Model"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-buffer-name-prefix "codex"
  "Prefix used when creating Codex session buffer names."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-use-side-window nil
  "Whether to display Codex buffers in a side window."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-window-side 'right
  "Side of the frame where Codex should be displayed."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-window-width 90
  "Width of the Codex side window when using left or right placement."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-window-height 20
  "Height of the Codex side window when using top or bottom placement."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-focus-on-open t
  "Whether to focus the Codex window after showing it."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-session-enable-visual-line-mode t
  "Whether Codex session buffers should enable `visual-line-mode' by default."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-approval-policy "on-request"
  "Approval policy for new or resumed Codex threads."
  :type '(choice (const "untrusted")
                 (const "on-failure")
                 (const "on-request")
                 (const "never"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-sandbox-mode "workspace-write"
  "Sandbox mode for new or resumed Codex threads."
  :type '(choice (const "read-only")
                 (const "workspace-write")
                 (const "danger-full-access"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-personality "pragmatic"
  "Personality for new or resumed Codex threads."
  :type '(choice (const "none")
                 (const "friendly")
                 (const "pragmatic"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-request-timeout 10
  "Seconds to wait for synchronous app-server responses."
  :type 'number
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-log-max-lines 10000
  "Maximum number of lines to keep in each Codex log buffer.

When a log buffer grows beyond this limit, older lines are removed from the
top of the buffer."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-include-active-buffer-context 'when-changed
  "How `codex-ide' should include Emacs active-buffer context in prompts.
When set to `when-changed', include the active file context only when it has
changed since the last prompt sent to that session.
When set to `always', include the active file context on every prompt.
When nil, never include active-buffer context automatically."
  :type '(choice (const :tag "When changed" when-changed)
                 (const :tag "Always" always)
                 (const :tag "Disabled" nil))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-resume-summary-turn-limit 100
  "How many recent turns to summarize when resuming a stored thread."
  :type 'integer
  :group 'codex-ide)

(defvar codex-ide--cli-available nil
  "Whether the Codex CLI has been detected successfully.")

(defvar codex-ide--sessions (make-hash-table :test 'equal)
  "Hash table mapping working directories to active Codex sessions.")

(defvar codex-ide--last-accessed-buffer nil
  "Most recently displayed Codex session buffer.")

(defvar codex-ide--active-buffer-contexts (make-hash-table :test 'equal)
  "Hash table mapping working directories to the latest Emacs buffer context.")

(defvar codex-ide--active-buffer-objects (make-hash-table :test 'equal)
  "Hash table mapping working directories to the latest live Emacs file buffer.")

(defvar codex-ide--last-sent-buffer-contexts (make-hash-table :test 'equal)
  "Hash table mapping working directories to the last context sent to Codex.")

(defvar codex-ide--prompt-origin-buffer nil
  "Buffer to treat as the authoritative prompt context for one submission.")

(defvar codex-ide-persisted-project-state (make-hash-table :test 'equal)
  "Hash table mapping project directories to persisted Codex IDE state.
Each value is a plist reserved for state that should survive Emacs restarts.
Add this variable to `savehist-additional-variables' to persist it.")

(defvar codex-ide--session-metadata (make-hash-table :test 'eq)
  "Ephemeral metadata keyed by live `codex-ide-session' objects.")

(defclass codex-ide-session ()
  ((directory
    :initarg :directory
    :initform nil
    :accessor codex-ide-session-directory)
   (process
   :initarg :process
    :initform nil
    :accessor codex-ide-session-process)
   (stderr-process
    :initarg :stderr-process
    :initform nil
    :accessor codex-ide-session-stderr-process)
   (buffer
    :initarg :buffer
    :initform nil
    :accessor codex-ide-session-buffer)
   (log-buffer
    :initarg :log-buffer
    :initform nil
    :accessor codex-ide-session-log-buffer)
   (thread-id
    :initarg :thread-id
    :initform nil
    :accessor codex-ide-session-thread-id)
   (current-turn-id
    :initarg :current-turn-id
    :initform nil
    :accessor codex-ide-session-current-turn-id)
   (request-counter
    :initarg :request-counter
    :initform 0
    :accessor codex-ide-session-request-counter)
   (pending-requests
    :initarg :pending-requests
    :initform nil
    :accessor codex-ide-session-pending-requests)
   (partial-line
    :initarg :partial-line
    :initform ""
    :accessor codex-ide-session-partial-line)
   (current-message-item-id
    :initarg :current-message-item-id
    :initform nil
    :accessor codex-ide-session-current-message-item-id)
   (current-message-prefix-inserted
    :initarg :current-message-prefix-inserted
    :initform nil
    :accessor codex-ide-session-current-message-prefix-inserted)
   (current-message-start-marker
    :initarg :current-message-start-marker
    :initform nil
    :accessor codex-ide-session-current-message-start-marker)
   (output-prefix-inserted
    :initarg :output-prefix-inserted
    :initform nil
    :accessor codex-ide-session-output-prefix-inserted)
   (item-states
    :initarg :item-states
    :initform nil
    :accessor codex-ide-session-item-states)
   (input-overlay
    :initarg :input-overlay
    :initform nil
    :accessor codex-ide-session-input-overlay)
   (input-start-marker
    :initarg :input-start-marker
    :initform nil
    :accessor codex-ide-session-input-start-marker)
   (input-prompt-start-marker
    :initarg :input-prompt-start-marker
    :initform nil
    :accessor codex-ide-session-input-prompt-start-marker)
   (prompt-history-index
    :initarg :prompt-history-index
    :initform nil
    :accessor codex-ide-session-prompt-history-index)
   (prompt-history-draft
    :initarg :prompt-history-draft
    :initform nil
    :accessor codex-ide-session-prompt-history-draft)
   (interrupt-requested
    :initarg :interrupt-requested
    :initform nil
    :accessor codex-ide-session-interrupt-requested)
   (status
    :initarg :status
    :initform "starting"
    :accessor codex-ide-session-status))
  "State for a Codex app-server session.")

(defun make-codex-ide-session (&rest initargs)
  "Create a `codex-ide-session' object with INITARGS."
  (apply #'make-instance 'codex-ide-session initargs))

(defvar codex-ide-session-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    map)
  "Keymap for `codex-ide-session-mode'.")

(defvar codex-ide-session-prompt-minor-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-prompt-minor-mode'.")

(define-key codex-ide-session-mode-map (kbd "C-c C-c") #'codex-ide-interrupt)
(define-key codex-ide-session-mode-map (kbd "C-c RET") #'codex-ide-submit)
(define-key codex-ide-session-mode-map (kbd "C-c C-k") #'codex-ide-interrupt)
(define-key codex-ide-session-mode-map (kbd "C-c C-o") #'codex-ide-send-active-buffer-context)
(define-key codex-ide-session-mode-map (kbd "C-M-p") #'codex-ide-previous-prompt-line)
(define-key codex-ide-session-mode-map (kbd "C-M-n") #'codex-ide-next-prompt-line)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "M-p") #'codex-ide-previous-prompt-history)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "M-n") #'codex-ide-next-prompt-history)

(define-minor-mode codex-ide-session-prompt-minor-mode
  "Minor mode enabled only while point is in the active Codex prompt."
  :lighter " Prompt"
  :keymap codex-ide-session-prompt-minor-mode-map)

(defun codex-ide--point-in-active-prompt-p (&optional session pos)
  "Return non-nil when POS is inside SESSION's active prompt region."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (setq pos (or pos (point)))
  (when-let ((overlay (and session (codex-ide-session-input-overlay session))))
    (let ((start (overlay-start overlay))
          (end (overlay-end overlay)))
      (and start
           end
           (<= start pos)
           (<= pos end)))))

(defun codex-ide--sync-prompt-minor-mode (&optional session)
  "Enable or disable `codex-ide-session-prompt-minor-mode' for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (and session (derived-mode-p 'codex-ide-session-mode))
    (let ((inside (codex-ide--point-in-active-prompt-p session)))
      (unless (eq inside codex-ide-session-prompt-minor-mode)
        (codex-ide-session-prompt-minor-mode (if inside 1 -1))))))

;;;###autoload
(define-derived-mode codex-ide-session-mode text-mode "Codex-IDE"
  "Major mode for Codex app-server session buffers."
  (setq-local truncate-lines nil)
  (when codex-ide-session-enable-visual-line-mode
    (visual-line-mode 1))
  (setq-local mode-line-process '((:eval (codex-ide--mode-line-status))))
  (add-hook 'post-command-hook #'codex-ide--sync-prompt-minor-mode nil t))

;;;###autoload
(define-derived-mode codex-ide-log-mode special-mode "Codex-IDE-Log"
  "Major mode for Codex IDE log buffers."
  (setq-local truncate-lines t))

(defun codex-ide--project-name (directory)
  "Return the display name for DIRECTORY."
  (file-name-nondirectory (directory-file-name directory)))

(defun codex-ide--default-buffer-name (directory)
  "Generate a default Codex session buffer name for DIRECTORY."
  (format "*%s[%s]*"
          codex-ide-buffer-name-prefix
          (codex-ide--project-name directory)))

(defun codex-ide--log-buffer-name (directory)
  "Generate the default Codex log buffer name for DIRECTORY."
  (format "*%s-log[%s]*"
          codex-ide-buffer-name-prefix
          (codex-ide--project-name directory)))

(defun codex-ide--status-label (status)
  "Return a display label for STATUS."
  (pcase (and (stringp status) (downcase status))
    ("running" "Running")
    ("idle" "Idle")
    ("starting" "Starting")
    ("interrupting" "Interrupting")
    ("submitted" "Submitted")
    ("disconnected" "Disconnected")
    ((pred stringp) (capitalize status))
    (_ "Disconnected")))

(defun codex-ide--status-face (status)
  "Return the face to use for STATUS."
  (let ((status (and (stringp status) (downcase status))))
    (cond
     ((equal status "idle") 'codex-ide-status-idle-face)
     ((member status '("running" "submitted")) 'codex-ide-status-running-face)
     ((member status '("starting" "interrupting")) 'codex-ide-status-busy-face)
     ((or (member status '("failed" "error" "disconnected" "finished" "killed"))
          (and status
               (string-match-p (rx (or "exit" "exited" "abnormally")) status)))
      'codex-ide-status-error-face)
     (t 'codex-ide-status-busy-face))))

(defun codex-ide--mode-line-status (&optional session)
  "Return the current modeline status segment for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (codex-ide-session-p session)
    (let* ((status (or (codex-ide-session-status session) "disconnected"))
           (label (codex-ide--status-label status))
           (face (codex-ide--status-face status)))
      (concat
       " "
       (propertize "Codex" 'face 'mode-line-emphasis)
       ":"
       (propertize label 'face face)
       " "))))

(defun codex-ide--update-mode-line (&optional session)
  "Refresh the mode line indicator for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((buffer (and session (codex-ide-session-buffer session))))
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defun codex-ide--initialize-log-buffer (buffer directory)
  "Prepare BUFFER for logging for DIRECTORY."
  (with-current-buffer buffer
    (codex-ide-log-mode)
    (setq-local default-directory directory)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Codex log for %s\n\n"
                      (abbreviate-file-name directory))))))

(defun codex-ide--ensure-log-buffer (session)
  "Return SESSION's log buffer, creating it when needed."
  (or (and (buffer-live-p (codex-ide-session-log-buffer session))
           (codex-ide-session-log-buffer session))
      (let* ((directory (codex-ide-session-directory session))
             (buffer (get-buffer-create (codex-ide--log-buffer-name directory))))
        (codex-ide--initialize-log-buffer buffer directory)
        (setf (codex-ide-session-log-buffer session) buffer)
        buffer)))

(defun codex-ide--kill-session-log-buffer (session)
  "Kill SESSION's log buffer, if it exists."
  (when-let ((buffer (codex-ide-session-log-buffer session)))
    (setf (codex-ide-session-log-buffer session) nil)
    (when (buffer-live-p buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun codex-ide--append-log-output (session output)
  "Append OUTPUT to SESSION's log buffer."
  (when-let ((buffer (codex-ide--ensure-log-buffer session)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (moving (= (point) (point-max))))
        (goto-char (point-max))
        (insert output)
        (codex-ide--trim-log-buffer)
        (when moving
          (goto-char (point-max)))))))

(defun codex-ide--stderr-filter (process chunk)
  "Append stderr CHUNK from PROCESS to the owning session log."
  (when-let ((session (process-get process 'codex-session)))
    (codex-ide--append-log-output session chunk)))

(defun codex-ide--session-buffer-p (buffer)
  "Return non-nil when BUFFER looks like a Codex session buffer."
  (when-let ((name (cond
                    ((stringp buffer) buffer)
                    ((buffer-live-p buffer) (buffer-name buffer)))))
    (string-prefix-p (format "*%s[" codex-ide-buffer-name-prefix) name)))

(defun codex-ide--get-working-directory ()
  "Return the current project root or `default-directory'."
  (codex-ide--normalize-directory
   (if-let ((project (project-current)))
       (project-root project)
     default-directory)))

(defun codex-ide--normalize-directory (directory)
  "Return a canonical directory key for DIRECTORY."
  (when directory
    (directory-file-name
     (file-truename (expand-file-name directory)))))

(defun codex-ide--persisted-state-key (&optional session directory)
  "Return the persisted-state key for SESSION or DIRECTORY."
  (codex-ide--normalize-directory
   (or directory
       (and session (codex-ide-session-directory session))
       (codex-ide--get-working-directory))))

(defun codex-ide--project-persisted-state (&optional session directory)
  "Return persisted state plist for SESSION or DIRECTORY."
  (gethash (codex-ide--persisted-state-key session directory)
           codex-ide-persisted-project-state))

(defun codex-ide--set-project-persisted-state (state &optional session directory)
  "Persist STATE plist for SESSION or DIRECTORY."
  (puthash (codex-ide--persisted-state-key session directory)
           state
           codex-ide-persisted-project-state))

(defun codex-ide--project-persisted-get (key &optional session directory)
  "Return persisted value for KEY in SESSION or DIRECTORY state."
  (plist-get (codex-ide--project-persisted-state session directory) key))

(defun codex-ide--project-persisted-put (key value &optional session directory)
  "Store VALUE for KEY in SESSION or DIRECTORY persisted state."
  (let* ((state (copy-sequence (or (codex-ide--project-persisted-state session directory)
                                   '()))))
    (setq state (plist-put state key value))
    (codex-ide--set-project-persisted-state state session directory)
    value))

(defun codex-ide--get-buffer-name ()
  "Return the Codex session buffer name for the current working directory."
  (funcall codex-ide-buffer-name-function
           (codex-ide--get-working-directory)))

(defun codex-ide--get-session ()
  "Return the Codex session associated with the current working directory."
  (gethash (codex-ide--get-working-directory)
           codex-ide--sessions))

(defun codex-ide--get-process ()
  "Return the Codex process associated with the current working directory."
  (when-let ((session (codex-ide--get-session)))
    (codex-ide-session-process session)))

(defun codex-ide--get-default-session-for-current-buffer ()
  "Infer the default Codex session for the current buffer.
Prefer a session buffer's local session object.  Otherwise fall back to the
current buffer's project directory."
  (or (and (boundp 'codex-ide--session)
           (codex-ide-session-p codex-ide--session)
           codex-ide--session)
      (codex-ide--get-session)))

(defun codex-ide--set-session (&optional session)
  "Associate SESSION with the current working directory."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (puthash (codex-ide--get-working-directory)
           session
           codex-ide--sessions))

(defun codex-ide--has-live-sessions-p ()
  "Return non-nil when any tracked Codex session still has a live process."
  (let ((found nil))
    (maphash
     (lambda (_directory session)
       (when (process-live-p (codex-ide-session-process session))
         (setq found t)))
     codex-ide--sessions)
    found))

(define-minor-mode codex-ide-track-active-buffer-mode
  "Globally track the active Emacs file buffer for Codex sessions."
  :global t
  :group 'codex-ide
  (if codex-ide-track-active-buffer-mode
      (progn
        (add-hook 'window-buffer-change-functions #'codex-ide--track-active-buffer)
        (add-hook 'window-selection-change-functions #'codex-ide--track-active-buffer)
        (add-hook 'post-command-hook #'codex-ide--track-active-buffer-post-command))
    (remove-hook 'window-buffer-change-functions #'codex-ide--track-active-buffer)
    (remove-hook 'window-selection-change-functions #'codex-ide--track-active-buffer)
    (remove-hook 'post-command-hook #'codex-ide--track-active-buffer-post-command)))

(defun codex-ide--ensure-active-buffer-tracking ()
  "Enable active buffer tracking for Codex session management."
  (unless codex-ide-track-active-buffer-mode
    (codex-ide-track-active-buffer-mode 1)))

(defun codex-ide--maybe-disable-active-buffer-tracking ()
  "Disable active buffer tracking when no live Codex sessions remain."
  (unless (codex-ide--has-live-sessions-p)
    (codex-ide-track-active-buffer-mode -1)))

(defun codex-ide--cleanup-dead-sessions ()
  "Remove stale sessions from `codex-ide--sessions'."
  (maphash
   (lambda (directory session)
     (unless (process-live-p (codex-ide-session-process session))
       (codex-ide-log-message session "Cleaning up dead session entry for %s" directory)
       (remhash directory codex-ide--sessions)))
   codex-ide--sessions)
  (codex-ide--maybe-disable-active-buffer-tracking))

(defun codex-ide--cleanup-session (&optional session)
  "Drop internal state for SESSION's working directory."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((directory (and session (codex-ide-session-directory session)))
        (stderr-process (and session (codex-ide-session-stderr-process session))))
    (when session
      (codex-ide-log-message session "Cleaning up session state"))
    (when (process-live-p stderr-process)
      (delete-process stderr-process))
    (when session
      (setf (codex-ide-session-stderr-process session) nil))
    (when session
      (remhash session codex-ide--session-metadata))
    (remhash directory codex-ide--sessions)
    (remhash directory codex-ide--active-buffer-contexts)
    (remhash directory codex-ide--active-buffer-objects)
    (remhash directory codex-ide--last-sent-buffer-contexts)
    (codex-ide--maybe-disable-active-buffer-tracking)))

(defun codex-ide--teardown-session (session &optional kill-log-buffer)
  "Stop SESSION and clear its internal state.
When KILL-LOG-BUFFER is non-nil, also kill SESSION's log buffer."
  (when session
    (let ((process (codex-ide-session-process session))
          (stderr-process (codex-ide-session-stderr-process session))
          (log-buffer (codex-ide-session-log-buffer session)))
      (when (process-live-p process)
        (codex-ide-log-message session "Stopping process during session teardown")
        (delete-process process))
      (when (process-live-p stderr-process)
        (delete-process stderr-process))
      (codex-ide--cleanup-session session)
      (when (and kill-log-buffer
                 (buffer-live-p log-buffer))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer log-buffer))))))

(defun codex-ide--handle-session-buffer-killed ()
  "Clean up the owning Codex session when its session buffer is killed."
  (when (and (boundp 'codex-ide--session)
             (codex-ide-session-p codex-ide--session)
             (eq (current-buffer) (codex-ide-session-buffer codex-ide--session)))
    (codex-ide--teardown-session codex-ide--session t)))

(defun codex-ide--cleanup-all-sessions ()
  "Terminate all active Codex sessions."
  (maphash
   (lambda (_directory session)
     (when (process-live-p (codex-ide-session-process session))
       (delete-process (codex-ide-session-process session))))
   codex-ide--sessions))

(add-hook 'kill-emacs-hook #'codex-ide--cleanup-all-sessions)

(defun codex-ide--detect-cli ()
  "Detect whether the Codex CLI is available."
  (setq codex-ide--cli-available
        (condition-case nil
            (eq (call-process codex-ide-cli-path nil nil nil "--version") 0)
          (error nil))))

(defun codex-ide--ensure-cli ()
  "Ensure the Codex CLI is available."
  (unless codex-ide--cli-available
    (codex-ide--detect-cli))
  codex-ide--cli-available)

(defun codex-ide--app-server-command ()
  "Build the `codex app-server` command list."
  (append (list codex-ide-cli-path "app-server" "--listen" "stdio://")
          (codex-ide-mcp-bridge-mcp-config-args)
          (when (not (string-empty-p codex-ide-cli-extra-flags))
            (split-string-shell-command codex-ide-cli-extra-flags))))

(defun codex-ide--display-buffer-in-side-window (buffer)
  "Display BUFFER according to Codex window customizations."
  (codex-ide--remember-buffer-context-before-switch)
  (let ((window
         (if codex-ide-use-side-window
             (let* ((side codex-ide-window-side)
                    (window-parameters '((no-delete-other-windows . t)))
                    (display-buffer-alist
                     `((,(regexp-quote (buffer-name buffer))
                        (display-buffer-in-side-window)
                        (side . ,side)
                        (slot . 0)
                        ,@(when (memq side '(left right))
                            `((window-width . ,codex-ide-window-width)))
                        ,@(when (memq side '(top bottom))
                            `((window-height . ,codex-ide-window-height)))
                        (window-parameters . ,window-parameters)))))
               (display-buffer buffer))
           (display-buffer buffer))))
    (setq codex-ide--last-accessed-buffer buffer)
    (when (and window codex-ide-focus-on-open)
      (select-window window))
    (when (and window
               codex-ide-use-side-window
               (memq codex-ide-window-side '(top bottom)))
      (set-window-text-height window codex-ide-window-height)
      (set-window-dedicated-p window t))
    window))

(defun codex-ide--display-buffer-in-codex-window (buffer)
  "Show BUFFER in an existing Codex window when possible.
Prefer a window already displaying BUFFER.  Otherwise reuse any visible Codex
window by switching it to BUFFER.  Fall back to creating a new Codex window."
  (or (get-buffer-window buffer)
      (when-let ((window
                  (seq-find
                   (lambda (candidate)
                     (codex-ide--session-buffer-p (window-buffer candidate)))
                   (window-list nil 'no-minibuf))))
        (let ((was-dedicated (window-dedicated-p window)))
          (when was-dedicated
            (set-window-dedicated-p window nil))
          (set-window-buffer window buffer)
          (when was-dedicated
            (set-window-dedicated-p window was-dedicated))
          (setq codex-ide--last-accessed-buffer buffer)
          (when codex-ide-focus-on-open
            (select-window window))
          window))
      (codex-ide--display-buffer-in-side-window buffer)))

(defun codex-ide--make-region-writable (start end)
  "Make the region from START to END writable."
  (when (< start end)
    (remove-text-properties start end
                            '(read-only t
                              rear-nonsticky (read-only)
                              front-sticky (read-only)))))

(defconst codex-ide-log-marker-property 'codex-ide-log-marker
  "Text property storing the log marker for transcript text.")

(defconst codex-ide-agent-item-type-property 'codex-ide-agent-item-type
  "Text property storing the originating agent item type for transcript text.")

(defvar codex-ide--current-transcript-log-marker nil
  "Marker for the log line associated with the transcript text being inserted.")

(defvar codex-ide--current-agent-item-type nil
  "Item type associated with the agent transcript text being inserted.")

(defun codex-ide--current-agent-text-properties ()
  "Return text properties for agent-originated transcript text."
  (append
   (when (markerp codex-ide--current-transcript-log-marker)
     (list codex-ide-log-marker-property codex-ide--current-transcript-log-marker))
   (when (stringp codex-ide--current-agent-item-type)
     (list codex-ide-agent-item-type-property codex-ide--current-agent-item-type))))

(defun codex-ide--append-to-buffer (buffer text &optional face properties)
  "Append TEXT to BUFFER as read-only transcript text.
When FACE is non-nil, apply it to the inserted text.
When PROPERTIES is non-nil, it should be a property list applied to the
inserted text."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (moving (= (point) (point-max)))
            start)
        (goto-char (point-max))
        (setq start (point))
        (insert text)
        (when (or face properties)
          (add-text-properties
           start
           (point)
           (append (when face (list 'face face))
                   properties)))
        (codex-ide--freeze-region start (point))
        (when moving
          (goto-char (point-max)))))))

(defun codex-ide--append-agent-text (buffer text &optional face properties)
  "Append agent-originated TEXT to BUFFER with FACE and PROPERTIES."
  (codex-ide--append-to-buffer
   buffer
   text
   face
   (append properties (codex-ide--current-agent-text-properties))))

(defun codex-ide--ensure-output-spacing (buffer)
  "Ensure BUFFER is ready for a new rendered output block."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            start)
        (goto-char (point-max))
        (setq start (point))
        (cond
         ((= (point) (point-min)))
         ((and (eq (char-before (point)) ?\n)
               (save-excursion
                 (forward-char -1)
                 (or (bobp)
                     (eq (char-before (point)) ?\n)))))
         ((eq (char-before (point)) ?\n)
          (insert "\n"))
         (t
          (insert "\n\n")))
        (codex-ide--freeze-region start (point))))))

(defun codex-ide--output-separator-string ()
  "Return the separator rule used between transcript sections."
  (concat (make-string 72 ?-) "\n"))

(defun codex-ide--append-output-separator (buffer)
  "Append a transcript separator rule to BUFFER."
  (codex-ide--append-agent-text
   buffer
   (codex-ide--output-separator-string)
   'codex-ide-output-separator-face))

(defun codex-ide--restored-transcript-separator-string ()
  "Return the separator shown between restored history and the live prompt."
  (let* ((label "[End of restored session]")
         (width (length (string-trim-right (codex-ide--output-separator-string))))
         (padding (max 0 (- width (length label))))
         (left (/ padding 2))
         (right (- padding left)))
    (format "\n%s%s%s\n\n"
            (make-string left ?-)
            label
            (make-string right ?-))))

(defun codex-ide--append-restored-transcript-separator (buffer)
  "Append the restored-history boundary separator to BUFFER."
  (codex-ide--append-agent-text
   buffer
   (codex-ide--restored-transcript-separator-string)
   'codex-ide-output-separator-face))

(defun codex-ide--parse-file-link-target (target)
  "Parse markdown file TARGET into (PATH LINE COLUMN), or nil."
  (cond
   ((string-match "\\`\\(/[^#\n]+\\)#L\\([0-9]+\\)\\(?:C\\([0-9]+\\)\\)?\\'" target)
    (list (match-string 1 target)
          (string-to-number (match-string 2 target))
          (when-let ((column (match-string 3 target)))
            (string-to-number column))))
   ((string-match "\\`\\(/[^:\n]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'" target)
    (list (match-string 1 target)
          (string-to-number (match-string 2 target))
          (when-let ((column (match-string 3 target)))
            (string-to-number column))))
   ((string-prefix-p "/" target)
    (list target nil nil))
   (t nil)))

(defun codex-ide--open-file-link (_button)
  "Open the file link described by text properties at point."
  (interactive)
  (let ((path (get-text-property (point) 'codex-ide-path))
        (line (get-text-property (point) 'codex-ide-line))
        (column (get-text-property (point) 'codex-ide-column)))
    (unless (and path (file-exists-p path))
      (user-error "File does not exist: %s" (or path "unknown")))
    (find-file path)
    (goto-char (point-min))
    (when line
      (forward-line (1- line)))
    (when column
      (move-to-column (max 0 (1- column))))))

(defun codex-ide--clear-markdown-properties (start end)
  "Clear Codex markdown rendering properties between START and END."
  (let ((pos start))
    ;; Only remove properties from regions previously marked as markdown.
    ;; Clearing `face' across the whole agent-message span can wipe faces from
    ;; later non-markdown transcript entries like "* Ran ..." summaries.
    (while (< pos end)
      (let ((next (next-single-property-change pos 'codex-ide-markdown nil end)))
        (when (get-text-property pos 'codex-ide-markdown)
          (remove-text-properties
           pos next
           '(font-lock-face nil
             face nil
             mouse-face nil
             help-echo nil
             keymap nil
             category nil
             button nil
             action nil
             follow-link nil
             display nil
             codex-ide-path nil
             codex-ide-line nil
             codex-ide-column nil
             codex-ide-markdown nil)))
        (setq pos next)))))

(defun codex-ide--normalize-markdown-link-label (label)
  "Return LABEL with markdown code delimiters stripped when present."
  (save-match-data
    (if (string-match "\\``\\([^`\n]+\\)`\\'" label)
        (match-string 1 label)
      label)))

(defun codex-ide--markdown-language-mode (language)
  "Return an Emacs major mode function for fenced code block LANGUAGE."
  (let* ((lang (downcase (string-trim (or language ""))))
         (mode
          (alist-get
           lang
           '(("bash" . sh-mode)
             ("c" . c-mode)
             ("c++" . c++-mode)
             ("cpp" . c++-mode)
             ("elisp" . emacs-lisp-mode)
             ("emacs-lisp" . emacs-lisp-mode)
             ("go" . go-mode)
             ("java" . java-mode)
             ("javascript" . js-mode)
             ("js" . js-mode)
             ("json" . json-mode)
             ("python" . python-mode)
             ("py" . python-mode)
             ("ruby" . ruby-mode)
             ("rust" . rust-mode)
             ("shell" . sh-mode)
             ("sh" . sh-mode)
             ("typescript" . typescript-mode)
             ("ts" . typescript-mode)
             ("tsx" . tsx-ts-mode)
             ("yaml" . yaml-mode)
             ("yml" . yaml-mode))
           nil nil #'string=)))
    (cond
     ((and mode (fboundp mode))
      mode)
     ((string-empty-p lang)
      nil)
     (t
      (let ((candidate (intern-soft (format "%s-mode" lang))))
        (when (fboundp candidate)
          candidate))))))

(defun codex-ide--fontify-code-block-region (start end language)
  "Apply syntax highlighting to region START END using LANGUAGE."
  (when-let ((mode (codex-ide--markdown-language-mode language)))
    (let ((source-buffer (current-buffer))
          (code (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert code)
        (funcall mode)
        (font-lock-ensure (point-min) (point-max))
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let* ((next (next-property-change pos (current-buffer) (point-max)))
                   (face (get-text-property pos 'face))
                   (font-lock-face (get-text-property pos 'font-lock-face))
                   (props (append
                           (when face (list 'face face))
                           (when font-lock-face
                             (list 'font-lock-face font-lock-face))))
                   (target-start (+ start (1- pos)))
                   (target-end (min end (+ start (1- next)))))
              (when props
                (with-current-buffer source-buffer
                  (add-text-properties
                   target-start
                   target-end
                  props)))
              (setq pos next))))))))

(defun codex-ide--render-fenced-code-blocks (start end)
  "Render fenced code blocks between START and END."
  (goto-char start)
  (while (re-search-forward "^```\\([^`\n]*\\)[ \t]*$" end t)
    (let* ((fence-start (match-beginning 0))
           (language (string-trim (or (match-string-no-properties 1) "")))
           (code-start (min (1+ (match-end 0)) end)))
      (when (and (< code-start end)
                 (re-search-forward "^```[ \t]*$" end t))
        (let* ((closing-start (match-beginning 0))
               (closing-end (min (if (eq (char-after (match-end 0)) ?\n)
                                     (1+ (match-end 0))
                                   (match-end 0))
                                 end)))
          (add-text-properties
           fence-start code-start
           '(display ""
             codex-ide-markdown t))
          (add-text-properties
           code-start closing-start
           '(codex-ide-markdown t))
          (codex-ide--fontify-code-block-region code-start closing-start language)
          (add-text-properties
           closing-start closing-end
           '(display ""
             codex-ide-markdown t))
          (goto-char closing-end))))))

(defun codex-ide--render-markdown-region (start end)
  "Apply lightweight markdown rendering between START and END."
  (save-excursion
    (let ((inhibit-read-only t))
      (codex-ide--clear-markdown-properties start end)
      (goto-char start)
      (codex-ide--render-fenced-code-blocks start end)
      (goto-char start)
      (while (re-search-forward "\\(\\[\\([^]\n]+\\)\\](\\(/[^)\n]+\\))\\)" end t)
        (unless (or (get-text-property (match-beginning 1) 'codex-ide-markdown)
                    (get-text-property (1- (match-end 1)) 'codex-ide-markdown))
          (let* ((match-start (match-beginning 1))
                 (match-end (match-end 1))
                 (label (match-string-no-properties 2))
                 (display-label (codex-ide--normalize-markdown-link-label label))
                 (target (match-string-no-properties 3))
                 (parsed (codex-ide--parse-file-link-target target)))
            (when parsed
              (make-text-button
               match-start match-end
               'action #'codex-ide--open-file-link
               'follow-link t
               'help-echo target
               'face 'link
               'codex-ide-markdown t
               'codex-ide-path (nth 0 parsed)
               'codex-ide-line (nth 1 parsed)
               'codex-ide-column (nth 2 parsed))
              (add-text-properties
               match-start match-end
               `(display ,display-label))))))
      (goto-char start)
      (while (re-search-forward "`\\([^`\n]+\\)`" end t)
        (unless (or (get-text-property (match-beginning 0) 'codex-ide-markdown)
                    (get-text-property (1- (match-end 0)) 'codex-ide-markdown))
          (let ((code-start (match-beginning 1))
                (code-end (match-end 1)))
            (add-text-properties
             code-start code-end
             '(face font-lock-keyword-face
               codex-ide-markdown t))
            (add-text-properties
             (match-beginning 0) code-start
             '(display ""
               codex-ide-markdown t))
            (add-text-properties
             code-end (match-end 0)
             '(display ""
               codex-ide-markdown t))))))))

(defun codex-ide--trim-log-buffer ()
  "Trim the current log buffer to `codex-ide-log-max-lines' lines."
  (when (> codex-ide-log-max-lines 0)
    (let ((line-count (count-lines (point-min) (point-max))))
      (when (> line-count codex-ide-log-max-lines)
        (let ((excess (- line-count codex-ide-log-max-lines))
              (point-marker (copy-marker (point) t))
              (moving (= (point) (point-max))))
          (save-excursion
            (goto-char (point-min))
            (forward-line excess)
            (delete-region (point-min) (point)))
          (if moving
              (goto-char (point-max))
            (goto-char (marker-position point-marker)))
          (set-marker point-marker nil))))))

(defun codex-ide-log-message (session format-string &rest args)
  "Append a formatted log message for SESSION.
FORMAT-STRING and ARGS are passed to `format'."
  (unless (codex-ide-session-p session)
    (error "Invalid Codex session: %S" session))
  (when-let ((buffer (codex-ide--ensure-log-buffer session)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (moving (= (point) (point-max)))
            start)
        (goto-char (point-max))
        (setq start (point))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
        (insert (apply #'format format-string args))
        (insert "\n")
        (codex-ide--trim-log-buffer)
        (when moving
          (goto-char (point-max)))
        (copy-marker start)))))

(defun codex-ide--trace-back-to-log ()
  "Jump to the log line associated with the transcript text at point."
  (interactive)
  (let ((marker (get-text-property (point) codex-ide-log-marker-property)))
    (unless (markerp marker)
      (user-error "No log trace available at point"))
    (unless (buffer-live-p (marker-buffer marker))
      (user-error "The originating log buffer is no longer available"))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (beginning-of-line)))

(defun codex-ide--item-type-at-point ()
  "Return the agent item type property at point.
When called interactively, echo the item type in the minibuffer."
  (interactive)
  (let ((item-type (get-text-property (point) codex-ide-agent-item-type-property)))
    (when (called-interactively-p 'interactive)
      (if item-type
          (message "%s" item-type)
        (user-error "No agent item type at point")))
    item-type))

(defun codex-ide--freeze-region (start end)
  "Make the region from START to END read-only."
  (when (< start end)
    (remove-text-properties start end
                            '(read-only nil
                              rear-nonsticky nil
                              front-sticky nil))
    (add-text-properties start end '(read-only t
                                     rear-nonsticky (read-only)
                                     front-sticky (read-only)))))

(defun codex-ide--delete-input-overlay (session)
  "Delete the active input overlay for SESSION, if any."
  (when-let ((overlay (codex-ide-session-input-overlay session)))
    (delete-overlay overlay)
    (setf (codex-ide-session-input-overlay session) nil)))

(defun codex-ide--style-user-prompt-region (start end)
  "Apply prompt styling to the user prompt region from START to END."
  (when (< start end)
    (add-text-properties start end '(face codex-ide-user-prompt-face))))

(defun codex-ide--session-metadata-get (session key)
  "Return metadata value for KEY associated with SESSION."
  (plist-get (gethash session codex-ide--session-metadata) key))

(defun codex-ide--session-metadata-put (session key value)
  "Store VALUE as metadata KEY for SESSION."
  (let ((metadata (copy-sequence (or (gethash session codex-ide--session-metadata)
                                     '()))))
    (setq metadata (plist-put metadata key value))
    (puthash session metadata codex-ide--session-metadata)
    value))

(defun codex-ide--format-compact-number (value)
  "Format numeric VALUE in a compact human-readable form."
  (cond
   ((not (numberp value)) "?")
   ((>= value 1000000)
    (format "%.1fM" (/ value 1000000.0)))
   ((>= value 1000)
    (format "%.1fk" (/ value 1000.0)))
   (t
    (number-to-string value))))

(defun codex-ide--format-token-usage-summary (token-usage)
  "Return a compact header summary for TOKEN-USAGE."
  (when-let* ((total (alist-get 'total token-usage))
              (window (alist-get 'modelContextWindow token-usage))
              (used (alist-get 'totalTokens total)))
    (let* ((remaining (max 0 (- window used)))
           (remaining-percent
            (if (> window 0)
                (/ (* 100.0 remaining) window)
              0.0))
           (last (or (alist-get 'last token-usage) total))
           (last-input (alist-get 'inputTokens last))
           (last-cached (alist-get 'cachedInputTokens last))
           (last-output (alist-get 'outputTokens last))
           (last-reasoning (alist-get 'reasoningOutputTokens last)))
      (string-join
       (delq nil
             (list
              (format "ctx: %s/%s"
                      (codex-ide--format-compact-number used)
                      (codex-ide--format-compact-number window))
              (format "left: %s (%.0f%%%%)"
                      (codex-ide--format-compact-number remaining)
                      remaining-percent)
              (when (numberp last-input)
                (format "last in:%s" (codex-ide--format-compact-number last-input)))
              (when (numberp last-cached)
                (format "cache:%s" (codex-ide--format-compact-number last-cached)))
              (when (numberp last-output)
                (format "out:%s" (codex-ide--format-compact-number last-output)))
              (when (numberp last-reasoning)
                (format "reason:%s" (codex-ide--format-compact-number last-reasoning)))))
       "  "))))

(defun codex-ide--format-rate-limit-summary (rate-limits)
  "Return a compact header summary for RATE-LIMITS."
  (when-let* ((primary (alist-get 'primary rate-limits))
              (used-percent (alist-get 'usedPercent primary)))
    (format "quota: %s%%%% used%s"
            used-percent
            (if-let ((plan-type (alist-get 'planType rate-limits)))
                (format " (%s)" plan-type)
              ""))))

(defun codex-ide--update-header-line (&optional session)
  "Refresh the header line for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((buffer (codex-ide-session-buffer session)))
    (with-current-buffer buffer
      (let* ((context (with-current-buffer buffer
                        (codex-ide--get-active-buffer-context)))
             (focus (if context
                        (format "%s:%s"
                                (alist-get 'display-file context)
                                (alist-get 'line context))
                      "none"))
             (token-summary
              (codex-ide--format-token-usage-summary
               (codex-ide--session-metadata-get session :token-usage)))
             (rate-limit-summary
              (codex-ide--format-rate-limit-summary
               (codex-ide--session-metadata-get session :rate-limits))))
        (setq header-line-format
              (propertize
               (string-join
                (delq nil
                      (list
                       (format "focus: %s" focus)
                       token-summary
                       rate-limit-summary))
                "  ")
               'face 'codex-ide-header-line-face)))
      (codex-ide--update-mode-line session))))

(defun codex-ide--make-buffer-context (&optional buffer)
  "Build Codex context for BUFFER or the current buffer.
Returns nil for buffers that should not be tracked."
  (when-let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((file-path (buffer-file-name)))
          (when file-path
            (let* ((working-dir (codex-ide--normalize-directory
                                 (codex-ide--get-working-directory)))
                   (expanded-file (expand-file-name file-path))
                   (working-dir-path (file-name-as-directory working-dir)))
              (when (file-in-directory-p expanded-file working-dir-path)
                (let ((line (line-number-at-pos))
                      (column (current-column)))
                  `((file . ,expanded-file)
                    (display-file . ,(file-relative-name expanded-file working-dir-path))
                    (buffer-name . ,(buffer-name))
                    (line . ,line)
                    (column . ,column)
                    (project-dir . ,working-dir)))))))))))

(defun codex-ide--make-explicit-buffer-context (buffer working-dir)
  "Build Codex context for BUFFER using WORKING-DIR, even for non-file buffers.
This is used for explicit prompt submissions where the invoking buffer should
be treated as authoritative context for the next turn."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((working-dir (codex-ide--normalize-directory working-dir))
             (working-dir-path (file-name-as-directory working-dir))
             (file-path (buffer-file-name))
             (expanded-file (and file-path (expand-file-name file-path)))
             (display-file
              (cond
               ((not expanded-file)
                (format "[buffer] %s" (buffer-name)))
               ((file-in-directory-p expanded-file working-dir-path)
                (file-relative-name expanded-file working-dir-path))
               (t
                (abbreviate-file-name expanded-file)))))
        `((file . ,expanded-file)
          (display-file . ,display-file)
          (buffer-name . ,(buffer-name))
          (line . ,(line-number-at-pos))
          (column . ,(current-column))
          (project-dir . ,working-dir))))))

(defun codex-ide--safe-current-buffer ()
  "Return the current buffer, or nil during buffer teardown.
Global hooks can run while the selected buffer is being killed, in which case
`current-buffer' may signal \"Selecting deleted buffer\"."
  (condition-case err
      (current-buffer)
    (error
     (if (string= (error-message-string err) "Selecting deleted buffer")
         nil
       (signal (car err) (cdr err))))))

(defun codex-ide--track-active-buffer (&rest args)
  "Track the active Emacs file buffer for its project.
This cache is maintained even when no Codex session is currently active."
  (when-let* ((buffer (or (car (seq-filter #'bufferp args))
                          (codex-ide--safe-current-buffer)))
              (context (codex-ide--make-buffer-context buffer))
              (working-dir (alist-get 'project-dir context)))
    (puthash working-dir context codex-ide--active-buffer-contexts)
    (puthash working-dir buffer codex-ide--active-buffer-objects)
    (when-let ((session (gethash working-dir codex-ide--sessions)))
      (when (process-live-p (codex-ide-session-process session))
        (codex-ide--update-header-line session)))))

(defun codex-ide--track-active-buffer-post-command ()
  "Track the last focused real file buffer after commands.
This keeps project file context available when switching into the Codex buffer."
  (when-let ((buffer (codex-ide--safe-current-buffer)))
    (when (and (buffer-live-p buffer)
               (not (minibufferp buffer))
               (not (codex-ide--session-buffer-p buffer)))
      (codex-ide--track-active-buffer buffer))))

(defun codex-ide--remember-buffer-context-before-switch (&optional buffer)
  "Capture BUFFER's file context before switching into a Codex buffer.
When BUFFER is nil, use the current buffer."
  (when-let ((target (or buffer (codex-ide--safe-current-buffer))))
    (unless (or (minibufferp target)
                (codex-ide--session-buffer-p target))
      (when-let ((context (codex-ide--make-buffer-context target)))
        (let ((working-dir (alist-get 'project-dir context)))
          (puthash working-dir context codex-ide--active-buffer-contexts)
          (puthash working-dir target codex-ide--active-buffer-objects)
          (when-let ((session (gethash working-dir codex-ide--sessions)))
            (when (process-live-p (codex-ide-session-process session))
              (codex-ide--update-header-line session))))))))

(defun codex-ide--infer-recent-file-context ()
  "Infer the most recently used real file buffer context for the current project."
  (let ((working-dir (codex-ide--get-working-directory)))
    (seq-some
     (lambda (buffer)
       (unless (or (minibufferp buffer)
                   (codex-ide--session-buffer-p buffer))
         (let ((context (codex-ide--make-buffer-context buffer)))
           (when (and context
                      (string= (alist-get 'project-dir context)
                               working-dir))
             context))))
     (buffer-list))))

(defun codex-ide--infer-recent-file-buffer ()
  "Infer the most recently used real file buffer for the current project."
  (let ((working-dir (codex-ide--get-working-directory)))
    (seq-some
     (lambda (buffer)
       (unless (or (minibufferp buffer)
                   (codex-ide--session-buffer-p buffer))
         (let ((context (codex-ide--make-buffer-context buffer)))
           (when (and context
                      (string= (alist-get 'project-dir context)
                               working-dir))
             buffer))))
     (buffer-list))))

(defun codex-ide--get-active-buffer-object ()
  "Return the best available active file buffer for the current project."
  (let* ((working-dir (codex-ide--get-working-directory))
         (buffer (gethash working-dir codex-ide--active-buffer-objects)))
    (cond
     ((buffer-live-p buffer) buffer)
     ((when-let ((inferred (codex-ide--infer-recent-file-buffer)))
        (puthash working-dir inferred codex-ide--active-buffer-objects)
        inferred))
     (t nil))))

(defun codex-ide--get-active-buffer-context ()
  "Return the best available active file context for the current project."
  (let ((working-dir (codex-ide--get-working-directory)))
    (or (gethash working-dir codex-ide--active-buffer-contexts)
        (when-let ((context (codex-ide--infer-recent-file-context)))
          (puthash working-dir context codex-ide--active-buffer-contexts)
          context))))

(defun codex-ide--buffer-selection-context (&optional buffer)
  "Return BUFFER's active region bounds as an alist, or nil.
The return value contains 1-based line numbers and 0-based columns."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          `((start-line . ,(line-number-at-pos start))
            (start-column . ,(save-excursion
                               (goto-char start)
                               (current-column)))
            (end-line . ,(line-number-at-pos end))
            (end-column . ,(save-excursion
                             (goto-char end)
                             (current-column)))))))))

(defun codex-ide--context-with-selected-region (context &optional buffer)
  "Return CONTEXT augmented with BUFFER's active region, when present."
  (if-let ((selection (codex-ide--buffer-selection-context buffer)))
      (append context `((selection . ,selection)))
    context))

(defun codex-ide--format-buffer-context (context)
  "Format CONTEXT for insertion into a Codex prompt."
  (let ((selection (alist-get 'selection context)))
    (format (concat "[Emacs context]\n"
                    "You are Codex running inside Emacs.\n"
                    "Prefer Emacs-aware behavior and treat the active file/buffer context below as authoritative unless I say otherwise.\n"
                    "Last file/buffer focused in Emacs: %s\n"
                    "Buffer: %s\n"
                    "Cursor: line %s, column %s\n"
                    "%s"
                    "Treat references like \"this file\", \"this buffer\", or \"the current file\" as referring to this buffer unless I say otherwise.\n"
                    "[/Emacs context]\n")
            (alist-get 'display-file context)
            (alist-get 'buffer-name context)
            (alist-get 'line context)
            (alist-get 'column context)
            (if selection
                (format "Selected region: line %s, column %s to line %s, column %s\n"
                        (alist-get 'start-line selection)
                        (alist-get 'start-column selection)
                        (alist-get 'end-line selection)
                        (alist-get 'end-column selection))
              ""))))

(defun codex-ide--format-buffer-context-summary (context)
  "Return a compact transcript summary line for CONTEXT."
  (let ((selection (alist-get 'selection context)))
    (string-join
     (delq nil
           (list
            (format "Context: file=%S" (alist-get 'display-file context))
            (format "buffer=%S" (alist-get 'buffer-name context))
            (format "line=%s" (alist-get 'line context))
            (format "column=%s" (alist-get 'column context))
            (when selection
              (format "selection=%S"
                      (format "%s:%s-%s:%s"
                              (alist-get 'start-line selection)
                              (alist-get 'start-column selection)
                              (alist-get 'end-line selection)
                              (alist-get 'end-column selection))))))
     " ")))

(defun codex-ide--insert-input-prompt (&optional session initial-text)
  "Insert a writable `>` prompt for SESSION.
Optionally seed it with INITIAL-TEXT."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (moving (= (point) (point-max)))
              transcript-start
              prompt-start)
          (goto-char (point-max))
          (setq transcript-start (point))
          (unless (or (= (point) (point-min))
                      (bolp))
            (insert "\n"))
          (codex-ide--freeze-region transcript-start (point))
          (codex-ide--delete-input-overlay session)
          (setf (codex-ide-session-input-prompt-start-marker session)
                (copy-marker (point)))
          (setq prompt-start (point))
          (insert (propertize "> " 'face 'codex-ide-user-prompt-face))
          (codex-ide--freeze-region prompt-start (point))
          (setf (codex-ide-session-input-start-marker session)
                (copy-marker (point)))
          (codex-ide--reset-prompt-history-navigation session)
          (when initial-text
            (insert initial-text))
          (codex-ide--make-region-writable
           (marker-position (codex-ide-session-input-start-marker session))
           (point))
          (let ((overlay (make-overlay
                          (marker-position
                           (codex-ide-session-input-prompt-start-marker session))
                          (point-max)
                          buffer
                          nil
                          t)))
            (overlay-put overlay 'face 'codex-ide-user-prompt-face)
            (overlay-put overlay 'evaporate t)
            (setf (codex-ide-session-input-overlay session) overlay))
          (when moving
            (goto-char (point-max)))
          (codex-ide--sync-prompt-minor-mode session))))))

(defun codex-ide--current-input (&optional session)
  "Return the current editable input text for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session))
        (marker (codex-ide-session-input-start-marker session)))
    (unless (and (buffer-live-p buffer) marker)
      "")
    (with-current-buffer buffer
      (string-trim-right
       (buffer-substring-no-properties marker (point-max))))))

(defun codex-ide--replace-current-input (session text)
  "Replace SESSION's editable input region with TEXT."
  (let ((buffer (codex-ide-session-buffer session))
        (marker (codex-ide-session-input-start-marker session)))
    (unless (and (buffer-live-p buffer) marker)
      (user-error "No editable Codex prompt in this buffer"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char marker)
        (delete-region marker (point-max))
        (insert text)
        (goto-char (point-max))))))

(defun codex-ide--reset-prompt-history-navigation (session)
  "Reset history navigation state for SESSION."
  (setf (codex-ide-session-prompt-history-index session) nil
        (codex-ide-session-prompt-history-draft session) nil))

(defun codex-ide--push-prompt-history (session prompt)
  "Record PROMPT in SESSION history."
  (let ((trimmed (string-trim-right prompt)))
    (unless (string-empty-p trimmed)
      (codex-ide--project-persisted-put
       :prompt-history
       (cons trimmed
             (delete trimmed
                     (copy-sequence
                      (or (codex-ide--project-persisted-get :prompt-history session)
                          '()))))
       session)
      (codex-ide--reset-prompt-history-navigation session))))

(defun codex-ide--browse-prompt-history (direction)
  "Browse prompt history in DIRECTION for the current Codex session.
DIRECTION should be -1 for older history and 1 for newer history."
  (let* ((session (codex-ide--session-for-current-project))
         (history (or (codex-ide--project-persisted-get :prompt-history session)
                      '())))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (user-error "Prompt history is only available in the Codex session buffer"))
    (unless (codex-ide-session-input-start-marker session)
      (user-error "No editable Codex prompt in this buffer"))
    (unless history
      (user-error "No prompt history"))
    (let ((index (codex-ide-session-prompt-history-index session)))
      (when (null index)
        (setf (codex-ide-session-prompt-history-draft session)
              (codex-ide--current-input session)))
      (pcase direction
        (-1
         (cond
          ((null index)
           (setq index 0))
          ((>= index (1- (length history)))
           (user-error "End of prompt history"))
          (t
           (setq index (1+ index)))))
        (1
         (setq index (if (or (null index)
                             (<= index 0))
                         nil
                       (1- index)))))
      (if (null index)
          (progn
            (setf (codex-ide-session-prompt-history-index session) nil)
            (codex-ide--replace-current-input session ""))
        (setf (codex-ide-session-prompt-history-index session) index)
        (codex-ide--replace-current-input session (nth index history))))))

(defun codex-ide--goto-prompt-line (direction)
  "Move point to another user prompt line in DIRECTION.
DIRECTION should be -1 for a previous prompt line and 1 for a next prompt line."
  (let ((session (codex-ide--session-for-current-project)))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (user-error "Prompt-line navigation is only available in the Codex session buffer"))
    (save-match-data
      (beginning-of-line)
      (let ((found
             (pcase direction
               (-1
                (when (looking-at-p "> ")
                  (forward-line -1))
                (re-search-backward "^> " nil t))
               (1
                (when (looking-at-p "> ")
                  (forward-line 1))
                (re-search-forward "^> " nil t))
               (_
                (error "Unsupported prompt-line direction: %s" direction)))))
        (unless found
          (user-error (if (< direction 0)
                          "No earlier prompt line"
                        "No later prompt line")))
        (goto-char (match-beginning 0))))))

(defun codex-ide--begin-turn-display (&optional session context-summary)
  "Freeze the current prompt and show immediate pending output for SESSION.
When CONTEXT-SUMMARY is non-nil, insert it beneath the submitted prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              context-start
              spacing-start)
          (when-let ((start (codex-ide-session-input-prompt-start-marker session)))
            (codex-ide--style-user-prompt-region start (point-max))
            (codex-ide--freeze-region start (point-max))
            (when context-summary
              (setq context-start (point-max))
              (goto-char context-start)
              (insert "\n")
              (insert (propertize context-summary
                                  'face
                                  'codex-ide-item-detail-face))
              (codex-ide--freeze-region context-start (point))))
          (codex-ide--delete-input-overlay session)
          (codex-ide--sync-prompt-minor-mode session)
          (goto-char (point-max))
          (setq spacing-start (point))
          (insert "\n\n")
          (codex-ide--freeze-region spacing-start (point))
          (setf (codex-ide-session-output-prefix-inserted session) t
                (codex-ide-session-status session) "running")
          (codex-ide--update-header-line session))))))

(defun codex-ide--item-state (&optional session item-id)
  "Return tracked state for ITEM-ID in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((states (and session (codex-ide-session-item-states session))))
    (gethash item-id states)))

(defun codex-ide--put-item-state (&optional session item-id state)
  "Store STATE for ITEM-ID in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (puthash item-id state (codex-ide-session-item-states session)))

(defun codex-ide--clear-item-state (&optional session item-id)
  "Clear tracked state for ITEM-ID in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((states (and session (codex-ide-session-item-states session))))
    (remhash item-id states)))

(defun codex-ide--shell-command-string (command)
  "Render COMMAND as a shell-like string."
  (cond
   ((stringp command) command)
   ((listp command)
    (mapconcat (lambda (arg)
                 (if (stringp arg)
                     (shell-quote-argument arg)
                   (format "%s" arg)))
               command
               " "))
   (t (format "%s" command))))

(defun codex-ide--item-detail-line (text)
  "Format TEXT as an indented detail line."
  (format "  └ %s\n" text))

(defun codex-ide--item-detail-block (text)
  "Format TEXT as a block of indented detail lines."
  (mapconcat (lambda (line)
               (codex-ide--item-detail-line
                (if (string-empty-p line) "" line)))
             (split-string text "\n")
             ""))

(defun codex-ide--file-change-diff-face (line)
  "Return the face to use for file-change diff LINE."
  (cond
   ((string-prefix-p "@@" line) 'codex-ide-file-diff-hunk-face)
   ((or (string-prefix-p "diff --git" line)
        (string-prefix-p "--- " line)
        (string-prefix-p "+++ " line)
        (string-prefix-p "index " line))
    'codex-ide-file-diff-header-face)
   ((string-prefix-p "+" line) 'codex-ide-file-diff-added-face)
   ((string-prefix-p "-" line) 'codex-ide-file-diff-removed-face)
   (t 'codex-ide-file-diff-context-face)))

(defun codex-ide--render-file-change-diff-text (buffer text)
  "Render file-change diff TEXT into BUFFER."
  (when (and (stringp text)
             (not (string-empty-p text)))
    (let ((trimmed (string-trim-right text)))
      (unless (string-empty-p trimmed)
        (codex-ide--append-agent-text
         buffer
         "diff:\n"
         'codex-ide-item-detail-face)
        (dolist (line (split-string trimmed "\n"))
          (codex-ide--append-agent-text
           buffer
           (concat line "\n")
           (codex-ide--file-change-diff-face line)))))))

(defun codex-ide--file-change-diff-text (item)
  "Extract a human-readable diff string from file-change ITEM."
  (let ((item-diff
         (or (alist-get 'diff item)
             (alist-get 'patch item)
             (alist-get 'output item)
             (alist-get 'text item))))
    (cond
     ((and (stringp item-diff)
           (not (string-empty-p item-diff)))
      item-diff)
     (t
      (string-join
       (delq nil
             (mapcar
              (lambda (change)
                (let ((path (alist-get 'path change))
                      (diff (or (alist-get 'diff change)
                                (alist-get 'patch change)
                                (alist-get 'output change)
                                (alist-get 'text change))))
                  (when (and (stringp diff)
                             (not (string-empty-p diff)))
                    (if (and path
                             (not (string-match-p
                                   (regexp-quote (format "+++ %s" path))
                                   diff)))
                        (format "diff -- %s\n%s" path diff)
                      diff))))
              (or (alist-get 'changes item) '())))
       "\n")))))

(defun codex-ide--summarize-item-start (item)
  "Build a one-line summary for ITEM start notifications."
  (let ((item-type (alist-get 'type item)))
    (pcase item-type
      ("commandExecution"
       (format "Ran %s"
               (codex-ide--shell-command-string (alist-get 'command item))))
      ("webSearch"
       (let* ((action (alist-get 'action item))
              (action-type (alist-get 'type action))
              (queries (delq nil
                             (append (and (alist-get 'query action)
                                          (list (alist-get 'query action)))
                                     (alist-get 'queries action)
                                     (and (alist-get 'query item)
                                          (list (alist-get 'query item))))))
              (query-text (string-join queries " | ")))
         (pcase action-type
           ("openPage"
            (format "Opened page %s" (or (alist-get 'url action) "unknown page")))
           ("findInPage"
            (format "Searched in page %s for %s"
                    (or (alist-get 'url action) "unknown page")
                    (or (alist-get 'pattern action) "")))
           (_
            (if (string-empty-p query-text)
                "Searched the web"
              (format "Searched the web for %s" query-text))))))
      ("mcpToolCall"
       (format "Called %s/%s"
               (or (alist-get 'server item) "mcp")
               (or (alist-get 'tool item) "tool")))
      ("dynamicToolCall"
       (format "Called tool %s" (or (alist-get 'tool item) "tool")))
      ("collabToolCall"
       (format "Delegated with %s" (or (alist-get 'tool item) "collab tool")))
      ("fileChange"
       (let ((count (length (or (alist-get 'changes item) '()))))
         (format "Prepared %d file change%s" count (if (= count 1) "" "s"))))
      ("contextCompaction"
       "Compacted conversation context")
      ("imageView"
       (format "Viewed image %s" (or (alist-get 'path item) "")))
      ("enteredReviewMode"
       "Entered review mode")
      ("exitedReviewMode"
       "Exited review mode")
      (_ nil))))

(defun codex-ide--render-item-start-details (session item)
  "Render detail lines for ITEM in SESSION."
  (let ((buffer (codex-ide-session-buffer session))
        (item-type (alist-get 'type item)))
    (pcase item-type
      ("commandExecution"
       (when-let ((cwd (alist-get 'cwd item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "cwd: %s" (abbreviate-file-name cwd)))
          'codex-ide-item-detail-face)))
      ("webSearch"
       (let* ((action (alist-get 'action item))
              (action-type (alist-get 'type action))
              (queries (delq nil
                             (append (alist-get 'queries action)
                                     (and (alist-get 'query action)
                                          (list (alist-get 'query action)))
                                     (and (alist-get 'query item)
                                          (list (alist-get 'query item)))))))
         (cond
          ((and (equal action-type "findInPage")
                (alist-get 'pattern action))
            (codex-ide--append-agent-text
             buffer
             (codex-ide--item-detail-line
              (format "pattern: %s" (alist-get 'pattern action)))
            'codex-ide-item-detail-face))
          ((> (length queries) 1)
           (dolist (query queries)
             (codex-ide--append-agent-text
              buffer
              (codex-ide--item-detail-line query)
              'codex-ide-item-detail-face))))))
      ("mcpToolCall"
       (when-let ((arguments (alist-get 'arguments item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "args: %s" (json-encode arguments)))
          'codex-ide-item-detail-face)))
      ("dynamicToolCall"
       (when-let ((arguments (alist-get 'arguments item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "args: %s" (json-encode arguments)))
          'codex-ide-item-detail-face)))
      ("fileChange"
       (dolist (change (or (alist-get 'changes item) '()))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "%s %s"
                   (or (alist-get 'kind change) "change")
                   (or (alist-get 'path change) "unknown")))
          'codex-ide-item-detail-face)))
      ("imageView"
       (when-let ((path (alist-get 'path item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line path)
          'codex-ide-item-detail-face))))))

(defun codex-ide--render-item-start (&optional session item)
  "Render a newly started ITEM for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((buffer (codex-ide-session-buffer session))
         (item-id (alist-get 'id item))
         (item-type (alist-get 'type item))
         (summary (codex-ide--summarize-item-start item)))
    (let ((codex-ide--current-agent-item-type item-type))
      (when summary
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* %s\n" summary)
         'codex-ide-item-summary-face)
        (codex-ide--render-item-start-details session item)
        (codex-ide--put-item-state
         session
         item-id
         (list :type item-type
               :summary summary
               :details-rendered t
               :saw-output nil))))))

(defun codex-ide--render-plan-delta (&optional session params)
  "Render a plan delta PARAMS for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((delta (or (alist-get 'delta params) ""))
        (buffer (codex-ide-session-buffer session)))
    (let ((codex-ide--current-agent-item-type "plan"))
      (unless (string-empty-p delta)
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* Plan: %s\n" delta)
         'font-lock-doc-face)))))

(defun codex-ide--render-reasoning-delta (&optional session params)
  "Render a reasoning summary delta PARAMS for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((delta (or (alist-get 'delta params)
                   (alist-get 'text params)
                   ""))
        (buffer (codex-ide-session-buffer session)))
    (let ((codex-ide--current-agent-item-type "reasoning"))
      (unless (string-empty-p delta)
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* Reasoning: %s\n" delta)
         'shadow)))))

(defun codex-ide--render-item-completion (&optional session item)
  "Render any completion-only details for ITEM in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((item-id (alist-get 'id item))
         (buffer (codex-ide-session-buffer session))
         (state (codex-ide--item-state session item-id))
         (item-type (alist-get 'type item))
         (status (alist-get 'status item)))
    (let ((codex-ide--current-agent-item-type item-type))
      (pcase item-type
        ("commandExecution"
         (cond
          ((equal status "failed")
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line
             (format "failed%s"
                     (if-let ((exit-code (alist-get 'exitCode item)))
                         (format " with exit code %s" exit-code)
                       "")))
            'error))
          ((equal status "declined")
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line "declined")
            'warning))))
        ("mcpToolCall"
         (when-let ((error-info (alist-get 'error item)))
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line
             (format "error: %s"
                     (or (alist-get 'message error-info) error-info)))
            'error)))
        ("dynamicToolCall"
         (when (eq (alist-get 'success item) :json-false)
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line "tool call failed")
            'error)))
        ("fileChange"
         (let ((diff-text (codex-ide--file-change-diff-text item))
               (streamed-diff (plist-get state :diff-text)))
           (codex-ide--render-file-change-diff-text
            buffer
            (if (and (stringp diff-text)
                     (not (string-empty-p diff-text)))
                diff-text
              streamed-diff))))
        ("exitedReviewMode"
         (when-let ((review (alist-get 'review item)))
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-block review)
            'codex-ide-item-detail-face)))))
    (codex-ide--clear-item-state session item-id)))

(defun codex-ide--ensure-agent-message-prefix (&optional session item-id)
  "Ensure the assistant message prefix has been inserted for ITEM-ID in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (codex-ide-session-buffer session)))
    (unless (and (equal item-id (codex-ide-session-current-message-item-id session))
                 (codex-ide-session-current-message-prefix-inserted session))
      (unless (codex-ide-session-output-prefix-inserted session)
        (codex-ide--begin-turn-display session))
      (codex-ide--ensure-output-spacing buffer)
      (codex-ide--append-output-separator buffer)
      (codex-ide--append-agent-text buffer "\n")
      (setf (codex-ide-session-current-message-start-marker session)
            (copy-marker (with-current-buffer buffer (point-max))))
      (setf (codex-ide-session-current-message-item-id session) item-id
            (codex-ide-session-current-message-prefix-inserted session) t))))

(defun codex-ide--reopen-input-after-submit-error (&optional session prompt err)
  "Show ERR for SESSION and reopen a prompt seeded with PROMPT."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (setf (codex-ide-session-current-turn-id session) nil
        (codex-ide-session-current-message-item-id session) nil
        (codex-ide-session-current-message-prefix-inserted session) nil
        (codex-ide-session-current-message-start-marker session) nil
        (codex-ide-session-output-prefix-inserted session) nil
        (codex-ide-session-item-states session) (make-hash-table :test 'equal)
        (codex-ide-session-status session) "idle")
  (codex-ide--update-header-line session)
  (codex-ide--append-to-buffer
   (codex-ide-session-buffer session)
   (format "\n[Submit failed] %s\n\n" (error-message-string err))
   'error)
  (codex-ide--insert-input-prompt session prompt))

(defun codex-ide--finish-turn (&optional session closing-note)
  "Reset SESSION after a turn and reopen the prompt.
When CLOSING-NOTE is non-nil, append it before restoring the prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (setf (codex-ide-session-current-turn-id session) nil
          (codex-ide-session-current-message-item-id session) nil
          (codex-ide-session-current-message-prefix-inserted session) nil
          (codex-ide-session-current-message-start-marker session) nil
          (codex-ide-session-output-prefix-inserted session) nil
          (codex-ide-session-item-states session) (make-hash-table :test 'equal)
          (codex-ide-session-interrupt-requested session) nil
          (codex-ide-session-status session) "idle")
    (codex-ide--update-header-line session)
    (when closing-note
      (codex-ide--append-to-buffer buffer (format "\n%s\n" closing-note) 'warning))
    (codex-ide--append-to-buffer buffer "\n\n")
    (codex-ide--insert-input-prompt session)))

(defun codex-ide--context-payload-for-prompt ()
  "Return context payload metadata for the current prompt, or nil.
The result is an alist with `formatted' and `summary' entries."
  (let ((working-dir (codex-ide--get-working-directory)))
    (when-let* ((context-buffer (or codex-ide--prompt-origin-buffer
                                    (codex-ide--get-active-buffer-object)))
                (context (or (and codex-ide--prompt-origin-buffer
                                  (codex-ide--make-explicit-buffer-context
                                   codex-ide--prompt-origin-buffer
                                   working-dir))
                             (codex-ide--get-active-buffer-context))))
      (let* ((context-with-selection
              (codex-ide--context-with-selected-region
               context
               context-buffer))
             (formatted-context
              (codex-ide--format-buffer-context context-with-selection))
             (context-summary
              (codex-ide--format-buffer-context-summary context-with-selection)))
        (pcase codex-ide-include-active-buffer-context
          ('always
           (puthash working-dir context codex-ide--last-sent-buffer-contexts)
           `((formatted . ,formatted-context)
             (summary . ,context-summary)))
          ('when-changed
           (unless (equal context (gethash working-dir codex-ide--last-sent-buffer-contexts))
             (puthash working-dir context codex-ide--last-sent-buffer-contexts)
             `((formatted . ,formatted-context)
               (summary . ,context-summary))))
          (_ nil))))))

(defun codex-ide--get-buffer-context-for-prompt ()
  "Return the current buffer context string for the current project, or nil."
  (alist-get 'formatted (codex-ide--context-payload-for-prompt)))

(defun codex-ide--next-request-id (&optional session)
  "Return the next request id for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (setf (codex-ide-session-request-counter session)
        (1+ (or (codex-ide-session-request-counter session) 0))))

(defun codex-ide--jsonrpc-send (&optional session payload)
  "Send PAYLOAD to SESSION as newline-delimited JSON."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((process (codex-ide-session-process session)))
    (unless (process-live-p process)
      (error "Codex app-server process is not running"))
    (codex-ide-log-message
     session
     "Sending JSON-RPC payload: %s"
     (json-encode payload))
    (process-send-string process (concat (json-encode payload) "\n"))))

(defun codex-ide--jsonrpc-send-response (&optional session id result)
  "Send a JSON-RPC RESULT response with ID for SESSION."
  (codex-ide--jsonrpc-send session `((id . ,id) (result . ,result))))

(defun codex-ide--jsonrpc-send-error (&optional session id code message)
  "Send a JSON-RPC error response for SESSION."
  (codex-ide--jsonrpc-send
   session
   `((id . ,id)
     (error . ((code . ,code)
               (message . ,message))))))

(defun codex-ide--request-sync (&optional session method params)
  "Send METHOD with PARAMS to SESSION and wait for the response."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((id (codex-ide--next-request-id session))
         (done nil)
         (result nil)
         (err nil)
         (pending (codex-ide-session-pending-requests session))
         (deadline (+ (float-time) codex-ide-request-timeout)))
    (codex-ide-log-message session "Starting synchronous request %s (id=%s)" method id)
    (puthash id
             (lambda (response-result response-error)
               (setq result response-result
                     err response-error
                     done t))
             pending)
    (codex-ide--jsonrpc-send session `((jsonrpc . "2.0")
                                       (id . ,id)
                                       (method . ,method)
                                       (params . ,params)))
    (while (and (not done)
                (process-live-p (codex-ide-session-process session))
                (< (float-time) deadline))
      (accept-process-output (codex-ide-session-process session) 0.1))
    (remhash id pending)
    (cond
     (err
      (codex-ide-log-message session "Request %s (id=%s) failed: %S" method id err)
      (error "Codex app-server request %s failed: %s" method err))
     ((not done)
      (codex-ide-log-message session "Request %s (id=%s) timed out" method id)
      (error "Timed out waiting for %s" method))
     (t
      (codex-ide-log-message session "Request %s (id=%s) completed" method id)
      result))))

(defun codex-ide--thread-start-params ()
  "Build `thread/start` params for the current working directory."
  (let ((working-dir (codex-ide--get-working-directory)))
    (delq nil
          `((cwd . ,working-dir)
          (approvalPolicy . ,codex-ide-approval-policy)
          (sandbox . ,codex-ide-sandbox-mode)
          (personality . ,codex-ide-personality)
          ,@(when codex-ide-model
              `((model . ,codex-ide-model)))))))

(defun codex-ide--thread-resume-params (thread-id)
  "Build `thread/resume` params for THREAD-ID in the current working directory."
  (let ((working-dir (codex-ide--get-working-directory)))
    (delq nil
          `((threadId . ,thread-id)
          (cwd . ,working-dir)
          (approvalPolicy . ,codex-ide-approval-policy)
          (sandbox . ,codex-ide-sandbox-mode)
          (personality . ,codex-ide-personality)
          ,@(when codex-ide-model
              `((model . ,codex-ide-model)))))))

(defun codex-ide--thread-read-params (thread-id &optional include-turns)
  "Build `thread/read` params for THREAD-ID.
When INCLUDE-TURNS is non-nil, request the stored turn history too."
  (delq nil
        `((threadId . ,thread-id)
          ,@(when include-turns
              '((includeTurns . t))))))

(defun codex-ide--read-thread (&optional session thread-id include-turns)
  "Read stored metadata for THREAD-ID using SESSION.
When INCLUDE-TURNS is non-nil, request the stored turn history too."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (unless (and (stringp thread-id)
               (not (string-empty-p thread-id)))
    (error "Invalid thread id: %S" thread-id))
  (codex-ide--request-sync
   session
   "thread/read"
   (codex-ide--thread-read-params thread-id include-turns)))

(defun codex-ide--extract-thread-id (result)
  "Extract the thread id from RESULT."
  (alist-get 'id (alist-get 'thread result)))

(defun codex-ide--thread-read-turns (thread-read)
  "Return turn history from THREAD-READ."
  (or (alist-get 'turns thread-read)
      (alist-get 'turns (alist-get 'thread thread-read))
      []))

(defun codex-ide--thread-read--message-text (message)
  "Extract readable text from a MESSAGE-like alist."
  (let ((text (or (alist-get 'text message)
                  (alist-get 'message message)
                  (alist-get 'prompt message)
                  (alist-get 'summary message)
                  (alist-get 'content message))))
    (cond
     ((stringp text)
      text)
     ((vectorp text)
      (string-join
       (delq nil
             (mapcar #'codex-ide--thread-read--message-text
                     (append text nil)))
       "\n"))
     ((listp text)
      (string-join
       (delq nil
             (mapcar #'codex-ide--thread-read--message-text text))
       "\n"))
     (t nil))))

(defun codex-ide--thread-read--item-kind (item)
  "Return a normalized kind symbol for thread ITEM."
  (let ((type (alist-get 'type item)))
    (cond
     ((member type '("userMessage" userMessage "user" user))
      'user)
     ((member type '("agentMessage" agentMessage
                     "assistantMessage" assistantMessage
                     "assistant" assistant))
      'assistant)
     ((member (alist-get 'role item) '("user" user))
      'user)
     ((member (alist-get 'role item) '("assistant" assistant))
      'assistant)
     ((member (alist-get 'source item) '("user" user))
      'user)
     ((member (alist-get 'source item) '("assistant" assistant))
      'assistant)
     ((member (alist-get 'author item) '("user" user))
      'user)
     ((member (alist-get 'author item) '("assistant" assistant))
      'assistant)
     (t nil))))

(defun codex-ide--thread-read--collect-item-texts (items &optional kind)
  "Collect readable texts from ITEMS, optionally filtered by KIND."
  (let ((sequence (cond
                   ((vectorp items) (append items nil))
                   ((listp items) items)
                   (t nil)))
        texts)
    (dolist (item sequence (nreverse texts))
      (when (and (listp item)
                 (or (null kind)
                     (eq (codex-ide--thread-read--item-kind item) kind)))
        (let ((text (codex-ide--thread-read--message-text item)))
          (when (and (stringp text)
                     (not (string-empty-p (string-trim text))))
            (push text texts)))))))

(defun codex-ide--thread-read-user-text (turn)
  "Return a compact user prompt text for TURN, if available."
  (or (let ((text (alist-get 'userMessage turn)))
        (when (stringp text) text))
      (let ((text (alist-get 'prompt turn)))
        (when (stringp text) text))
      (let ((texts (codex-ide--thread-read--collect-item-texts (alist-get 'input turn))))
        (when texts
          (string-join texts "\n")))
      (let ((texts (codex-ide--thread-read--collect-item-texts
                    (alist-get 'messages turn) 'user)))
        (when texts
          (car (last texts))))
      (let ((texts (codex-ide--thread-read--collect-item-texts
                    (alist-get 'items turn) 'user)))
        (when texts
          (car (last texts))))))

(defun codex-ide--thread-read-agent-text (turn)
  "Return a compact agent summary text for TURN, if available."
  (or (let ((text (alist-get 'assistantMessage turn)))
        (when (stringp text) text))
      (let ((text (alist-get 'summary turn)))
        (when (stringp text) text))
      (let ((texts (codex-ide--thread-read--collect-item-texts
                    (alist-get 'messages turn) 'assistant)))
        (when texts
          (car (last texts))))
      (let ((texts (codex-ide--thread-read--collect-item-texts
                    (alist-get 'items turn) 'assistant)))
        (when texts
          (car (last texts))))))

(defun codex-ide--thread-read-format-snippet (text)
  "Format TEXT as a compact single-line snippet."
  (when (stringp text)
    (let* ((trimmed (string-trim text))
           (single-line (replace-regexp-in-string "[ \t\n\r]+" " " trimmed)))
      (unless (string-empty-p single-line)
        (truncate-string-to-width single-line 120 nil nil t)))))

(defun codex-ide--strip-emacs-context-prefix (text)
  "Remove any leading `[Emacs context]` block from TEXT."
  (let ((context-end "[/Emacs context]"))
    (if (and (stringp text)
             (string-match (regexp-quote context-end) text))
        (string-trim (substring text (match-end 0)))
      text)))

(defun codex-ide--thread-read-display-user-text (text)
  "Normalize stored user TEXT for transcript display."
  (when (stringp text)
    (let ((display-text (string-trim (codex-ide--strip-emacs-context-prefix text))))
      (unless (string-empty-p display-text)
        display-text))))

(defun codex-ide--thread-read-items (turn)
  "Return ordered transcript items for TURN."
  (or (alist-get 'items turn)
      (alist-get 'messages turn)
      []))

(defun codex-ide--append-restored-user-message (session text)
  "Append restored user TEXT to SESSION like a submitted prompt."
  (let ((buffer (codex-ide-session-buffer session))
        (display-text (codex-ide--thread-read-display-user-text text)))
    (when (and (buffer-live-p buffer)
               (stringp display-text)
               (not (string-empty-p display-text)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              prompt-start)
          (goto-char (point-max))
          (cond
           ((= (point) (point-min)))
           ((and (eq (char-before (point)) ?\n)
                 (save-excursion
                   (forward-char -1)
                   (or (bobp)
                       (eq (char-before (point)) ?\n)))))
           ((eq (char-before (point)) ?\n)
            (insert "\n"))
           (t
            (insert "\n\n")))
          (setq prompt-start (point))
          (insert (propertize "> " 'face 'codex-ide-user-prompt-face))
          (insert display-text)
          (codex-ide--style-user-prompt-region prompt-start (point))
          (codex-ide--freeze-region prompt-start (point))
          (insert "\n\n")
          (codex-ide--freeze-region prompt-start (point))))
      t)))

(defun codex-ide--append-restored-agent-message (session item)
  "Append restored agent ITEM to SESSION like live agent output."
  (let* ((buffer (codex-ide-session-buffer session))
         (item-id (or (alist-get 'id item) "restored-agent-message"))
         (text (codex-ide--thread-read--message-text item)))
    (when (and (buffer-live-p buffer)
               (stringp text)
               (not (string-empty-p (string-trim text))))
      (let ((codex-ide--current-agent-item-type "agentMessage"))
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-output-separator buffer)
        (codex-ide--append-agent-text buffer "\n")
        (setf (codex-ide-session-current-message-start-marker session)
              (copy-marker (with-current-buffer buffer (point-max))))
        (setf (codex-ide-session-current-message-item-id session) item-id
              (codex-ide-session-current-message-prefix-inserted session) t)
        (codex-ide--append-agent-text buffer text)
        (when-let ((start (codex-ide-session-current-message-start-marker session)))
          (with-current-buffer buffer
            (codex-ide--render-markdown-region start (point-max)))))
      t)))

(defun codex-ide--replay-thread-read-turn (session turn)
  "Replay stored TURN into SESSION.
Return non-nil when any transcript content was restored."
  (let ((items (append (codex-ide--thread-read-items turn) nil))
        (restored nil))
    (dolist (item items restored)
      (pcase (codex-ide--thread-read--item-kind item)
        ('user
         (setq restored
               (or (codex-ide--append-restored-user-message
                    session
                    (codex-ide--thread-read--message-text item))
                   restored))
         (setf (codex-ide-session-output-prefix-inserted session) t
               (codex-ide-session-current-message-item-id session) nil
               (codex-ide-session-current-message-prefix-inserted session) nil
               (codex-ide-session-current-message-start-marker session) nil))
        ('assistant
         (setq restored
               (or (codex-ide--append-restored-agent-message session item)
                   restored)))
        (_ nil)))))

(defun codex-ide--restore-thread-read-transcript (&optional session thread-read)
  "Replay a stored transcript from THREAD-READ into SESSION.
Signal an error when THREAD-READ lacks replayable transcript items."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((limit (max 0 codex-ide-resume-summary-turn-limit))
         (turns (append (codex-ide--thread-read-turns thread-read) nil))
         (recent-turns (cond
                        ((<= limit 0) nil)
                        ((> (length turns) limit) (last turns limit))
                        (t turns)))
         (restored nil))
    (unless recent-turns
      (error "Stored thread has no replayable turns"))
    (dolist (turn recent-turns restored)
      (setq restored
            (or (codex-ide--replay-thread-read-turn session turn)
                restored)))
    (unless restored
      (error
       (concat
        "Stored thread transcript could not be replayed. "
        "Expected replayable userMessage/agentMessage turn items.")))
    (when restored
      (codex-ide--append-to-buffer (codex-ide-session-buffer session) "\n")
      (codex-ide--append-restored-transcript-separator
       (codex-ide-session-buffer session)))
    (setf (codex-ide-session-current-turn-id session) nil
          (codex-ide-session-current-message-item-id session) nil
          (codex-ide-session-current-message-prefix-inserted session) nil
          (codex-ide-session-current-message-start-marker session) nil
          (codex-ide-session-output-prefix-inserted session) nil
          (codex-ide-session-item-states session) (make-hash-table :test 'equal))
    restored))

(defun codex-ide--format-thread-updated-at (updated-at)
  "Format UPDATED-AT for thread labels."
  (cond
   ((numberp updated-at)
    (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                        (seconds-to-time updated-at)))
   ((stringp updated-at) updated-at)
   (t "")))

(defun codex-ide--thread-choice-preview (value)
  "Format thread preview VALUE for completion labels."
  (let* ((text (or value ""))
         (stripped (codex-ide--strip-emacs-context-prefix text)))
    (string-trim
     (if (and (stringp text)
              (string-prefix-p "[Emacs context]" text)
              (equal stripped text))
         ""
       stripped))))

(defun codex-ide--thread-choice-short-id (thread)
  "Return a short id for THREAD."
  (let ((thread-id (alist-get 'id thread)))
    (substring thread-id 0 (min 8 (length thread-id)))))

(defun codex-ide--thread-choice-candidates (threads)
  "Return completion candidates alist for THREADS."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (thread threads)
      (let* ((raw (or (alist-get 'name thread)
                      (alist-get 'preview thread)
                      "Untitled"))
             (preview (codex-ide--thread-choice-preview raw))
             (candidate (if (string-empty-p preview) "Untitled" preview)))
        (puthash candidate (1+ (gethash candidate counts 0)) counts)))
    (mapcar
     (lambda (thread)
       (let* ((raw (or (alist-get 'name thread)
                       (alist-get 'preview thread)
                       "Untitled"))
              (preview (codex-ide--thread-choice-preview raw))
              (candidate (if (string-empty-p preview) "Untitled" preview)))
         (cons (if (> (gethash candidate counts 0) 1)
                   (format "%s [%s]" candidate
                           (codex-ide--thread-choice-short-id thread))
                 candidate)
               thread)))
     threads)))

(defun codex-ide--thread-choice-affixation (candidates choices)
  "Return affixation data for CANDIDATES using CHOICES."
  (mapcar
   (lambda (candidate)
     (let ((thread (cdr (assoc candidate choices))))
       (list candidate
             (format "%s " (codex-ide--format-thread-updated-at
                            (alist-get 'createdAt thread)))
             (format " [%s]" (codex-ide--thread-choice-short-id thread)))))
   candidates))

(defun codex-ide--list-threads (&optional session)
  "List threads for the current working directory using SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((working-dir (codex-ide--get-working-directory))
         (result (codex-ide--request-sync
                  session
                  "thread/list"
                  `((cwd . ,working-dir)
                    (limit . 50)
                    (sortKey . "updated_at")
                    )))
         (data (alist-get 'data result)))
    (append data nil)))

(defun codex-ide--pick-thread (&optional session omit-thread-id)
  "Prompt to select a thread for the current working directory using SESSION.
When OMIT-THREAD-ID is non-nil, exclude that thread from the choices."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((working-dir (codex-ide--get-working-directory))
         (threads (seq-remove
                   (lambda (thread)
                     (equal (alist-get 'id thread) omit-thread-id))
                   (codex-ide--list-threads session)))
         (choices (codex-ide--thread-choice-candidates threads))
         (completion-extra-properties
          `(:affixation-function
            ,(lambda (candidates)
               (codex-ide--thread-choice-affixation candidates choices))
            :display-sort-function identity
            :cycle-sort-function identity)))
    (unless choices
      (user-error "%s for %s"
                  (if omit-thread-id
                      "No other Codex threads found"
                    "No Codex threads found")
                  (abbreviate-file-name working-dir)))
    (cdr (assoc (completing-read "Resume Codex thread: " choices nil t)
                choices))))

(defun codex-ide--latest-thread (&optional session)
  "Return the most recent thread for the current working directory using SESSION."
  (car (codex-ide--list-threads session)))

(defun codex-ide--initialize-session (&optional session)
  "Initialize SESSION with the app-server."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (codex-ide-log-message session "Initializing app-server session")
  (codex-ide--request-sync
   session
   "initialize"
   `((clientInfo . ((name . "emacs")
                    (version . "0.2.0")))
     (capabilities . ((experimentalApi . t)
                      ,@(codex-ide-mcp-elicitation-capabilities)))))
  (setf (codex-ide-session-status session) "idle")
  (codex-ide-log-message session "Initialization complete")
  (codex-ide--update-header-line session))

(defun codex-ide--create-process-session ()
  "Create a new app-server-backed session for the current working directory."
  (let ((working-dir (codex-ide--get-working-directory)))
    (let* ((buffer (get-buffer-create (codex-ide--get-buffer-name)))
           (process-connection-type nil)
           (session (make-codex-ide-session
                     :directory working-dir
                     :buffer buffer
                     :log-buffer nil
                     :request-counter 0
                     :pending-requests (make-hash-table :test 'equal)
                     :item-states (make-hash-table :test 'equal)
                     :prompt-history-index nil
                     :prompt-history-draft nil
                     :partial-line ""
                     :status "starting"))
           (stderr-process (make-pipe-process
                            :name (format "codex-ide-stderr[%s]"
                                          (file-name-nondirectory
                                           (directory-file-name working-dir)))
                            :buffer nil
                            :coding 'utf-8-unix
                            :noquery t
                            :filter #'codex-ide--stderr-filter))
           (process (make-process
                     :name (format "codex-ide[%s]"
                                   (file-name-nondirectory
                                    (directory-file-name working-dir)))
                     :buffer nil
                     :command (codex-ide--app-server-command)
                     :coding 'utf-8-unix
                     :filter #'codex-ide--process-filter
                     :sentinel #'codex-ide--process-sentinel
                     :stderr stderr-process)))
      (setf (codex-ide-session-process session) process)
      (setf (codex-ide-session-stderr-process session) stderr-process)
      (process-put process 'codex-session session)
      (process-put stderr-process 'codex-session session)
      (with-current-buffer buffer
        (codex-ide-session-mode)
        (setq-local default-directory working-dir)
        (setq-local codex-ide--session session)
        (add-hook 'kill-buffer-hook #'codex-ide--handle-session-buffer-killed nil t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Codex session for %s\n\n"
                          (abbreviate-file-name working-dir)))
          (codex-ide--freeze-region (point-min) (point-max))))
      (codex-ide--ensure-log-buffer session)
      (set-process-query-on-exit-flag process nil)
      (set-process-query-on-exit-flag stderr-process nil)
      (with-current-buffer buffer
        (codex-ide--set-session session))
      (codex-ide-log-message
       session
       "Created session buffer %s and log buffer %s"
       (buffer-name buffer)
       (buffer-name (codex-ide-session-log-buffer session)))
      (codex-ide-log-message
       session
       "Starting process: %s"
       (string-join (codex-ide--app-server-command) " "))
      session)))

(defun codex-ide--start-session (&optional mode)
  "Start a Codex session for the current project.
MODE can be nil or `new', `continue', or `resume'."
  (unless (codex-ide--ensure-cli)
    (user-error "Codex CLI not available. Install it and ensure it is on PATH"))
  (codex-ide--cleanup-dead-sessions)
  (codex-ide--ensure-active-buffer-tracking)
  (codex-ide-mcp-bridge-prompt-to-enable)
  (codex-ide-mcp-bridge-ensure-server)
  (let* ((working-dir (codex-ide--get-working-directory))
         (existing-session (codex-ide--get-session))
         (existing-buffer (and existing-session
                               (codex-ide-session-buffer existing-session)))
         (resume-thread nil))
    (when (and existing-session
               (not (buffer-live-p existing-buffer)))
      (codex-ide--teardown-session existing-session t)
      (setq existing-session nil
            existing-buffer nil))
    (when (and (eq (or mode 'new) 'resume)
               existing-session
               (process-live-p (codex-ide-session-process existing-session))
               (buffer-live-p existing-buffer))
      (setq resume-thread
            (with-current-buffer existing-buffer
              (codex-ide--pick-thread
               existing-session
               (codex-ide-session-thread-id existing-session))))
      (codex-ide--teardown-session existing-session t)
      (setq existing-session nil
            existing-buffer nil))
    (if (and existing-session
             (process-live-p (codex-ide-session-process existing-session))
             (buffer-live-p existing-buffer))
        (codex-ide--toggle-existing-window existing-buffer)
      (let ((session (codex-ide--create-process-session)))
        (condition-case err
            (progn
              (codex-ide-log-message session "Starting session in mode %s" (or mode 'new))
              (codex-ide-mcp-bridge-ensure-server)
              (codex-ide--initialize-session session)
              (pcase (or mode 'new)
                ('new
                 (let ((result (codex-ide--request-sync
                                session
                                "thread/start"
                                (with-current-buffer (codex-ide-session-buffer session)
                                  (codex-ide--thread-start-params)))))
                   (setf (codex-ide-session-thread-id session)
                         (codex-ide--extract-thread-id result))
                   (codex-ide-log-message
                    session
                    "Started new thread %s"
                    (codex-ide-session-thread-id session))))
                ('continue
                 (let* ((thread (or (with-current-buffer (codex-ide-session-buffer session)
                                      (codex-ide--latest-thread session))
                                    (user-error "No Codex threads found for %s"
                                                (abbreviate-file-name working-dir))))
                        (thread-id (alist-get 'id thread))
                        (thread-read
                         (condition-case err
                             (codex-ide--read-thread session thread-id t)
                           (error
                            (codex-ide-log-message
                             session
                             "Unable to read stored thread %s before continue: %s"
                             thread-id
                             (error-message-string err))
                            nil))))
                   (codex-ide--request-sync
                    session
                    "thread/resume"
                    (with-current-buffer (codex-ide-session-buffer session)
                   (codex-ide--thread-resume-params thread-id)))
                   (setf (codex-ide-session-thread-id session) thread-id)
                   (codex-ide-log-message session "Continued thread %s" thread-id)
                   (when thread-read
                     (codex-ide--restore-thread-read-transcript
                      session thread-read))))
                ('resume
                 (let* ((thread (or resume-thread
                                    (with-current-buffer (codex-ide-session-buffer session)
                                      (codex-ide--pick-thread session))))
                        (thread-id (alist-get 'id thread))
                        (thread-read
                         (condition-case err
                             (codex-ide--read-thread session thread-id t)
                           (error
                            (codex-ide-log-message
                             session
                             "Unable to read stored thread %s before resume: %s"
                             thread-id
                             (error-message-string err))
                            nil))))
                   (codex-ide--request-sync
                    session
                    "thread/resume"
                    (with-current-buffer (codex-ide-session-buffer session)
                   (codex-ide--thread-resume-params thread-id)))
                   (setf (codex-ide-session-thread-id session) thread-id)
                   (codex-ide-log-message session "Resumed thread %s" thread-id)
                   (when thread-read
                     (codex-ide--restore-thread-read-transcript
                      session thread-read)))))
              (setf (codex-ide-session-status session) "idle")
              (codex-ide--update-header-line session)
              (codex-ide--display-buffer-in-side-window (codex-ide-session-buffer session))
              (codex-ide--track-active-buffer)
              (codex-ide--insert-input-prompt session)
              (message "Codex started in %s"
                       (file-name-nondirectory (directory-file-name working-dir)))
              session)
          (error
           (codex-ide-log-message session "Session startup failed: %s" (error-message-string err))
           (when (process-live-p (codex-ide-session-process session))
             (delete-process (codex-ide-session-process session)))
           (when (buffer-live-p (codex-ide-session-buffer session))
             (kill-buffer (codex-ide-session-buffer session)))
           (codex-ide--cleanup-session session)
           (signal (car err) (cdr err)))
          (quit
           (codex-ide-log-message session "Session startup aborted")
           (when (process-live-p (codex-ide-session-process session))
             (delete-process (codex-ide-session-process session)))
           (when (buffer-live-p (codex-ide-session-buffer session))
             (kill-buffer (codex-ide-session-buffer session)))
           (codex-ide--cleanup-session session)
           (signal 'quit nil)))))))

(defun codex-ide--toggle-existing-window (buffer)
  "Toggle BUFFER visibility."
  (if-let ((window (get-buffer-window buffer)))
      (progn
        (setq codex-ide--last-accessed-buffer buffer)
        (delete-window window)
        (message "Codex window hidden"))
    (codex-ide--remember-buffer-context-before-switch)
    (codex-ide--display-buffer-in-side-window buffer)
    (message "Codex window shown")))

(defun codex-ide--handle-response (&optional session message)
  "Handle a JSON-RPC response MESSAGE for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((id (alist-get 'id message))
         (pending (gethash id (codex-ide-session-pending-requests session))))
    (codex-ide-log-message
     session
     "Received response for id=%s%s"
     id
     (if (alist-get 'error message) " with error" ""))
    (when pending
      (funcall pending
               (alist-get 'result message)
               (alist-get 'error message)))))

(defun codex-ide--approval-decision (prompt choices)
  "Prompt the user with PROMPT and return one of CHOICES.
CHOICES is an alist of labels to returned values."
  (cdr (assoc (completing-read prompt choices nil t) choices)))

(defun codex-ide--auto-approve-emacs-bridge-request-p (params)
  "Return non-nil when PARAMS should bypass user approval for the Emacs bridge."
  (and (fboundp 'codex-ide-mcp-bridge-request-exempt-from-approval-p)
       (codex-ide-mcp-bridge-request-exempt-from-approval-p params)))

(defun codex-ide--format-command-approval-prompt (command params)
  "Build a command approval prompt for COMMAND using PARAMS."
  (let ((reason (alist-get 'reason params))
        (amendment (alist-get 'proposedExecpolicyAmendment params)))
    (concat
     (format "Codex command approval: %s" command)
     (if reason
         (format " (%s)" reason)
       "")
     (if amendment
         (format " [proposed prefix: %s]"
                 (mapconcat #'identity amendment " "))
       "")
     " ")))

(defun codex-ide--command-approval-choices (params)
  "Build completion choices for a command approval request from PARAMS."
  (let ((amendment (alist-get 'proposedExecpolicyAmendment params)))
    (append
     '(("accept" . "accept")
       ("accept for session" . "acceptForSession"))
     (when amendment
       `((,(format "accept and allow prefix (%s)"
                   (mapconcat #'identity amendment " "))
          . ,`((acceptWithExecpolicyAmendment
                . ((execpolicy_amendment . ,amendment)))))))
     '(("decline" . "decline")
       ("cancel turn" . "cancel")))))

(defun codex-ide--handle-command-approval (&optional session id params)
  "Handle a command approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (run-at-time
   0 nil
   (lambda ()
     (let* ((command (or (alist-get 'command params) "unknown command"))
            (decision (if (codex-ide--auto-approve-emacs-bridge-request-p params)
                          "acceptForSession"
                        (codex-ide--approval-decision
                         (codex-ide--format-command-approval-prompt command params)
                         (codex-ide--command-approval-choices params)))))
       (codex-ide-log-message
        session
        "Command approval for %s resolved as %s"
        command
        decision)
       (codex-ide--jsonrpc-send-response session id `((decision . ,decision)))))))

(defun codex-ide--handle-file-change-approval (&optional session id params)
  "Handle a file-change approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (run-at-time
   0 nil
   (lambda ()
     (let* ((reason (or (alist-get 'reason params) "approve file changes"))
            (decision (if (codex-ide--auto-approve-emacs-bridge-request-p params)
                          "acceptForSession"
                        (codex-ide--approval-decision
                         (format "Codex file-change approval: %s " reason)
                         '(("accept" . "accept")
                           ("accept for session" . "acceptForSession")
                           ("decline" . "decline")
                           ("cancel turn" . "cancel"))))))
       (codex-ide-log-message
        session
        "File-change approval for %s resolved as %s"
        reason
        decision)
       (codex-ide--jsonrpc-send-response session id `((decision . ,decision)))))))

(defun codex-ide--handle-permissions-approval (&optional session id params)
  "Handle a permissions approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (run-at-time
   0 nil
   (lambda ()
     (let* ((permissions (or (alist-get 'permissions params) '()))
            (choice (if (codex-ide--auto-approve-emacs-bridge-request-p params)
                        'session
                      (codex-ide--approval-decision
                       (format "Grant Codex permissions%s? "
                               (if-let ((reason (alist-get 'reason params)))
                                   (format " (%s)" reason)
                                 ""))
                       '(("grant for turn" . turn)
                         ("grant for session" . session)
                         ("decline" . decline))))))
       (codex-ide-log-message
        session
        "Permissions approval resolved as %s for %S"
        choice
        permissions)
       (if (eq choice 'decline)
           (codex-ide--jsonrpc-send-response session id '((permissions . ())))
         (codex-ide--jsonrpc-send-response
          session id
          `((permissions . ,permissions)
            (scope . ,(symbol-name choice)))))))))

(defun codex-ide--handle-elicitation-request (&optional session id params)
  "Handle an MCP elicitation request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (run-at-time
   0 nil
   (lambda ()
     (condition-case err
         (let ((result (if (codex-ide--auto-approve-emacs-bridge-request-p params)
                           '((action . "accept"))
                         (codex-ide-mcp-elicitation-handle-request params))))
           (codex-ide-log-message
            session
            "Elicitation request resolved as %s"
            (alist-get 'action result))
           (codex-ide--jsonrpc-send-response session id result))
       (error
        (codex-ide-log-message
         session
         "Elicitation request failed: %s"
         (error-message-string err))
        (codex-ide--jsonrpc-send-error session id -32603
                                       (error-message-string err)))))))

(defun codex-ide--handle-server-request (&optional session message)
  "Handle a server-initiated request MESSAGE for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message))
        (params (alist-get 'params message)))
    (codex-ide-log-message session "Received server request %s (id=%s)" method id)
    (pcase method
      ((or "elicitation/create"
           "mcpServer/elicitation/request")
       (codex-ide--append-to-buffer
        (codex-ide-session-buffer session)
        (format "\n[%s]\n"
                (codex-ide-mcp-elicitation-format-request params))
        'shadow)
       (codex-ide--handle-elicitation-request session id params))
      ("item/commandExecution/requestApproval"
       (codex-ide--handle-command-approval session id params))
      ("item/fileChange/requestApproval"
       (codex-ide--handle-file-change-approval session id params))
      ("item/permissions/requestApproval"
       (codex-ide--handle-permissions-approval session id params))
      (_
       (codex-ide-log-message session "Unsupported server request %s" method)
       (codex-ide--append-to-buffer
        (codex-ide-session-buffer session)
        (format "\n[Codex requested unsupported method %s]\n" method)
        'warning)
       (codex-ide--jsonrpc-send-error session id -32601
                                      (format "Unsupported method: %s" method))))))

(defun codex-ide--handle-notification (&optional session message)
  "Handle a notification MESSAGE for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((method (alist-get 'method message))
        (params (alist-get 'params message))
        (buffer (codex-ide-session-buffer session)))
    (codex-ide-log-message session "Received notification %s" method)
    (pcase method
      ("thread/started"
       (when-let ((thread-id (alist-get 'id (alist-get 'thread params))))
         (setf (codex-ide-session-thread-id session) thread-id))
       (codex-ide-log-message
        session
        "Thread started: %s"
        (codex-ide-session-thread-id session))
       (setf (codex-ide-session-status session) "idle")
       (codex-ide--update-header-line session))
      ("thread/status/changed"
       (let* ((thread (alist-get 'thread params))
              (status (alist-get 'status thread)))
         (setf (codex-ide-session-status session)
               (cond
                ((stringp status) status)
                ((alist-get 'type status))
                (t (format "%S" status)))))
       (codex-ide-log-message
        session
        "Thread status changed to %s"
        (codex-ide-session-status session))
       (codex-ide--update-header-line session))
      ("thread/tokenUsage/updated"
       (let ((token-usage (alist-get 'tokenUsage params)))
         (codex-ide--session-metadata-put session :token-usage token-usage)
         (codex-ide-log-message
          session
          "Token usage updated: total=%s window=%s"
          (alist-get 'totalTokens (alist-get 'total token-usage))
          (alist-get 'modelContextWindow token-usage))
         (codex-ide--update-header-line session)))
      ("account/rateLimits/updated"
       (let ((rate-limits (alist-get 'rateLimits params)))
         (codex-ide--session-metadata-put session :rate-limits rate-limits)
         (codex-ide-log-message
          session
          "Rate limits updated: used=%s%% plan=%s"
          (alist-get 'usedPercent (alist-get 'primary rate-limits))
          (or (alist-get 'planType rate-limits) "unknown"))
         (codex-ide--update-header-line session)))
      ("turn/started"
       (setf (codex-ide-session-current-turn-id session)
             (or (alist-get 'id (alist-get 'turn params))
                 (alist-get 'turnId params))
             (codex-ide-session-current-message-item-id session) nil
             (codex-ide-session-current-message-prefix-inserted session) nil
             (codex-ide-session-current-message-start-marker session) nil
             (codex-ide-session-item-states session) (make-hash-table :test 'equal)
             (codex-ide-session-status session) "running")
       (codex-ide-log-message
        session
        "Turn started: %s"
        (codex-ide-session-current-turn-id session))
       (codex-ide--update-header-line session)
       (unless (codex-ide-session-output-prefix-inserted session)
         (codex-ide--begin-turn-display session)))
      ("item/started"
       (when-let ((item (alist-get 'item params)))
         (codex-ide-log-message
          session
          "Item started: %s (%s)"
          (alist-get 'id item)
          (alist-get 'type item))
         (codex-ide--render-item-start session item)))
      ("item/agentMessage/delta"
       (let ((item-id (alist-get 'itemId params))
             (delta (or (alist-get 'delta params) "")))
         (let ((codex-ide--current-agent-item-type "agentMessage"))
           (codex-ide--ensure-agent-message-prefix session item-id)
           (unless (string-empty-p delta)
             (codex-ide-log-message
              session
              "Agent delta for item %s (%d chars)"
              item-id
              (length delta)))
           (codex-ide--append-agent-text buffer delta)
           (when-let ((start (codex-ide-session-current-message-start-marker session)))
             (with-current-buffer buffer
               (codex-ide--render-markdown-region start (point-max)))))))
      ("item/commandExecution/outputDelta"
       (let ((item-id (alist-get 'itemId params)))
         (codex-ide-log-message
          session
          "Command output delta for item %s (%d chars)"
          item-id
          (length (or (alist-get 'delta params) "")))))
      ("item/fileChange/outputDelta"
       (let ((item-id (alist-get 'itemId params))
             (delta (or (alist-get 'delta params) "")))
         (codex-ide-log-message
          session
          "File-change delta for item %s (%d chars)"
          item-id
          (length delta))
         (when-let ((state (codex-ide--item-state session item-id)))
           (codex-ide--put-item-state
            session
            item-id
            (plist-put state :diff-text
                       (concat (or (plist-get state :diff-text) "") delta))))))
      ("item/plan/delta"
       (codex-ide-log-message
        session
        "Plan delta (%d chars)"
        (length (or (alist-get 'delta params) "")))
       (codex-ide--render-plan-delta session params))
      ("item/reasoning/summaryTextDelta"
       (codex-ide-log-message
        session
        "Reasoning summary delta (%d chars)"
        (length (or (alist-get 'delta params)
                    (alist-get 'text params)
                    "")))
       (codex-ide--render-reasoning-delta session params))
      ("item/completed"
       (when-let ((item (alist-get 'item params)))
         (codex-ide-log-message
          session
          "Item completed: %s (%s, status=%s)"
          (alist-get 'id item)
          (alist-get 'type item)
          (alist-get 'status item))
         (codex-ide--render-item-completion session item)))
      ("turn/completed"
       (let ((interrupted (codex-ide-session-interrupt-requested session)))
       (codex-ide-log-message
        session
        "Turn completed: %s"
        (codex-ide-session-current-turn-id session))
       (when interrupted
         (codex-ide-log-message session "Turn completed after interrupt request"))
       (codex-ide--finish-turn
        session
        (when interrupted "[Agent interrupted]"))))
      ("error"
       (let ((codex-ide--current-agent-item-type "error"))
         (codex-ide-log-message session "Error notification: %S" params)
         (codex-ide--append-agent-text
          buffer
          (format "\n[Codex error] %S\n" params)
          'error)))
      ((or "notifications/elicitation/complete"
           "mcpServer/elicitation/complete")
       (codex-ide-log-message
        session
        "Elicitation completed: %s"
        (alist-get 'elicitationId params))
       (codex-ide--append-to-buffer
        buffer
        (format "\n[%s]\n"
                (codex-ide-mcp-elicitation-format-completion params))
        'shadow))
      (_
       nil))))

(defun codex-ide--process-message (&optional session line)
  "Process a single JSON LINE for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((message (ignore-errors
                   (json-parse-string line
                                      :object-type 'alist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object :json-false))))
    (cond
     ((null message)
      (codex-ide-log-message session "Processing incoming line: %s" line)
      (codex-ide-log-message session "Received non-JSON output")
      (codex-ide--append-to-buffer
       (codex-ide-session-buffer session)
       (concat line "\n")
       'shadow))
     ((alist-get 'method message)
      (if (alist-get 'id message)
          (progn
            (codex-ide-log-message session "Processing incoming line: %s" line)
            (codex-ide--handle-server-request session message))
        (let ((codex-ide--current-transcript-log-marker
               (codex-ide-log-message
                session
                "Processing incoming notification line: %s"
                line)))
          (codex-ide--handle-notification session message))))
     ((alist-get 'id message)
      (codex-ide-log-message session "Processing incoming line: %s" line)
      (codex-ide--handle-response session message)))))

(defun codex-ide--process-filter (process chunk)
  "Handle app-server PROCESS output CHUNK."
  (when-let ((session (process-get process 'codex-session)))
    (codex-ide-log-message session "Received process chunk (%d chars)" (length chunk))
    (let* ((pending (concat (or (codex-ide-session-partial-line session) "")
                            chunk))
           (lines (split-string pending "\n")))
      (setf (codex-ide-session-partial-line session) (car (last lines)))
      (dolist (line (butlast lines))
        (unless (string-empty-p line)
          (codex-ide--process-message session line))))))

(defun codex-ide--process-sentinel (process event)
  "Handle app-server PROCESS EVENT."
  (when-let ((session (process-get process 'codex-session)))
    (let ((buffer (codex-ide-session-buffer session)))
      (codex-ide-log-message session "Process event: %s" (string-trim event))
      (setf (codex-ide-session-status session) (string-trim event))
      (codex-ide--update-header-line session)
      (codex-ide--append-to-buffer
       buffer
       (format "\n[Codex process %s]\n" (string-trim event))
       'shadow)
      (unless (process-live-p process)
        (codex-ide-log-message session "Process exited")
        (codex-ide--cleanup-session session)))))

(defun codex-ide--compose-turn-payload (prompt)
  "Build prompt payload metadata for PROMPT in the current working directory."
  (let* ((context-payload (codex-ide--context-payload-for-prompt))
         (context-prefix (alist-get 'formatted context-payload))
         (full-prompt (string-join (delq nil (list context-prefix prompt)) "\n\n")))
    `((context-summary . ,(alist-get 'summary context-payload))
      (input . [((type . "text")
                 (text . ,full-prompt))]))))

(defun codex-ide--compose-turn-input (prompt)
  "Build `turn/start` input items for the current working directory and PROMPT."
  (alist-get 'input (codex-ide--compose-turn-payload prompt)))

(defun codex-ide--session-for-current-project ()
  "Return the active session for the current buffer or project."
  (let ((session (codex-ide--get-default-session-for-current-buffer)))
    (unless (and session (process-live-p (codex-ide-session-process session)))
      (user-error "No Codex session for this buffer or project"))
    session))

(defun codex-ide--ensure-session-for-current-project ()
  "Return the active session for the current buffer or project.
If no live session exists, prompt to start one."
  (or (let ((session (codex-ide--get-default-session-for-current-buffer)))
        (when (and session (process-live-p (codex-ide-session-process session)))
          session))
      (when (y-or-n-p "No Codex session for this buffer. Start one? ")
        (codex-ide--start-session 'new))
      (codex-ide--session-for-current-project)))

;;;###autoload
(defun codex-ide ()
  "Start Codex for the current project or directory."
  (interactive)
  (codex-ide--start-session 'new))

;;;###autoload
(defun codex-ide-resume ()
  "Resume a Codex session using an Emacs picker."
  (interactive)
  (condition-case nil
      (codex-ide--start-session 'resume)
    (quit
     (message "Codex resume canceled")
     nil)))

;;;###autoload
(defun codex-ide-continue ()
  "Resume the most recent Codex session for the current directory."
  (interactive)
  (codex-ide--start-session 'continue))

;;;###autoload
(defun codex-ide-check-status ()
  "Report Codex CLI availability and version."
  (interactive)
  (codex-ide--detect-cli)
  (if codex-ide--cli-available
      (let* ((version
              (with-temp-buffer
                (call-process codex-ide-cli-path nil t nil "--version")
                (string-trim (buffer-string))))
             (bridge-status (codex-ide-mcp-bridge-status))
             (bridge-enabled (alist-get 'enabled bridge-status))
             (bridge-ready (alist-get 'ready bridge-status))
             (bridge-script (alist-get 'scriptPath bridge-status))
             (bridge-server-running (alist-get 'serverRunning bridge-status))
             (bridge-summary
              (cond
               ((not bridge-enabled)
                "disabled")
               (bridge-ready
                (format "enabled; script=%s; server=%s"
                        (abbreviate-file-name bridge-script)
                        (if bridge-server-running "running" "stopped")))
               (t
                (format "enabled but not ready; script=%s"
                        (abbreviate-file-name bridge-script))))))
        (message "Codex CLI version: %s | Emacs bridge: %s"
                 version
                 bridge-summary))
    (message "Codex CLI is not installed or not on PATH")))

;;;###autoload
(defun codex-ide-stop ()
  "Stop the Codex session for the current project or directory."
  (interactive)
  (let* ((working-dir (codex-ide--get-working-directory))
         (session (codex-ide--get-session))
         (buffer (and session (codex-ide-session-buffer session))))
    (cond
     ((and session (process-live-p (codex-ide-session-process session)))
      (when (codex-ide-session-thread-id session)
        (codex-ide-log-message
         session
         "Unsubscribing thread %s before stop"
         (codex-ide-session-thread-id session))
        (ignore-errors
          (codex-ide--request-sync
           session
           "thread/unsubscribe"
           `((threadId . ,(codex-ide-session-thread-id session))))))
      (codex-ide-log-message session "Stopping process")
      (delete-process (codex-ide-session-process session))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer)))
      (codex-ide--cleanup-session session)
      (message "Stopped Codex in %s"
               (file-name-nondirectory (directory-file-name working-dir))))
     ((buffer-live-p buffer)
      (when session
        (codex-ide-log-message session "Removing stale session buffer"))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))
      (codex-ide--cleanup-session session)
      (message "Removed stale Codex buffer in %s"
               (file-name-nondirectory (directory-file-name working-dir))))
     (t
      (message "No Codex session is running in this directory")))))

;;;###autoload
(defun codex-ide-switch-to-buffer ()
  "Show the Codex buffer for the current project."
  (interactive)
  (let* ((session (codex-ide--session-for-current-project))
         (buffer (codex-ide-session-buffer session)))
    (codex-ide--display-buffer-in-codex-window buffer)))

;;;###autoload
(defun codex-ide-list-session-buffers ()
  "List active Codex session buffers and switch to the selected one."
  (interactive)
  (codex-ide--cleanup-dead-sessions)
  (let (sessions)
    (maphash
     (lambda (directory session)
       (when (buffer-live-p (codex-ide-session-buffer session))
         (push (cons (abbreviate-file-name directory) session) sessions)))
     codex-ide--sessions)
    (if sessions
        (let* ((choice (completing-read "Switch to Codex session buffer: "
                                        sessions nil t))
               (session (alist-get choice sessions nil nil #'string=)))
          (codex-ide--display-buffer-in-side-window (codex-ide-session-buffer session)))
      (message "No active Codex session buffers"))))

;;;###autoload
(defun codex-ide-interrupt ()
  "Interrupt the active Codex turn for the current project."
  (interactive)
  (let ((session (codex-ide--session-for-current-project)))
    (if-let ((turn-id (codex-ide-session-current-turn-id session)))
        (progn
          (codex-ide-log-message session "Sending interrupt for turn %s" turn-id)
          (setf (codex-ide-session-interrupt-requested session) t
                (codex-ide-session-status session) "interrupting")
          (codex-ide--update-header-line session)
          (condition-case err
              (codex-ide--request-sync
               session
               "turn/interrupt"
               `((threadId . ,(codex-ide-session-thread-id session))
                 (turnId . ,turn-id)))
            (error
             (setf (codex-ide-session-interrupt-requested session) nil
                   (codex-ide-session-status session) "running")
             (codex-ide--update-header-line session)
             (signal (car err) (cdr err))))
          (message "Sent interrupt to Codex"))
      (user-error "No active Codex turn to interrupt"))))

(defun codex-ide-insert-newline ()
  "Open a prompt in the minibuffer that supports literal newlines."
  (interactive)
  (let ((prompt (read-from-minibuffer "Codex prompt (RET inserts newline, C-j to submit): ")))
    (unless (string-empty-p prompt)
      (codex-ide--submit-prompt prompt))))

;;;###autoload
(defun codex-ide-prompt ()
  "Prompt for a Codex message in the minibuffer and submit it from the Codex buffer.
If no live session exists for the current buffer, prompt to start one first."
  (interactive)
  (let ((origin-buffer (current-buffer))
        (session (codex-ide--ensure-session-for-current-project)))
    (let ((prompt (read-from-minibuffer
                   "Codex prompt (RET inserts newline, C-j to submit): ")))
      (unless (string-empty-p prompt)
        (let* ((buffer (codex-ide-session-buffer session))
               (window (codex-ide--display-buffer-in-codex-window buffer)))
          (with-selected-window window
            (with-current-buffer buffer
              (if (codex-ide-session-input-overlay session)
                  (codex-ide--replace-current-input session prompt)
                (codex-ide--insert-input-prompt session prompt))
              (let ((codex-ide--prompt-origin-buffer origin-buffer))
                (codex-ide--submit-prompt)))))))))

;;;###autoload
(defun codex-ide-previous-prompt-history ()
  "Replace the current prompt with the previous prompt from history."
  (interactive)
  (codex-ide--browse-prompt-history -1))

;;;###autoload
(defun codex-ide-next-prompt-history ()
  "Replace the current prompt with the next prompt from history."
  (interactive)
  (codex-ide--browse-prompt-history 1))

;;;###autoload
(defun codex-ide-previous-prompt-line ()
  "Jump to the previous user prompt line in the session buffer."
  (interactive)
  (codex-ide--goto-prompt-line -1))

;;;###autoload
(defun codex-ide-next-prompt-line ()
  "Jump to the next user prompt line in the session buffer."
  (interactive)
  (codex-ide--goto-prompt-line 1))

(defun codex-ide--submit-prompt (&optional prompt)
  "Submit PROMPT to the current Codex session."
  (interactive)
  (let* ((session (codex-ide--session-for-current-project))
         (thread-id (codex-ide-session-thread-id session))
         (prompt-to-send (or prompt
                             (if (eq (current-buffer) (codex-ide-session-buffer session))
                                 (codex-ide--current-input session)
                               (read-string "Codex prompt: "))))
         payload)
    (when (codex-ide-session-current-turn-id session)
      (user-error "A Codex turn is already running"))
    (unless thread-id
      (user-error "Codex session has no active thread"))
    (when (string-empty-p prompt-to-send)
      (user-error "Prompt is empty"))
    (codex-ide--push-prompt-history session prompt-to-send)
    (codex-ide-log-message
     session
     "Sending prompt to thread %s (%d chars)"
     thread-id
     (length prompt-to-send))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (codex-ide--insert-input-prompt session prompt-to-send))
    (setq payload
          (with-current-buffer (codex-ide-session-buffer session)
            (codex-ide--compose-turn-payload prompt-to-send)))
    (codex-ide--begin-turn-display session (alist-get 'context-summary payload))
    (redisplay)
    (condition-case err
        (codex-ide--request-sync
         session
         "turn/start"
         `((threadId . ,thread-id)
           (input . ,(alist-get 'input payload))))
      (error
       (codex-ide-log-message session "Prompt submission failed: %s" (error-message-string err))
       (codex-ide--reopen-input-after-submit-error session prompt-to-send err)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun codex-ide-submit ()
  "Submit the current in-buffer prompt to Codex."
  (interactive)
  (codex-ide--submit-prompt))

;;;###autoload
(defun codex-ide-send-active-buffer-context ()
  "Send the currently tracked Emacs buffer context to the Codex session."
  (interactive)
  (let* ((session (codex-ide--session-for-current-project))
         (working-dir (codex-ide-session-directory session))
         (context (or (codex-ide--make-buffer-context)
                      (gethash working-dir codex-ide--active-buffer-contexts))))
    (cond
     ((not context)
      (user-error "Current buffer is not a tracked project file"))
     ((codex-ide-session-current-turn-id session)
      (user-error "A Codex turn is already running"))
     (t
      (puthash working-dir context codex-ide--last-sent-buffer-contexts)
      (codex-ide-log-message
       session
       "Sending active buffer context for %s"
       (alist-get 'display-file context))
      (codex-ide--submit-prompt
       (codex-ide--format-buffer-context
        (codex-ide--context-with-selected-region
         context
         (or (and (not (codex-ide--session-buffer-p (current-buffer)))
                  (current-buffer))
             (codex-ide--get-active-buffer-object)))))
      (message "Sent active buffer context to Codex")))))

;;;###autoload
(defun codex-ide-toggle ()
  "Toggle visibility of the Codex window for the current project."
  (interactive)
  (if-let* ((session (codex-ide--get-session))
            (buffer (codex-ide-session-buffer session)))
      (codex-ide--toggle-existing-window buffer)
    (user-error "No Codex session for this project")))

;;;###autoload
(defun codex-ide-toggle-recent ()
  "Toggle the most recently used Codex window globally."
  (interactive)
  (let ((found-visible nil))
    (maphash
     (lambda (_directory session)
       (let ((buffer (codex-ide-session-buffer session)))
         (when (and (buffer-live-p buffer)
                    (get-buffer-window buffer))
           (codex-ide--toggle-existing-window buffer)
           (setq found-visible t))))
     codex-ide--sessions)
    (cond
     (found-visible
      (message "Closed all Codex windows"))
     ((buffer-live-p codex-ide--last-accessed-buffer)
      (codex-ide--display-buffer-in-side-window codex-ide--last-accessed-buffer)
      (message "Opened most recent Codex session"))
     (t
      (user-error "No recent Codex session to toggle")))))

(provide 'codex-ide)

;;; codex-ide.el ends here
