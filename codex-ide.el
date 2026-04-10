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
(require 'button)
(require 'eieio)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)

;;;###autoload
(defgroup codex-ide nil
  "Codex app-server integration for Emacs."
  :group 'tools
  :prefix "codex-ide-")

(require 'codex-ide-core)
(require 'codex-ide-mcp-elicitation)
(require 'codex-ide-renderer)
(require 'codex-ide-transient)
(require 'codex-ide-mcp-bridge)

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
(defcustom codex-ide-session-baseline-prompt "
- You are a Codex server running inside Emacs.
- You can use MCP tools to inspect and interact with the running Emacs session.
- Interpret Emacs terminology as relevant context to the user's request: buffers, regions, windows, point, mark, current file, etc.
- Do not needlessly use Emacs commands to accomplish agent tasks."
  "Optionally baseline prompt injected into the first real prompt of a new thread.
When set to a non-empty string, `codex-ide' prepends it once as an
`[Emacs session context]' block on the first submitted user turn for a
brand-new thread. Resume and continue flows do not resend it."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Prompt"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-buffer-name-prefix "codex"
  "Prefix used when creating Codex session buffer names."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-focus-on-open t
  "Whether to focus the Codex window after showing it."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defconst codex-ide-display-buffer-options nil
  "Ordered display policy keys for `codex-ide-display-buffer'.

Supported keys are `:reuse-buffer-window', `:reuse-mode-window',
`:other-window', and `:new-window'.")

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
(defcustom codex-ide-resume-summary-turn-limit 100
  "How many recent turns to summarize when resuming a stored thread."
  :type 'integer
  :group 'codex-ide)

(defvar codex-ide--cli-available nil
  "Whether the Codex CLI has been detected successfully.")

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
(define-key codex-ide-session-mode-map (kbd "C-M-p") #'codex-ide-previous-prompt-line)
(define-key codex-ide-session-mode-map (kbd "C-M-n") #'codex-ide-next-prompt-line)
(define-key codex-ide-session-mode-map (kbd "TAB") #'forward-button)
(define-key codex-ide-session-mode-map (kbd "<backtab>") #'backward-button)
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
             (buffer (get-buffer-create
                      (codex-ide--log-buffer-name
                       directory
                       (codex-ide-session-name-suffix session)))))
        (codex-ide--initialize-log-buffer buffer directory)
        (setf (codex-ide-session-log-buffer session) buffer)
        buffer)))

(defun codex-ide--stderr-filter (process chunk)
  "Append stderr CHUNK from PROCESS to the owning session log."
  (when-let ((session (process-get process 'codex-session)))
    (let* ((sanitized (codex-ide--sanitize-ansi-text chunk))
           (pending (concat (or (codex-ide--session-metadata-get session :stderr-partial) "")
                            sanitized))
           (lines (split-string pending "\n"))
           (complete-lines (butlast lines))
           (partial (car (last lines))))
      (codex-ide--session-metadata-put
       session
       :stderr-tail
       (let* ((previous-tail (or (codex-ide--session-metadata-get session :stderr-tail) ""))
              (combined-tail (concat previous-tail sanitized)))
         (if (> (length combined-tail) 4000)
             (substring combined-tail (- (length combined-tail) 4000))
           combined-tail)))
      (codex-ide--session-metadata-put session :stderr-partial partial)
      (when complete-lines
        (dolist (line complete-lines)
          (unless (string-empty-p line)
            (codex-ide-log-message session "stderr: %s" line)))))))

(defun codex-ide--discard-process-buffer (process)
  "Detach and kill any buffer associated with PROCESS."
  (when process
    (let ((buffer (ignore-errors (process-buffer process))))
      (when buffer
        (ignore-errors (set-process-buffer process nil))
        (when (buffer-live-p buffer)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer)))))))

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
    (setq codex-ide--sessions (delq session codex-ide--sessions))
    (remhash directory codex-ide--active-buffer-contexts)
    (remhash directory codex-ide--active-buffer-objects)
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
  (dolist (session codex-ide--sessions)
    (when (process-live-p (codex-ide-session-process session))
      (delete-process (codex-ide-session-process session)))))

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

(defun codex-ide-display-buffer (buffer)
  "Display BUFFER according to Codex window selection preferences."
  (codex-ide--remember-buffer-context-before-switch)
  (let* ((selected-window (selected-window))
         (windows (window-list nil 'no-minibuf selected-window))
         (config codex-ide-display-buffer-options)
         (window
          (or (and (memq :reuse-buffer-window config)
                   (get-buffer-window buffer 0))
              (and (memq :reuse-mode-window config)
                   (seq-find
                    (lambda (candidate)
                      (codex-ide--session-buffer-p (window-buffer candidate)))
                    windows))
              (and (memq :other-window config)
                   (seq-find (lambda (candidate)
                               (not (eq candidate selected-window)))
                             windows))
              (and (memq :new-window config)
                   (split-window-sensibly selected-window))
              selected-window)))
    (when window
      (let ((was-dedicated (window-dedicated-p window)))
        (when was-dedicated
          (set-window-dedicated-p window nil))
        (set-window-buffer window buffer)
        (when was-dedicated
          (set-window-dedicated-p window was-dedicated))
        (when codex-ide-focus-on-open
          (select-window window))))
    window))


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

(defun codex-ide--stringify-error-payload (value)
  "Return a concise string for error VALUE."
  (cond
   ((stringp value) (string-trim value))
   ((and (listp value) (alist-get 'message value))
    (string-trim (format "%s" (alist-get 'message value))))
   ((null value) "")
   (t (string-trim (format "%s" value)))))

(defun codex-ide--extract-error-text (&rest values)
  "Return the first useful error string from VALUES."
  (let ((parts (delq nil
                     (mapcar (lambda (value)
                               (let ((text (codex-ide--stringify-error-payload value)))
                                 (unless (string-empty-p text)
                                   text)))
                             values))))
    (string-join (delete-dups parts) "\n")))

(defun codex-ide--sanitize-ansi-text (text)
  "Strip ANSI escape sequences from TEXT."
  (if (stringp text)
      (replace-regexp-in-string "\x1b\\[[0-9;]*[[:alpha:]]" "" text)
    ""))

(defun codex-ide--alist-get-safe (key value)
  "Return KEY from VALUE when VALUE is an alist, else nil."
  (when (listp value)
    (alist-get key value)))

(defun codex-ide--notification-error-info (params)
  "Extract normalized error details from notification PARAMS."
  (let* ((error-info (or (alist-get 'error params) params))
         (codex-info (codex-ide--alist-get-safe 'codexErrorInfo error-info))
         (stream-disconnected
          (or (codex-ide--alist-get-safe 'responseStreamDisconnected codex-info)
              (codex-ide--alist-get-safe 'responseStreamDisconnected error-info)))
         (status-code (or (codex-ide--alist-get-safe 'httpStatusCode stream-disconnected)
                          (codex-ide--alist-get-safe 'httpStatusCode codex-info)
                          (codex-ide--alist-get-safe 'httpStatusCode error-info)))
         (message (or (codex-ide--alist-get-safe 'message error-info)
                      (codex-ide--alist-get-safe 'message params)))
         (details (or (codex-ide--alist-get-safe 'additionalDetails error-info)
                      (codex-ide--alist-get-safe 'additionalDetails params)))
         (will-retry (let ((value (alist-get 'willRetry params)))
                       (not (memq value '(nil :json-false)))))
         (turn-id (or (alist-get 'turnId params)
                      (codex-ide--alist-get-safe 'turnId error-info))))
    `((message . ,message)
      (details . ,details)
      (http-status . ,status-code)
      (will-retry . ,will-retry)
      (turn-id . ,turn-id))))

(defun codex-ide--notification-error-display-detail (info)
  "Build concise display detail for normalized notification error INFO."
  (let ((message (codex-ide--sanitize-ansi-text (alist-get 'message info)))
        (details (codex-ide--sanitize-ansi-text (alist-get 'details info))))
    (string-join
     (delq nil
           (list (unless (string-empty-p message) message)
                 (unless (or (string-empty-p details)
                             (equal details message))
                   details)))
     "\n")))

(defun codex-ide--notification-error-message (info)
  "Return the primary notification error message for INFO."
  (codex-ide--sanitize-ansi-text (or (alist-get 'message info) "")))

(defun codex-ide--notification-error-additional-details (info)
  "Return the secondary additional details string for INFO, or nil."
  (let ((message (codex-ide--notification-error-message info))
        (details (codex-ide--sanitize-ansi-text (alist-get 'details info))))
    (unless (or (string-empty-p details)
                (equal details message))
      details)))

(defun codex-ide--append-notification-additional-details (session details)
  "Append a dimmed additional-details line for SESSION when DETAILS is non-empty."
  (when (and (stringp details)
             (not (string-empty-p details)))
    (let ((codex-ide--current-agent-item-type "error"))
      (codex-ide--append-agent-text
       (codex-ide-session-buffer session)
       (codex-ide--item-detail-line
        (format "additionalDetails: %s" details))
       'codex-ide-item-detail-face))))

(defun codex-ide--handle-retryable-notification-error (session info)
  "Handle a retryable notification error for SESSION using INFO."
  (let* ((turn-id (alist-get 'turn-id info))
         (message (or (codex-ide--notification-error-message info) "Retrying request"))
         (details (codex-ide--notification-error-additional-details info))
         (retry-key (format "%s:%s" turn-id message))
         (previous (codex-ide--session-metadata-get session :last-retry-notice)))
    (unless (equal retry-key previous)
      (codex-ide--session-metadata-put session :last-retry-notice retry-key)
      (codex-ide-log-message session "Retryable Codex error: %s" message)
      (codex-ide--append-to-buffer
       (codex-ide-session-buffer session)
       (format "\n[Codex retrying] %s\n" message)
       'warning)
      (codex-ide--append-notification-additional-details session details))))

(defun codex-ide--classify-session-error (&rest values)
  "Classify Codex session failure details from VALUES.
Return a plist with :kind, :summary, and :guidance."
  (let* ((text (downcase (apply #'codex-ide--extract-error-text values)))
         (match (lambda (&rest needles)
                  (seq-some (lambda (needle)
                              (and needle (string-match-p (regexp-quote needle) text)))
                            needles))))
    (cond
     ((funcall match "authentication" "unauthorized" "unauthenticated" "invalid api key"
               "login required" "not logged in" "auth")
      '(:kind auth
        :summary "Codex authentication failed."
        :guidance "Run `codex login` or refresh your credentials, then retry."))
     ((funcall match "rate limit" "rate-limit" "too many requests" "429")
      '(:kind rate-limit
        :summary "Codex is rate limited."
        :guidance "Wait for quota to recover or switch accounts/models before retrying."))
     ((funcall match "no such file or directory" "does not exist" "not found"
               "cannot find" "enoent")
      '(:kind missing-path
        :summary "Codex startup failed because a required path does not exist."
        :guidance "Check `codex-ide-cli-path`, `CODEX_HOME`, and the project working directory."))
     ((funcall match "executable file not found" "command not found" "spawn codex"
               "cannot run program" "failed to execute")
      '(:kind executable
        :summary "The Codex executable could not be started."
        :guidance "Install the Codex CLI or update `codex-ide-cli-path` so Emacs can launch it."))
     ((funcall match "startup" "initialize" "app-server" "config" "configuration")
      '(:kind startup
        :summary "Codex app-server startup failed."
        :guidance "Inspect the log buffer for the exact startup error and fix the local Codex configuration."))
     (t
      '(:kind generic
        :summary "Codex reported an error."
        :guidance "Inspect the session log for details, then retry once the underlying issue is fixed.")))))

(defun codex-ide--format-session-error-summary (classification &optional prefix)
  "Format the headline summary for CLASSIFICATION with PREFIX."
  (format "[%s%s]"
          (or prefix "Codex error")
          (if-let ((summary (plist-get classification :summary)))
              (format ": %s" summary)
            "")))

(defun codex-ide--format-session-error-message (classification detail &optional prefix)
  "Format a full message string from CLASSIFICATION and DETAIL with PREFIX."
  (string-join
   (delq nil
         (list (codex-ide--format-session-error-summary classification prefix)
               (unless (string-empty-p detail)
                 detail)
               (plist-get classification :guidance)))
   "\n"))

(defun codex-ide--recover-from-session-error (session classification)
  "Reset SESSION after a recoverable error using CLASSIFICATION."
  (when (and (memq (plist-get classification :kind) '(auth rate-limit generic startup))
             (or (codex-ide-session-current-turn-id session)
                 (codex-ide-session-output-prefix-inserted session)))
    (codex-ide--session-metadata-put session :last-retry-notice nil)
    (codex-ide--finish-turn
     session
     (format "[%s]"
             (or (plist-get classification :summary)
                 "Codex request failed")))))

(defconst codex-ide--session-context-open-tag "[Emacs session context]")
(defconst codex-ide--session-context-close-tag "[/Emacs session context]")
(defconst codex-ide--prompt-context-open-tag "[Emacs prompt context]")
(defconst codex-ide--prompt-context-close-tag "[/Emacs prompt context]")

(defun codex-ide--format-session-context ()
  "Format the one-time session baseline prompt block."
  (when-let ((prompt (and (stringp codex-ide-session-baseline-prompt)
                          (string-trim codex-ide-session-baseline-prompt))))
    (unless (string-empty-p prompt)
      (format (concat "%s\n"
                      "Take the following into account in this prompt and all following ones:\n"
                      "%s\n"
                      "%s\n")
              codex-ide--session-context-open-tag
              prompt
              codex-ide--session-context-close-tag))))

(defun codex-ide--format-buffer-context (context)
  "Format CONTEXT for insertion into a Codex prompt."
  (let ((selection (alist-get 'selection context)))
    (format (concat "%s\n"
                    "Last file/buffer focused in Emacs: %s\n"
                    "Buffer: %s\n"
                    "Cursor: line %s, column %s\n"
                    "%s"
                    "%s\n")
            codex-ide--prompt-context-open-tag
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
              "")
            codex-ide--prompt-context-close-tag)))

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

(defun codex-ide--context-payload-for-prompt ()
  "Return context payload metadata for the current prompt, or nil.
The result is an alist with `formatted' and `summary' entries."
  (let ((working-dir (codex-ide--get-working-directory))
        (session (codex-ide--get-default-session-for-current-buffer)))
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
        `((formatted . ,formatted-context)
          (summary . ,context-summary))))))

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
      (error "Codex app-server request %s failed: %s"
             method
             (or (alist-get 'message err)
                 (codex-ide--stringify-error-payload err))))
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

(defun codex-ide--strip-leading-context-block (text open-tag close-tag)
  "Remove a leading context block delimited by OPEN-TAG and CLOSE-TAG from TEXT."
  (if (and (stringp text)
           (string-prefix-p open-tag text)
           (string-match (regexp-quote close-tag) text))
      (string-trim-left (substring text (match-end 0)))
    text))

(defun codex-ide--strip-emacs-context-prefix (text)
  "Remove any leading Emacs session or prompt context block from TEXT."
  (let ((stripped text)
        (changed t))
    (while changed
      (setq changed nil)
      (dolist (tags `((,codex-ide--session-context-open-tag . ,codex-ide--session-context-close-tag)
                      (,codex-ide--prompt-context-open-tag . ,codex-ide--prompt-context-close-tag)
                      ("[Emacs context]" . "[/Emacs context]")))
        (let ((next (codex-ide--strip-leading-context-block
                     stripped
                     (car tags)
                     (cdr tags))))
          (unless (equal next stripped)
            (setq stripped next
                  changed t)))))
    stripped))

(defun codex-ide--leading-emacs-context-prefix-p (text)
  "Return non-nil when TEXT begins with a known Emacs context prefix marker."
  (and (stringp text)
       (or (string-prefix-p codex-ide--session-context-open-tag text)
           (string-prefix-p codex-ide--prompt-context-open-tag text)
           (string-prefix-p "[Emacs context]" text))))


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
              (codex-ide--leading-emacs-context-prefix-p text)
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
  (let* ((working-dir (codex-ide-session-directory session))
         (result (codex-ide--request-sync
                  session
                  "thread/list"
                  `((cwd . ,working-dir)
                    (limit . 50)
                    (sortKey . "updated_at")
                    )))
         (data (alist-get 'data result)))
    (append data nil)))

(defun codex-ide--thread-list-data (&optional session omit-thread-id)
  "Return thread list data using SESSION.
When OMIT-THREAD-ID is non-nil, exclude that thread from the result."
  (seq-remove
   (lambda (thread)
     (equal (alist-get 'id thread) omit-thread-id))
   (codex-ide--list-threads session)))

(defun codex-ide--pick-thread (&optional session omit-thread-id)
  "Prompt to select a thread for the current working directory using SESSION.
When OMIT-THREAD-ID is non-nil, exclude that thread from the choices."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((working-dir (codex-ide-session-directory session))
         (threads (codex-ide--thread-list-data session omit-thread-id))
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

(defun codex-ide--resume-thread-into-session (session thread-id action)
  "Attach SESSION to THREAD-ID and optionally replay prior transcript.
ACTION is a short past-tense label used in log messages, such as
\"Continued\" or \"Resumed\"."
  (unless session
    (error "No Codex session available"))
  (unless (and (stringp thread-id)
               (not (string-empty-p thread-id)))
    (error "Invalid thread id: %S" thread-id))
  (let ((thread-read
         (condition-case err
             (codex-ide--read-thread session thread-id t)
           (error
            (codex-ide-log-message
             session
             "Unable to read stored thread %s before %s: %s"
             thread-id
             (downcase action)
             (error-message-string err))
            nil))))
    (codex-ide--request-sync
     session
     "thread/resume"
     (with-current-buffer (codex-ide-session-buffer session)
       (codex-ide--thread-resume-params thread-id)))
    (setf (codex-ide-session-thread-id session) thread-id)
    (codex-ide--session-metadata-put session :session-context-sent t)
    (codex-ide-log-message session "%s thread %s" action thread-id)
    (when thread-read
      (codex-ide--restore-thread-read-transcript session thread-read)))
  session)

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
    (let* ((name-suffix (codex-ide--next-session-name-suffix working-dir))
           (buffer (get-buffer-create
                    (codex-ide--session-buffer-name working-dir name-suffix)))
           (process-label
            (if name-suffix
                (format "%s<%d>"
                        (file-name-nondirectory (directory-file-name working-dir))
                        name-suffix)
              (file-name-nondirectory (directory-file-name working-dir))))
           (process-connection-type nil)
           (session (make-codex-ide-session
                     :directory working-dir
                     :name-suffix name-suffix
                     :buffer buffer
                     :log-buffer nil
                     :request-counter 0
                     :pending-requests (make-hash-table :test 'equal)
                     :item-states (make-hash-table :test 'equal)
                     :prompt-history-index nil
                     :prompt-history-draft nil
                     :partial-line ""
                     :status "starting"))
           (stderr-process nil)
           (process nil))
      (condition-case err
          (progn
            (setq stderr-process
                  (make-pipe-process
                   :name (format "codex-ide-stderr[%s]"
                                 process-label)
                   :buffer nil
                   :coding 'utf-8-unix
                   :noquery t
                   :filter #'codex-ide--stderr-filter))
            (codex-ide--discard-process-buffer stderr-process)
            (setq process
                  (make-process
                   :name (format "codex-ide[%s]"
                                 process-label)
                   :buffer nil
                   :command (codex-ide--app-server-command)
                   :coding 'utf-8-unix
                   :filter #'codex-ide--process-filter
                   :sentinel #'codex-ide--process-sentinel
                   :stderr stderr-process))
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
            session)
        (error
         (when (process-live-p stderr-process)
           (delete-process stderr-process))
         (signal 'user-error
                 (list
                  (codex-ide--format-session-error-message
                   (codex-ide--classify-session-error
                    (error-message-string err)
                    (codex-ide--app-server-command))
                   (codex-ide--extract-error-text
                    (error-message-string err)
                    (codex-ide--app-server-command))
                   "Codex startup failed"))))))))

(defun codex-ide--show-session-buffer (session)
  "Display SESSION's buffer and return SESSION."
  (codex-ide-display-buffer (codex-ide-session-buffer session))
  (codex-ide--ensure-input-prompt session)
  session)

(defun codex-ide--reset-session-buffer (session)
  "Reset SESSION's transcript buffer to an empty session header."
  (let ((buffer (codex-ide-session-buffer session))
        (working-dir (codex-ide-session-directory session)))
    (with-current-buffer buffer
      (setq-local default-directory working-dir)
      (setq-local codex-ide--session session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Codex session for %s\n\n"
                        (abbreviate-file-name working-dir)))
        (codex-ide--freeze-region (point-min) (point-max)))))
  (setf (codex-ide-session-current-turn-id session) nil
        (codex-ide-session-current-message-item-id session) nil
        (codex-ide-session-current-message-prefix-inserted session) nil
        (codex-ide-session-current-message-start-marker session) nil
        (codex-ide-session-output-prefix-inserted session) nil
        (codex-ide-session-item-states session) (make-hash-table :test 'equal)
        (codex-ide-session-input-overlay session) nil
        (codex-ide-session-input-start-marker session) nil
        (codex-ide-session-input-prompt-start-marker session) nil
        (codex-ide-session-prompt-history-index session) nil
        (codex-ide-session-prompt-history-draft session) nil
        (codex-ide-session-interrupt-requested session) nil
        (codex-ide-session-status session) "idle"))

(defun codex-ide--query-session-for-thread-selection (&optional directory)
  "Return a live session suitable for thread selection in DIRECTORY."
  (or (let ((session (codex-ide--session-for-current-buffer)))
        (when (and session
                   (equal (codex-ide-session-directory session)
                          (codex-ide--normalize-directory
                           (or directory (codex-ide--get-working-directory))))
                   (codex-ide--live-session-p session))
          session))
      (codex-ide--canonical-session-for-directory directory)))

(defun codex-ide--prepare-session-operations ()
  "Ensure Codex prerequisites needed for session-backed operations."
  (unless (codex-ide--ensure-cli)
    (user-error "Codex CLI not available. Install it and ensure it is on PATH"))
  (codex-ide--cleanup-dead-sessions)
  (codex-ide--ensure-active-buffer-tracking)
  (codex-ide-mcp-bridge-prompt-to-enable)
  (codex-ide-mcp-bridge-ensure-server))

(defun codex-ide--ensure-query-session-for-thread-selection (&optional directory)
  "Return a live query session for DIRECTORY, creating one when needed."
  (let* ((directory (codex-ide--normalize-directory
                     (or directory (codex-ide--get-working-directory))))
         (session (codex-ide--query-session-for-thread-selection directory)))
    (unless session
      (let ((default-directory directory))
        (setq session (codex-ide--create-process-session)))
      (codex-ide-log-message session "Initializing background query session")
      (codex-ide--initialize-session session))
    session))

(defun codex-ide--reusable-idle-session-for-directory (&optional directory)
  "Return an idle live session without a thread in DIRECTORY, if any."
  (let ((directory (codex-ide--normalize-directory
                    (or directory (codex-ide--get-working-directory)))))
    (seq-find
     (lambda (session)
       (and (codex-ide--live-session-p session)
            (equal (codex-ide-session-directory session) directory)
            (not (codex-ide-session-thread-id session))
            (string= (codex-ide-session-status session) "idle")))
     (codex-ide--sessions-for-directory directory t))))

(defun codex-ide--show-or-resume-thread (thread-id &optional directory)
  "Show THREAD-ID in DIRECTORY, resuming it into a session when needed."
  (let* ((directory (codex-ide--normalize-directory
                     (or directory (codex-ide--get-working-directory))))
         (session (or (codex-ide--session-for-thread-id thread-id directory)
                      (codex-ide--reusable-idle-session-for-directory directory))))
    (if session
        (progn
          (when (not (codex-ide-session-thread-id session))
            (codex-ide--reset-session-buffer session)
            (codex-ide--resume-thread-into-session session thread-id "Resumed")
            (codex-ide--update-header-line session))
          (codex-ide--show-session-buffer session)
          session)
      (let ((default-directory directory))
        (setq session (codex-ide--create-process-session)))
      (codex-ide--initialize-session session)
      (codex-ide--resume-thread-into-session session thread-id "Resumed")
      (codex-ide--update-header-line session)
      (codex-ide--show-session-buffer session)
      (codex-ide--ensure-input-prompt session)
      session)))

(defun codex-ide--start-session (&optional mode)
  "Start a Codex session for the current project.
MODE can be nil or `new', `continue', or `resume'."
  (codex-ide--prepare-session-operations)
  (let* ((working-dir (codex-ide--get-working-directory))
         (mode (or mode 'new))
         (query-session nil)
         (session nil)
         (created-session nil)
         (reused-session nil)
         (thread nil)
         (thread-id nil)
         (omit-thread-id (and (eq mode 'resume)
                              (when-let ((current-session
                                          (codex-ide--session-for-current-buffer)))
                                (codex-ide-session-thread-id current-session)))))
    (condition-case err
        (progn
          (unless (eq mode 'new)
            (setq query-session (codex-ide--query-session-for-thread-selection working-dir))
            (unless query-session
              (setq query-session (codex-ide--ensure-query-session-for-thread-selection
                                   working-dir)
                    created-session query-session)
              (codex-ide-log-message query-session "Starting session in mode %s" mode))
            (setq thread
                  (pcase mode
                    ('continue
                     (or (with-current-buffer (codex-ide-session-buffer query-session)
                           (codex-ide--latest-thread query-session))
                         (user-error "No Codex threads found for %s"
                                     (abbreviate-file-name working-dir))))
                    ('resume
                     (with-current-buffer (codex-ide-session-buffer query-session)
                       (codex-ide--pick-thread query-session omit-thread-id)))))
            (setq thread-id (alist-get 'id thread))
            (when-let ((existing-session
                        (codex-ide--session-for-thread-id thread-id working-dir)))
              (setq reused-session existing-session)))
          (if reused-session
              (progn
                (message "Showing Codex session for thread %s" thread-id)
                (codex-ide--show-session-buffer reused-session))
            (setq session (or created-session
                              (codex-ide--create-process-session))
                  created-session session)
            (codex-ide-log-message session "Starting session in mode %s" mode)
            (unless (eq session query-session)
              (codex-ide--initialize-session session))
            (when (and (eq session query-session)
                       (memq mode '(continue resume)))
              (codex-ide--reset-session-buffer session))
            (pcase mode
              ('new
               (let ((result (codex-ide--request-sync
                              session
                              "thread/start"
                              (with-current-buffer (codex-ide-session-buffer session)
                                (codex-ide--thread-start-params)))))
                 (setf (codex-ide-session-thread-id session)
                       (codex-ide--extract-thread-id result))
                 (codex-ide--session-metadata-put session :session-context-sent nil)
                 (codex-ide-log-message
                  session
                  "Started new thread %s"
                  (codex-ide-session-thread-id session))))
              ((or 'continue 'resume)
               (codex-ide--resume-thread-into-session
                session
                thread-id
                (if (eq mode 'continue) "Continued" "Resumed"))))
            (setf (codex-ide-session-status session) "idle")
            (codex-ide--update-header-line session)
            (codex-ide--show-session-buffer session)
            (codex-ide--track-active-buffer)
            (unless (codex-ide-session-output-prefix-inserted session)
              (codex-ide--ensure-input-prompt session))
            (message "Codex started in %s"
                     (file-name-nondirectory (directory-file-name working-dir)))
            session))
      (error
       (when created-session
         (let* ((stderr-tail (codex-ide--session-metadata-get created-session :stderr-tail))
                (classification
                 (codex-ide--render-session-error
                  created-session
                  (list (error-message-string err) stderr-tail)
                  "Codex startup failed")))
           (when (process-live-p (codex-ide-session-process created-session))
             (delete-process (codex-ide-session-process created-session)))
           (codex-ide--show-session-buffer created-session)
           (codex-ide--cleanup-session created-session)
           (signal 'user-error
                   (list (codex-ide--format-session-error-message
                          classification
                          (codex-ide--extract-error-text
                           (error-message-string err)
                           stderr-tail)
                          "Codex startup failed")))))
       (signal (car err) (cdr err)))
      (quit
       (when created-session
         (codex-ide-log-message created-session "Session startup aborted")
         (when (process-live-p (codex-ide-session-process created-session))
           (delete-process (codex-ide-session-process created-session)))
         (when (buffer-live-p (codex-ide-session-buffer created-session))
           (kill-buffer (codex-ide-session-buffer created-session)))
         (codex-ide--cleanup-session created-session))
       (signal 'quit nil)))))

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

(defun codex-ide--pending-approvals (&optional session)
  "Return the pending approval table for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (or (codex-ide--session-metadata-get session :pending-approvals)
      (codex-ide--session-metadata-put
       session
       :pending-approvals
       (make-hash-table :test 'equal))))

(defun codex-ide--approval-display-value (value)
  "Return a compact display string for approval VALUE."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%S" value))))

(defun codex-ide--approval-result (kind value params)
  "Build the JSON-RPC result for approval KIND with VALUE and PARAMS."
  (pcase kind
    ('permissions
     (if (eq value 'decline)
         '((permissions . []))
       `((permissions . ,(or (alist-get 'permissions params) '()))
         (scope . ,(symbol-name value)))))
    (_
     `((decision . ,value)))))

(defun codex-ide--mark-approval-resolved (approval label)
  "Update APPROVAL's transcript block to show resolved LABEL."
  (let ((buffer (marker-buffer (plist-get approval :status-marker)))
        (status-marker (plist-get approval :status-marker))
        (start-marker (plist-get approval :start-marker))
        (end-marker (plist-get approval :end-marker)))
    (when (and (buffer-live-p buffer)
               (markerp status-marker)
               (markerp start-marker)
               (markerp end-marker))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (status-pos (marker-position status-marker))
              (block-start (marker-position start-marker))
              (block-end (marker-position end-marker)))
          (when status-pos
            (save-excursion
              (goto-char status-pos)
              (insert (propertize "Selected: "
                                  'face
                                  'codex-ide-approval-label-face))
              (insert label)
              (insert "\n")))
          (when (and block-start block-end)
            (remove-text-properties
             block-start block-end
             '(action nil mouse-face nil help-echo nil follow-link nil
               keymap nil button nil category nil)))
          (when (and block-start block-end)
            (codex-ide--freeze-region block-start block-end)))))))

(defun codex-ide--resolve-buffer-approval (session id value label)
  "Resolve pending approval ID for SESSION as VALUE with display LABEL."
  (let* ((approvals (codex-ide--pending-approvals session))
         (approval (gethash id approvals)))
    (if (not approval)
        (message "Codex approval already resolved")
      (remhash id approvals)
      (codex-ide-log-message
       session
       "%s approval resolved as %s"
       (capitalize (symbol-name (plist-get approval :kind)))
       (codex-ide--approval-display-value value))
      (codex-ide--mark-approval-resolved approval label)
      (setf (codex-ide-session-status session)
            (if (codex-ide-session-current-turn-id session) "running" "idle"))
      (codex-ide--update-header-line session)
      (codex-ide--jsonrpc-send-response
       session
       id
       (codex-ide--approval-result
        (plist-get approval :kind)
        value
        (plist-get approval :params))))))

(defun codex-ide--insert-approval-choice-button (session id label value)
  "Insert an approval button for SESSION request ID with LABEL and VALUE."
  (make-text-button
   (point)
   (progn (insert (format "[%s]" label)) (point))
   'follow-link t
   'help-echo (format "Resolve Codex approval as %s" label)
   'action (lambda (_button)
             (codex-ide--resolve-buffer-approval session id value label))))

(defun codex-ide--insert-approval-label (label)
  "Insert an emphasized approval field LABEL."
  (insert (propertize label 'face 'codex-ide-approval-label-face)))

(defun codex-ide--insert-approval-detail (detail)
  "Insert one formatted approval DETAIL entry."
  (pcase (plist-get detail :kind)
    ('command
     (codex-ide--insert-approval-label "Run the following command?")
     (insert "\n\n    ")
     (insert (propertize (or (plist-get detail :text) "")
                         'face
                         'codex-ide-item-summary-face))
     (insert "\n\n"))
    (_
     (when-let ((label (plist-get detail :label)))
       (codex-ide--insert-approval-label (format "%s: " label)))
     (insert (or (plist-get detail :text) ""))
     (insert "\n"))))

(defun codex-ide--render-buffer-approval (session id kind title details choices params)
  "Render an inline approval block for SESSION request ID.
KIND identifies the approval result shape.  TITLE, DETAILS, CHOICES, and
PARAMS describe the request."
  (let ((buffer (codex-ide-session-buffer session))
        start-marker
        status-marker
        end-marker)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (moving (= (point) (point-max))))
        (codex-ide--ensure-output-spacing buffer)
        (setq start-marker (copy-marker (point)))
        (insert (propertize
                 (codex-ide--output-separator-string)
                 'face
                 'codex-ide-output-separator-face))
        (insert "\n")
        (insert (propertize title 'face 'codex-ide-approval-header-face))
        (insert "\n\n")
        (dolist (detail details)
          (codex-ide--insert-approval-detail detail))
        (dolist (choice choices)
          (codex-ide--insert-approval-choice-button
           session id (car choice) (cdr choice))
          (insert "\n"))
        (insert "\n")
        (setq status-marker (copy-marker (point)))
        (setq end-marker (copy-marker (point) t))
        (codex-ide--freeze-region (marker-position start-marker)
                                  (marker-position end-marker))
        (when moving
          (goto-char (point-max)))))
    (puthash id
             (list :kind kind
                   :params params
                   :start-marker start-marker
                   :status-marker status-marker
                   :end-marker end-marker)
             (codex-ide--pending-approvals session))
    (setf (codex-ide-session-status session) "approval")
    (codex-ide--update-header-line session)
    (codex-ide--show-session-buffer session)))

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
     (condition-case err
         (let* ((command (or (alist-get 'command params) "unknown command"))
                (choices (codex-ide--command-approval-choices params)))
           (if (codex-ide--auto-approve-emacs-bridge-request-p params)
               (let ((decision "acceptForSession"))
                 (codex-ide-log-message
                  session
                  "Command approval for %s resolved as %s"
                  command
                  decision)
                 (codex-ide--jsonrpc-send-response
                  session id `((decision . ,decision))))
             (codex-ide--render-buffer-approval
              session
              id
              'command
              "[Approval required]"
              (delq nil
                    (list
                     (list :kind 'command :text command)
                     (when-let ((reason (alist-get 'reason params)))
                       (list :label "Reason" :text reason))))
              choices
              params)))
       (quit
        (codex-ide-log-message session "Command approval prompt quit; canceling turn")
        (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))
       (error
        (codex-ide-log-message
         session
         "Command approval failed: %s"
         (error-message-string err))
        (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))))))

(defun codex-ide--handle-file-change-approval (&optional session id params)
  "Handle a file-change approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (run-at-time
   0 nil
   (lambda ()
     (condition-case err
         (let* ((reason (or (alist-get 'reason params) "approve file changes"))
                (choices '(("accept" . "accept")
                           ("accept for session" . "acceptForSession")
                           ("decline" . "decline")
                           ("cancel turn" . "cancel"))))
           (if (codex-ide--auto-approve-emacs-bridge-request-p params)
               (let ((decision "acceptForSession"))
                 (codex-ide-log-message
                  session
                  "File-change approval for %s resolved as %s"
                  reason
                  decision)
                 (codex-ide--jsonrpc-send-response
                  session id `((decision . ,decision))))
             (codex-ide--render-buffer-approval
             session
             id
             'file-change
              "[Approval required]"
              (list (list :label "Approve file changes" :text reason))
              choices
              params)))
       (quit
        (codex-ide-log-message session "File-change approval prompt quit; canceling turn")
        (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))
       (error
        (codex-ide-log-message
         session
         "File-change approval failed: %s"
         (error-message-string err))
        (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))))))

(defun codex-ide--handle-permissions-approval (&optional session id params)
  "Handle a permissions approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (run-at-time
   0 nil
   (lambda ()
     (condition-case err
         (let* ((permissions (or (alist-get 'permissions params) '()))
                (choices '(("grant for turn" . turn)
                           ("grant for session" . session)
                           ("decline" . decline))))
           (if (codex-ide--auto-approve-emacs-bridge-request-p params)
               (let ((choice 'session))
                 (codex-ide-log-message
                  session
                  "Permissions approval resolved as %s for %S"
                  choice
                  permissions)
                 (codex-ide--jsonrpc-send-response
                  session id
                  `((permissions . ,permissions)
                    (scope . ,(symbol-name choice)))))
             (codex-ide--render-buffer-approval
             session
             id
             'permissions
              "[Approval required]"
              (append
               (when-let ((reason (alist-get 'reason params)))
                 (list (list :label "Reason" :text reason)))
               (when permissions
                 (list (list :label "Permissions"
                             :text (format "%S" permissions)))))
              choices
              params)))
       (quit
        (codex-ide-log-message session "Permissions approval prompt quit; declining")
        (codex-ide--jsonrpc-send-response session id '((permissions . []))))
       (error
        (codex-ide-log-message
         session
         "Permissions approval failed: %s"
         (error-message-string err))
        (codex-ide--jsonrpc-send-response session id '((permissions . []))))))))

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
              (status (or (alist-get 'status params)
                          (alist-get 'status thread)))
              (normalized-status (codex-ide--normalize-session-status status)))
         (when normalized-status
           (setf (codex-ide-session-status session) normalized-status)))
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
           (unless (string-empty-p delta)
             (codex-ide-log-message
              session
              "Agent delta for item %s (%d chars)"
              item-id
              (length delta)))
           (codex-ide--ensure-agent-message-prefix session item-id)
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
       (let ((interrupted (codex-ide-session-interrupt-requested session))
             (turn-id (codex-ide-session-current-turn-id session)))
         (codex-ide-log-message
          session
          "Turn completed: %s"
          turn-id)
         (if turn-id
             (progn
               (when interrupted
                 (codex-ide-log-message session "Turn completed after interrupt request"))
               (codex-ide--finish-turn
                session
                (when interrupted "[Agent interrupted]")))
           (codex-ide-log-message
            session
            "Ignoring duplicate turn/completed notification for an already-closed turn"))))
      ("error"
       (let* ((codex-ide--current-agent-item-type "error")
              (info (codex-ide--notification-error-info params))
              (message (codex-ide--notification-error-message info))
              (details (codex-ide--notification-error-additional-details info))
              (detail (codex-ide--notification-error-display-detail info))
              (classification
               (codex-ide--classify-session-error
                detail
                (alist-get 'http-status info))))
         (codex-ide-log-message session "Error notification: %S" params)
         (if (alist-get 'will-retry info)
             (codex-ide--handle-retryable-notification-error session info)
           (progn
             (codex-ide--render-session-error
              session
              (list message (alist-get 'http-status info))
              "Codex notification")
             (codex-ide--append-notification-additional-details session details)
             (codex-ide--recover-from-session-error session classification)))))
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
      (if (process-live-p process)
          (progn
            (setf (codex-ide-session-status session) (string-trim event))
            (codex-ide--update-header-line session)
            (codex-ide--append-to-buffer
             buffer
             (format "\n[Codex process %s]\n" (string-trim event))
             'shadow))
        (let ((classification
               (codex-ide--render-session-error
                session
                (list (string-trim event)
                      (codex-ide--session-metadata-get session :stderr-tail))
                "Codex process exited")))
          (codex-ide--recover-from-session-error session classification)))
      (unless (process-live-p process)
        (codex-ide-log-message session "Process exited")
        (codex-ide--cleanup-session session)))))

(defun codex-ide--compose-turn-payload (prompt)
  "Build prompt payload metadata for PROMPT in the current working directory."
  (let* ((context-payload (codex-ide--context-payload-for-prompt))
         (context-prefix (alist-get 'formatted context-payload))
         (session (codex-ide--get-default-session-for-current-buffer))
         (session-prefix (unless (codex-ide--session-metadata-get session :session-context-sent)
                           (codex-ide--format-session-context)))
         (prompt-prefix (unless (codex-ide--leading-emacs-context-prefix-p prompt)
                          context-prefix))
         (full-prompt (string-join (delq nil (list session-prefix prompt-prefix prompt)) "\n\n")))
    `((context-summary . ,(alist-get 'summary context-payload))
      (included-session-context . ,(and session-prefix t))
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
      (when (y-or-n-p "No Codex session for this workspace. Start one? ")
        (codex-ide--start-session 'new))
      (codex-ide--session-for-current-project)))

;;;###autoload
(defun codex-ide ()
  "Start Codex for the current project or directory."
  (interactive)
  (codex-ide--start-session 'new))

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
  "Stop the Codex session associated with the current session buffer."
  (interactive)
  (let* ((session (and (derived-mode-p 'codex-ide-session-mode)
                       (codex-ide--session-for-current-buffer)))
         (working-dir (and session (codex-ide-session-directory session)))
         (buffer (and session (codex-ide-session-buffer session))))
    (unless session
      (user-error "Codex stop is only available in a Codex session buffer"))
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
      (message "No Codex session is running in this buffer")))))

;;;###autoload
(defun codex-ide-switch-to-buffer ()
  "Show the Codex buffer for the current project."
  (interactive)
  (let* ((session (codex-ide--ensure-session-for-current-project))
         (window (let ((codex-ide-display-buffer-options
                        '(:reuse-buffer-window :reuse-mode-window :new-window)))
                   (codex-ide-display-buffer
                    (codex-ide-session-buffer session)))))
    (when window
      (select-window window))
    (codex-ide--ensure-input-prompt session)
    session))

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
               (window (let ((codex-ide-display-buffer-options
                              '(:reuse-buffer-window :reuse-mode-window :new-window)))
                         (codex-ide-display-buffer buffer))))
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
        (progn
          (codex-ide--request-sync
           session
           "turn/start"
           `((threadId . ,thread-id)
             (input . ,(alist-get 'input payload))))
          (when (alist-get 'included-session-context payload)
            (codex-ide--session-metadata-put session :session-context-sent t)))
      (error
       (codex-ide-log-message session "Prompt submission failed: %s" (error-message-string err))
       (codex-ide--reopen-input-after-submit-error session prompt-to-send err)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun codex-ide-submit ()
  "Submit the current in-buffer prompt to Codex."
  (interactive)
  (codex-ide--submit-prompt))

(require 'codex-ide-delete-session-thread)

(provide 'codex-ide)

;;; codex-ide.el ends here
