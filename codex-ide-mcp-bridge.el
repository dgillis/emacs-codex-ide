;;; codex-ide-mcp-bridge.el --- Emacs MCP bridge helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; This module provides the Emacs-side half of the optional Codex MCP bridge.
;; The external MCP server talks to the running Emacs instance via emacsclient
;; and dispatches JSON tool calls into `codex-ide-mcp-bridge--tool-call'.

;;; Code:

(require 'json)
(require 'seq)
(require 'server)
(require 'subr-x)

(defconst codex-ide-mcp-bridge--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the codex-ide bridge files.")

;;;###autoload
(defcustom codex-ide-enable-emacs-tool-bridge nil
  "Whether codex-ide should expose Emacs tools to Codex via MCP.

When non-nil, codex-ide starts an MCP bridge server alongside `codex app-server'
and ensures the current Emacs instance is reachable via `emacsclient'."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-want-mcp-bridge 'prompt
  "Whether codex-ide should start the Emacs MCP bridge.

When nil, do not start the bridge.  When t, start the bridge without prompting.
When `prompt', ask before enabling the bridge, matching the historical startup
behavior."
  :type '(choice (const :tag "Do not start" nil)
                 (const :tag "Start without prompting" t)
                 (const :tag "Prompt at startup" prompt))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-tool-bridge-name "codex-ide-emacs-mcp"
  "Name used when registering the Emacs MCP bridge with Codex."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-bridge-python-command "python3"
  "Python executable used to launch the standalone Emacs MCP bridge."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-bridge-emacsclient-command "emacsclient"
  "Path to the `emacsclient' executable used by the bridge."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-bridge-script-path nil
  "Path to the standalone Emacs MCP bridge script.

When nil, codex-ide uses `bin/codex-ide-mcp-server.py' from the package directory."
  :type '(choice (const :tag "Default" nil)
                 (file :tag "Bridge script"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-bridge-server-name nil
  "Server name the bridge should use with `emacsclient'.

When nil, use the current value of `server-name'."
  :type '(choice (const :tag "Current server" nil)
                 (string :tag "Named server"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-suppress-server-start-prompts nil
  "When non-nil, start the Emacs server for the bridge without prompting.

This only affects explicit calls to `codex-ide-mcp-bridge-ensure-server'.  Session
startup now prompts once about enabling the Emacs tool bridge, and enabling the
bridge starts the Emacs server automatically when needed."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-bridge-startup-timeout 10
  "Startup timeout in seconds for the Emacs MCP bridge."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-bridge-tool-timeout 60
  "Tool-call timeout in seconds for the Emacs MCP bridge."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-emacs-bridge-require-approval nil
  "Whether Emacs MCP bridge tool calls should require user approval.

When nil, `codex-ide' auto-accepts approval-like MCP elicitations that clearly
refer to the configured Emacs MCP bridge server or one of its tools."
  :type 'boolean
  :group 'codex-ide)

(defconst codex-ide-mcp-bridge--tool-names
  '("emacs_get_all_buffers"
    "emacs_get_buffer_info"
    "emacs_get_buffer_text"
    "emacs_get_buffer_diagnostics"
    "emacs_get_all_windows"
    "emacs_ensure_file_buffer_open"
    "emacs_show_file_buffer"
    "emacs_kill_file_buffer"
    "emacs_lisp_check_parens")
  "Tool names exposed by the Emacs MCP bridge.")

(defun codex-ide-mcp-bridge--tool-handler-suffix (name)
  "Return the handler suffix for external bridge tool NAME."
  (if (string-prefix-p "emacs_" name)
      (substring name (length "emacs_"))
    name))

(defun codex-ide-mcp-bridge--toml-string (value)
  "Encode VALUE as a TOML string."
  (format "\"%s\""
          (replace-regexp-in-string
           "\"" "\\\\\""
           (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                     (or value "")
                                     t t)
           t t)))

(defun codex-ide-mcp-bridge--toml-array (values)
  "Encode VALUES as a TOML array."
  (format "[%s]"
          (string-join (mapcar #'codex-ide-mcp-bridge--toml-string values) ",")))

(defun codex-ide-mcp-bridge--resolved-script-path ()
  "Return the absolute path to the standalone bridge script."
  (expand-file-name
   (or codex-ide-emacs-bridge-script-path "bin/codex-ide-mcp-server.py")
   codex-ide-mcp-bridge--directory))

(defun codex-ide-mcp-bridge--resolved-server-name ()
  "Return the emacsclient server name the bridge should target."
  (or codex-ide-emacs-bridge-server-name server-name))

;;;###autoload
(defun codex-ide-mcp-bridge-request-exempt-from-approval-p (params)
  "Return non-nil when PARAMS describe an Emacs MCP bridge request.

This is used to bypass bridge-originated elicitation prompts when
`codex-ide-emacs-bridge-require-approval' is nil."
  (and (not codex-ide-emacs-bridge-require-approval)
       (equal (alist-get 'serverName params)
              codex-ide-emacs-tool-bridge-name)))

;;;###autoload
(defun codex-ide-mcp-bridge-enabled-p ()
  "Return non-nil when the Emacs MCP bridge should be enabled."
  (cond
   ((eq codex-ide-want-mcp-bridge nil) nil)
   ((eq codex-ide-want-mcp-bridge t) t)
   ((eq codex-ide-want-mcp-bridge 'prompt)
    codex-ide-enable-emacs-tool-bridge)
   (t nil)))

;;;###autoload
(defun codex-ide-mcp-bridge-enable ()
  "Enable the Emacs MCP bridge and ensure the target Emacs server is running."
  (setq codex-ide-enable-emacs-tool-bridge t)
  (when (eq codex-ide-want-mcp-bridge nil)
    (setq codex-ide-want-mcp-bridge t))
  (codex-ide-mcp-bridge-ensure-server))

;;;###autoload
(defun codex-ide-mcp-bridge-disable ()
  "Disable the Emacs MCP bridge."
  (setq codex-ide-want-mcp-bridge nil)
  (setq codex-ide-enable-emacs-tool-bridge nil)
  codex-ide-enable-emacs-tool-bridge)

;;;###autoload
(defun codex-ide-mcp-bridge-prompt-to-enable ()
  "Prompt once to enable the Emacs MCP bridge for session startup."
  (cond
   ((eq codex-ide-want-mcp-bridge t)
    (codex-ide-mcp-bridge-enable))
   ((eq codex-ide-want-mcp-bridge 'prompt)
    (when (and (not (codex-ide-mcp-bridge-enabled-p))
               (y-or-n-p "Enable the Emacs tool bridge for this Codex session? "))
      (codex-ide-mcp-bridge-enable)))))

(defun codex-ide-mcp-bridge--ensure-server-running-p (target-server-name)
  "Return non-nil when TARGET-SERVER-NAME is running.
Errors from `server-running-p' are treated as nil."
  (server-running-p target-server-name))

;;;###autoload
(defun codex-ide-mcp-bridge-status ()
  "Return an alist describing the current Emacs bridge configuration."
  (let* ((enabled (codex-ide-mcp-bridge-enabled-p))
         (script-path (codex-ide-mcp-bridge--resolved-script-path))
         (python-path (and enabled
                           (executable-find codex-ide-emacs-bridge-python-command)))
         (emacsclient-path (and enabled
                                (executable-find
                                 codex-ide-emacs-bridge-emacsclient-command)))
         (server-name (codex-ide-mcp-bridge--resolved-server-name))
         (server-running (and enabled
                              (codex-ide-mcp-bridge--ensure-server-running-p
                               server-name)))
         (ready (and enabled
                     (file-exists-p script-path)
                     python-path
                     emacsclient-path)))
    `((enabled . ,enabled)
      (want . ,codex-ide-want-mcp-bridge)
      (ready . ,ready)
      (scriptPath . ,script-path)
      (scriptExists . ,(file-exists-p script-path))
      (pythonCommand . ,codex-ide-emacs-bridge-python-command)
      (pythonPath . ,python-path)
      (emacsclientCommand . ,codex-ide-emacs-bridge-emacsclient-command)
      (emacsclientPath . ,emacsclient-path)
      (serverName . ,server-name)
      (serverRunning . ,server-running))))

;;;###autoload
(defun codex-ide-mcp-bridge-ensure-server ()
  "Ensure the target Emacs server for the bridge is running."
  (when (codex-ide-mcp-bridge-enabled-p)
    (let ((server-name (codex-ide-mcp-bridge--resolved-server-name)))
      (unless (codex-ide-mcp-bridge--ensure-server-running-p server-name)
        (server-start nil codex-ide-suppress-server-start-prompts)))))

;;;###autoload
(defun codex-ide-mcp-bridge-mcp-config-args ()
  "Return `codex app-server' CLI args that register the Emacs MCP bridge."
  (when (codex-ide-mcp-bridge-enabled-p)
    (let* ((bridge-name codex-ide-emacs-tool-bridge-name)
           (prefix (format "mcp_servers.%s" bridge-name))
           (script-path (codex-ide-mcp-bridge--resolved-script-path))
           (python-command
            (or (executable-find codex-ide-emacs-bridge-python-command)
                codex-ide-emacs-bridge-python-command))
           (emacsclient-command
            (or (executable-find codex-ide-emacs-bridge-emacsclient-command)
                codex-ide-emacs-bridge-emacsclient-command))
           (server-name codex-ide-emacs-bridge-server-name)
           (script-args
            (append (list script-path
                          "--emacsclient"
                          emacsclient-command)
                    (when (and server-name
                               (not (string-empty-p server-name)))
                      (list "--server-name" server-name)))))
      (list "-c" (format "%s.command=%s"
                         prefix
                         (codex-ide-mcp-bridge--toml-string
                          python-command))
            "-c" (format "%s.args=%s"
                         prefix
                         (codex-ide-mcp-bridge--toml-array script-args))
            "-c" (format "%s.startup_timeout_sec=%s"
                         prefix
                         codex-ide-emacs-bridge-startup-timeout)
            "-c" (format "%s.tool_timeout_sec=%s"
                         prefix
                         codex-ide-emacs-bridge-tool-timeout)))))

(defun codex-ide-mcp-bridge--json-bool (value)
  "Return VALUE as an explicit JSON boolean."
  (if value t :json-false))

(defun codex-ide-mcp-bridge--json-nullable (value)
  "Return VALUE or an explicit JSON null."
  (or value :json-null))

(defun codex-ide-mcp-bridge--json-array (values)
  "Return VALUES as a JSON array."
  (vconcat values))

(defun codex-ide-mcp-bridge--buffer-info (buffer)
  "Return a buffer-info alist for BUFFER."
  (with-current-buffer buffer
    `((buffer . ,(buffer-name buffer))
      (file . ,(codex-ide-mcp-bridge--json-nullable
                (when-let ((file (buffer-file-name buffer)))
                  (expand-file-name file))))
      (major-mode . ,(symbol-name major-mode))
      (modified . ,(codex-ide-mcp-bridge--json-bool
                    (buffer-modified-p buffer)))
      (read-only . ,(codex-ide-mcp-bridge--json-bool
                     buffer-read-only)))))

(defun codex-ide-mcp-bridge--diagnostic-severity (diagnostic)
  "Return a normalized severity string for DIAGNOSTIC."
  (cond
   ((and (fboundp 'flymake-diagnostic-type)
         (ignore-errors (flymake-diagnostic-type diagnostic)))
    (pcase (flymake-diagnostic-type diagnostic)
      ('eglot-note "note")
      ('eglot-warning "warning")
      ('eglot-error "error")
      ('warning "warning")
      ('error "error")
      (_ (format "%s" (flymake-diagnostic-type diagnostic)))))
   ((and (fboundp 'flycheck-error-level)
         (ignore-errors (flycheck-error-level diagnostic)))
    (let* ((level (flycheck-error-level diagnostic))
           (severity (and (fboundp 'flycheck-error-level-severity)
                          (flycheck-error-level-severity level)))
           (level-id (or (and (fboundp 'flycheck-error-level-id)
                              (flycheck-error-level-id level))
                         level)))
      (cond
       ((and severity (<= severity 0)) "error")
       ((and severity (= severity 1)) "warning")
       ((symbolp level-id) (symbol-name level-id))
       (level-id (format "%s" level-id))
       (t "unknown"))))
   (t "unknown")))

(defun codex-ide-mcp-bridge--flymake-diagnostics ()
  "Return current Flymake diagnostics as a list of alists."
  (when (and (boundp 'flymake-mode)
             flymake-mode
             (fboundp 'flymake-diagnostics))
    (mapcar
     (lambda (diag)
       `((source . "flymake")
         (buffer . ,(buffer-name))
         (file . ,(codex-ide-mcp-bridge--json-nullable
                   (when-let ((file (buffer-file-name)))
                     (expand-file-name file))))
         (message . ,(flymake-diagnostic-text diag))
         (severity . ,(codex-ide-mcp-bridge--diagnostic-severity diag))
         (line . ,(line-number-at-pos (flymake-diagnostic-beg diag)))
         (column . ,(save-excursion
                      (goto-char (flymake-diagnostic-beg diag))
                      (1+ (current-column))))
         (end-line . ,(line-number-at-pos (flymake-diagnostic-end diag)))
         (end-column . ,(save-excursion
                          (goto-char (flymake-diagnostic-end diag))
                          (1+ (current-column))))))
     (flymake-diagnostics))))

(defun codex-ide-mcp-bridge--flycheck-diagnostics ()
  "Return current Flycheck diagnostics as a list of alists."
  (when (and (boundp 'flycheck-mode)
             flycheck-mode
             (boundp 'flycheck-current-errors)
             flycheck-current-errors)
    (mapcar
     (lambda (err)
       `((source . "flycheck")
         (buffer . ,(buffer-name))
         (file . ,(codex-ide-mcp-bridge--json-nullable
                   (when-let ((file (or (and (fboundp 'flycheck-error-filename)
                                             (flycheck-error-filename err))
                                        (buffer-file-name))))
                     (expand-file-name file))))
         (message . ,(or (and (fboundp 'flycheck-error-message)
                              (flycheck-error-message err))
                         ""))
         (severity . ,(codex-ide-mcp-bridge--diagnostic-severity err))
         (line . ,(or (and (fboundp 'flycheck-error-line)
                           (flycheck-error-line err))
                      1))
         (column . ,(or (and (fboundp 'flycheck-error-column)
                             (flycheck-error-column err))
                        1))
         (end-line . ,(or (and (fboundp 'flycheck-error-end-line)
                               (flycheck-error-end-line err))
                          :json-null))
         (end-column . ,(or (and (fboundp 'flycheck-error-end-column)
                                 (flycheck-error-end-column err))
                            :json-null))))
     flycheck-current-errors)))

(defun codex-ide-mcp-bridge--tool-call (name params)
  "Dispatch bridge tool NAME using PARAMS."
  (let* ((suffix (codex-ide-mcp-bridge--tool-handler-suffix name))
         (handler (intern-soft (format "codex-ide-mcp-bridge--tool-call--%s" suffix))))
    (if (fboundp handler)
        (funcall handler params)
      (let ((error-message (format "Bridge tool not implemented: %s" name)))
        (message "%s" error-message)
        `((error . ,error-message))))))

;;;###autoload
(defun codex-ide-mcp-bridge--json-tool-call (payload)
  "Decode JSON PAYLOAD, dispatch a bridge tool call, and return JSON."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-false :json-false)
         (json-null :json-null)
         (request (json-read-from-string payload))
         (name (alist-get 'name request))
         (params (or (alist-get 'params request) '())))
    (unless (stringp name)
      (error "Missing tool name"))
    (json-encode (codex-ide-mcp-bridge--tool-call name params))))

;; These functions are the Elisp implementations of the MCP bridge commands.

(defun codex-ide-mcp-bridge--resolve-file-buffer (path)
  "Return the buffer visiting PATH, opening it if needed."
  (find-file-noselect (expand-file-name path)))

(defun codex-ide-mcp-bridge--goto-line-and-column (line column)
  "Move point to LINE and COLUMN when provided."
  (goto-char (point-min))
  (when (and (integerp line) (> line 0))
    (forward-line (1- line)))
  (when (and (integerp column) (> column 0))
    (move-to-column (1- column))))

(defun codex-ide-mcp-bridge--find-target-window (origin)
  "Return a non-ORIGIN window, splitting ORIGIN if needed."
  (or (seq-find (lambda (window)
                  (not (eq window origin)))
                (window-list (selected-frame) 'no-minibuf origin))
      (or (ignore-errors (split-window origin nil 'right))
          (ignore-errors (split-window origin nil 'below))
          (let ((split-width-threshold 0)
                (split-height-threshold 0))
            (ignore-errors
              (with-selected-window origin
                (split-window-sensibly origin))))
          (error "Unable to create a window for file buffer view"))))

(defun codex-ide-mcp-bridge--file-buffer-response (buffer &optional extra)
  "Return a bridge response for BUFFER merged with EXTRA."
  (append
   `((path . ,(codex-ide-mcp-bridge--json-nullable
               (buffer-file-name buffer)))
     (buffer . ,(buffer-name buffer))
     (line . ,(with-current-buffer buffer
                (line-number-at-pos)))
     (column . ,(with-current-buffer buffer
                  (1+ (current-column)))))
   extra))

(defun codex-ide-mcp-bridge--tool-call--ensure_file_buffer_open (params)
  "Handle an `ensure_file_buffer_open' bridge request with PARAMS."
  (let ((path (alist-get 'path params))
        buffer
        already-open)
    (unless (and (stringp path) (not (string-empty-p path)))
      (error "Missing file path"))
    (setq path (expand-file-name path))
    (setq already-open (and (find-buffer-visiting path) t))
    (setq buffer (codex-ide-mcp-bridge--resolve-file-buffer path))
    (codex-ide-mcp-bridge--file-buffer-response
     buffer
     `((already-open . ,(codex-ide-mcp-bridge--json-bool already-open))))))

(defun codex-ide-mcp-bridge--tool-call--show_file_buffer (params)
  "Handle a `show_file_buffer' bridge request with PARAMS."
  (let ((path (alist-get 'path params))
        (line (alist-get 'line params))
        (column (alist-get 'column params))
        origin
        target
        buffer)
    (unless (and (stringp path) (not (string-empty-p path)))
      (error "Missing file path"))
    (setq path (expand-file-name path))
    (setq buffer (codex-ide-mcp-bridge--resolve-file-buffer path))
    (setq origin (selected-window))
    (save-selected-window
      (setq target (codex-ide-mcp-bridge--find-target-window origin))
      (set-window-buffer target buffer)
      (with-selected-window target
        (codex-ide-mcp-bridge--goto-line-and-column line column)))
    (codex-ide-mcp-bridge--file-buffer-response
     buffer
     `((window-id . ,(format "%s" target))))))

(defun codex-ide-mcp-bridge--tool-call--kill_file_buffer (params)
  "Handle a `kill_file_buffer' bridge request with PARAMS."
  (let* ((path (alist-get 'path params))
         (expanded-path (and (stringp path)
                             (not (string-empty-p path))
                             (expand-file-name path)))
         (buffer (and expanded-path
                      (find-buffer-visiting expanded-path))))
    (unless expanded-path
      (error "Missing file path"))
    (if (not buffer)
        `((path . ,expanded-path)
          (buffer . :json-null)
          (killed . :json-false))
      (let ((buffer-name (buffer-name buffer))
            (killed (kill-buffer buffer)))
        `((path . ,expanded-path)
          (buffer . ,(codex-ide-mcp-bridge--json-nullable buffer-name))
          (killed . ,(codex-ide-mcp-bridge--json-bool killed)))))))

(defun codex-ide-mcp-bridge--tool-call--lisp_check_parens (params)
  "Handle a `lisp_check_parens' bridge request with PARAMS."
  (let ((path (alist-get 'path params)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (error "Missing file path"))
    (setq path (expand-file-name path))
    (with-current-buffer (codex-ide-mcp-bridge--resolve-file-buffer path)
      (save-mark-and-excursion
        (save-restriction
          (widen)
          (let ((inhibit-message t))
            (condition-case err
                (progn
                  (check-parens)
                  `((path . ,path)
                    (balanced . t)
                    (mismatch . :json-false)))
              (user-error
               (let ((mismatch-point (point)))
                 `((path . ,path)
                   (balanced . :json-false)
                   (mismatch . t)
                   (point . ,mismatch-point)
                   (line . ,(line-number-at-pos mismatch-point))
                   (column . ,(save-excursion
                                (goto-char mismatch-point)
                                (1+ (current-column))))
                   (message . ,(error-message-string err))))))))))))

(defun codex-ide-mcp-bridge--tool-call--get_all_buffers (_params)
  "Handle a `get_all_buffers' bridge request."
  `((files . ,(codex-ide-mcp-bridge--json-array
               (seq-filter
                #'identity
                (mapcar
                 (lambda (buffer)
                   (when (buffer-file-name buffer)
                     (codex-ide-mcp-bridge--buffer-info buffer)))
                 (buffer-list)))))))

(defun codex-ide-mcp-bridge--tool-call--get_buffer_info (params)
  "Handle a `get_buffer_info' bridge request with PARAMS."
  (let* ((buffer-name (alist-get 'buffer params))
         (buffer (and (stringp buffer-name)
                      (get-buffer buffer-name))))
    (unless buffer
      (error "Unknown buffer: %s" (or buffer-name "nil")))
    (codex-ide-mcp-bridge--buffer-info buffer)))

(defun codex-ide-mcp-bridge--tool-call--get_buffer_text (params)
  "Handle a `get_buffer_text' bridge request with PARAMS."
  (let* ((buffer-name (alist-get 'buffer params))
         (buffer (and (stringp buffer-name)
                      (get-buffer buffer-name))))
    (unless buffer
      (error "Unknown buffer: %s" (or buffer-name "nil")))
    (with-current-buffer buffer
      `((buffer . ,(buffer-name buffer))
        (text . ,(buffer-substring-no-properties (point-min) (point-max)))))))

(defun codex-ide-mcp-bridge--tool-call--get_buffer_diagnostics (params)
  "Handle a `get_buffer_diagnostics' bridge request with PARAMS."
  (let* ((buffer-name (alist-get 'buffer params))
         (buffer (and (stringp buffer-name)
                      (get-buffer buffer-name))))
    (unless buffer
      (error "Unknown buffer: %s" (or buffer-name "nil")))
    (with-current-buffer buffer
      `((buffer . ,(buffer-name buffer))
        (file . ,(codex-ide-mcp-bridge--json-nullable
                  (when-let ((file (buffer-file-name buffer)))
                    (expand-file-name file))))
        (diagnostics . ,(codex-ide-mcp-bridge--json-array
                         (or (codex-ide-mcp-bridge--flymake-diagnostics)
                             (codex-ide-mcp-bridge--flycheck-diagnostics)
                             '())))))))

(defun codex-ide-mcp-bridge--tool-call--get_all_windows (_params)
  "Handle a `get_all_windows' bridge request."
  (let ((windows
         (mapcar
          (lambda (window)
            (let ((buffer (window-buffer window)))
              `((window-id . ,(format "%s" window))
                (selected . ,(codex-ide-mcp-bridge--json-bool
                              (eq window (selected-window))))
                (dedicated . ,(codex-ide-mcp-bridge--json-bool
                               (window-dedicated-p window)))
                (point . ,(window-point window))
                (start . ,(window-start window))
                (edges . ,(append (window-edges window) nil))
                (buffer-info . ,(codex-ide-mcp-bridge--buffer-info buffer)))))
          (window-list (selected-frame) 'no-minibuf (frame-first-window)))))
    `((windows . ,(codex-ide-mcp-bridge--json-array windows)))))

(provide 'codex-ide-mcp-bridge)

;;; codex-ide-mcp-bridge.el ends here
