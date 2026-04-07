;;; codex-ide-bridge.el --- Emacs MCP bridge helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; This module provides the Emacs-side half of the optional Codex MCP bridge.
;; The external MCP server talks to the running Emacs instance via emacsclient
;; and dispatches JSON tool calls into `codex-ide-bridge--tool-call'.

;;; Code:

(require 'json)
(require 'server)
(require 'subr-x)

(defconst codex-ide-bridge--directory
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
(defcustom codex-ide-emacs-tool-bridge-name "emacs"
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

When nil, codex-ide uses `codex-ide-mcp.py' from the package directory."
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

This only affects explicit calls to `codex-ide-bridge-ensure-server'.  Session
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

(defun codex-ide-bridge--toml-string (value)
  "Encode VALUE as a TOML string."
  (format "\"%s\""
          (replace-regexp-in-string
           "\"" "\\\\\""
           (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                     (or value "")
                                     t t)
           t t)))

(defun codex-ide-bridge--toml-array (values)
  "Encode VALUES as a TOML array."
  (format "[%s]"
          (string-join (mapcar #'codex-ide-bridge--toml-string values) ",")))

(defun codex-ide-bridge--resolved-script-path ()
  "Return the absolute path to the standalone bridge script."
  (expand-file-name
   (or codex-ide-emacs-bridge-script-path "codex-ide-mcp.py")
   codex-ide-bridge--directory))

(defun codex-ide-bridge--resolved-server-name ()
  "Return the emacsclient server name the bridge should target."
  (or codex-ide-emacs-bridge-server-name server-name))

;;;###autoload
(defun codex-ide-bridge-enabled-p ()
  "Return non-nil when the Emacs MCP bridge should be enabled."
  codex-ide-enable-emacs-tool-bridge)

;;;###autoload
(defun codex-ide-bridge-enable ()
  "Enable the Emacs MCP bridge and ensure the target Emacs server is running."
  (setq codex-ide-enable-emacs-tool-bridge t)
  (codex-ide-bridge-ensure-server))

;;;###autoload
(defun codex-ide-bridge-disable ()
  "Disable the Emacs MCP bridge."
  (setq codex-ide-enable-emacs-tool-bridge nil)
  codex-ide-enable-emacs-tool-bridge)

;;;###autoload
(defun codex-ide-bridge-prompt-to-enable ()
  "Prompt once to enable the Emacs MCP bridge for session startup."
  (when (and (not (codex-ide-bridge-enabled-p))
             (y-or-n-p "Enable the Emacs tool bridge for this Codex session? "))
    (codex-ide-bridge-enable)))

(defun codex-ide-bridge--ensure-server-running-p (target-server-name)
  "Return non-nil when TARGET-SERVER-NAME is running.
Errors from `server-running-p' are treated as nil."
  (server-running-p target-server-name))

;;;###autoload
(defun codex-ide-bridge-status ()
  "Return an alist describing the current Emacs bridge configuration."
  (let* ((enabled (codex-ide-bridge-enabled-p))
         (script-path (codex-ide-bridge--resolved-script-path))
         (python-path (and enabled
                           (executable-find codex-ide-emacs-bridge-python-command)))
         (emacsclient-path (and enabled
                                (executable-find
                                 codex-ide-emacs-bridge-emacsclient-command)))
         (server-name (codex-ide-bridge--resolved-server-name))
         (server-running (and enabled
                              (codex-ide-bridge--ensure-server-running-p
                               server-name)))
         (ready (and enabled
                     (file-exists-p script-path)
                     python-path
                     emacsclient-path)))
    `((enabled . ,enabled)
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
(defun codex-ide-bridge-ensure-server ()
  "Ensure the target Emacs server for the bridge is running."
  (when (codex-ide-bridge-enabled-p)
    (let ((server-name (codex-ide-bridge--resolved-server-name)))
      (unless (codex-ide-bridge--ensure-server-running-p server-name)
        (server-start nil codex-ide-suppress-server-start-prompts)))))

;;;###autoload
(defun codex-ide-bridge-mcp-config-args ()
  "Return `codex app-server' CLI args that register the Emacs MCP bridge."
  (when (codex-ide-bridge-enabled-p)
    (let* ((bridge-name codex-ide-emacs-tool-bridge-name)
           (prefix (format "mcp_servers.%s" bridge-name))
           (script-path (codex-ide-bridge--resolved-script-path))
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
                         (codex-ide-bridge--toml-string
                          python-command))
            "-c" (format "%s.args=%s"
                         prefix
                         (codex-ide-bridge--toml-array script-args))
            "-c" (format "%s.startup_timeout_sec=%s"
                         prefix
                         codex-ide-emacs-bridge-startup-timeout)
            "-c" (format "%s.tool_timeout_sec=%s"
                         prefix
                         codex-ide-emacs-bridge-tool-timeout)))))

(defun codex-ide-bridge--current-context ()
  "Return the current Emacs buffer context as an alist."
  (or (and (fboundp 'codex-ide--make-buffer-context)
           (codex-ide--make-buffer-context (current-buffer)))
      (let ((file (buffer-file-name)))
        (when file
          `((file . ,(expand-file-name file))
            (display-file . ,(file-name-nondirectory file))
            (buffer-name . ,(buffer-name))
            (line . ,(line-number-at-pos))
            (column . ,(current-column)))))))

(defun codex-ide-bridge--tool-call (name params)
  "Dispatch bridge tool NAME using PARAMS."
  (let ((handler (intern-soft (format "codex-ide-bridge--tool-call--%s" name))))
    (if (fboundp handler)
        (funcall handler params)
      (let ((error-message (format "Bridge tool not implemented: %s" name)))
        (message "%s" error-message)
        `((error . ,error-message))))))

;;;###autoload
(defun codex-ide-bridge--json-tool-call (payload)
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
    (json-encode (codex-ide-bridge--tool-call name params))))

;; These functions are the Elisp implementations of the MCP bridge commands.

(defun codex-ide-bridge--tool-call--emacs_get_context (_params)
  "Handle an `emacs_get_context' bridge request."
  (or (codex-ide-bridge--current-context)
      '((context . :json-null))))

(defun codex-ide-bridge--tool-call--emacs_open_file (params)
  "Handle an `emacs_open_file' bridge request with PARAMS."
  (let ((path (alist-get 'path params))
        (line (alist-get 'line params))
        (column (alist-get 'column params)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (error "Missing file path"))
    (setq path (expand-file-name path))
    (find-file path)
    (goto-char (point-min))
    (when (and (integerp line) (> line 0))
      (forward-line (1- line)))
    (when (and (integerp column) (> column 0))
      (move-to-column (1- column)))
    `((path . ,path)
      (buffer . ,(buffer-name))
      (line . ,(line-number-at-pos))
      (column . ,(1+ (current-column))))))

(defun codex-ide-bridge--tool-call--emacs_run_command (params)
  "Handle an `emacs_run_command' bridge request with PARAMS."
  (let* ((command-name (alist-get 'command params))
         (command-symbol (and (stringp command-name)
                              (intern-soft command-name))))
    (unless (and command-symbol
                 (commandp command-symbol))
      (error "Unknown command: %s" (or command-name "unknown")))
    (call-interactively command-symbol)
    `((command . ,command-name)
      (buffer . ,(buffer-name))
      (context . ,(or (codex-ide-bridge--current-context) :json-null)))))

(defun codex-ide-bridge--tool-call--emacs_eval (params)
  "Handle an `emacs_eval' bridge request with PARAMS."
  (let* ((expression (alist-get 'expression params))
         (form (car (read-from-string expression)))
         (result (eval form t)))
    `((value . ,(format "%S" result)))))

(provide 'codex-ide-bridge)

;;; codex-ide-bridge.el ends here
