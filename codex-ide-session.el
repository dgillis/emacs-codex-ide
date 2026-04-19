;;; codex-ide-session.el --- Session and process lifecycle for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns lifecycle management for live Codex app-server sessions.
;;
;; It is the place where codex-ide decides how to:
;;
;; - Detect and launch the Codex CLI.
;; - Create, initialize, tear down, and clean up session objects and their
;;   backing processes.
;; - Parse process output into JSON messages and dispatch them to protocol or
;;   transcript handlers.
;; - Start new sessions, continue stored threads, resume selected threads, stop
;;   sessions, reset the current session, and interrupt running turns.
;; - Show the session buffer in a window after lifecycle transitions.
;;
;; This file deliberately does not own transcript mutation details or prompt UI
;; state transitions.  It delegates those to `codex-ide-transcript.el`.  It
;; also delegates JSON-RPC request semantics to `codex-ide-protocol.el`.  The
;; goal is a clear controller boundary: this file decides *which session/process
;; operation should happen now*, while transcript and protocol modules handle
;; their narrower concerns.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-context)
(require 'codex-ide-core)
(require 'codex-ide-errors)
(require 'codex-ide-log)
(require 'codex-ide-mcp-bridge)
(require 'codex-ide-mcp-elicitation)
(require 'codex-ide-protocol)
(require 'codex-ide-renderer)
(require 'codex-ide-session-mode)
(require 'codex-ide-threads)
(require 'codex-ide-transcript)
(require 'codex-ide-window)

(defvar codex-ide-cli-path)
(defvar codex-ide-cli-extra-flags)
(defvar codex-ide-new-session-split)
(defvar codex-ide-display-buffer-options)
(defvar codex-ide--cli-available nil
  "Whether the Codex CLI has been detected successfully.")
(defvar codex-ide--current-transcript-log-marker)

(declare-function codex-ide-delete-session-thread "codex-ide-delete-session-thread"
                  (thread-id &optional skip-confirmation))

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

(defun codex-ide--environment-variable-value (name environment)
  "Return NAME's value in ENVIRONMENT, or nil when unset."
  (let ((prefix (concat name "=")))
    (when-let ((entry (cl-find-if
                       (lambda (value)
                         (string-prefix-p prefix value))
                       environment)))
      (substring entry (length prefix)))))

(defun codex-ide--set-environment-variable (environment name value)
  "Return ENVIRONMENT with NAME set to VALUE."
  (let ((prefix (concat name "=")))
    (cons (concat prefix value)
          (cl-remove-if
           (lambda (entry)
             (string-prefix-p prefix entry))
           environment))))

(defun codex-ide--app-server-process-environment (&optional environment)
  "Return ENVIRONMENT adjusted for color-capable app-server tools."
  (let ((env (copy-sequence (or environment process-environment))))
    (unless (codex-ide--environment-variable-value "NO_COLOR" env)
      (let ((term (codex-ide--environment-variable-value "TERM" env)))
        (when (or (null term)
                  (string-empty-p term)
                  (string= term "dumb"))
          (setq env (codex-ide--set-environment-variable
                     env
                     "TERM"
                     "xterm-256color"))))
      (unless (codex-ide--environment-variable-value "COLORTERM" env)
        (setq env (codex-ide--set-environment-variable
                   env
                   "COLORTERM"
                   "truecolor")))
      (unless (codex-ide--environment-variable-value "CLICOLOR" env)
        (setq env (codex-ide--set-environment-variable
                   env
                   "CLICOLOR"
                   "1"))))
    env))

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
          (log-buffer (codex-ide-session-log-buffer session))
          (directory (codex-ide-session-directory session))
          (buffer (codex-ide-session-buffer session))
          (thread-id (codex-ide-session-thread-id session))
          (status (codex-ide-session-status session)))
      (when (process-live-p process)
        (codex-ide-log-message session "Stopping process during session teardown")
        (delete-process process))
      (when (process-live-p stderr-process)
        (delete-process stderr-process))
      (codex-ide--run-session-event
       'destroyed
       session
       :directory directory
       :buffer buffer
       :thread-id thread-id
       :status status)
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
  (codex-ide--set-session-status session "idle" 'initialized)
  (codex-ide-log-message session "Initialization complete")
  (codex-ide--update-header-line session))

(defun codex-ide--create-process-session (&optional reuse-buffer reuse-name-suffix)
  "Create a new app-server-backed session for the current working directory."
  (let ((working-dir (codex-ide--get-working-directory)))
    (let* ((process-environment
            (codex-ide--app-server-process-environment process-environment))
           (name-suffix (if reuse-buffer
                            reuse-name-suffix
                          (codex-ide--next-session-name-suffix working-dir)))
           (buffer (or reuse-buffer
                       (get-buffer-create
                        (codex-ide--session-buffer-name working-dir name-suffix))))
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
                     :created-at (codex-ide--timestamp-now)
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
                   :name (format "codex-ide-stderr[%s]" process-label)
                   :buffer nil
                   :coding 'utf-8-unix
                   :noquery t
                   :filter #'codex-ide--stderr-filter))
            (codex-ide--discard-process-buffer stderr-process)
            (setq process
                  (make-process
                   :name (format "codex-ide[%s]" process-label)
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
                (mapc #'delete-overlay
                      (append (car (overlay-lists))
                              (cdr (overlay-lists))))
                (erase-buffer)
                (insert (format "Codex session for %s\n\n"
                                (abbreviate-file-name working-dir)))
                (codex-ide-renderer-freeze-region (point-min) (point-max))))
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
            (codex-ide--run-session-event
             'created
             session
             :directory working-dir
             :buffer buffer
             :status (codex-ide-session-status session))
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

(cl-defun codex-ide--show-session-buffer (session &key newly-created)
  "Display SESSION's buffer and return SESSION."
  (if newly-created
      (codex-ide--display-new-session-buffer (codex-ide-session-buffer session))
    (codex-ide-display-buffer (codex-ide-session-buffer session)))
  (codex-ide--ensure-input-prompt session)
  session)

(defun codex-ide--query-session-for-thread-selection (&optional directory)
  "Return a live session suitable for thread selection in DIRECTORY."
  (or (let ((session (codex-ide--session-for-current-buffer)))
        (when (and session
                   (equal (codex-ide-session-directory session)
                          (codex-ide--normalize-directory
                           (or directory (codex-ide--get-working-directory))))
                   (codex-ide--live-session-p session))
          session))
      (codex-ide--last-active-session-for-directory directory)))

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
    (codex-ide--ensure-input-prompt session)
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
          (unless (codex-ide-session-thread-id session)
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
      (codex-ide--show-session-buffer session :newly-created t)
      session)))

(defun codex-ide--resume-thread-into-session (session thread-id action)
  "Attach SESSION to THREAD-ID and optionally replay prior transcript."
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
    (codex-ide--clear-session-model-name session)
    (codex-ide--remember-model-name session thread-read)
    (let ((result
           (codex-ide--request-sync
            session
            "thread/resume"
            (with-current-buffer (codex-ide-session-buffer session)
              (codex-ide--thread-resume-params thread-id)))))
      (codex-ide--remember-reasoning-effort session result)
      (codex-ide--remember-model-name session result))
    (setf (codex-ide-session-thread-id session) thread-id)
    (codex-ide--mark-session-thread-attached session)
    (codex-ide--run-session-event
     'thread-attached
     session
     :thread-id thread-id
     :action action)
    (codex-ide--session-metadata-put session :session-context-sent t)
    (codex-ide-log-message session "%s thread %s" action thread-id)
    (when thread-read
      (codex-ide--restore-thread-read-transcript session thread-read)))
  session)

(defun codex-ide--session-for-current-project ()
  "Return the active session for the current buffer or project."
  (let ((session (codex-ide--get-default-session-for-current-buffer)))
    (unless (and session (process-live-p (codex-ide-session-process session)))
      (user-error "No Codex session for this buffer or project"))
    session))

(defun codex-ide--ensure-session-for-current-project ()
  "Return the active session for the current buffer or project."
  (or (let ((session (codex-ide--get-default-session-for-current-buffer)))
        (when (and session (process-live-p (codex-ide-session-process session)))
          session))
      (when (y-or-n-p "No Codex session for this workspace. Start one? ")
        (codex-ide--start-session 'new))
      (codex-ide--session-for-current-project)))

(defun codex-ide--start-session (&optional mode)
  "Start a Codex session for the current project."
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
               (codex-ide--clear-session-model-name session)
               (let ((result (codex-ide--request-sync
                              session
                              "thread/start"
                              (with-current-buffer (codex-ide-session-buffer session)
                                (codex-ide--thread-start-params)))))
                 (codex-ide--remember-reasoning-effort session result)
                 (codex-ide--remember-model-name session result)
                 (setf (codex-ide-session-thread-id session)
                       (codex-ide--extract-thread-id result))
                 (codex-ide--mark-session-thread-attached session)
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
            (codex-ide--set-session-status session "idle" 'started)
            (codex-ide--update-header-line session)
            (codex-ide--show-session-buffer session :newly-created created-session)
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
      (codex-ide-transcript-append-to-buffer
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
            (codex-ide--set-session-status
             session
             (string-trim event)
             'process-event)
            (codex-ide--update-header-line session)
            (codex-ide-transcript-append-to-buffer
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
(defun codex-ide-show-cli-info ()
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
(defun codex-ide-reset-current-session ()
  "Stop the current Codex session and start a new one in the same buffer."
  (interactive)
  (let* ((session (and (derived-mode-p 'codex-ide-session-mode)
                       (codex-ide--session-for-current-buffer)))
         (working-dir (and session (codex-ide-session-directory session)))
         (buffer (and session (codex-ide-session-buffer session)))
         (name-suffix (and session (codex-ide-session-name-suffix session)))
         (new-session nil))
    (unless session
      (user-error "Codex reset is only available in a Codex session buffer"))
    (unless (buffer-live-p buffer)
      (user-error "Current Codex session buffer is no longer live"))
    (codex-ide--prepare-session-operations)
    (when (and (process-live-p (codex-ide-session-process session))
               (codex-ide-session-thread-id session))
      (codex-ide-log-message
       session
       "Unsubscribing thread %s before reset"
       (codex-ide-session-thread-id session))
      (ignore-errors
        (codex-ide--request-sync
         session
         "thread/unsubscribe"
         `((threadId . ,(codex-ide-session-thread-id session))))))
    (codex-ide-log-message session "Resetting current session")
    (codex-ide--teardown-session session)
    (let ((default-directory (file-name-as-directory working-dir)))
      (setq new-session
            (codex-ide--create-process-session buffer name-suffix)))
    (condition-case err
        (progn
          (codex-ide--initialize-session new-session)
          (let ((result (codex-ide--request-sync
                         new-session
                         "thread/start"
                         (with-current-buffer buffer
                           (codex-ide--thread-start-params)))))
            (codex-ide--remember-reasoning-effort new-session result)
            (setf (codex-ide-session-thread-id new-session)
                  (codex-ide--extract-thread-id result))
            (codex-ide--mark-session-thread-attached new-session)
            (codex-ide--session-metadata-put new-session :session-context-sent nil)
            (codex-ide-log-message
             new-session
             "Started new thread %s after reset"
             (codex-ide-session-thread-id new-session)))
          (codex-ide--set-session-status new-session "idle" 'reset)
          (codex-ide--update-header-line new-session)
          (codex-ide--run-session-event 'reset new-session)
          (codex-ide--show-session-buffer new-session)
          (codex-ide--track-active-buffer)
          (message "Reset Codex in %s"
                   (file-name-nondirectory (directory-file-name working-dir)))
          new-session)
      (error
       (when new-session
         (let* ((stderr-tail (codex-ide--session-metadata-get new-session :stderr-tail))
                (classification
                 (codex-ide--render-session-error
                  new-session
                  (list (error-message-string err) stderr-tail)
                  "Codex reset failed")))
           (when (process-live-p (codex-ide-session-process new-session))
             (delete-process (codex-ide-session-process new-session)))
           (codex-ide--show-session-buffer new-session)
           (codex-ide--cleanup-session new-session)
           (signal 'user-error
                   (list (codex-ide--format-session-error-message
                          classification
                          (codex-ide--extract-error-text
                           (error-message-string err)
                           stderr-tail)
                          "Codex reset failed")))))
       (signal (car err) (cdr err))))))

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
          (setf (codex-ide-session-interrupt-requested session) t)
          (codex-ide--set-session-status session "interrupting" 'interrupt-requested)
          (codex-ide--update-header-line session)
          (condition-case err
              (codex-ide--request-sync
               session
               "turn/interrupt"
               `((threadId . ,(codex-ide-session-thread-id session))
                 (turnId . ,turn-id)))
            (error
             (setf (codex-ide-session-interrupt-requested session) nil)
             (codex-ide--set-session-status session "running" 'interrupt-failed)
             (codex-ide--update-header-line session)
             (signal (car err) (cdr err))))
          (message "Sent interrupt to Codex"))
      (user-error "No active Codex turn to interrupt"))))

(provide 'codex-ide-session)

;;; codex-ide-session.el ends here
