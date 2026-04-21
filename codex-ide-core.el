;;; codex-ide-core.el --- Core session state for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; Session/state primitives shared across codex-ide modules.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function codex-ide--update-header-line "codex-ide-transcript" (&optional session))
(declare-function codex-ide-log-message "codex-ide-log" (session format-string &rest args))

(defvar codex-ide-buffer-name-function)
(defvar codex-ide-buffer-name-prefix)

(defvar codex-ide--sessions nil
  "List of active Codex session objects.")

(defvar codex-ide--active-buffer-contexts (make-hash-table :test 'equal)
  "Hash table mapping working directories to the latest Emacs buffer context.")

(defvar codex-ide--active-buffer-objects (make-hash-table :test 'equal)
  "Hash table mapping working directories to the latest live Emacs file buffer.")

(defvar codex-ide--prompt-origin-buffer nil
  "Buffer to treat as the authoritative prompt context for one submission.")

(defvar codex-ide-session-event-hook nil
  "Hook run when a Codex session changes.
Each hook function receives three arguments: EVENT, SESSION, and PAYLOAD.
EVENT is a symbol naming the lifecycle change, SESSION is the affected
`codex-ide-session' object, and PAYLOAD is a plist with event-specific data.")

(defmacro codex-ide--without-undo-recording (&rest body)
  "Run BODY without recording undo entries in the current buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer-undo-list t))
     ,@body))

(defun codex-ide--discard-buffer-undo-history ()
  "Discard undo history for the current buffer."
  (unless (eq buffer-undo-list t)
    (setq buffer-undo-list nil))
  (when (boundp 'pending-undo-list)
    (setq pending-undo-list nil))
  (when (boundp 'undo-in-progress)
    (setq undo-in-progress nil))
  (when (fboundp 'undo-tree-clear-history)
    (ignore-errors
      (undo-tree-clear-history))))

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
   (name-suffix
    :initarg :name-suffix
    :initform nil
    :accessor codex-ide-session-name-suffix)
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
   (created-at
    :initarg :created-at
    :initform nil
    :accessor codex-ide-session-created-at)
   (last-thread-attached-at
    :initarg :last-thread-attached-at
    :initform nil
    :accessor codex-ide-session-last-thread-attached-at)
   (last-prompt-submitted-at
    :initarg :last-prompt-submitted-at
    :initform nil
    :accessor codex-ide-session-last-prompt-submitted-at)
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
    :accessor codex-ide-session-status)
   (query-only
    :initarg :query-only
    :initform nil
    :accessor codex-ide-session-query-only))
  "State for a Codex app-server session.")

(defun make-codex-ide-session (&rest initargs)
  "Create a `codex-ide-session' object with INITARGS."
  (apply #'make-instance 'codex-ide-session initargs))

(defun codex-ide--run-session-event (event session &rest payload)
  "Notify listeners that SESSION changed with EVENT and PAYLOAD."
  (when (codex-ide-session-p session)
    (run-hook-with-args 'codex-ide-session-event-hook event session payload)))

(defun codex-ide--set-session-status (session status &optional reason)
  "Set SESSION status to STATUS and emit an event when it changes.
REASON is stored in the emitted payload when non-nil."
  (let ((old-status (and session (codex-ide-session-status session))))
    (setf (codex-ide-session-status session) status)
    (unless (equal old-status status)
      (codex-ide--run-session-event
       'status-changed
       session
       :old-status old-status
       :status status
       :reason reason)))
  status)

(defun codex-ide--project-name (directory)
  "Return the display name for DIRECTORY."
  (file-name-nondirectory (directory-file-name directory)))

(defun codex-ide--append-buffer-name-suffix (buffer-name suffix)
  "Return BUFFER-NAME with numeric SUFFIX inserted before the closing `*'.
When SUFFIX is nil, return BUFFER-NAME unchanged."
  (if suffix
      (replace-regexp-in-string
       (rx "*" string-end)
       (format "<%d>*" suffix)
       buffer-name
       t t)
    buffer-name))

(defun codex-ide--default-buffer-name (directory)
  "Generate the base Codex session buffer name for DIRECTORY."
  (format "*%s[%s]*"
          codex-ide-buffer-name-prefix
          (codex-ide--project-name directory)))

(defun codex-ide--session-buffer-name (directory &optional suffix)
  "Generate the Codex session buffer name for DIRECTORY and SUFFIX."
  (codex-ide--append-buffer-name-suffix
   (funcall codex-ide-buffer-name-function directory)
   suffix))

(defun codex-ide--log-buffer-name (directory &optional suffix)
  "Generate the Codex log buffer name for DIRECTORY and SUFFIX."
  (codex-ide--append-buffer-name-suffix
   (format "*%s-log[%s]*"
           codex-ide-buffer-name-prefix
           (codex-ide--project-name directory))
   suffix))

(defun codex-ide--query-log-buffer-name (directory &optional suffix)
  "Generate the query-session log buffer name for DIRECTORY and SUFFIX."
  (codex-ide--append-buffer-name-suffix
   (format "*%s-log[%s]-query*"
           codex-ide-buffer-name-prefix
           (codex-ide--project-name directory))
   suffix))

(defun codex-ide--normalize-session-status (status)
  "Return a normalized session STATUS string, or nil when unknown."
  (let ((raw
         (cond
          ((stringp status) status)
          ((listp status) (alist-get 'type status))
          (t nil))))
    (when (stringp raw)
      (let ((trimmed (string-trim raw)))
        (unless (string-empty-p trimmed)
          (pcase (downcase trimmed)
            ((or "active" "inprogress" "in_progress") "running")
            ((or "completed" "complete" "success") "idle")
            ((or "systemerror" "system_error") "error")
            ((or "failed" "error") "error")
            ((or "idle" "running" "submitted" "starting" "interrupting" "approval"
                 "disconnected" "finished" "killed")
             (downcase trimmed))
            (_ trimmed)))))))

(defun codex-ide--session-buffer-p (buffer)
  "Return non-nil when BUFFER looks like a Codex session buffer."
  (when-let ((name (cond
                    ((stringp buffer) buffer)
                    ((buffer-live-p buffer) (buffer-name buffer)))))
    (string-prefix-p (format "*%s[" codex-ide-buffer-name-prefix) name)))

(defun codex-ide--normalize-directory (directory)
  "Return a canonical directory key for DIRECTORY."
  (when directory
    (directory-file-name
     (file-truename (expand-file-name directory)))))

(defun codex-ide--get-working-directory ()
  "Return the current project root or `default-directory'."
  (codex-ide--normalize-directory
   (if-let ((project (project-current)))
       (project-root project)
     default-directory)))

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

(defun codex-ide--session-for-current-buffer ()
  "Return the Codex session attached to the current buffer, if any."
  (and (boundp 'codex-ide--session)
       (codex-ide-session-p codex-ide--session)
       codex-ide--session))

(defun codex-ide--next-session-name-suffix (&optional directory)
  "Return the next available session name suffix for DIRECTORY.
The first live session in a workspace uses no suffix."
  (let* ((sessions (codex-ide--sessions-for-directory
                    (or directory (codex-ide--get-working-directory))
                    t))
         (used-suffixes
          (mapcar #'codex-ide-session-name-suffix
                  (seq-remove #'codex-ide--query-only-session-p sessions)))
         (suffix nil))
    (while (member suffix used-suffixes)
      (setq suffix (if suffix (1+ suffix) 1)))
    suffix))

(defun codex-ide--live-session-p (session)
  "Return non-nil when SESSION is a live `codex-ide-session' object."
  (and (codex-ide-session-p session)
       (process-live-p (codex-ide-session-process session))))

(defun codex-ide--query-only-session-p (session)
  "Return non-nil when SESSION is query-only."
  (and (codex-ide-session-p session)
       (codex-ide-session-query-only session)))

(defun codex-ide--timestamp-now ()
  "Return the current time as a sortable timestamp."
  (float-time))

(defun codex-ide--mark-session-thread-attached (session)
  "Record that SESSION was attached to a thread now."
  (setf (codex-ide-session-last-thread-attached-at session)
        (codex-ide--timestamp-now))
  session)

(defun codex-ide--mark-session-prompt-submitted (session)
  "Record that SESSION submitted a prompt now."
  (setf (codex-ide-session-last-prompt-submitted-at session)
        (codex-ide--timestamp-now))
  session)

(defun codex-ide--sessions-for-directory (directory &optional live-only)
  "Return tracked sessions for DIRECTORY.
When LIVE-ONLY is non-nil, only include sessions with live processes."
  (let ((directory (codex-ide--normalize-directory directory)))
    (seq-filter
     (lambda (session)
       (and (codex-ide-session-p session)
            (equal (codex-ide-session-directory session) directory)
            (or (not live-only)
                (codex-ide--live-session-p session))))
     codex-ide--sessions)))

(defun codex-ide--session-activity-time (session)
  "Return SESSION's most recent activity timestamp."
  (or (codex-ide-session-last-prompt-submitted-at session)
      (codex-ide-session-last-thread-attached-at session)
      (codex-ide-session-created-at session)
      0))

(defun codex-ide--most-recent-session (sessions)
  "Return the most recently active session from SESSIONS."
  (seq-reduce
   (lambda (best session)
     (if (or (not best)
             (> (codex-ide--session-activity-time session)
                (codex-ide--session-activity-time best)))
         session
       best))
   sessions
   nil))

(defun codex-ide--last-active-session-for-directory (&optional directory)
  "Return the most recently active live Codex session for DIRECTORY."
  (codex-ide--most-recent-session
   (seq-remove
    #'codex-ide--query-only-session-p
    (codex-ide--sessions-for-directory
     (or directory (codex-ide--get-working-directory))
     t))))

(defun codex-ide--last-active-session ()
  "Return the most recently active live Codex session across all projects."
  (codex-ide--most-recent-session
   (seq-remove
    #'codex-ide--query-only-session-p
    (seq-filter #'codex-ide--live-session-p codex-ide--sessions))))

(defun codex-ide--live-session-directories ()
  "Return directories that currently have live Codex sessions."
  (let (directories)
    (dolist (session codex-ide--sessions)
      (when (codex-ide--live-session-p session)
        (cl-pushnew (codex-ide-session-directory session)
                    directories
                    :test #'equal)))
    (sort directories #'string-lessp)))

(defun codex-ide--last-active-sessions-by-directory ()
  "Return cons cells of live directory and most recently active session."
  (mapcar
   (lambda (directory)
     (cons directory (codex-ide--last-active-session-for-directory directory)))
   (codex-ide--live-session-directories)))

(defun codex-ide--refresh-project-header-lines (directory)
  "Refresh header lines for all live sessions in DIRECTORY."
  (dolist (session (codex-ide--sessions-for-directory directory t))
    (when (process-live-p (codex-ide-session-process session))
      (codex-ide--update-header-line session))))

(defun codex-ide--get-last-active-buffer-for-project (&optional directory)
  "Return the most recently active live Codex session buffer for DIRECTORY."
  (when-let ((session (codex-ide--last-active-session-for-directory directory))
             (buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      buffer)))

(defun codex-ide--get-last-active-buffer-all-projects ()
  "Return the most recently active live Codex session buffer across projects."
  (when-let ((session (codex-ide--last-active-session))
             (buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      buffer)))

(defun codex-ide--get-session ()
  "Return the most recently active Codex session for the current working directory."
  (codex-ide--last-active-session-for-directory
   (codex-ide--get-working-directory)))

(defun codex-ide--get-process ()
  "Return the Codex process associated with the current working directory."
  (when-let ((session (codex-ide--get-session)))
    (codex-ide-session-process session)))

(defun codex-ide--get-default-session-for-current-buffer ()
  "Infer the default Codex session for the current buffer.
Prefer a session buffer's local session object. Otherwise fall back to the
current buffer's project directory."
  (or (codex-ide--session-for-current-buffer)
      (codex-ide--get-session)))

(defun codex-ide--session-for-thread-id (thread-id &optional directory)
  "Return the live session for THREAD-ID in DIRECTORY, if any."
  (seq-find
   (lambda (session)
     (equal (codex-ide-session-thread-id session) thread-id))
   (codex-ide--sessions-for-directory
    (or directory (codex-ide--get-working-directory))
    t)))

(defun codex-ide--set-session (&optional session)
  "Register SESSION in the global session list."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (setq codex-ide--sessions (delq session codex-ide--sessions))
  (push session codex-ide--sessions)
  session)

(defun codex-ide--has-live-sessions-p ()
  "Return non-nil when any tracked Codex session still has a live process."
  (seq-some #'codex-ide--live-session-p codex-ide--sessions))

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
  (setq codex-ide--sessions
        (seq-filter
         (lambda (session)
           (if (codex-ide--live-session-p session)
               t
             (codex-ide-log-message
              session
              "Cleaning up dead session entry for %s"
              (codex-ide-session-directory session))
             nil))
         codex-ide--sessions))
  (codex-ide--maybe-disable-active-buffer-tracking))

(defun codex-ide--session-buffer-sessions ()
  "Return tracked sessions with live session buffers."
  (codex-ide--cleanup-dead-sessions)
  (seq-filter
   (lambda (session)
     (buffer-live-p (codex-ide-session-buffer session)))
   codex-ide--sessions))

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

(defun codex-ide--reset-prompt-history-navigation (session)
  "Reset history navigation state for SESSION."
  (setf (codex-ide-session-prompt-history-index session) nil
        (codex-ide-session-prompt-history-draft session) nil))

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
    (codex-ide--refresh-project-header-lines working-dir)))

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
          (codex-ide--refresh-project-header-lines working-dir))))))

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

(provide 'codex-ide-core)

;;; codex-ide-core.el ends here
