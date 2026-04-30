;;; codex-ide-protocol.el --- JSON-RPC and app-server helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns codex-ide's protocol-facing interaction with `codex
;; app-server`.
;;
;; It is responsible for:
;;
;; - Sending and awaiting JSON-RPC requests.
;; - Tracking pending request callbacks.
;; - Building request parameter payloads for thread/config/model operations.
;; - Extracting normalized thread/model metadata from app-server payloads.
;; - Handling raw JSON-RPC response messages.
;;
;; It is not responsible for process lifecycle, transcript rendering, or UI
;; decisions about when to start or resume sessions.  Those concerns live in the
;; session and transcript controller modules.  This file stays focused on the
;; request/response protocol contract and helper transformations around it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-core)
(require 'codex-ide-config)
(require 'codex-ide-errors)

(declare-function codex-ide-log-message "codex-ide-log" (session format-string &rest args))
(declare-function codex-ide--ensure-cli "codex-ide-session" ())
(declare-function codex-ide--cleanup-dead-sessions "codex-ide-core" ())
(declare-function codex-ide--ensure-active-buffer-tracking "codex-ide-core" ())
(declare-function codex-ide--query-session-for-thread-selection "codex-ide-session" (&optional directory))
(declare-function codex-ide--ensure-query-session-for-thread-selection "codex-ide-session" (&optional directory))
(declare-function codex-ide--update-header-line "codex-ide-transcript" (&optional session))

(defvar codex-ide-request-timeout)
(defvar codex-ide-model)
(defvar codex-ide-approval-policy)
(defvar codex-ide-sandbox-mode)
(defvar codex-ide-personality)
(defvar codex-ide-thread-list-default-limit)

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

(defun codex-ide--request-async (session method params callback)
  "Send METHOD with PARAMS to SESSION and invoke CALLBACK on response."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((id (codex-ide--next-request-id session))
         (pending (codex-ide-session-pending-requests session)))
    (codex-ide-log-message session "Starting asynchronous request %s (id=%s)" method id)
    (puthash id
             (lambda (response-result response-error)
               (remhash id pending)
               (funcall callback response-result response-error))
             pending)
    (codex-ide--jsonrpc-send session `((jsonrpc . "2.0")
                                       (id . ,id)
                                       (method . ,method)
                                       (params . ,params)))
    id))

(defun codex-ide--thread-start-params (&optional session)
  "Build `thread/start` params for SESSION's current working directory."
  (let ((working-dir (codex-ide--get-working-directory)))
    (delq nil
          `((cwd . ,working-dir)
            (approvalPolicy . ,(codex-ide-config-effective-value
                                'approval-policy
                                session))
            (sandbox . ,(codex-ide-config-effective-value 'sandbox-mode session))
            (personality . ,(codex-ide-config-effective-value 'personality session))
            ,@(when-let ((model (codex-ide-config-effective-value 'model session)))
                `((model . ,model)))))))

(defun codex-ide--turn-start-sandbox-policy (&optional session)
  "Build a `sandboxPolicy` object for `turn/start` from SESSION settings."
  (when-let ((mode (codex-ide-config-effective-value 'sandbox-mode session)))
    (pcase mode
      ("read-only"
       '((type . "readOnly")))
      ("workspace-write"
       `((type . "workspaceWrite")
         (writableRoots . [,(codex-ide--get-working-directory)])))
      ;; `danger-full-access` remains the local UI spelling while app-server
      ;; expects the camelCase sandbox policy type on turn-scoped overrides.
      ("danger-full-access"
       '((type . "dangerFullAccess")))
      (_
       (error "Unsupported Codex sandbox mode: %S" mode)))))

(defun codex-ide--thread-resume-params (thread-id &optional session)
  "Build `thread/resume` params for THREAD-ID in SESSION's current working directory."
  (let ((working-dir (codex-ide--get-working-directory)))
    (delq nil
          `((threadId . ,thread-id)
            (cwd . ,working-dir)
            (approvalPolicy . ,(codex-ide-config-effective-value
                                'approval-policy
                                session))
            (sandbox . ,(codex-ide-config-effective-value 'sandbox-mode session))
            (personality . ,(codex-ide-config-effective-value 'personality session))
            ,@(when-let ((model (codex-ide-config-effective-value 'model session)))
                `((model . ,model)))))))

(defun codex-ide--thread-read-params (thread-id &optional include-turns)
  "Build `thread/read` params for THREAD-ID."
  (delq nil
        `((threadId . ,thread-id)
          ,@(when include-turns
              '((includeTurns . t))))))

(defun codex-ide--read-thread (&optional session thread-id include-turns)
  "Read stored metadata for THREAD-ID using SESSION."
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

(defun codex-ide--extract-reasoning-effort (payload)
  "Extract a reasoning effort string from PAYLOAD, if present."
  (or (alist-get 'reasoningEffort payload)
      (alist-get 'reasoningEffort (alist-get 'thread payload))
      (alist-get 'reasoningEffort (alist-get 'turn payload))))

(defun codex-ide--remember-reasoning-effort (session payload)
  "Persist reasoning effort from PAYLOAD into SESSION metadata."
  (when-let ((effort (codex-ide--extract-reasoning-effort payload)))
    (codex-ide--session-metadata-put session :reasoning-effort effort)))

(defun codex-ide--extract-model-name (payload)
  "Extract a model string from PAYLOAD, if present."
  (let* ((thread (and (listp payload) (alist-get 'thread payload)))
         (turn (and (listp payload) (alist-get 'turn payload)))
         (item (and (listp payload) (alist-get 'item payload)))
         (result (and (listp payload) (alist-get 'result payload)))
         (root (or (and (listp payload)
                        (or (alist-get 'config payload)
                            (alist-get 'effectiveConfig payload)))
                   payload))
         (settings (and (listp root)
                        (or (codex-ide--alist-get-safe 'settings root)
                            (codex-ide--alist-get-safe 'config root)))))
    (seq-find
     (lambda (value)
       (and (stringp value)
            (not (string-empty-p value))))
     (list (and (listp payload) (alist-get 'model payload))
           (and (listp payload) (alist-get 'modelName payload))
           (and (listp thread) (alist-get 'model thread))
           (and (listp thread) (alist-get 'modelName thread))
           (and (listp turn) (alist-get 'model turn))
           (and (listp turn) (alist-get 'modelName turn))
           (and (listp item) (alist-get 'model item))
           (and (listp item) (alist-get 'modelName item))
           (and (listp result) (alist-get 'model result))
           (and (listp result) (alist-get 'modelName result))
           (and (listp root) (codex-ide--alist-get-safe 'model root))
           (and (listp settings) (codex-ide--alist-get-safe 'model settings))))))

(defun codex-ide--set-session-model-name (session model)
  "Store MODEL as SESSION's effective model."
  (codex-ide--session-metadata-put
   session
   :model-name
   (and (stringp model)
        (not (string-empty-p model))
        model)))

(defun codex-ide--clear-session-model-name (session)
  "Clear SESSION's remembered model state."
  (codex-ide--session-metadata-put session :model-name nil)
  (codex-ide--session-metadata-put session :model-name-requested nil))

(defun codex-ide--remember-model-name (session payload)
  "Persist model information from PAYLOAD into SESSION metadata."
  (when-let ((model (codex-ide--extract-model-name payload)))
    (codex-ide--set-session-model-name session model)
    t))

(defun codex-ide--remember-or-request-model-name (session payload)
  "Persist model from PAYLOAD, or request it if SESSION does not know it."
  (or (codex-ide--remember-model-name session payload)
      (progn
        (codex-ide--request-server-model-name session)
        nil)))

(defun codex-ide--thread-read-turns (thread-read)
  "Return turn history from THREAD-READ."
  (or (alist-get 'turns thread-read)
      (alist-get 'turns (alist-get 'thread thread-read))
      []))

(defun codex-ide--thread-read-items (turn)
  "Return ordered transcript items for TURN."
  (or (alist-get 'items turn)
      (alist-get 'messages turn)
      []))

(defun codex-ide--thread-read--message-text (message)
  "Extract readable text from a MESSAGE-like alist."
  (let ((text (or (alist-get 'text message)
                  (alist-get 'message message)
                  (alist-get 'prompt message)
                  (alist-get 'summary message)
                  (alist-get 'content message))))
    (cond
     ((stringp text) text)
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
     ((member type '("userMessage" userMessage "user" user)) 'user)
     ((member type '("agentMessage" agentMessage
                     "assistantMessage" assistantMessage
                     "assistant" assistant)) 'assistant)
     ((member (alist-get 'role item) '("user" user)) 'user)
     ((member (alist-get 'role item) '("assistant" assistant)) 'assistant)
     ((member (alist-get 'source item) '("user" user)) 'user)
     ((member (alist-get 'source item) '("assistant" assistant)) 'assistant)
     ((member (alist-get 'author item) '("user" user)) 'user)
     ((member (alist-get 'author item) '("assistant" assistant)) 'assistant)
     (t nil))))

(cl-defun codex-ide--list-threads (&optional session &key limit sort-key)
  "List threads for the current working directory using SESSION.

When LIMIT is nil, use `codex-ide-thread-list-default-limit'.  When
SORT-KEY is nil, sort by `updated_at'."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((working-dir (codex-ide-session-directory session))
         (limit (or limit codex-ide-thread-list-default-limit))
         (sort-key (or sort-key "updated_at"))
         (result (codex-ide--request-sync
                  session
                  "thread/list"
                  `((cwd . ,working-dir)
                    (limit . ,limit)
                    (sortKey . ,sort-key))))
         (data (alist-get 'data result)))
    (append data nil)))

(defun codex-ide--list-models (&optional session)
  "List available models using SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((cursor nil)
        (models nil)
        (page nil))
    (while
        (progn
          (setq page
                (codex-ide--request-sync
                 session
                 "model/list"
                 (delq nil
                       `((limit . 100)
                         ,@(when cursor
                             `((cursor . ,cursor)))))))
          (setq models (nconc models (append (alist-get 'data page) nil))
                cursor (alist-get 'nextCursor page))
          cursor))
    models))

(defun codex-ide--config-read-params (&optional session)
  "Build `config/read` params for SESSION."
  (let ((directory (and session (codex-ide-session-directory session))))
    `((includeLayers . :json-false)
      ,@(when directory
          `((cwd . ,directory))))))

(defun codex-ide--config-read (&optional session)
  "Read the effective app-server configuration using SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (codex-ide--request-sync
   session
   "config/read"
   (codex-ide--config-read-params session)))

(defun codex-ide--default-model-name (&optional session)
  "Return the server-recommended default model name using SESSION."
  (when-let* ((models (codex-ide--list-models session))
              (default-model (seq-find
                              (lambda (model)
                                (not (memq (alist-get 'isDefault model)
                                           '(nil :json-false))))
                              models))
              (name (or (alist-get 'model default-model)
                        (alist-get 'id default-model))))
    (and (stringp name)
         (not (string-empty-p name))
         name)))

(defun codex-ide--server-model-name (&optional session)
  "Return the cached session model name for SESSION, if known."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((cached (codex-ide--session-metadata-get session :model-name)))
    (cond
     ((eq cached :unknown) "unknown")
     ((stringp cached) cached)
     (t nil))))

(defun codex-ide--session-model-needs-refresh-p (session)
  "Return non-nil when SESSION should retry fetching its server model."
  (not (stringp (codex-ide--session-metadata-get session :model-name))))

(defun codex-ide--handle-server-model-name-resolved (session model)
  "Store MODEL for SESSION and refresh the header line."
  (codex-ide--session-metadata-put session :model-name (or model :unknown))
  (codex-ide-log-message
   session
   "Server model resolved as %s"
   (or model "unknown"))
  (when (buffer-live-p (codex-ide-session-buffer session))
    (codex-ide--update-header-line session)))

(defun codex-ide--request-server-model-name (&optional session)
  "Request SESSION's server-derived model name without blocking."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((token (list 'model-name-request)))
    (when (and session
               (codex-ide--session-model-needs-refresh-p session)
               (not (consp (codex-ide--session-metadata-get
                            session
                            :model-name-requested)))
               (process-live-p (codex-ide-session-process session)))
      (codex-ide--session-metadata-put session :model-name-requested token)
      (codex-ide--request-async
       session
       "config/read"
       (codex-ide--config-read-params session)
       (lambda (result error)
         (when (eq (codex-ide--session-metadata-get
                    session
                    :model-name-requested)
                   token)
           (codex-ide--session-metadata-put session :model-name-requested nil)
           (when (codex-ide--session-model-needs-refresh-p session)
             (let ((model (and (not error)
                               (codex-ide--extract-model-name result))))
               (codex-ide--handle-server-model-name-resolved session model)))))))))

(defun codex-ide--ensure-server-model-name (&optional session)
  "Request SESSION's server-derived model name once, without blocking."
  (codex-ide--request-server-model-name session))

(defun codex-ide--available-model-names ()
  "Return visible model names for the current workspace, or nil on failure."
  (condition-case nil
      (progn
        (unless (codex-ide--ensure-cli)
          (error "Codex CLI not available"))
        (codex-ide--cleanup-dead-sessions)
        (codex-ide--ensure-active-buffer-tracking)
        (let* ((working-dir (codex-ide--get-working-directory))
               (session (or (codex-ide--query-session-for-thread-selection working-dir)
                            (codex-ide--ensure-query-session-for-thread-selection
                             working-dir)))
               (models (codex-ide--list-models session)))
          (delete-dups
           (delq nil
                 (mapcar (lambda (model)
                           (or (alist-get 'model model)
                               (alist-get 'id model)))
                         models)))))
    (error nil)))

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

(provide 'codex-ide-protocol)

;;; codex-ide-protocol.el ends here
