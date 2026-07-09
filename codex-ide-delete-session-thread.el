;;; codex-ide-delete-session-thread.el --- Internal Codex thread deletion -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: ai, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Thread deletion support.  Prefer the public app-server `thread/delete` RPC
;; and keep the old storage deletion path only as a compatibility fallback for
;; older app-server builds that do not expose that method.

;;; Code:

(require 'subr-x)

(declare-function codex-ide--ensure-query-session-for-thread-selection
                  "codex-ide-session"
                  (&optional directory))

(defun codex-ide--session-for-thread-id-any (thread-id)
  "Return any live session tracking THREAD-ID."
  (seq-find
   (lambda (session)
     (equal (codex-ide-session-thread-id session) thread-id))
   (codex-ide--session-buffer-sessions)))

(defun codex-ide--codex-home ()
  "Return the active Codex home directory."
  (expand-file-name (or (getenv "CODEX_HOME")
                        "~/.codex")))

(defun codex-ide--codex-sessions-directory ()
  "Return the active Codex sessions directory."
  (expand-file-name "sessions" (codex-ide--codex-home)))

(defun codex-ide--thread-rollout-path (thread-id)
  "Return the rollout file path for THREAD-ID, or nil when not found."
  (let* ((sessions-directory (codex-ide--codex-sessions-directory))
         (pattern (format "rollout-.*-%s\\.jsonl\\'" (regexp-quote thread-id))))
    (when (file-directory-p sessions-directory)
      (car (directory-files-recursively sessions-directory pattern nil t)))))

(defun codex-ide--delete-empty-session-directories (path)
  "Delete empty parent directories for PATH inside the Codex sessions root."
  (let ((sessions-root (file-name-as-directory
                        (expand-file-name (codex-ide--codex-sessions-directory))))
        (directory (file-name-directory (expand-file-name path))))
    (while (and directory
                (file-in-directory-p directory sessions-root)
                (not (equal (file-name-as-directory directory) sessions-root))
                (null (directory-files directory nil directory-files-no-dot-files-regexp t)))
      (delete-directory directory)
      (setq directory (file-name-directory (directory-file-name directory))))))

(defun codex-ide--delete-thread-storage (rollout-path)
  "Delete stored Codex rollout file at ROLLOUT-PATH."
  (let* ((sessions-root (file-name-as-directory
                         (expand-file-name (codex-ide--codex-sessions-directory))))
         (rollout-file (expand-file-name rollout-path)))
    (unless (file-in-directory-p rollout-file sessions-root)
      (error "Refusing to delete rollout outside %s: %s"
             (abbreviate-file-name sessions-root)
             (abbreviate-file-name rollout-file)))
    (unless (file-exists-p rollout-file)
      (user-error "Stored Codex rollout file no longer exists: %s"
                  (abbreviate-file-name rollout-file)))
    (delete-file rollout-file)
    (codex-ide--delete-empty-session-directories rollout-file)))

(defun codex-ide--delete-live-thread-session (session &optional skip-unsubscribe)
  "Tear down SESSION after its thread has been deleted.

When SKIP-UNSUBSCRIBE is nil, first ask app-server to unsubscribe the thread.
This is kept for storage-fallback deletion where app-server did not already
delete the thread."
  (when (and (not skip-unsubscribe)
             session
             (codex-ide-session-thread-id session))
    (codex-ide-log-message
     session
     "Unsubscribing thread %s before deletion"
     (codex-ide-session-thread-id session))
    (ignore-errors
      (codex-ide--request-sync
       session
       "thread/unsubscribe"
       `((threadId . ,(codex-ide-session-thread-id session))))))
  (when session
    (let ((buffer (codex-ide-session-buffer session)))
      (codex-ide--teardown-session session t)
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defun codex-ide--thread-delete-request-session (thread-id)
  "Return an app-server session suitable for deleting THREAD-ID."
  (or (codex-ide--session-for-thread-id-any thread-id)
      (codex-ide--ensure-query-session-for-thread-selection default-directory)))

(defun codex-ide--thread-delete-unsupported-error-p (error)
  "Return non-nil when ERROR means app-server lacks `thread/delete'."
  (let ((message (downcase (error-message-string error))))
    (or (string-match-p "method not found" message)
        (string-match-p "unknown method" message)
        (and (string-match-p "unknown variant" message)
             (string-match-p "thread/delete" message))
        (string-match-p "method .*thread/delete.*not.*found" message)
        (string-match-p "-32601" message))))

(defun codex-ide--delete-thread-via-app-server (thread-id)
  "Delete THREAD-ID through app-server.

Return non-nil when the public API handled deletion.  Return nil only when the
connected app-server reports that `thread/delete' is unsupported."
  (let ((session (codex-ide--thread-delete-request-session thread-id)))
    (condition-case err
        (progn
          (codex-ide--request-sync
           session
           "thread/delete"
           `((threadId . ,thread-id)))
          t)
      (error
       (unless (codex-ide--thread-delete-unsupported-error-p err)
         (signal (car err) (cdr err)))
       (codex-ide-log-message
        session
        "Falling back to storage deletion for %s because thread/delete is unsupported: %s"
        thread-id
        (error-message-string err))
       nil))))

(defun codex-ide--delete-thread-via-storage-fallback (thread-id session)
  "Delete THREAD-ID through the legacy storage fallback."
  (let ((rollout-path (codex-ide--thread-rollout-path thread-id)))
    (unless rollout-path
      (user-error "No stored Codex thread found for %s" thread-id))
    (when session
      (codex-ide--delete-live-thread-session session))
    (codex-ide--delete-thread-storage rollout-path)))

;;;###autoload
(defun codex-ide-delete-session-thread (thread-id &optional skip-confirmation)
  "Delete Codex THREAD-ID from the active `CODEX_HOME`.

Prefer the public app-server `thread/delete' API.  For compatibility with older
Codex app-server builds, fall back to current Codex internal storage details
under `CODEX_HOME`, specifically persisted rollout files under the sessions
directory, only when app-server reports that `thread/delete' is unsupported.

If a live session buffer is attached to THREAD-ID, prompt before deleting the
thread and then tear down the local session buffer.

When SKIP-CONFIRMATION is non-nil, delete without prompting.  This is intended
for batch callers that already presented a single confirmation."
  (interactive
   (list
    (read-string "Delete Codex thread ID: "
                 (when-let* ((session (codex-ide--get-default-session-for-current-buffer)))
                   (codex-ide-session-thread-id session)))))
  (unless (and (stringp thread-id)
               (not (string-empty-p (string-trim thread-id))))
    (user-error "Invalid thread id: %S" thread-id))
  (setq thread-id (string-trim thread-id))
  (let* ((session (codex-ide--session-for-thread-id-any thread-id))
         (buffer (and session (codex-ide-session-buffer session)))
         (buffer-name (and (buffer-live-p buffer) (buffer-name buffer))))
    (unless (or skip-confirmation
                (yes-or-no-p
                 (if buffer-name
                     (format "Delete Codex buffer %s and permanently remove thread %s from %s? "
                             buffer-name
                             thread-id
                             (abbreviate-file-name (codex-ide--codex-home)))
                   (format "Permanently remove Codex thread %s from %s? "
                           thread-id
                           (abbreviate-file-name (codex-ide--codex-home))))))
      (user-error "Canceled deletion of Codex thread %s" thread-id))
    (if (codex-ide--delete-thread-via-app-server thread-id)
        (when session
          (codex-ide--delete-live-thread-session session t))
      (codex-ide--delete-thread-via-storage-fallback thread-id session))
    (message "Deleted Codex thread %s" thread-id)))

(provide 'codex-ide-delete-session-thread)

;;; codex-ide-delete-session-thread.el ends here
