;;; codex-ide-status-api.el --- Public status extension API for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Gunnar Wrobel

;;; Commentary:

;; Public, package-neutral extension points for status rows, annotations, and
;; actions.  Optional integrations can use this API without depending on the
;; internal section or app-server response representation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-core)
(require 'codex-ide-threads)
(require 'codex-ide-session)

(declare-function codex-ide-status-mode-refresh "codex-ide-status-mode"
                  (&optional ignore-auto noconfirm))

(defcustom codex-ide-status-before-title-functions nil
  "Functions that return content rendered before a Codex status row title.

Each function receives one row plist as documented by
`codex-ide-thread-row' and returns nil, a string, or a list of strings."
  :type 'hook
  :group 'codex-ide)

(defcustom codex-ide-status-after-title-functions nil
  "Functions that return content rendered after a Codex status row title.

Each function receives one row plist as documented by
`codex-ide-thread-row' and returns nil, a string, or a list of strings."
  :type 'hook
  :group 'codex-ide)

(defcustom codex-ide-status-actions nil
  "Registered actions for normalized Codex status rows.

Each entry is a plist with `:name' and `:function'.  `:name' is the completion
label.  `:function' receives one normalized row plist.  An optional
`:predicate' receives the same row and controls whether the action is offered.
Use `codex-ide-register-status-action' to add entries."
  :type '(repeat sexp)
  :group 'codex-ide)

(defvar codex-ide-status-annotations-changed-hook nil
  "Hook run before status buffers refresh after external annotation changes.")

(defun codex-ide-status--thread-directory (thread fallback-directory)
  "Return THREAD's normalized directory, or FALLBACK-DIRECTORY."
  (let ((directory (alist-get 'cwd thread)))
    (when (or (and (stringp directory) (not (string-empty-p directory)))
              fallback-directory)
      (codex-ide--normalize-directory
       (if (and (stringp directory) (not (string-empty-p directory)))
           directory
         fallback-directory)))))

(defun codex-ide-thread-row (thread &optional fallback-directory)
  "Return a normalized public status row for raw THREAD metadata.

FALLBACK-DIRECTORY is used only when THREAD has no `cwd'.  The returned plist
contains `:kind', `:thread-id', `:title', `:directory', `:created-at',
`:updated-at', `:technical-status', `:thread', and `:session'."
  (let* ((directory (codex-ide-status--thread-directory
                     thread fallback-directory))
         (thread-id (alist-get 'id thread))
         (raw-title (or (alist-get 'name thread)
                        (alist-get 'preview thread)
                        "Untitled"))
         (title (codex-ide--thread-choice-preview raw-title))
         (session (and directory
                       thread-id
                       (codex-ide--session-for-thread-id thread-id directory))))
    (list :kind 'thread
          :thread-id thread-id
          :title (if (string-empty-p title) "Untitled" title)
          :directory directory
          :created-at (alist-get 'createdAt thread)
          :updated-at (alist-get 'updatedAt thread)
          :technical-status (if session
                                (codex-ide-session-status session)
                              "stored")
          :thread thread
          :session session)))

(defun codex-ide-session-row (session)
  "Return a normalized public status row for live SESSION."
  (unless (codex-ide-session-p session)
    (signal 'wrong-type-argument (list 'codex-ide-session-p session)))
  (let ((buffer (codex-ide-session-buffer session)))
    (list :kind 'session
          :thread-id (codex-ide-session-thread-id session)
          :title (if (buffer-live-p buffer)
                     (buffer-name buffer)
                   "Codex session")
          :directory (codex-ide-session-directory session)
          :created-at (codex-ide-session-created-at session)
          :updated-at nil
          :technical-status (codex-ide-session-status session)
          :thread nil
          :session session)))

(cl-defun codex-ide-list-thread-rows (&key directory session)
  "Return normalized thread rows for DIRECTORY using SESSION.

When SESSION is nil, create or reuse a query session for DIRECTORY."
  (codex-ide--prepare-session-operations)
  (let* ((directory (codex-ide--normalize-directory
                     (or directory (codex-ide--get-working-directory))))
         (session (or session
                      (codex-ide--ensure-query-session-for-thread-selection
                       directory)))
         (threads (codex-ide--thread-list-data session)))
    (mapcar (lambda (thread)
              (codex-ide-thread-row thread directory))
            threads)))

(defun codex-ide-status--provider-text (row functions)
  "Return combined text produced for ROW by FUNCTIONS."
  (let (annotations)
    (dolist (function functions)
      (let ((value (funcall function row)))
        (dolist (annotation (if (listp value) value (list value)))
          (when (and (stringp annotation) (not (string-empty-p annotation)))
            (push annotation annotations)))))
    (string-join (nreverse annotations) "  ")))

(defun codex-ide-status-before-title-text (row)
  "Return the combined external before-title text for ROW."
  (codex-ide-status--provider-text row codex-ide-status-before-title-functions))

(defun codex-ide-status-after-title-text (row)
  "Return the combined external after-title text for ROW."
  (codex-ide-status--provider-text row codex-ide-status-after-title-functions))

(defun codex-ide-register-status-action (name function &optional predicate)
  "Register a status action called NAME using FUNCTION.

FUNCTION receives a normalized status row.  Optional PREDICATE receives the
same row and controls whether the action is available.  Replace an existing
action with the same NAME."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "Status action name must be a non-empty string"))
  (unless (functionp function)
    (signal 'wrong-type-argument (list 'functionp function)))
  (when (and predicate (not (functionp predicate)))
    (signal 'wrong-type-argument (list 'functionp predicate)))
  (setq codex-ide-status-actions
        (seq-remove (lambda (action)
                      (equal (plist-get action :name) name))
                    codex-ide-status-actions))
  (push (list :name name :function function :predicate predicate)
        codex-ide-status-actions))

(defun codex-ide-unregister-status-action (name)
  "Unregister the status action called NAME."
  (setq codex-ide-status-actions
        (seq-remove (lambda (action)
                      (equal (plist-get action :name) name))
                    codex-ide-status-actions)))

(defun codex-ide-status-available-actions (row)
  "Return registered status actions available for ROW."
  (seq-filter
   (lambda (action)
     (let ((predicate (plist-get action :predicate)))
       (or (null predicate) (funcall predicate row))))
   codex-ide-status-actions))

;;;###autoload
(defun codex-ide-status-notify-annotations-changed ()
  "Notify listeners and refresh live Codex status buffers.

Optional integrations should call this after their external annotation data
changes."
  (interactive)
  (run-hooks 'codex-ide-status-annotations-changed-hook)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'codex-ide-status-mode)
                 (fboundp 'codex-ide-status-mode-refresh))
        (codex-ide-status-mode-refresh)))))

(provide 'codex-ide-status-api)

;;; codex-ide-status-api.el ends here
