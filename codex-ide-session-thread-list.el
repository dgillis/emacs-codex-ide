;;; codex-ide-session-thread-list.el --- List workspace Codex threads -*- lexical-binding: t; -*-

;;; Commentary:

;; Tabulated listing of stored Codex threads for the current workspace.

;;; Code:

(require 'codex-ide)
(require 'codex-ide-session-list)

(defvar-local codex-ide-session-thread-list--directory nil
  "Workspace directory displayed by the current thread list buffer.")

(defvar codex-ide-session-thread-list-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-thread-list-mode'.")

(set-keymap-parent codex-ide-session-thread-list-mode-map
                   codex-ide-session-list-mode-map)
(define-key codex-ide-session-thread-list-mode-map
            (kbd "D")
            #'codex-ide-session-thread-list-delete-thread)
(define-key codex-ide-session-thread-list-mode-map
            (kbd "l")
            #'codex-ide-session-thread-list-redisplay)

(define-derived-mode codex-ide-session-thread-list-mode codex-ide-session-list-mode
  "Codex-Threads"
  "Mode for listing stored Codex threads in one workspace.")

(defun codex-ide-session-thread-list--preview-text (thread)
  "Return a single-line preview string for THREAD."
  (replace-regexp-in-string
   "[\n\r]+"
   "↵"
   (let ((preview (codex-ide--thread-choice-preview
                   (or (alist-get 'name thread)
                       (alist-get 'preview thread)
                       "Untitled"))))
     (if (string-empty-p preview) "Untitled" preview))))

(defun codex-ide-session-thread-list--thread-buffer-name (thread directory)
  "Return the live session buffer name for THREAD in DIRECTORY, if any."
  (when-let ((session (codex-ide--session-for-thread-id
                       (alist-get 'id thread)
                       directory)))
    (buffer-name (codex-ide-session-buffer session))))

(defun codex-ide-session-thread-list--thread-status (thread directory)
  "Return the session status label for THREAD in DIRECTORY."
  (if-let ((session (codex-ide--session-for-thread-id
                     (alist-get 'id thread)
                     directory)))
      (codex-ide--status-label (codex-ide-session-status session))
    "Stored"))

(defun codex-ide-session-thread-list--entries ()
  "Return tabulated entries for workspace threads."
  (let* ((directory codex-ide-session-thread-list--directory)
         (default-directory directory)
         (session (codex-ide--ensure-query-session-for-thread-selection directory))
         (threads (codex-ide--thread-list-data session)))
    (mapcar
     (lambda (thread)
       (let ((thread-id (alist-get 'id thread)))
         (list thread-id
               (vector
                (codex-ide-session-list-cell
                 (codex-ide--thread-choice-short-id thread)
                 'codex-ide-session-list-id-face)
                (codex-ide-session-list-cell
                 (codex-ide-session-thread-list--preview-text thread)
                 'codex-ide-session-list-primary-face)
                (codex-ide-session-list-cell
                 (codex-ide-session-thread-list--thread-status thread directory)
                 'codex-ide-session-list-status-face)
                (codex-ide-session-list-cell
                 (or (codex-ide-session-thread-list--thread-buffer-name
                      thread directory)
                     "")
                 'codex-ide-session-list-secondary-face)
                (codex-ide-session-list-cell
                 (codex-ide--format-thread-updated-at
                  (alist-get 'createdAt thread))
                 'codex-ide-session-list-time-face)
                (codex-ide-session-list-cell
                 (codex-ide--format-thread-updated-at
                  (alist-get 'updatedAt thread))
                 'codex-ide-session-list-time-face)))))
     threads)))

(defun codex-ide-session-thread-list--visit (thread-id)
  "Visit THREAD-ID in the current thread list workspace."
  (codex-ide--prepare-session-operations)
  (codex-ide--show-or-resume-thread thread-id codex-ide-session-thread-list--directory))

(defun codex-ide-session-thread-list-delete-thread ()
  "Delete the stored thread on the current row or every row in the active region."
  (interactive)
  (codex-ide--prepare-session-operations)
  (let* ((thread-ids (codex-ide-session-list-selected-ids))
         (count (length thread-ids))
         (codex-home (abbreviate-file-name (codex-ide--codex-home))))
    (when (yes-or-no-p
           (if (= count 1)
               (format "Permanently remove 1 Codex thread from %s? " codex-home)
             (format "Permanently remove %d Codex threads from %s? "
                     count
                     codex-home)))
      (dolist (thread-id thread-ids)
        (codex-ide-delete-session-thread thread-id t))
      (tabulated-list-print t))))

(defun codex-ide-session-thread-list-redisplay ()
  "Regenerate the workspace thread list using current thread state."
  (interactive)
  (codex-ide--prepare-session-operations)
  (tabulated-list-print t))

;;;###autoload
(defun codex-ide-session-thread-list ()
  "Show a tabulated list of stored Codex threads for the current workspace."
  (interactive)
  (let* ((directory (codex-ide--normalize-directory
                     (codex-ide--get-working-directory)))
         (default-directory directory)
         (buffer nil))
    (codex-ide--prepare-session-operations)
    (setq buffer
          (codex-ide-session-list--setup
           (format "*Codex Threads: %s*"
                   (file-name-nondirectory (directory-file-name directory)))
           #'codex-ide-session-thread-list-mode
           [("ID" 12 t)
            ("Preview" 100 t)
            ("Status" 12 t)
            ("Buffer" 28 t)
            ("Created" 25 t)
            ("Updated" 25 t)]
           #'codex-ide-session-thread-list--entries
           #'codex-ide-session-thread-list--visit
           '("Updated" . t)
           (lambda ()
             (setq-local default-directory directory)
             (setq-local codex-ide-session-thread-list--directory directory))))
    (pop-to-buffer buffer)))

(provide 'codex-ide-session-thread-list)

;;; codex-ide-session-thread-list.el ends here
