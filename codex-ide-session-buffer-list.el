;;; codex-ide-session-buffer-list.el --- List Codex session buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tabulated listing of live Codex session buffers across all workspaces.

;;; Code:

(require 'codex-ide)
(require 'codex-ide-session-list)

(defvar-local codex-ide-session-buffer-list--sessions nil
  "Sessions shown in the current Codex session buffer list.")

(defvar codex-ide-session-buffer-list-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-buffer-list-mode'.")

(set-keymap-parent codex-ide-session-buffer-list-mode-map
                   codex-ide-session-list-mode-map)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "D")
            #'codex-ide-session-buffer-list-delete-buffer)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "l")
            #'codex-ide-session-buffer-list-redisplay)

(define-derived-mode codex-ide-session-buffer-list-mode codex-ide-session-list-mode
  "Codex-Buffers"
  "Mode for listing live Codex session buffers.")

(defun codex-ide-session-buffer-list--entries ()
  "Return tabulated entries for live Codex session buffers."
  (setq codex-ide-session-buffer-list--sessions
        (sort (copy-sequence (codex-ide--session-buffer-sessions))
              (lambda (left right)
                (string-lessp (buffer-name (codex-ide-session-buffer left))
                              (buffer-name (codex-ide-session-buffer right))))))
  (mapcar
   (lambda (session)
       (let* ((buffer (codex-ide-session-buffer session))
            (directory (abbreviate-file-name (codex-ide-session-directory session)))
            (thread-id (or (codex-ide-session-thread-id session) ""))
            (status (codex-ide--status-label (codex-ide-session-status session))))
       (list session
             (vector (codex-ide-session-list-cell
                      (buffer-name buffer)
                      'codex-ide-session-list-primary-face)
                     (codex-ide-session-list-cell
                      directory
                      'codex-ide-session-list-secondary-face)
                     (codex-ide-session-list-cell
                      thread-id
                      'codex-ide-session-list-id-face)
                     (codex-ide-session-list-cell
                      status
                      'codex-ide-session-list-status-face)))))
   codex-ide-session-buffer-list--sessions))

(defun codex-ide-session-buffer-list--visit (session)
  "Visit SESSION's buffer."
  (when (buffer-live-p (codex-ide-session-buffer session))
    (codex-ide--show-session-buffer session)))

(defun codex-ide-session-buffer-list-delete-buffer ()
  "Kill the session buffer for the row at point and refresh the list."
  (interactive)
  (let* ((session (tabulated-list-get-id))
         (buffer (and (codex-ide-session-p session)
                      (codex-ide-session-buffer session)))
         (buffer-name (and (buffer-live-p buffer)
                           (buffer-name buffer))))
    (unless session
      (user-error "No list entry at point"))
    (unless (buffer-live-p buffer)
      (user-error "Session buffer is no longer live"))
    (when (y-or-n-p (format "Kill Codex session buffer %s? " buffer-name))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))
      (tabulated-list-print t))))

(defun codex-ide-session-buffer-list-redisplay ()
  "Regenerate the session buffer list using current session state."
  (interactive)
  (tabulated-list-print t))

;;;###autoload
(defun codex-ide-session-buffer-list ()
  "Show a tabulated list of live Codex session buffers."
  (interactive)
  (let ((buffer
         (codex-ide-session-list--setup
          "*Codex Session Buffers*"
          #'codex-ide-session-buffer-list-mode
          [("Buffer" 28 t)
           ("Workspace" 40 t)
           ("Thread" 24 t)
           ("Status" 14 t)]
          #'codex-ide-session-buffer-list--entries
          #'codex-ide-session-buffer-list--visit
          '("Buffer" . nil))))
    (pop-to-buffer buffer)))

(provide 'codex-ide-session-buffer-list)

;;; codex-ide-session-buffer-list.el ends here
