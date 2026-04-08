;;; codex-ide-session-buffer-list.el --- List Codex session buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tabulated listing of live Codex session buffers across all workspaces.

;;; Code:

(require 'codex-ide)
(require 'codex-ide-session-list)

(defvar-local codex-ide-session-buffer-list--sessions nil
  "Sessions shown in the current Codex session buffer list.")

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
             (vector (buffer-name buffer)
                     directory
                     thread-id
                     status))))
   codex-ide-session-buffer-list--sessions))

(defun codex-ide-session-buffer-list--visit (session)
  "Visit SESSION's buffer."
  (when (buffer-live-p (codex-ide-session-buffer session))
    (codex-ide--display-buffer-in-codex-window (codex-ide-session-buffer session))))

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
