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

(defun codex-ide-session-buffer-list--user-prompt-face-p (face)
  "Return non-nil when FACE includes `codex-ide-user-prompt-face'."
  (if (listp face)
      (memq 'codex-ide-user-prompt-face face)
    (eq face 'codex-ide-user-prompt-face)))

(defun codex-ide-session-buffer-list--last-prompt-text (session)
  "Return the last non-empty prompt text from SESSION's live buffer."
  (when-let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let ((pos (point-max))
                candidate)
            (while (and (> pos (point-min)) (not candidate))
              (setq pos (previous-single-char-property-change pos 'face nil (point-min)))
              (when (codex-ide-session-buffer-list--user-prompt-face-p
                     (get-char-property pos 'face))
                (let* ((end (next-single-char-property-change pos 'face nil (point-max)))
                       (start pos)
                       text)
                  (while (and (> start (point-min))
                              (codex-ide-session-buffer-list--user-prompt-face-p
                               (get-char-property (1- start) 'face)))
                    (setq start (1- start)))
                  (setq text (string-remove-prefix
                              "> "
                              (buffer-substring-no-properties start end)))
                  (setq text (string-trim text))
                  (unless (string-empty-p text)
                    (setq candidate text))
                  (setq pos start))))
            candidate))))))

(defun codex-ide-session-buffer-list--last-prompt (session)
  "Return a single-line preview of SESSION's last non-empty prompt line."
  (if-let ((prompt (codex-ide-session-buffer-list--last-prompt-text session)))
      (let ((preview (replace-regexp-in-string
                      "[\n\r]+"
                      "↵"
                      (codex-ide--thread-choice-preview prompt))))
        (if (string-empty-p preview) "Untitled" preview))
    ""))

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
            (status (codex-ide-renderer-status-label (codex-ide-session-status session))))
       (list session
             (vector (codex-ide-session-list-cell
                      (buffer-name buffer)
                      'codex-ide-session-list-primary-face)
                     (codex-ide-session-list-cell
                      directory
                      'codex-ide-session-list-secondary-face)
                     (codex-ide-session-list-cell
                      (codex-ide-session-buffer-list--last-prompt session)
                      'codex-ide-session-list-primary-face)
                     (codex-ide-session-list-cell
                      status
                      'codex-ide-session-list-status-face)
                     (codex-ide-session-list-cell
                      thread-id
                      'codex-ide-session-list-id-face)))))
   codex-ide-session-buffer-list--sessions))

(defun codex-ide-session-buffer-list--visit (session)
  "Visit SESSION's buffer."
  (when (buffer-live-p (codex-ide-session-buffer session))
    (codex-ide--show-session-buffer session)))

(defun codex-ide-session-buffer-list-delete-buffer ()
  "Kill the session buffer for the row at point or every row in the active region."
  (interactive)
  (let* ((sessions (codex-ide-session-list-selected-ids))
         (count (length sessions)))
    (dolist (session sessions)
      (let ((buffer (and (codex-ide-session-p session)
                         (codex-ide-session-buffer session))))
        (unless (buffer-live-p buffer)
          (user-error "Session buffer is no longer live"))))
    (when (y-or-n-p
           (if (= count 1)
               (let* ((session (car sessions))
                      (buffer (codex-ide-session-buffer session)))
                 (format "Kill Codex session buffer %s? " (buffer-name buffer)))
             (format "Kill %d Codex session buffers? " count)))
      (let ((kill-buffer-query-functions nil))
        (dolist (session sessions)
          (kill-buffer (codex-ide-session-buffer session))))
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
           ("Last Prompt" 48 t)
           ("Status" 14 t)
           ("Thread" 24 t)]
          #'codex-ide-session-buffer-list--entries
          #'codex-ide-session-buffer-list--visit
          '("Buffer" . nil))))
    (pop-to-buffer buffer)))

(provide 'codex-ide-session-buffer-list)

;;; codex-ide-session-buffer-list.el ends here
