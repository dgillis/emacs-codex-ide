;;; codex-ide-session-buffer-list.el --- List Codex session buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tabulated listing of live Codex session buffers across all workspaces.

;;; Code:

(require 'seq)
(require 'codex-ide)
(require 'codex-ide-session-list)

(autoload 'codex-ide-monitor-layout-for-sessions
  "codex-ide-monitor" nil nil)
(autoload 'codex-ide-monitor-layout
  "codex-ide-monitor" nil t)

(defvar codex-ide-session-buffer-list-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-buffer-list-mode'.")

(set-keymap-parent codex-ide-session-buffer-list-mode-map
                   codex-ide-session-list-mode-map)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "K")
            #'codex-ide-session-buffer-list-delete-buffer)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "l")
            #'codex-ide-session-buffer-list-redisplay)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "m")
            #'codex-ide-session-buffer-list-mark)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "u")
            #'codex-ide-session-buffer-list-unmark)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "U")
            #'codex-ide-session-buffer-list-unmark-all)
(define-key codex-ide-session-buffer-list-mode-map
            (kbd "M")
            #'codex-ide-session-buffer-list-monitor-marked-or-all)

(define-derived-mode codex-ide-session-buffer-list-mode codex-ide-session-list-mode
  "Codex-Buffers"
  "Mode for listing live Codex session buffers.")

(defvar codex-ide-session-buffer-list--marked-sessions nil
  "Live Codex sessions marked for monitor layout display.")

(defvar-local codex-ide-session-buffer-list--thread-metadata-cache nil
  "Cached thread metadata for the current live session buffer list.")

(defvar-local codex-ide-session-buffer-list--suppress-thread-metadata-refresh nil
  "Whether list redisplay should reuse cached thread metadata.")

(defun codex-ide-session-buffer-list--marked-live-sessions ()
  "Return marked sessions that still have live session buffers."
  (let ((live-sessions (codex-ide--session-buffer-sessions)))
    (setq codex-ide-session-buffer-list--marked-sessions
          (seq-filter
           (lambda (session)
             (memq session live-sessions))
           codex-ide-session-buffer-list--marked-sessions))))

(defun codex-ide-session-buffer-list--mark-cell (session)
  "Return the monitor mark display cell for SESSION."
  (if (memq session (codex-ide-session-buffer-list--marked-live-sessions))
      (codex-ide-session-list-cell "*" 'codex-ide-session-list-status-face)
    ""))

(defun codex-ide-session-buffer-list--last-prompt-text (session)
  "Return the last non-empty prompt text from SESSION's live buffer."
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let (candidate)
            (goto-char (point-max))
            (while (and (not candidate)
                        (re-search-backward "^> " nil t))
              (when (get-text-property (point) codex-ide-prompt-start-property)
                (let* ((end (next-single-char-property-change
                             (point)
                             'face
                             nil
                             (point-max)))
                       (text (string-trim
                              (string-remove-prefix
                               "> "
                               (buffer-substring-no-properties (point) end)))))
                  (unless (string-empty-p text)
                    (setq candidate text)))))
            candidate))))))

(defun codex-ide-session-buffer-list--preview (session)
  "Return the display preview for SESSION."
  (let ((preview (or (codex-ide-session-buffer-list--last-prompt-text session)
                     "")))
    (setq preview
          (replace-regexp-in-string
           "[\n\r]+"
           "↵"
           (codex-ide--thread-choice-preview preview)))
    (if (string-empty-p preview) "Untitled" preview)))

(defun codex-ide-session-buffer-list--thread-metadata (sessions)
  "Return a hash table of thread metadata for SESSIONS.

Keys are cons cells of the form `(DIRECTORY . THREAD-ID)'."
  (if codex-ide-session-buffer-list--suppress-thread-metadata-refresh
      (or codex-ide-session-buffer-list--thread-metadata-cache
          (make-hash-table :test #'equal))
    (let ((metadata (make-hash-table :test #'equal))
          (loaded-directories (make-hash-table :test #'equal)))
      (dolist (session sessions)
        (let ((directory (codex-ide-session-directory session)))
          (unless (gethash directory loaded-directories)
            (puthash directory t loaded-directories)
            (condition-case nil
                (dolist (thread (codex-ide--thread-list-data session))
                  (puthash (cons directory (alist-get 'id thread))
                           thread
                           metadata))
              (error nil)))))
      (setq codex-ide-session-buffer-list--thread-metadata-cache metadata))))

(defun codex-ide-session-buffer-list--entries ()
  "Return tabulated entries for live Codex session buffers."
  (let ((sessions
         (sort (copy-sequence (codex-ide--session-buffer-sessions))
               (lambda (left right)
                 (string-lessp (buffer-name (codex-ide-session-buffer left))
                               (buffer-name (codex-ide-session-buffer right)))))))
    (let ((thread-metadata
           (codex-ide-session-buffer-list--thread-metadata
            sessions)))
      (mapcar
       (lambda (session)
         (let* ((buffer (codex-ide-session-buffer session))
                (directory (codex-ide-session-directory session))
                (thread-id (codex-ide-session-thread-id session))
                (thread (and thread-id
                             (gethash (cons directory thread-id) thread-metadata)))
                (updated (if thread
                             (codex-ide--format-thread-updated-at
                              (alist-get 'updatedAt thread))
                           ""))
                (status (codex-ide-renderer-status-label
                         (codex-ide-session-status session))))
           (list session
                 (vector (codex-ide-session-buffer-list--mark-cell session)
                         (codex-ide-session-list-cell
                          (buffer-name buffer)
                          'codex-ide-session-list-primary-face)
                         (codex-ide-session-list-cell
                          status
                          'codex-ide-session-list-status-face)
                         (codex-ide-session-list-cell
                          updated
                          'codex-ide-session-list-time-face)
                         (codex-ide-session-list-cell
                          (codex-ide-session-buffer-list--preview session)
                          'codex-ide-session-list-primary-face)))))
       sessions))))

(defun codex-ide-session-buffer-list--visit (session)
  "Visit SESSION's buffer."
  (when (buffer-live-p (codex-ide-session-buffer session))
    (codex-ide--show-session-buffer session)))

(defun codex-ide-session-buffer-list--update-marked-sessions (sessions marked)
  "Set whether SESSIONS are MARKED for monitor layout display."
  (dolist (session sessions)
    (setq codex-ide-session-buffer-list--marked-sessions
          (delq session codex-ide-session-buffer-list--marked-sessions))
    (when marked
      (setq codex-ide-session-buffer-list--marked-sessions
            (append codex-ide-session-buffer-list--marked-sessions
                    (list session)))))
  (codex-ide-session-buffer-list--marked-live-sessions)
  (let ((codex-ide-session-buffer-list--suppress-thread-metadata-refresh t))
    (tabulated-list-print t)))

(defun codex-ide-session-buffer-list--focused-marked-session (marked-sessions)
  "Return the current focused session when it is in MARKED-SESSIONS."
  (let ((current-session
         (if (derived-mode-p 'codex-ide-session-buffer-list-mode)
             (tabulated-list-get-id)
           (codex-ide--session-for-current-buffer))))
    (and (memq current-session marked-sessions)
         current-session)))

(defun codex-ide-session-buffer-list-mark ()
  "Mark the selected session buffer rows for monitor layout display."
  (interactive)
  (codex-ide-session-buffer-list--update-marked-sessions
   (codex-ide-session-list-selected-ids)
   t))

(defun codex-ide-session-buffer-list-unmark ()
  "Remove monitor marks from the selected session buffer rows."
  (interactive)
  (codex-ide-session-buffer-list--update-marked-sessions
   (codex-ide-session-list-selected-ids)
   nil))

(defun codex-ide-session-buffer-list-unmark-all ()
  "Remove every monitor mark from live session buffer rows."
  (interactive)
  (setq codex-ide-session-buffer-list--marked-sessions nil)
  (let ((codex-ide-session-buffer-list--suppress-thread-metadata-refresh t))
    (tabulated-list-print t)))

(defun codex-ide-session-buffer-list-monitor-marked ()
  "Open a monitor layout for the marked live session buffer rows."
  (interactive)
  (let* ((marked-sessions
          (codex-ide-session-buffer-list--marked-live-sessions))
         (focused-session
          (codex-ide-session-buffer-list--focused-marked-session
           marked-sessions)))
    (unless marked-sessions
      (user-error "No marked Codex session buffers to monitor"))
    (codex-ide-monitor-layout-for-sessions marked-sessions focused-session)))

;;;###autoload
(defun codex-ide-session-buffer-list-monitor-marked-or-all ()
  "Open a monitor layout for marked sessions, or recent live sessions."
  (interactive)
  (let ((marked-sessions
         (codex-ide-session-buffer-list--marked-live-sessions)))
    (if marked-sessions
        (codex-ide-monitor-layout-for-sessions
         marked-sessions
         (codex-ide-session-buffer-list--focused-marked-session
          marked-sessions))
      (codex-ide-monitor-layout))))

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
          [("Mark" 4 nil)
           ("Buffer" 28 t)
           ("Status" 14 t)
           ("Updated" 16 t)
           ("Preview" 48 t)]
          #'codex-ide-session-buffer-list--entries
          #'codex-ide-session-buffer-list--visit
          '("Buffer" . nil))))
    (pop-to-buffer buffer)))

(provide 'codex-ide-session-buffer-list)

;;; codex-ide-session-buffer-list.el ends here
