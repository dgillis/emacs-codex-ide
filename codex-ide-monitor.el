;;; codex-ide-monitor.el --- Monitor layout for Codex sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; This module owns monitor-oriented layouts for live Codex session buffers.
;; It keeps one focused session large and arranges other live sessions in a
;; compact right rail.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'codex-ide-core)

(declare-function codex-ide--transcript-tail-point-position "codex-ide-transcript" ())

(defvar codex-ide-session-mode--last-point)
(defvar codex-ide-session-mode--last-window-start)

(defconst codex-ide-monitor--default-rail-sessions 3
  "Default number of unmarked live Codex sessions shown in the monitor rail.")

(defcustom codex-ide-monitor-rail-width 50
  "Preferred width in columns for the compact right monitor rail."
  :type 'integer
  :group 'codex-ide)

(defconst codex-ide-monitor--rail-sessions-frame-parameter
  'codex-ide-monitor-rail-sessions)

(defconst codex-ide-monitor--focused-session-frame-parameter
  'codex-ide-monitor-focused-session)

(defconst codex-ide-monitor--session-scope-frame-parameter
  'codex-ide-monitor-session-scope)

(defun codex-ide-monitor--live-sessions ()
  "Return live sessions with live session buffers."
  (codex-ide--session-buffer-sessions))

(defun codex-ide-monitor--live-subset (sessions)
  "Return live SESSIONS in their existing order."
  (let ((live-sessions (codex-ide-monitor--live-sessions)))
    (seq-filter (lambda (session) (memq session live-sessions))
                sessions)))

(defun codex-ide-monitor--recent-sessions (&optional sessions)
  "Return SESSIONS sorted by most recent activity first."
  (mapcar
   #'cdr
   (sort (cl-loop for session in (or sessions (codex-ide-monitor--live-sessions))
                  for index from 0
                  collect (cons index session))
         (lambda (left right)
           (let ((left-time (codex-ide--session-activity-time (cdr left)))
                 (right-time (codex-ide--session-activity-time (cdr right))))
             (if (= left-time right-time)
                 (< (car left) (car right))
               (> left-time right-time)))))))

(defun codex-ide-monitor--default-sessions ()
  "Return the default unmarked monitor sessions."
  (seq-take (codex-ide-monitor--recent-sessions)
            (1+ codex-ide-monitor--default-rail-sessions)))

(defun codex-ide-monitor--focused-session (&optional sessions)
  "Return the monitor-focused session from SESSIONS."
  (let* ((sessions (or sessions (codex-ide-monitor--live-sessions)))
         (current (codex-ide--session-for-current-buffer)))
    (or (and (memq current sessions) current)
        (codex-ide--most-recent-session sessions))))

(defun codex-ide-monitor--rail-sessions (sessions focused-session)
  "Return monitor rail sessions from SESSIONS excluding FOCUSED-SESSION."
  (seq-take (delq focused-session (copy-sequence sessions))
            codex-ide-monitor--default-rail-sessions))

(defun codex-ide-monitor--tail-window (window)
  "Move WINDOW to the end of its buffer and bottom-align the tail."
  (when (window-live-p window)
    (save-selected-window
      (select-window window)
      (with-current-buffer (window-buffer window)
        (goto-char
         (if (fboundp 'codex-ide--transcript-tail-point-position)
             (codex-ide--transcript-tail-point-position)
           (point-max)))
        (set-window-point window (point))
        (recenter -1)
        (set-window-parameter window 'codex-ide-tail-follow-suspended nil)
        (when (local-variable-p 'codex-ide-session-mode--last-point)
          (setq-local codex-ide-session-mode--last-point (point)
                      codex-ide-session-mode--last-window-start
                      (window-start window)))))))

(defun codex-ide-monitor--capture-rail-window-states (sessions frame)
  "Return scroll states for visible rail SESSIONS in FRAME."
  (cl-loop for session in sessions
           for buffer = (codex-ide-session-buffer session)
           for window = (and buffer (get-buffer-window buffer frame))
           when window
           collect
           (cons session
                 (list :point (window-point window)
                       :start (window-start window)
                       :suspended
                       (window-parameter
                        window
                        'codex-ide-tail-follow-suspended)))))

(defun codex-ide-monitor--restore-window-state (window state)
  "Restore WINDOW point and scroll STATE."
  (when (window-live-p window)
    (let ((point (plist-get state :point))
          (start (plist-get state :start)))
      (set-window-start window start t)
      (set-window-point window point)
      (set-window-parameter
       window
       'codex-ide-tail-follow-suspended
       (plist-get state :suspended))
      (with-current-buffer (window-buffer window)
        (when (local-variable-p 'codex-ide-session-mode--last-point)
          (setq-local codex-ide-session-mode--last-point point
                      codex-ide-session-mode--last-window-start start))))))

(defun codex-ide-monitor--tail-or-restore-rail-window (window session states)
  "Tail WINDOW for SESSION, or restore a suspended rail state from STATES."
  (let ((state (cdr (assq session states))))
    (if (and state (plist-get state :suspended))
        (codex-ide-monitor--restore-window-state window state)
      (codex-ide-monitor--tail-window window))))

(defun codex-ide-monitor--split-rail (window count)
  "Split WINDOW into COUNT stacked rail windows and return them top to bottom."
  (let ((windows nil)
        (current window)
        (remaining count))
    (while (> remaining 1)
      (let* ((height (max window-min-height
                          (/ (window-total-height current)
                             remaining)))
             (next (split-window current height 'below)))
        (push current windows)
        (setq current next
              remaining (1- remaining))))
    (when (> count 0)
      (nreverse (cons current windows)))))

(defun codex-ide-monitor--rail-window-width (window)
  "Return the compact right rail width for WINDOW."
  (max window-min-width
       (min codex-ide-monitor-rail-width
            (max window-min-width
                 (/ (window-total-width window) 3)))))

(defun codex-ide-monitor--visible-rail-sessions (&optional frame)
  "Return monitor rail sessions currently visible in FRAME."
  (let* ((frame (or frame (selected-frame)))
         (live-sessions (codex-ide-monitor--live-sessions)))
    (seq-filter
     (lambda (session)
       (and (memq session live-sessions)
            (get-buffer-window (codex-ide-session-buffer session) frame)))
     (frame-parameter frame
                      codex-ide-monitor--rail-sessions-frame-parameter))))

(defun codex-ide-monitor--display-sessions (focused-session rail-sessions
                                                           &optional session-scope)
  "Display FOCUSED-SESSION and RAIL-SESSIONS in the selected frame."
  (let* ((live-sessions (codex-ide-monitor--live-sessions))
         (session-scope (and session-scope
                             (codex-ide-monitor--live-subset session-scope)))
         (focused-session (and (memq focused-session live-sessions)
                               focused-session))
         (rail-sessions (codex-ide-monitor--live-subset rail-sessions))
         (fallback-session (car rail-sessions))
         (focused-session (or focused-session fallback-session))
         (rail-sessions (delq focused-session (copy-sequence rail-sessions)))
         (main-buffer (and focused-session
                           (codex-ide-session-buffer focused-session)))
         (frame (selected-frame))
         (old-rail-sessions
          (frame-parameter frame
                           codex-ide-monitor--rail-sessions-frame-parameter))
         (old-rail-states
          (codex-ide-monitor--capture-rail-window-states
           old-rail-sessions
           frame)))
    (unless focused-session
      (user-error "No live Codex sessions to monitor"))
    (set-frame-parameter frame
                         codex-ide-monitor--rail-sessions-frame-parameter
                         rail-sessions)
    (set-frame-parameter frame
                         codex-ide-monitor--focused-session-frame-parameter
                         focused-session)
    (set-frame-parameter frame
                         codex-ide-monitor--session-scope-frame-parameter
                         session-scope)
    (delete-other-windows)
    (let ((main-window (selected-window)))
      (set-window-buffer main-window main-buffer)
      (when rail-sessions
        (let* ((rail-root (split-window
                           main-window
                           (- (codex-ide-monitor--rail-window-width
                               main-window))
                           'right))
               (rail-windows (codex-ide-monitor--split-rail
                              rail-root
                              (length rail-sessions))))
          (cl-mapc
           (lambda (window session)
             (set-window-buffer window (codex-ide-session-buffer session))
             (codex-ide-monitor--tail-or-restore-rail-window
              window
              session
              old-rail-states))
           rail-windows
           rail-sessions)))
      (codex-ide-monitor--tail-window main-window)
      (select-window main-window))))

;;;###autoload
(defun codex-ide-monitor-layout (&optional focused-session)
  "Display live Codex sessions in a main window plus compact right rail."
  (interactive)
  (let* ((sessions (codex-ide-monitor--default-sessions))
         (focused (if (memq focused-session sessions)
                      focused-session
                    (codex-ide-monitor--focused-session sessions))))
    (unless focused
      (user-error "No live Codex sessions to monitor"))
    (codex-ide-monitor--display-sessions
     focused
     (codex-ide-monitor--rail-sessions sessions focused))))

;;;###autoload
(defun codex-ide-monitor-layout-for-sessions (sessions &optional focused-session)
  "Display selected live SESSIONS in a main window plus compact right rail.
FOCUSED-SESSION is used as the main window when it is present in SESSIONS.
Every live selected session is displayed; the first selected session is used
when FOCUSED-SESSION is nil or not in SESSIONS."
  (let ((live-sessions (codex-ide-monitor--live-sessions))
        (deduped-sessions nil))
    (dolist (session sessions)
      (when (and (memq session live-sessions)
                 (not (memq session deduped-sessions)))
        (push session deduped-sessions)))
    (setq deduped-sessions (nreverse deduped-sessions))
    (unless deduped-sessions
      (user-error "No live Codex sessions to monitor"))
    (let ((focused (if (memq focused-session deduped-sessions)
                       focused-session
                     (car deduped-sessions))))
      (codex-ide-monitor--display-sessions
       focused
       (delq focused (copy-sequence deduped-sessions))
       deduped-sessions))))

(defun codex-ide-monitor--swap-rail-session (focused-session rail-sessions
                                                             session)
  "Return RAIL-SESSIONS with SESSION replaced by FOCUSED-SESSION."
  (mapcar (lambda (rail-session)
            (if (eq rail-session session)
                focused-session
              rail-session))
          rail-sessions))

(defun codex-ide-monitor--promote-session (session)
  "Promote SESSION while preserving the current monitor layout order."
  (let* ((frame (selected-frame))
         (focused-session
          (frame-parameter
           frame
           codex-ide-monitor--focused-session-frame-parameter))
         (rail-sessions
          (frame-parameter
           frame
           codex-ide-monitor--rail-sessions-frame-parameter))
         (session-scope
          (frame-parameter
           frame
           codex-ide-monitor--session-scope-frame-parameter)))
    (cond
     ((and focused-session
           (memq session rail-sessions))
      (codex-ide-monitor--display-sessions
       session
       (codex-ide-monitor--swap-rail-session
        focused-session rail-sessions session)
       session-scope))
     (session-scope
      (codex-ide-monitor-layout-for-sessions session-scope session))
     (t
      (codex-ide-monitor-layout session)))))

;;;###autoload
(defun codex-ide-monitor-promote-session ()
  "Promote the selected Codex session buffer to the monitor main window."
  (interactive)
  (let ((session (codex-ide--session-for-current-buffer)))
    (unless (and session
                 (memq session (codex-ide-monitor--live-sessions)))
      (user-error "Current window does not contain a live Codex session"))
    (codex-ide-monitor--promote-session session)))

;;;###autoload
(defun codex-ide-monitor-promote-rail-session (index)
  "Promote the INDEXth visible monitor rail session to the main window."
  (interactive "nRail session number: ")
  (unless (and (integerp index) (> index 0))
    (user-error "Rail session number must be positive"))
  (let ((session (nth (1- index)
                      (codex-ide-monitor--visible-rail-sessions))))
    (unless session
      (user-error "No visible monitor rail session %d" index))
    (codex-ide-monitor--promote-session session)))

;;;###autoload
(defun codex-ide-monitor-promote-rail-session-1 ()
  "Promote the first visible monitor rail session to the main window."
  (interactive)
  (codex-ide-monitor-promote-rail-session 1))

;;;###autoload
(defun codex-ide-monitor-promote-rail-session-2 ()
  "Promote the second visible monitor rail session to the main window."
  (interactive)
  (codex-ide-monitor-promote-rail-session 2))

;;;###autoload
(defun codex-ide-monitor-promote-rail-session-3 ()
  "Promote the third visible monitor rail session to the main window."
  (interactive)
  (codex-ide-monitor-promote-rail-session 3))

(provide 'codex-ide-monitor)

;;; codex-ide-monitor.el ends here
