;;; codex-ide-monitor.el --- Monitor layout for Codex sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; This module owns monitor-oriented layouts for live Codex session buffers.
;; It keeps one focused session large and arranges other live sessions in a
;; compact right rail.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'codex-ide-core)

(defconst codex-ide-monitor--default-rail-sessions 3
  "Default number of unmarked live Codex sessions shown in the monitor rail.")

(defconst codex-ide-monitor--rail-sessions-frame-parameter
  'codex-ide-monitor-rail-sessions)

(defconst codex-ide-monitor--session-scope-frame-parameter
  'codex-ide-monitor-session-scope)

(defun codex-ide-monitor--live-sessions ()
  "Return live sessions with live session buffers."
  (codex-ide--session-buffer-sessions))

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
  "Move WINDOW to the end of its buffer."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (set-window-point window (point-max)))))

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
  (let ((main-buffer (codex-ide-session-buffer focused-session))
        (frame (selected-frame)))
    (set-frame-parameter frame
                         codex-ide-monitor--rail-sessions-frame-parameter
                         rail-sessions)
    (set-frame-parameter frame
                         codex-ide-monitor--session-scope-frame-parameter
                         session-scope)
    (delete-other-windows)
    (set-window-buffer (selected-window) main-buffer)
    (codex-ide-monitor--tail-window (selected-window))
    (when rail-sessions
      (let* ((main-window (selected-window))
             (rail-root (split-window main-window nil 'right))
             (rail-windows (codex-ide-monitor--split-rail
                            rail-root
                            (length rail-sessions))))
        (cl-mapc
         (lambda (window session)
           (set-window-buffer window (codex-ide-session-buffer session))
           (codex-ide-monitor--tail-window window))
         rail-windows
         rail-sessions)
        (select-window main-window)))))

;;;###autoload
(defun codex-ide-monitor-layout (&optional focused-session)
  "Display live Codex sessions in a main window plus compact right rail."
  (interactive)
  (let* ((sessions (codex-ide-monitor--default-sessions))
         (focused (if (memq focused-session sessions)
                      focused-session
                    (car sessions))))
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

(defun codex-ide-monitor--promote-session (session)
  "Promote SESSION while preserving any explicit monitor session scope."
  (if-let* ((session-scope
             (frame-parameter
              nil
              codex-ide-monitor--session-scope-frame-parameter)))
      (codex-ide-monitor-layout-for-sessions session-scope session)
    (codex-ide-monitor-layout session)))

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
