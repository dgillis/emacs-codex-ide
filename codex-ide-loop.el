;;; codex-ide-loop.el --- Timer-backed prompt loops for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; Dedicated loop buffers for periodically submitting an editable prompt to a
;; single Codex session.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-core)
(require 'codex-ide-header)
(require 'codex-ide-renderer)
(require 'codex-ide-session)
(require 'codex-ide-transcript)
(require 'codex-ide-window)

(defvar codex-ide-display-buffer-pop-up-action)

(defgroup codex-ide-loop nil
  "Timer-backed prompt loops for Codex IDE."
  :group 'codex-ide)

(defcustom codex-ide-loop-default-interval "15m"
  "Default loop interval used by `codex-ide-loop-create'.
Strings accept `s', `m', `h', and `d' suffixes.  Bare numbers are minutes."
  :type 'string
  :group 'codex-ide-loop)

(defcustom codex-ide-loop-prompt-placeholder-text
  "Loop prompt to send on each run..."
  "Placeholder text displayed in an empty Codex loop prompt."
  :type 'string
  :group 'codex-ide-loop)

(defface codex-ide-loop-title-face
  '((t :inherit codex-ide-item-summary-face :weight bold :height 1.1))
  "Face used for loop buffer titles."
  :group 'codex-ide-loop)

(defface codex-ide-loop-metadata-label-face
  '((t :inherit codex-ide-item-detail-face :weight bold))
  "Face used for loop buffer metadata labels."
  :group 'codex-ide-loop)

(defface codex-ide-loop-metadata-value-face
  '((t :inherit codex-ide-item-detail-face))
  "Face used for loop buffer metadata values."
  :group 'codex-ide-loop)

(defface codex-ide-loop-session-link-face
  '((t :inherit link))
  "Face used for the clickable session buffer name in loop buffers."
  :group 'codex-ide-loop)

(defface codex-ide-loop-action-button-face
  '((t :inherit button :weight bold))
  "Face used for loop buffer action buttons."
  :group 'codex-ide-loop)

(cl-defstruct codex-ide-loop
  session
  buffer
  timer
  interval-seconds
  state
  prompt-start-marker
  last-run-at
  next-run-at
  run-count
  last-skip-reason
  last-error)

(defvar codex-ide-loop--loops-by-session (make-hash-table :test 'eq)
  "Hash table mapping live sessions to loop objects.")

(defvar-local codex-ide-loop--loop nil
  "Loop object owned by the current loop buffer.")

(defvar-local codex-ide-loop--input-end-marker nil
  "Marker for the end of the editable loop prompt text.")

(defvar-local codex-ide-loop--prompt-display-start-marker nil
  "Marker for the start of the loop prompt display block.")

(defvar-local codex-ide-loop--placeholder-overlay nil
  "Overlay displaying placeholder text in an empty loop prompt.")

(defvar-local codex-ide-loop--input-face-overlay nil
  "Overlay giving editable loop prompt text the user prompt face.")

(defvar codex-ide-loop-mode-map (make-sparse-keymap)
  "Keymap for `codex-ide-loop-mode'.")

(defvar codex-ide-loop-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "TAB") #'codex-ide-loop-nav-forward)
    (define-key map (kbd "<backtab>") #'codex-ide-loop-nav-backward)
    map)
  "Button keymap used in Codex loop buffers.")

(define-key codex-ide-loop-mode-map (kbd "C-c RET") #'codex-ide-loop-start)
(define-key codex-ide-loop-mode-map (kbd "C-c C-c") #'codex-ide-loop-pause)
(define-key codex-ide-loop-mode-map (kbd "C-c C-p") nil)
(define-key codex-ide-loop-mode-map (kbd "C-c C-k") nil)
(define-key codex-ide-loop-mode-map (kbd "C-c C-s") nil)
(define-key codex-ide-loop-mode-map (kbd "C-c C-j") nil)
(define-key codex-ide-loop-mode-map (kbd "TAB") #'codex-ide-loop-nav-forward)
(define-key codex-ide-loop-mode-map (kbd "<backtab>") #'codex-ide-loop-nav-backward)

(define-derived-mode codex-ide-loop-mode text-mode "Codex-Loop"
  "Major mode for Codex prompt loop buffers.

\\<codex-ide-loop-mode-map>
* \\[codex-ide-loop-start] starts or resumes the loop.
* \\[codex-ide-loop-pause] pauses the loop.

Additional loop actions are available from the buttons in the loop buffer."
  (setq-local truncate-lines nil)
  (setq-local buffer-read-only nil)
  (add-hook 'kill-buffer-query-functions
            #'codex-ide-loop--confirm-kill-buffer nil t)
  (add-hook 'kill-buffer-hook #'codex-ide-loop--handle-buffer-killed nil t)
  (add-hook 'after-change-functions
            #'codex-ide-loop--refresh-placeholder-after-change nil t)
  (add-hook 'post-command-hook #'codex-ide-loop--sync-prompt-point nil t))

(defun codex-ide-loop--parse-interval (value)
  "Return interval seconds represented by VALUE.
VALUE may be a positive number of seconds or a string with an optional suffix:
`s' for seconds, `m' for minutes, `h' for hours, and `d' for days.  Bare string
numbers are interpreted as minutes."
  (cond
   ((and (numberp value) (> value 0))
    value)
   ((stringp value)
    (let ((text (string-trim value)))
      (unless (string-match
               "\\`\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\s-*\\([smhdSMHD]?\\)\\'"
               text)
        (user-error "Invalid interval: %s" value))
      (let* ((amount (string-to-number (match-string 1 text)))
             (unit (downcase (match-string 2 text)))
             (multiplier (pcase unit
                           ("s" 1)
                           ("m" 60)
                           ("h" 3600)
                           ("d" 86400)
                           ("" 60))))
        (unless (> amount 0)
          (user-error "Interval must be positive"))
        (* amount multiplier))))
   (t
    (user-error "Invalid interval: %S" value))))

(defun codex-ide-loop--format-duration (seconds)
  "Return a compact duration string for SECONDS."
  (cl-labels ((format-amount
                (amount unit)
                (format "%s%s"
                        (replace-regexp-in-string
                         "\\.?0+\\'"
                         ""
                         (format "%.2f" amount))
                        unit)))
    (cond
     ((not (numberp seconds)) "?")
     ((= 0 (mod seconds 86400))
      (format-amount (/ seconds 86400.0) "d"))
     ((= 0 (mod seconds 3600))
      (format-amount (/ seconds 3600.0) "h"))
     ((= 0 (mod seconds 60))
      (format-amount (/ seconds 60.0) "m"))
     (t
      (format-amount seconds "s")))))

(defun codex-ide-loop--format-time (time)
  "Return a compact time string for TIME."
  (if time
      (format-time-string "%Y-%m-%d %H:%M:%S" time)
    "never"))

(defun codex-ide-loop--state-text (loop)
  "Return LOOP's display state."
  (symbol-name (or (codex-ide-loop-state loop) 'stopped)))

(defun codex-ide-loop--session-name (session)
  "Return a display name for SESSION."
  (if-let* ((buffer (and session (codex-ide-session-buffer session)))
            (_ (buffer-live-p buffer)))
      (buffer-name buffer)
    "dead session"))

(defun codex-ide-loop--loop-buffer-name (session)
  "Return a loop buffer name for SESSION."
  (let ((session-buffer (and session (codex-ide-session-buffer session))))
    (format "%s-loop"
            (if (buffer-live-p session-buffer)
                (buffer-name session-buffer)
              (codex-ide--session-buffer-name
               (codex-ide-session-directory session)
               (codex-ide-session-name-suffix session))))))

(defun codex-ide-loop--read-interval ()
  "Read a loop interval from the minibuffer."
  (read-string
   "Codex loop interval: "
   codex-ide-loop-default-interval))

(defun codex-ide-loop--session-for-current-session-buffer ()
  "Return the Codex session attached to the current session buffer."
  (let ((session (codex-ide--session-for-current-buffer)))
    (unless (and session
                 (buffer-live-p (codex-ide-session-buffer session))
                 (eq (current-buffer) (codex-ide-session-buffer session)))
      (user-error "This command must be run from a Codex session buffer"))
    (unless (ignore-errors
              (process-live-p (codex-ide-session-process session)))
      (user-error "Codex session process is not running"))
    session))

(defun codex-ide-loop--live-loop-for-session (session)
  "Return SESSION's live loop object, or nil after clearing stale state."
  (when-let* ((loop (gethash session codex-ide-loop--loops-by-session)))
    (if (and (codex-ide-loop-p loop)
             (buffer-live-p (codex-ide-loop-buffer loop)))
        loop
      (remhash session codex-ide-loop--loops-by-session)
      nil)))

(defun codex-ide-loop--display-loop-buffer (loop)
  "Display LOOP's buffer and move point to its prompt."
  (let* ((buffer (codex-ide-loop-buffer loop))
         (window (codex-ide-display-buffer
                  buffer
                  codex-ide-display-buffer-pop-up-action)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (markerp (codex-ide-loop-prompt-start-marker loop))
          (let ((pos (marker-position (codex-ide-loop-prompt-start-marker loop))))
            (goto-char pos)
            (when (window-live-p window)
              (set-window-point window pos))))))
    window))

(defun codex-ide-loop--current-prompt-raw (loop)
  "Return LOOP's prompt text without properties."
  (let ((marker (codex-ide-loop-prompt-start-marker loop))
        (end-marker (and (buffer-live-p (codex-ide-loop-buffer loop))
                         (with-current-buffer (codex-ide-loop-buffer loop)
                           codex-ide-loop--input-end-marker)))
        (buffer (codex-ide-loop-buffer loop)))
    (when (and (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer)
               (markerp end-marker)
               (eq (marker-buffer end-marker) buffer))
      (with-current-buffer buffer
        (buffer-substring-no-properties marker end-marker)))))

(defun codex-ide-loop--current-prompt (loop)
  "Return LOOP's current prompt text for submission."
  (string-trim (or (codex-ide-loop--current-prompt-raw loop) "")))

(defun codex-ide-loop--state-face (loop)
  "Return the face for LOOP's current state value."
  (pcase (codex-ide-loop-state loop)
    ('active 'codex-ide-status-running-face)
    ('paused 'codex-ide-status-idle-face)
    ('error 'codex-ide-status-error-face)
    (_ 'codex-ide-loop-metadata-value-face)))

(defun codex-ide-loop--insert-metadata-label (label)
  "Insert a loop metadata LABEL prefix."
  (insert (propertize (format "* %s:" label)
                      'face
                      'codex-ide-loop-metadata-label-face))
  (insert " "))

(defun codex-ide-loop--insert-metadata-line (label value &optional value-face)
  "Insert a loop metadata row with LABEL and VALUE."
  (codex-ide-loop--insert-metadata-label label)
  (insert (propertize (or value "")
                      'face
                      (or value-face
                          'codex-ide-loop-metadata-value-face)))
  (insert "\n"))

(defun codex-ide-loop--insert-button (label action &optional help face)
  "Insert a loop buffer button labeled LABEL that runs ACTION.
HELP, when non-nil, is used as the button help text.
FACE, when non-nil, is used as the button face."
  (insert-text-button
   label
   'follow-link t
   'face (or face 'codex-ide-loop-action-button-face)
   'help-echo help
   'keymap codex-ide-loop-button-map
   'mouse-face 'highlight
   'action (lambda (_button) (funcall action))))

(defun codex-ide-loop--button-positions ()
  "Return visible button positions in the current loop buffer."
  (let ((pos (point-min))
        positions)
    (while (setq pos (next-button pos))
      (unless (invisible-p pos)
        (push pos positions))
      (setq pos (max (1+ pos) (button-end (button-at pos)))))
    (nreverse positions)))

(defun codex-ide-loop--nav-to-button (direction)
  "Move to the next loop button in DIRECTION.
DIRECTION should be 1 for forward or -1 for backward."
  (let* ((positions (codex-ide-loop--button-positions))
         (current (point))
         (candidates (if (> direction 0)
                         (seq-filter (lambda (pos) (> pos current)) positions)
                       (nreverse
                        (seq-filter (lambda (pos) (< pos current)) positions))))
         (target (car candidates)))
    (unless target
      (user-error "No %s loop button"
                  (if (> direction 0) "next" "previous")))
    (goto-char target)))

;;;###autoload
(defun codex-ide-loop-nav-forward ()
  "Move to the next interactive button in the current loop buffer."
  (interactive)
  (codex-ide-loop--nav-to-button 1))

;;;###autoload
(defun codex-ide-loop-nav-backward ()
  "Move to the previous interactive button in the current loop buffer."
  (interactive)
  (codex-ide-loop--nav-to-button -1))

(defun codex-ide-loop--prompt-empty-p ()
  "Return non-nil when the current loop buffer prompt is empty."
  (and (markerp codex-ide-loop--input-end-marker)
       (markerp (and (codex-ide-loop-p codex-ide-loop--loop)
                     (codex-ide-loop-prompt-start-marker codex-ide-loop--loop)))
       (= (marker-position (codex-ide-loop-prompt-start-marker
                            codex-ide-loop--loop))
          (marker-position codex-ide-loop--input-end-marker))))

(defun codex-ide-loop--input-end-position ()
  "Return the current loop prompt input end position, or nil."
  (when (and (markerp codex-ide-loop--input-end-marker)
             (eq (marker-buffer codex-ide-loop--input-end-marker)
                 (current-buffer)))
    (marker-position codex-ide-loop--input-end-marker)))

(defun codex-ide-loop--prompt-display-start-position ()
  "Return the current loop prompt display start position, or nil."
  (when (and (markerp codex-ide-loop--prompt-display-start-marker)
             (eq (marker-buffer codex-ide-loop--prompt-display-start-marker)
                 (current-buffer)))
    (marker-position codex-ide-loop--prompt-display-start-marker)))

(defun codex-ide-loop--delete-placeholder-overlay ()
  "Delete the loop prompt placeholder overlay, if any."
  (when (overlayp codex-ide-loop--placeholder-overlay)
    (delete-overlay codex-ide-loop--placeholder-overlay))
  (setq codex-ide-loop--placeholder-overlay nil))

(defun codex-ide-loop--delete-input-face-overlay ()
  "Delete the loop prompt input face overlay, if any."
  (when (overlayp codex-ide-loop--input-face-overlay)
    (delete-overlay codex-ide-loop--input-face-overlay))
  (setq codex-ide-loop--input-face-overlay nil))

(defun codex-ide-loop--style-input-region (start end)
  "Style the editable loop prompt region from START to END."
  (when (<= start end)
    (when (< start end)
      (remove-list-of-text-properties
       start end
       (list 'face 'field codex-ide-prompt-start-property))
      (put-text-property start end 'face 'codex-ide-user-prompt-face))
    (codex-ide-loop--delete-input-face-overlay)
    (setq-local codex-ide-loop--input-face-overlay
                (make-overlay start (point-max) (current-buffer) nil t))
    (overlay-put codex-ide-loop--input-face-overlay
                 'face 'codex-ide-user-prompt-face)
    (overlay-put codex-ide-loop--input-face-overlay
                 'field 'codex-ide-active-input)
    (overlay-put codex-ide-loop--input-face-overlay
                 'read-only nil)))

(defun codex-ide-loop--placeholder-string ()
  "Return the propertized loop prompt placeholder string."
  (let ((text (propertize codex-ide-loop-prompt-placeholder-text
                          'face 'codex-ide-prompt-placeholder-face)))
    (unless (string-empty-p text)
      (add-text-properties 0 1 '(cursor t) text))
    text))

(defun codex-ide-loop--refresh-placeholder ()
  "Refresh the current loop buffer's prompt placeholder."
  (when (and (boundp 'codex-ide-loop--loop)
             (codex-ide-loop-p codex-ide-loop--loop)
             (markerp (codex-ide-loop-prompt-start-marker codex-ide-loop--loop))
             (eq (marker-buffer (codex-ide-loop-prompt-start-marker
                                 codex-ide-loop--loop))
                 (current-buffer)))
    (if (codex-ide-loop--prompt-empty-p)
        (let ((start (codex-ide-loop-prompt-start-marker codex-ide-loop--loop)))
          (unless (overlayp codex-ide-loop--placeholder-overlay)
            (setq codex-ide-loop--placeholder-overlay
                  (make-overlay start start (current-buffer) nil t))
            (overlay-put codex-ide-loop--placeholder-overlay
                         'codex-ide-loop-placeholder t))
          (move-overlay codex-ide-loop--placeholder-overlay start start)
          (overlay-put codex-ide-loop--placeholder-overlay
                       'after-string
                       (codex-ide-loop--placeholder-string)))
      (codex-ide-loop--delete-placeholder-overlay))))

(defun codex-ide-loop--refresh-placeholder-after-change (&rest _args)
  "Refresh loop prompt placeholder after buffer edits."
  (codex-ide-loop--refresh-placeholder))

(defun codex-ide-loop--sync-prompt-point ()
  "Keep point inside the editable loop prompt text when it enters padding."
  (when (and (boundp 'codex-ide-loop--loop)
             (codex-ide-loop-p codex-ide-loop--loop)
             (markerp (codex-ide-loop-prompt-start-marker codex-ide-loop--loop)))
    (let ((display-start (codex-ide-loop--prompt-display-start-position))
          (input-start (marker-position
                        (codex-ide-loop-prompt-start-marker
                         codex-ide-loop--loop)))
          (input-end (codex-ide-loop--input-end-position)))
      (when (and display-start input-end)
        (cond
         ((and (>= (point) display-start)
               (< (point) input-start))
          (goto-char input-start))
         ((and (> (point) input-end)
               (<= (point) (point-max)))
          (goto-char input-end))))))
  (codex-ide-loop--refresh-placeholder))

(defun codex-ide-loop--render-buffer (loop)
  "Render LOOP's buffer while preserving its editable prompt text."
  (let ((buffer (codex-ide-loop-buffer loop)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((old-marker (codex-ide-loop-prompt-start-marker loop))
               (old-prompt-start (and (markerp old-marker)
                                      (marker-position old-marker)))
               (old-point (point))
               (old-point-offset (and old-prompt-start
                                      (>= old-point old-prompt-start)
                                      (- old-point old-prompt-start)))
               (prompt (or (codex-ide-loop--current-prompt-raw loop) ""))
               (inhibit-read-only t))
          (codex-ide--without-undo-recording
            (codex-ide-loop--delete-placeholder-overlay)
            (codex-ide-loop--delete-input-face-overlay)
            (erase-buffer)
            (setq-local codex-ide-loop--loop loop)
            (setq-local codex-ide-loop--input-end-marker nil)
            (setq-local codex-ide-loop--prompt-display-start-marker nil)
            (insert (propertize "Codex Loop"
                                'face 'codex-ide-loop-title-face)
                    "\n")
            (codex-ide-loop--insert-metadata-label "Session")
            (codex-ide-loop--insert-button
             (codex-ide-loop--session-name
              (codex-ide-loop-session loop))
             #'codex-ide-loop-jump-to-session
             "Jump to the attached Codex session"
             'codex-ide-loop-session-link-face)
            (insert "\n")
            (codex-ide-loop--insert-metadata-line
             "State"
             (codex-ide-loop--state-text loop)
             (codex-ide-loop--state-face loop))
            (codex-ide-loop--insert-metadata-line
             "Interval"
             (codex-ide-loop--format-duration
              (codex-ide-loop-interval-seconds loop)))
            (codex-ide-loop--insert-metadata-line
             "Last run"
             (codex-ide-loop--format-time
              (codex-ide-loop-last-run-at loop)))
            (codex-ide-loop--insert-metadata-line
             "Next run"
             (codex-ide-loop--format-time
              (codex-ide-loop-next-run-at loop)))
            (codex-ide-loop--insert-metadata-line
             "Runs"
             (number-to-string (or (codex-ide-loop-run-count loop) 0)))
            (codex-ide-loop--insert-metadata-line
             "Last skip"
             (or (codex-ide-loop-last-skip-reason loop) "none"))
            (codex-ide-loop--insert-metadata-line
             "Last error"
             (or (codex-ide-loop-last-error loop) "none"))
            (insert "\n")
            (codex-ide-loop--insert-metadata-label "Actions")
            (codex-ide-loop--insert-button
             "start"
             #'codex-ide-loop-start
             "Start or resume this loop")
            (insert "  ")
            (codex-ide-loop--insert-button
             "pause"
             #'codex-ide-loop-pause
             "Pause this loop")
            (insert "  ")
            (codex-ide-loop--insert-button
             "send now"
             #'codex-ide-loop-send-now
             "Send the current prompt immediately")
            (insert "\n\n")
            (let* ((read-only-end (point))
                   (prompt-state
                    (codex-ide-renderer-insert-input-prompt prompt))
                   (input-end nil))
              (setq-local codex-ide-loop--prompt-display-start-marker
                          (copy-marker read-only-end))
              (goto-char (plist-get prompt-state :prompt-start))
              (codex-ide-renderer-insert-user-prompt-top-padding)
              (set-marker (plist-get prompt-state :prompt-start) (point))
              (goto-char (point-max))
              (setq input-end (copy-marker (point)))
              (codex-ide-renderer-insert-user-prompt-bottom-padding)
              (set-marker-insertion-type input-end t)
              (setf (codex-ide-loop-prompt-start-marker loop)
                    (plist-get prompt-state :input-start))
              (setq-local codex-ide-loop--input-end-marker
                          input-end)
              (add-text-properties
               (point-min)
               read-only-end
               '(read-only t
                 front-sticky (read-only)
                 rear-nonsticky (read-only)))
              (codex-ide-renderer-make-region-writable
               (marker-position (plist-get prompt-state :input-start))
               (marker-position input-end))
              (codex-ide-loop--style-input-region
               (marker-position (plist-get prompt-state :input-start))
               (marker-position input-end)))
            (codex-ide-loop--refresh-placeholder)
            (goto-char
             (cond
              (old-point-offset
               (min (point-max)
                    (+ (marker-position
                        (codex-ide-loop-prompt-start-marker loop))
                       old-point-offset)))
              (old-prompt-start
               (min old-point (point-max)))
              (t
               (marker-position
                (codex-ide-loop-prompt-start-marker loop)))))))))))

(defun codex-ide-loop--loop-at-point ()
  "Return the loop associated with the current buffer."
  (unless (and (boundp 'codex-ide-loop--loop)
               (codex-ide-loop-p codex-ide-loop--loop))
    (user-error "Current buffer is not a Codex loop buffer"))
  codex-ide-loop--loop)

(defun codex-ide-loop--cancel-timer (loop)
  "Cancel LOOP's pending timer."
  (when-let* ((timer (codex-ide-loop-timer loop)))
    (when (timerp timer)
      (cancel-timer timer)))
  (setf (codex-ide-loop-timer loop) nil
        (codex-ide-loop-next-run-at loop) nil))

(defun codex-ide-loop--refresh-session-header (loop)
  "Refresh LOOP's attached session header, when possible."
  (when-let* ((session (codex-ide-loop-session loop)))
    (when (and (codex-ide-session-p session)
               (buffer-live-p (codex-ide-session-buffer session)))
      (codex-ide-transcript-update-header-line session))))

(defun codex-ide-loop--schedule-next (loop)
  "Schedule LOOP's next timer tick."
  (codex-ide-loop--cancel-timer loop)
  (when (eq (codex-ide-loop-state loop) 'active)
    (let* ((interval (codex-ide-loop-interval-seconds loop))
           (next-run-at (time-add (current-time) interval)))
      (setf (codex-ide-loop-next-run-at loop) next-run-at
            (codex-ide-loop-timer loop)
            (run-at-time interval nil #'codex-ide-loop--tick loop))
      (codex-ide-loop--render-buffer loop)
      (codex-ide-loop--refresh-session-header loop))))

(defun codex-ide-loop--session-skip-reason (loop)
  "Return a reason LOOP cannot submit now, or nil when ready."
  (let* ((session (codex-ide-loop-session loop))
         (buffer (and session (codex-ide-session-buffer session))))
    (cond
     ((not (codex-ide-session-p session))
      "Session is gone")
     ((not (buffer-live-p buffer))
      "Session buffer is dead")
     ((not (process-live-p (codex-ide-session-process session)))
      "Session process is not running")
     ((not (codex-ide-session-thread-id session))
      "Session has no active thread")
     ((codex-ide-session-current-turn-id session)
      "Session is busy")
     ((not (string= (or (codex-ide-session-status session) "") "idle"))
      (format "Session is %s" (or (codex-ide-session-status session)
                                  "not idle")))
     (t nil))))

(defun codex-ide-loop--record-skip (loop reason)
  "Record that LOOP skipped an iteration for REASON."
  (setf (codex-ide-loop-last-skip-reason loop) reason)
  (codex-ide-loop--render-buffer loop))

(defun codex-ide-loop--submit-now (loop)
  "Submit LOOP's current prompt once.
Return non-nil when a prompt was submitted."
  (if-let* ((reason (codex-ide-loop--session-skip-reason loop)))
      (progn
        (codex-ide-loop--record-skip loop reason)
        nil)
    (let ((prompt (codex-ide-loop--current-prompt loop)))
      (if (string-empty-p prompt)
          (progn
            (codex-ide-loop--record-skip loop "Prompt is empty")
            nil)
        (let ((submitted-at (current-time)))
          (codex-ide-transcript-submit-prompt-to-session
           (codex-ide-loop-session loop)
           prompt
           :metadata-line (codex-ide-loop--metadata-line loop submitted-at)
           :suppress-context t)
          (setf (codex-ide-loop-last-run-at loop) submitted-at))
        (setf
              (codex-ide-loop-run-count loop)
              (1+ (or (codex-ide-loop-run-count loop) 0))
              (codex-ide-loop-last-skip-reason loop) nil
              (codex-ide-loop-last-error loop) nil)
        (codex-ide-loop--render-buffer loop)
        t))))

(defun codex-ide-loop--tick (loop)
  "Run one scheduled LOOP iteration."
  (setf (codex-ide-loop-timer loop) nil
        (codex-ide-loop-next-run-at loop) nil)
  (cond
   ((not (eq (codex-ide-loop-state loop) 'active))
    (codex-ide-loop--render-buffer loop))
   ((not (buffer-live-p (codex-ide-loop-buffer loop)))
    (codex-ide-loop--cancel-timer loop))
   (t
    (condition-case err
        (progn
          (codex-ide-loop--submit-now loop)
          (codex-ide-loop--schedule-next loop))
      (error
       (setf (codex-ide-loop-state loop) 'error
             (codex-ide-loop-last-error loop) (error-message-string err))
       (codex-ide-loop--cancel-timer loop)
       (codex-ide-loop--render-buffer loop)
       (codex-ide-loop--refresh-session-header loop)
       (message "Codex loop paused after error: %s"
                (error-message-string err)))))))

(defun codex-ide-loop--detach (loop)
  "Detach LOOP from its session registry."
  (when-let* ((session (codex-ide-loop-session loop)))
    (remhash session codex-ide-loop--loops-by-session)))

(defun codex-ide-loop--confirm-kill-buffer ()
  "Ask before killing an active loop buffer."
  (let ((loop (and (boundp 'codex-ide-loop--loop)
                   codex-ide-loop--loop)))
    (or (not (and (codex-ide-loop-p loop)
                  (eq (codex-ide-loop-state loop) 'active)))
        (yes-or-no-p "Kill active Codex loop buffer? "))))

(defun codex-ide-loop--handle-buffer-killed ()
  "Clean up loop state when a loop buffer is killed."
  (when (and (boundp 'codex-ide-loop--loop)
             (codex-ide-loop-p codex-ide-loop--loop))
    (let ((loop codex-ide-loop--loop))
      (setf (codex-ide-loop-state loop) 'stopped)
      (codex-ide-loop--cancel-timer loop)
      (codex-ide-loop--detach loop)
      (codex-ide-loop--refresh-session-header loop))))

(defun codex-ide-loop--handle-session-event (event session &rest _payload)
  "Handle session lifecycle EVENT for SESSION."
  (when (eq event 'destroyed)
    (when-let* ((loop (gethash session codex-ide-loop--loops-by-session)))
      (setf (codex-ide-loop-state loop) 'stopped
            (codex-ide-loop-last-error loop) "Session destroyed")
      (codex-ide-loop--cancel-timer loop)
      (codex-ide-loop--detach loop)
      (codex-ide-loop--render-buffer loop))))

(defun codex-ide-loop--create (session interval-seconds)
  "Create and return a loop for SESSION with INTERVAL-SECONDS."
  (let* ((buffer (generate-new-buffer (codex-ide-loop--loop-buffer-name session)))
         (loop (make-codex-ide-loop
                :session session
                :buffer buffer
                :interval-seconds interval-seconds
                :state 'paused
                :run-count 0)))
    (with-current-buffer buffer
      (codex-ide-loop-mode)
      (setq-local codex-ide-loop--loop loop)
      (codex-ide-loop--render-buffer loop))
    (puthash session loop codex-ide-loop--loops-by-session)
    (codex-ide-loop--refresh-session-header loop)
    loop))

;;;###autoload
(defun codex-ide-loop-create (interval)
  "Create or show a loop buffer for the current Codex session.
INTERVAL is read interactively and accepts suffixes supported by
`codex-ide-loop-default-interval'.  The new loop starts paused."
  (interactive
   (list (codex-ide-loop--read-interval)))
  (let* ((session (codex-ide--session-for-current-project))
         (existing (codex-ide-loop--live-loop-for-session session))
         (loop (or existing
                   (codex-ide-loop--create
                    session
                    (codex-ide-loop--parse-interval interval)))))
    (codex-ide-loop--display-loop-buffer loop)
    (when existing
      (message "Codex loop already exists for this session"))
    loop))

;;;###autoload
(defun codex-ide-loop-jump-or-create (&optional interval)
  "Jump to this session's loop buffer, or create one with INTERVAL.

This command is session-buffer scoped.  It reuses the loop associated with the
current Codex session buffer when one exists.  Otherwise, it prompts for an
interval unless INTERVAL was supplied programmatically."
  (interactive)
  (let* ((session (codex-ide-loop--session-for-current-session-buffer))
         (existing (codex-ide-loop--live-loop-for-session session))
         (loop (or existing
                   (codex-ide-loop--create
                    session
                    (codex-ide-loop--parse-interval
                     (or interval (codex-ide-loop--read-interval)))))))
    (codex-ide-loop--display-loop-buffer loop)
    (message
     (if existing
         "Opened Codex loop buffer"
       "Created Codex loop buffer"))
    loop))

;;;###autoload
(defun codex-ide-loop-start ()
  "Start or resume the current Codex loop."
  (interactive)
  (let ((loop (codex-ide-loop--loop-at-point)))
    (setf (codex-ide-loop-state loop) 'active
          (codex-ide-loop-last-error loop) nil)
    (codex-ide-loop--schedule-next loop)
    (message "Codex loop started; next run in %s"
             (codex-ide-loop--format-duration
              (codex-ide-loop-interval-seconds loop)))))

;;;###autoload
(defun codex-ide-loop-pause ()
  "Pause the current Codex loop."
  (interactive)
  (let ((loop (codex-ide-loop--loop-at-point)))
    (setf (codex-ide-loop-state loop) 'paused)
    (codex-ide-loop--cancel-timer loop)
    (codex-ide-loop--render-buffer loop)
    (codex-ide-loop--refresh-session-header loop)
    (message "Codex loop paused")))

;;;###autoload
(defun codex-ide-loop-stop ()
  "Stop and detach the current Codex loop."
  (interactive)
  (let ((loop (codex-ide-loop--loop-at-point)))
    (setf (codex-ide-loop-state loop) 'stopped)
    (codex-ide-loop--cancel-timer loop)
    (codex-ide-loop--detach loop)
    (codex-ide-loop--render-buffer loop)
    (codex-ide-loop--refresh-session-header loop)
    (message "Codex loop stopped")))

;;;###autoload
(defun codex-ide-loop-send-now ()
  "Submit the current loop prompt immediately when the session is ready."
  (interactive)
  (let ((loop (codex-ide-loop--loop-at-point)))
    (when (eq (codex-ide-loop-state loop) 'active)
      (codex-ide-loop--cancel-timer loop))
    (condition-case err
        (progn
          (when (codex-ide-loop--submit-now loop)
            (message "Codex loop prompt submitted"))
          (when (eq (codex-ide-loop-state loop) 'active)
            (codex-ide-loop--schedule-next loop)))
      (error
       (setf (codex-ide-loop-state loop) 'error
             (codex-ide-loop-last-error loop) (error-message-string err))
       (codex-ide-loop--cancel-timer loop)
       (codex-ide-loop--render-buffer loop)
       (codex-ide-loop--refresh-session-header loop)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun codex-ide-loop-jump-to-session ()
  "Show the session buffer attached to the current Codex loop."
  (interactive)
  (let* ((loop (codex-ide-loop--loop-at-point))
         (session (codex-ide-loop-session loop))
         (buffer (and session (codex-ide-session-buffer session))))
    (unless (buffer-live-p buffer)
      (user-error "Attached Codex session buffer is no longer live"))
    (codex-ide-display-buffer buffer codex-ide-display-buffer-pop-up-action)))

;;;###autoload
(defun codex-ide-loop-jump-to-loop ()
  "Show the loop buffer attached to the current Codex session."
  (interactive)
  (let* ((session (codex-ide--session-for-current-project))
         (loop (gethash session codex-ide-loop--loops-by-session)))
    (unless (and loop (buffer-live-p (codex-ide-loop-buffer loop)))
      (user-error "No loop buffer for this Codex session"))
    (codex-ide-display-buffer
     (codex-ide-loop-buffer loop)
     codex-ide-display-buffer-pop-up-action)))

(defun codex-ide-loop--header-summary (session)
  "Return a session header loop summary for SESSION."
  (when-let* ((loop (gethash session codex-ide-loop--loops-by-session)))
    (when (buffer-live-p (codex-ide-loop-buffer loop))
      (let ((map (make-sparse-keymap)))
        (define-key map [header-line mouse-1] #'codex-ide-loop-jump-to-loop)
        (define-key map [mouse-1] #'codex-ide-loop-jump-to-loop)
        (propertize
         (format "Loop: %s" (codex-ide-loop--state-text loop))
         'mouse-face 'mode-line-highlight
         'help-echo "mouse-1: show Codex loop buffer"
         'local-map map)))))

(defun codex-ide-loop--metadata-line (loop time)
  "Return transcript metadata for LOOP submitted at TIME."
  (let ((buffer (codex-ide-loop-buffer loop)))
    (format "Loop: from %s at %s"
            (if (buffer-live-p buffer)
                (buffer-name buffer)
              "dead loop buffer")
            (format-time-string "%Y-%m-%d %H:%M:%S" time))))

(defun codex-ide-loop--session-placeholder (session)
  "Return loop-aware placeholder text for SESSION, or nil."
  (when-let* ((loop (gethash session codex-ide-loop--loops-by-session)))
    (when (buffer-live-p (codex-ide-loop-buffer loop))
      (pcase (codex-ide-loop-state loop)
        ('active "Loop active: waiting for next scheduled prompt...")
        ('paused "Loop paused")
        ('error "Loop paused after error")
        (_ nil)))))

(add-hook 'codex-ide-header-extra-summary-functions
          #'codex-ide-loop--header-summary)
(add-hook 'codex-ide-session-event-hook
          #'codex-ide-loop--handle-session-event)
(setq codex-ide-loop-session-placeholder-function
      #'codex-ide-loop--session-placeholder)

(provide 'codex-ide-loop)

;;; codex-ide-loop.el ends here
