;;; codex-ide-transcript.el --- Transcript controller for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; `codex-ide-transcript' is the controller layer that turns session state and
;; Codex protocol events into transcript updates.
;;
;; This file owns transcript-oriented coordination: deciding when to start or
;; finish visible turns, managing prompt/input lifecycle, tracking item-local
;; render state, formatting session-aware summaries, handling approvals and
;; command-output widgets, rendering session errors, and replaying stored thread
;; data back into a live transcript buffer.
;;
;; Unlike `codex-ide-renderer.el', this module is allowed to depend on
;; codex-ide session structures and controller helpers.  It should depend on
;; `codex-ide-renderer.el' for low-level view behavior instead of reintroducing
;; a second rendering subsystem.  New code here should answer "what transcript
;; change should happen now?" while the renderer answers "how is that change
;; drawn in the buffer?".  Transcript-scoped helpers that remain here should
;; encode transcript semantics such as insertion policy, active-boundary
;; handling, or agent-text property defaults; helpers that only forward to a
;; renderer primitive without adding transcript-specific meaning should live in
;; the renderer or be inlined at the callsite.
;;
;; This file should not become a dumping ground for unrelated application
;; logic.  Generic session primitives belong in `codex-ide-core.el`, command
;; surfaces belong in `codex-ide-transient.el`, and bridge-specific behavior
;; belongs in `codex-ide-mcp-bridge.el`.  When extracting from `codex-ide.el`,
;; prefer moving transcript/session orchestration here and pushing pure view code
;; down into the renderer.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-context)
(require 'codex-ide-core)
(require 'codex-ide-protocol)
(require 'codex-ide-diff-data)
(require 'codex-ide-diff-view)
(require 'codex-ide-errors)
(require 'codex-ide-mcp-elicitation)
(require 'codex-ide-nav)
(require 'codex-ide-renderer)
(require 'codex-ide-rollout)
(require 'codex-ide-window)

(declare-function codex-ide--ensure-session-for-current-project "codex-ide-session" ())
(declare-function codex-ide--session-for-current-project "codex-ide-session" ())
(declare-function codex-ide--show-session-buffer "codex-ide-session" (session &key newly-created))
(declare-function codex-ide--sync-prompt-minor-mode "codex-ide-session-mode" (&optional session))
(declare-function codex-ide-config-effective-value "codex-ide-config" (key &optional session))
(declare-function codex-ide-log-message "codex-ide-log" (session format-string &rest args))

(defvar codex-ide-log-max-lines)
(defvar codex-ide-renderer-render-markdown-during-streaming)
(defvar codex-ide-renderer-markdown-render-max-chars)
(defvar codex-ide-renderer-command-output-fold-on-start)
(defvar codex-ide-renderer-command-output-max-rendered-lines)
(defvar codex-ide-renderer-command-output-max-rendered-chars)
(defvar codex-ide-diff-inline-fold-threshold)
(defvar codex-ide-diff-auto-display-policy)
(defvar codex-ide-reasoning-effort)
(defvar codex-ide-resume-summary-turn-limit)
(defvar codex-ide-running-submit-action)
(defvar codex-ide-prompt-placeholder-text)
(defvar codex-ide-steering-placeholder-text)
(defvar codex-ide-model)
(defvar codex-ide-buffer-display-when-approval-required)
(defvar codex-ide-display-buffer-pop-up-action)
(defvar codex-ide--display-buffer-other-window-pop-up-action)
(defvar codex-ide-log-stream-deltas)
(defvar codex-ide--sessions)
(defvar codex-ide-item-result-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-ide-toggle-item-result-at-point)
    (define-key map (kbd "<return>") #'codex-ide-toggle-item-result-at-point)
    map)
  "Keymap used on expandable item-result transcript text.")

(defvar codex-ide-command-output-map codex-ide-item-result-map
  "Compatibility alias for `codex-ide-item-result-map'.")

(defvar codex-ide--current-transcript-log-marker nil
  "Marker for the log line associated with the transcript text being inserted.")

(defvar codex-ide--current-agent-item-type nil
  "Item type associated with the agent transcript text being inserted.")

(defvar codex-ide--preserve-transcript-window-follow-anchor t
  "When non-nil, transcript window restoration may keep following the anchor.

Streaming transcript appends should leave this enabled so windows already
tracking the live tail keep doing so.  Interactive in-place rewrites, such as
expanding or folding a command output block, should bind this to nil so the
clicked window stays where it was.")

(defun codex-ide--update-mode-line (&optional session)
  "Refresh the mode line indicator for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((buffer (and session (codex-ide-session-buffer session))))
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defun codex-ide--make-region-writable (start end)
  "Make the region from START to END writable."
  (codex-ide-renderer-make-region-writable start end))

(defun codex-ide--current-agent-text-properties ()
  "Return text properties for agent-originated transcript text."
  (append
   (when (markerp codex-ide--current-transcript-log-marker)
     (list codex-ide-log-marker-property codex-ide--current-transcript-log-marker))
   (when (stringp codex-ide--current-agent-item-type)
     (list codex-ide-agent-item-type-property codex-ide--current-agent-item-type))))

(defun codex-ide--freeze-region (start end)
  "Make the region from START to END read-only."
  (codex-ide-renderer-freeze-region start end))

(defun codex-ide--session-for-buffer (buffer)
  "Return the live Codex session associated with BUFFER."
  (or (and (buffer-live-p buffer)
           (with-current-buffer buffer
             (and (boundp 'codex-ide--session)
                  (codex-ide-session-p codex-ide--session)
                  codex-ide--session)))
      (let (found)
        (dolist (session codex-ide--sessions found)
          (when (and (codex-ide-session-p session)
                     (eq (codex-ide-session-buffer session) buffer))
            (setq found session))))))

(defun codex-ide--append-boundary-position (buffer)
  "Return where transcript text should be inserted in BUFFER.
When a running-input summary is displayed above the active prompt, new
transcript text is inserted before that summary so the summary and prompt remain
at the bottom of the live session."
  (when-let ((marker (codex-ide--append-boundary-marker buffer)))
    (marker-position marker)))

(defun codex-ide--append-boundary-marker (buffer)
  "Return BUFFER's running-input summary insertion marker."
  (when-let* ((session (codex-ide--session-for-buffer buffer))
              (list-marker (codex-ide--session-metadata-get
                            session
                            :running-input-list-boundary-marker)))
    (when (and (markerp list-marker)
               (eq (marker-buffer list-marker) buffer))
      list-marker)))

(defun codex-ide--active-input-boundary-position (buffer)
  "Return BUFFER's active prompt boundary while output is streaming."
  (when-let ((marker (codex-ide--active-input-boundary-marker buffer)))
    (marker-position marker)))

(defun codex-ide--active-input-boundary-marker (buffer)
  "Return BUFFER's active prompt marker while output is streaming."
  (when-let ((session (codex-ide--session-for-buffer buffer)))
    (when (and (or (codex-ide-session-current-turn-id session)
                   (codex-ide-session-output-prefix-inserted session))
               (codex-ide--input-prompt-active-p session))
      (let ((marker (or (codex-ide--session-metadata-get
                         session
                         :active-input-boundary-marker)
                        (codex-ide-session-input-prompt-start-marker session))))
        (when (and (markerp marker)
                   (eq (marker-buffer marker) buffer))
          marker)))))

(defun codex-ide--transcript-insertion-position (buffer)
  "Return the insertion point for appended transcript text in BUFFER."
  (or (codex-ide--append-boundary-position buffer)
      (codex-ide--active-input-boundary-position buffer)
      (point-max)))

(defun codex-ide--input-point-marker (session)
  "Return a marker preserving point when it is inside SESSION's input."
  (let ((buffer (and session (codex-ide-session-buffer session)))
        (prompt-start (and session
                           (codex-ide-session-input-prompt-start-marker session))))
    (when (and (buffer-live-p buffer)
               (eq (current-buffer) buffer)
               (codex-ide--input-prompt-active-p session)
               (markerp prompt-start)
               (eq (marker-buffer prompt-start) buffer)
               (>= (point) (marker-position prompt-start)))
      (copy-marker (point)))))

(defun codex-ide--restore-input-point-marker (marker)
  "Restore point to MARKER and clear it."
  (when (markerp marker)
    (when (marker-buffer marker)
      (goto-char marker))
    (set-marker marker nil)))

(defun codex-ide--transcript-window-follows-anchor-p (window anchor)
  "Return non-nil when WINDOW is already following transcript ANCHOR."
  (let ((anchor-pos (and anchor
                         (if (markerp anchor)
                             (marker-position anchor)
                           anchor)))
        (buffer-end (point-max))
        (window-point-pos (window-point window))
        (window-start-pos (window-start window))
        (window-end-pos (window-end window t))
        (tail-follow-suspended (window-parameter
                                window
                                'codex-ide-tail-follow-suspended)))
    (and (window-live-p window)
         (eq (window-buffer window) (current-buffer))
         (not tail-follow-suspended)
         (or (>= window-point-pos anchor-pos)
             (>= window-end-pos anchor-pos)
             (and (>= window-end-pos buffer-end)
                  (> window-point-pos window-start-pos))))))

(defun codex-ide--capture-transcript-window-positions (&optional anchor)
  "Capture current-buffer window positions relative to transcript ANCHOR."
  (mapcar
   (lambda (window)
     (list :window window
           :follow-anchor
           (and codex-ide--preserve-transcript-window-follow-anchor
                (codex-ide--transcript-window-follows-anchor-p window anchor))
           :start-marker (copy-marker (window-start window))
           :point-marker (copy-marker (window-point window))))
   (get-buffer-window-list (current-buffer) nil t)))

(defun codex-ide--restore-transcript-window-positions (states)
  "Restore transcript window positions recorded in STATES."
  (dolist (state states)
    (let ((window (plist-get state :window))
          (follow-anchor (plist-get state :follow-anchor))
          (start-marker (plist-get state :start-marker))
          (point-marker (plist-get state :point-marker))
          (tail-pos (point-max)))
      (unwind-protect
          (when (and (window-live-p window)
                     (eq (window-buffer window) (current-buffer))
                     (markerp point-marker)
                     (marker-buffer point-marker))
            (if follow-anchor
                (set-window-point window tail-pos)
              (when (and (markerp start-marker)
                         (marker-buffer start-marker))
                (set-window-start window (marker-position start-marker) t))
              (set-window-point window (marker-position point-marker))))
        (when (markerp start-marker)
          (set-marker start-marker nil))
        (when (markerp point-marker)
          (set-marker point-marker nil))))))

(defmacro codex-ide--maybe-save-transcript-position (anchor &rest body)
  "Run BODY while preserving non-following transcript windows around ANCHOR."
  (declare (indent 1) (debug (form body)))
  `(let ((window-states
          (codex-ide--capture-transcript-window-positions ,anchor)))
     (unwind-protect
         (progn ,@body)
       (codex-ide--restore-transcript-window-positions window-states))))

(defun codex-ide--ensure-active-input-prompt-spacing (session)
  "Ensure SESSION's live prompt is separated from preceding output."
  (let ((buffer (codex-ide-session-buffer session))
        (boundary (codex-ide--session-metadata-get
                   session
                   :active-input-boundary-marker))
        (prompt-start (codex-ide-session-input-prompt-start-marker session)))
    (when (and (buffer-live-p buffer)
               (markerp boundary)
               (markerp prompt-start)
               (eq (marker-buffer boundary) buffer)
               (eq (marker-buffer prompt-start) buffer)
               (< (marker-position boundary)
                  (marker-position prompt-start)))
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
         (let* ((inhibit-read-only t)
                (restore-point (codex-ide--input-point-marker session))
                (newline-count 0)
                (insert-start nil))
           (save-excursion
             (goto-char (marker-position prompt-start))
             (while (and (> (point) (point-min))
                         (eq (char-before) ?\n))
               (setq newline-count (1+ newline-count))
               (backward-char)))
           (when (< newline-count 2)
             (goto-char (marker-position boundary))
             (setq insert-start (point))
             (codex-ide-renderer-insert-read-only-newlines
              (- 2 newline-count))
             (codex-ide--freeze-region insert-start (point)))
           (codex-ide--restore-input-point-marker restore-point)))))))

(defun codex-ide--advance-active-boundary-after (buffer marker)
  "Move BUFFER's active prompt and append boundaries after MARKER when needed."
  (when-let ((active-boundary (codex-ide--active-input-boundary-marker buffer)))
    (when (and (markerp marker)
               (eq (marker-buffer marker) buffer)
               (<= (marker-position active-boundary)
                   (marker-position marker)))
      (set-marker active-boundary (marker-position marker))))
  (when-let ((append-boundary (codex-ide--append-boundary-marker buffer)))
    (when (and (markerp marker)
               (eq (marker-buffer marker) buffer)
               (<= (marker-position append-boundary)
                   (marker-position marker)))
      (set-marker append-boundary (marker-position marker)))))

(defun codex-ide--advance-append-boundary-after (buffer insertion-position end)
  "Move BUFFER's append boundary to END after inserting at INSERTION-POSITION."
  (when-let ((append-boundary (codex-ide--append-boundary-marker buffer)))
    (when (= insertion-position (marker-position append-boundary))
      (set-marker append-boundary end))))

(defun codex-ide--append-to-buffer (buffer text &optional face properties)
  "Append TEXT to BUFFER as read-only transcript text.
When FACE is non-nil, apply it to the inserted text.
When PROPERTIES is non-nil, it should be a property list applied to the
inserted text."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((restore-point (codex-ide--input-point-marker
                             (codex-ide--session-for-buffer buffer)))
             (moving (and (= (point) (point-max)) (not restore-point)))
             (active-boundary (codex-ide--active-input-boundary-marker buffer))
             (insertion-position (codex-ide--transcript-insertion-position buffer))
             (advance-active-boundary
              (and active-boundary
                   (= insertion-position (marker-position active-boundary)))))
        (codex-ide--maybe-save-transcript-position insertion-position
						   (codex-ide-renderer-append-to-buffer
						    text
						    :insertion-point insertion-position
						    :face face
						    :properties properties
						    :restore-point restore-point
						    :preserve-point t
						    :move-point-to-end moving
						    :after-insert
						    (lambda (_start end inserted-at)
						      (codex-ide--advance-append-boundary-after buffer inserted-at end)
						      (when advance-active-boundary
							(set-marker active-boundary end)
							(when-let ((session (codex-ide--session-for-buffer buffer)))
							  (codex-ide--ensure-active-input-prompt-spacing
							   session))))))))))

(defun codex-ide--append-agent-text (buffer text &optional face properties)
  "Append agent-originated TEXT to BUFFER with FACE and PROPERTIES."
  (codex-ide--append-to-buffer
   buffer
   text
   face
   (append properties (codex-ide--current-agent-text-properties))))

(defun codex-ide--ensure-output-spacing (buffer)
  "Ensure BUFFER is ready for a new rendered output block."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((restore-point (codex-ide--input-point-marker
                             (codex-ide--session-for-buffer buffer)))
             (active-boundary (codex-ide--active-input-boundary-marker buffer))
             (insertion-position (codex-ide--transcript-insertion-position buffer))
             (advance-active-boundary
              (and active-boundary
                   (= insertion-position (marker-position active-boundary)))))
        (codex-ide--maybe-save-transcript-position insertion-position
						   (codex-ide-renderer-append-to-buffer
						    ""
						    :insertion-point insertion-position
						    :restore-point restore-point
						    :preserve-point t
						    :after-insert
						    (lambda (_start _end inserted-at)
						      (goto-char inserted-at)
						      (let ((range (codex-ide-renderer-insert-output-spacing)))
							(codex-ide--advance-append-boundary-after buffer inserted-at (cdr range))
							(when advance-active-boundary
							  (set-marker active-boundary (cdr range)))))))))))

(defun codex-ide--append-output-separator (buffer)
  "Append a transcript separator rule to BUFFER."
  (codex-ide--append-agent-text
   buffer
   (codex-ide-renderer-output-separator-string)
   'codex-ide-output-separator-face))

(defun codex-ide--append-restored-transcript-separator (buffer)
  "Append the restored-history boundary separator to BUFFER."
  (codex-ide--append-agent-text
   buffer
   (concat "\n" (codex-ide-renderer-restored-transcript-separator-string))
   'codex-ide-output-separator-face)
  (codex-ide--append-to-buffer buffer "\n"))

(defun codex-ide--insert-pending-output-indicator (session &optional text)
  "Insert a temporary pending-output indicator for SESSION."
  (let ((buffer (codex-ide-session-buffer session))
        (indicator-text (or text "Working...\n")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((restore-point (codex-ide--input-point-marker session))
               (moving (and (= (point) (point-max)) (not restore-point)))
               (active-boundary (codex-ide--active-input-boundary-marker buffer))
               (insertion-position (codex-ide--transcript-insertion-position buffer))
               (advance-active-boundary
                (and active-boundary
                     (= insertion-position (marker-position active-boundary)))))
          (codex-ide--maybe-save-transcript-position insertion-position
						     (codex-ide-renderer-append-to-buffer
						      ""
						      :insertion-point insertion-position
						      :restore-point restore-point
						      :preserve-point t
						      :move-point-to-end moving
						      :after-insert
						      (lambda (start end inserted-at)
							(ignore start end)
							(goto-char inserted-at)
							(pcase-let ((`(,indicator-start ,indicator-end ,inserted-text)
								     (codex-ide-renderer-insert-pending-indicator
								      indicator-text)))
							  (codex-ide--advance-append-boundary-after
							   buffer inserted-at indicator-end)
							  (when advance-active-boundary
							    (set-marker active-boundary indicator-end)
							    (codex-ide--ensure-active-input-prompt-spacing
							     session))
							  (codex-ide--session-metadata-put
							   session
							   :pending-output-indicator-marker
							   (copy-marker indicator-start))
							  (codex-ide--session-metadata-put
							   session
							   :pending-output-indicator-text
							   inserted-text))))))))))

(defun codex-ide--clear-pending-output-indicator (session)
  "Remove SESSION's pending-output indicator, if it is still present."
  (when-let ((marker (codex-ide--session-metadata-get
                      session
                      :pending-output-indicator-marker)))
    (let ((buffer (marker-buffer marker))
          (indicator-text
           (or (codex-ide--session-metadata-get
                session
                :pending-output-indicator-text)
               "Working...\n")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let* ((restore-point (codex-ide--input-point-marker session))
                 (moving (and (= (point) (point-max)) (not restore-point)))
                 (start (marker-position marker)))
            (codex-ide--maybe-save-transcript-position start
						       (codex-ide-renderer--without-undo-recording
							(let ((inhibit-read-only t))
							  (codex-ide-renderer-delete-matching-text start indicator-text)
							  (cond
							   (restore-point
							    (codex-ide--restore-input-point-marker restore-point))
							   (moving
							    (goto-char (point-max))))))))))
      (set-marker marker nil))
    (codex-ide--session-metadata-put
     session
     :pending-output-indicator-marker
     nil)
    (codex-ide--session-metadata-put
     session
     :pending-output-indicator-text
     nil)))

(defun codex-ide--replace-pending-output-indicator (session text)
  "Replace SESSION's temporary pending-output indicator with TEXT."
  (codex-ide--clear-pending-output-indicator session)
  (codex-ide--insert-pending-output-indicator session text))

(defun codex-ide--delete-input-overlay (session)
  "Delete the active input overlay for SESSION, if any."
  (codex-ide--delete-input-placeholder-overlay session)
  (when-let ((overlay (codex-ide-session-input-overlay session)))
    (delete-overlay overlay)
    (setf (codex-ide-session-input-overlay session) nil)))

(defun codex-ide--delete-input-placeholder-overlay (session)
  "Delete SESSION's active input placeholder overlay, if any."
  (when-let ((overlay (codex-ide--session-metadata-get
                       session
                       :input-placeholder-overlay)))
    (delete-overlay overlay)
    (codex-ide--session-metadata-put
     session
     :input-placeholder-overlay
     nil)))

(defun codex-ide--input-placeholder-text (&optional session)
  "Return the placeholder text for SESSION's current prompt state."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (if (and session
           (or (codex-ide-session-current-turn-id session)
               (codex-ide-session-output-prefix-inserted session)))
      codex-ide-steering-placeholder-text
    codex-ide-prompt-placeholder-text))

(defun codex-ide--input-placeholder-display-string (&optional session)
  "Return the propertized placeholder display string for SESSION."
  (let ((text (propertize (codex-ide--input-placeholder-text session)
                          'face
                          'codex-ide-prompt-placeholder-face)))
    (unless (string-empty-p text)
      (add-text-properties 0 1 '(cursor t) text))
    text))

(defun codex-ide--input-placeholder-visible-p (&optional session)
  "Return non-nil when SESSION's input placeholder should be visible."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (and (codex-ide--input-prompt-active-p session)
       (string-empty-p (codex-ide--current-input session))))

(defun codex-ide--ensure-input-placeholder-overlay (&optional session)
  "Ensure SESSION has a display-only placeholder overlay at input start."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((buffer (codex-ide-session-buffer session))
         (marker (codex-ide-session-input-start-marker session))
         (overlay (codex-ide--session-metadata-get
                   session
                   :input-placeholder-overlay)))
    (unless (and (buffer-live-p buffer)
                 (markerp marker)
                 (eq (marker-buffer marker) buffer))
      (codex-ide--delete-input-placeholder-overlay session)
      (setq overlay nil))
    (unless (and (overlayp overlay)
                 (overlay-buffer overlay))
      (when (overlayp overlay)
        (delete-overlay overlay))
      (codex-ide--session-metadata-put
       session
       :input-placeholder-overlay
       nil)
      (setq overlay nil))
    (when (and (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer)
               (not (overlayp overlay)))
      (with-current-buffer buffer
        (setq overlay (make-overlay
                       (marker-position marker)
                       (marker-position marker)
                       buffer
                       nil
                       t))
        (overlay-put overlay 'codex-ide-input-placeholder t)
        (codex-ide--session-metadata-put
         session
         :input-placeholder-overlay
         overlay)))
    (when (and (overlayp overlay)
               (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer)
               (overlay-buffer overlay))
      (move-overlay overlay
                    (marker-position marker)
                    (marker-position marker)
                    buffer))
    overlay))

(defun codex-ide--refresh-input-placeholder (&optional session)
  "Refresh SESSION's active prompt placeholder visibility and text."
  (interactive)
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when session
    (let ((overlay (codex-ide--ensure-input-placeholder-overlay session)))
      (when (overlayp overlay)
        (overlay-put overlay 'before-string nil)
        (overlay-put
         overlay
         'after-string
         (and (codex-ide--input-placeholder-visible-p session)
              (codex-ide--input-placeholder-display-string session)))))))

(defun codex-ide--refresh-input-placeholder-after-change (&rest _args)
  "Refresh the active prompt placeholder after editable buffer changes."
  (when-let ((session (codex-ide--session-for-buffer (current-buffer))))
    (codex-ide--refresh-input-placeholder session)))

(defun codex-ide--setup-input-placeholder-hooks ()
  "Install buffer-local hooks that keep prompt placeholder display current."
  (add-hook 'post-command-hook #'codex-ide--refresh-input-placeholder nil t)
  (add-hook 'after-change-functions
            #'codex-ide--refresh-input-placeholder-after-change
            nil
            t))

(defun codex-ide--insert-prompt-prefix ()
  "Insert a visible `> ' prompt prefix at point."
  (let ((start (point)))
    (codex-ide-renderer-insert-prompt-prefix)
    (add-text-properties
     start
     (point)
     '(read-only t
		 rear-nonsticky (read-only)
		 front-sticky (read-only)))))

(defun codex-ide--line-has-prompt-start-p (&optional pos)
  "Return non-nil when the line at POS starts a user prompt."
  (codex-ide-renderer-line-has-prompt-start-p pos))

(defun codex-ide--delete-active-input-prompt (session)
  "Delete SESSION's active editable input prompt, if any."
  (let ((buffer (codex-ide-session-buffer session))
        (start (or (codex-ide--session-metadata-get
                    session
                    :active-input-boundary-marker)
                   (codex-ide-session-input-prompt-start-marker session))))
    (when (and (buffer-live-p buffer)
               (markerp start)
               (eq (marker-buffer start) buffer))
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
         (let ((inhibit-read-only t)
               (moving (= (point) (point-max))))
           (delete-region (marker-position start) (point-max))
           (when moving
             (goto-char (point-max)))))))
    (codex-ide--delete-input-overlay session)
    (codex-ide--session-metadata-put session :active-input-boundary-marker nil)
    (setf (codex-ide-session-input-start-marker session) nil
          (codex-ide-session-input-prompt-start-marker session) nil)
    (codex-ide--sync-prompt-minor-mode session)))

(defun codex-ide--delete-running-input-list (session)
  "Delete SESSION's rendered running queue list."
  (let ((buffer (codex-ide-session-buffer session))
        (start (codex-ide--session-metadata-get
                session
                :running-input-list-delete-start-marker))
        (boundary (codex-ide--session-metadata-get
                   session
                   :running-input-list-boundary-marker))
        (end (codex-ide--session-metadata-get
              session
              :running-input-list-end-marker)))
    (when (and (buffer-live-p buffer)
               (markerp start)
               (markerp boundary)
               (markerp end)
               (eq (marker-buffer start) buffer)
               (eq (marker-buffer boundary) buffer)
               (eq (marker-buffer end) buffer))
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
         (let ((inhibit-read-only t)
               (moving (= (point) (point-max))))
           (delete-region (marker-position boundary) (marker-position end))
           (when moving
             (goto-char (point-max)))))))
    (when (markerp start)
      (set-marker start nil))
    (when (markerp boundary)
      (set-marker boundary nil))
    (when (markerp end)
      (set-marker end nil))
    (codex-ide--session-metadata-put
     session
     :running-input-list-delete-start-marker
     nil)
    (codex-ide--session-metadata-put
     session
     :running-input-list-boundary-marker
     nil)
    (codex-ide--session-metadata-put
     session
     :running-input-list-end-marker
     nil)))

(defun codex-ide--running-list-prompt (entry)
  "Return the display prompt text for running-list ENTRY."
  (cond
   ((stringp entry) entry)
   ((and (consp entry) (plist-member entry :prompt))
    (plist-get entry :prompt))
   (t "")))

(defun codex-ide--running-list-format-prompt (prompt)
  "Return PROMPT formatted for the compact running-input list."
  (let ((text (string-trim (or prompt ""))))
    (if (string-empty-p text)
        "(empty)"
      (replace-regexp-in-string "\n" "\n     " text t t))))

(defun codex-ide--running-list-section (title prompts)
  "Return a rendered running-input list section titled TITLE for PROMPTS."
  (when prompts
    (concat
     title
     "\n"
     (mapconcat
      (lambda (indexed)
        (format "  %d. %s"
                (car indexed)
                (codex-ide--running-list-format-prompt (cdr indexed))))
      (cl-loop for prompt in prompts
               for index from 1
               collect (cons index prompt))
      "\n"))))

(defun codex-ide--running-input-list-text (session)
  "Return SESSION's visible running queue list text."
  (let* ((queued (mapcar #'codex-ide--running-list-prompt
                         (codex-ide--session-metadata-get
                          session
                          :queued-prompts)))
         (sections (delq nil
                         (list
                          (codex-ide--running-list-section
                           "Queued turns:"
                           queued)))))
    (unless (null sections)
      (concat (mapconcat #'identity sections "\n") "\n"))))

(defun codex-ide--insert-running-input-list (session)
  "Insert SESSION's running queue list above the active prompt."
  (when-let ((text (codex-ide--running-input-list-text session)))
    (let ((buffer (codex-ide-session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (codex-ide--without-undo-recording
           (let ((inhibit-read-only t)
                 render-state)
             (goto-char (point-max))
             (setq render-state
                   (codex-ide-renderer-insert-running-input-list text))
             (codex-ide--freeze-region
              (marker-position (plist-get render-state :delete-start))
              (marker-position (plist-get render-state :end)))
             (codex-ide--session-metadata-put
              session
              :running-input-list-delete-start-marker
              (plist-get render-state :delete-start))
             (codex-ide--session-metadata-put
              session
              :running-input-list-boundary-marker
              (plist-get render-state :boundary))
             (codex-ide--session-metadata-put
              session
              :running-input-list-end-marker
              (plist-get render-state :end)))))))))

(defun codex-ide--refresh-running-input-display (&optional session draft)
  "Refresh SESSION's running steer/queue list and editable prompt.
When DRAFT is nil, preserve the current active prompt text."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((text (or draft
                  (and (codex-ide--input-prompt-active-p session)
                       (codex-ide--current-input session)))))
    (codex-ide--delete-running-input-list session)
    (when (codex-ide--input-prompt-active-p session)
      (codex-ide--delete-active-input-prompt session))
    (codex-ide--insert-running-input-list session)
    (codex-ide--insert-input-prompt session text)))

(defun codex-ide--style-user-prompt-region (start end)
  "Apply prompt styling to the user prompt region from START to END."
  (codex-ide-renderer-style-user-prompt-region start end))

(defun codex-ide--format-compact-number (value)
  "Format numeric VALUE in a compact human-readable form."
  (cond
   ((not (numberp value)) "?")
   ((>= value 1000000)
    (format "%.1fM" (/ value 1000000.0)))
   ((>= value 1000)
    (format "%.1fk" (/ value 1000.0)))
   (t
    (number-to-string value))))

(defun codex-ide--format-token-usage-context-summary (token-usage)
  "Return the context portion of the header summary for TOKEN-USAGE."
  (when-let* ((total (alist-get 'total token-usage))
              (window (alist-get 'modelContextWindow token-usage))
              (used (alist-get 'totalTokens total)))
    (format "Context: %s/%s"
            (codex-ide--format-compact-number used)
            (codex-ide--format-compact-number window))))

(defun codex-ide--format-token-usage-last-summary (token-usage)
  "Return the last-usage portion of the header summary for TOKEN-USAGE."
  (when-let* ((total (alist-get 'total token-usage))
              (last (or (alist-get 'last token-usage) total)))
    (let ((last-input (alist-get 'inputTokens last))
          (last-cached (alist-get 'cachedInputTokens last))
          (last-output (alist-get 'outputTokens last))
          (last-reasoning (alist-get 'reasoningOutputTokens last)))
      (when (or (numberp last-input)
                (numberp last-cached)
                (numberp last-output)
                (numberp last-reasoning))
        (format "Last[in,cache,out,reason]: %s,%s,%s,%s"
                (if (numberp last-input)
                    (codex-ide--format-compact-number last-input)
                  "-")
                (if (numberp last-cached)
                    (codex-ide--format-compact-number last-cached)
                  "-")
                (if (numberp last-output)
                    (codex-ide--format-compact-number last-output)
                  "-")
                (if (numberp last-reasoning)
                    (codex-ide--format-compact-number last-reasoning)
                  "-"))))))

(defun codex-ide--format-model-summary (&optional session)
  "Return a compact header summary for SESSION's model."
  (let ((model (and session
                    (codex-ide--server-model-name session)))
        (effort (and session
                     (or (codex-ide--session-metadata-get session :reasoning-effort)
                         codex-ide-reasoning-effort))))
    (unless model
      (codex-ide--ensure-server-model-name session))
    (when model
      (format "Model: %s%s"
              model
              (if effort
                  (format " (%s)" effort)
                "")))))

(defun codex-ide--format-rate-limit-window-label (window)
  "Return a compact label for rate limit WINDOW."
  (when-let ((minutes (alist-get 'windowDurationMins window)))
    (cond
     ((= minutes 10080) "wk")
     ((and (> minutes 0) (= (mod minutes 1440) 0))
      (format "%sd" (/ minutes 1440)))
     ((and (> minutes 0) (= (mod minutes 60) 0))
      (format "%sh" (/ minutes 60)))
     (t
      (format "%sm" minutes)))))

(defun codex-ide--format-rate-limit-summary (rate-limits)
  "Return a compact header summary for RATE-LIMITS."
  (let* ((primary (alist-get 'primary rate-limits))
         (secondary (alist-get 'secondary rate-limits))
         (windows (delq nil
                        (list (and primary
                                   (let ((label (codex-ide--format-rate-limit-window-label primary))
                                         (percent (alist-get 'usedPercent primary)))
                                     (when (and label percent)
                                       (format "%%%%%s/%s" percent label))))
                              (and secondary
                                   (let ((label (codex-ide--format-rate-limit-window-label secondary))
                                         (percent (alist-get 'usedPercent secondary)))
                                     (when (and label percent)
                                       (format "%%%%%s/%s" percent label))))))))
    (when windows
      (format "Quota: %s%s"
              (string-join windows " ")
              (if-let ((plan-type (alist-get 'planType rate-limits)))
                  (format " (%s)" plan-type)
                "")))))

(defun codex-ide--update-header-line (&optional session)
  "Refresh the header line for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((buffer (codex-ide-session-buffer session)))
    (with-current-buffer buffer
      (let* ((context (with-current-buffer buffer
                        (codex-ide--get-active-buffer-context)))
             (focus (if context
                        (format "Focus: %s"
                                (alist-get 'buffer-name context))
                      "Focus: none"))
             (token-context-summary
              (codex-ide--format-token-usage-context-summary
               (codex-ide--session-metadata-get session :token-usage)))
             (token-last-summary
              (codex-ide--format-token-usage-last-summary
               (codex-ide--session-metadata-get session :token-usage)))
             (rate-limit-summary
              (codex-ide--format-rate-limit-summary
               (codex-ide--session-metadata-get session :rate-limits)))
             (model-summary
              (codex-ide--format-model-summary session)))
        (setq header-line-format
              (propertize
               (string-join
                (delq nil
                      (list
                       focus
                       model-summary
                       rate-limit-summary
                       token-context-summary
                       token-last-summary))
                " | ")
               'face 'codex-ide-header-line-face)))
      (codex-ide--update-mode-line session))))

(defun codex-ide--agent-message-render-end (buffer)
  "Return the end position for rendering the current agent message in BUFFER."
  (or (codex-ide--active-input-boundary-position buffer)
      (point-max)))

(defun codex-ide--maybe-render-markdown-region
    (start end &optional allow-trailing-tables)
  "Render markdown between START and END while preserving transcript markers."
  (let* ((active-boundary
          (codex-ide--active-input-boundary-marker (current-buffer)))
         (active-boundary-at-end
          (and active-boundary
               (= (marker-position active-boundary) end)))
         (render-end-marker
          (and active-boundary-at-end
               (copy-marker end t))))
    (unwind-protect
        (codex-ide--maybe-save-transcript-position end
						   (prog1
						       (codex-ide-renderer-maybe-render-markdown-region
							start end allow-trailing-tables)
						     (when render-end-marker
						       (set-marker active-boundary (marker-position render-end-marker)))))
      (when render-end-marker
        (set-marker render-end-marker nil)))))

(defun codex-ide--render-current-agent-message-markdown-streaming
    (&optional session item-id)
  "Incrementally render stream-safe markdown for SESSION's current message."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (or (null item-id)
                   (equal item-id
                          (codex-ide-session-current-message-item-id session))))
      (when-let ((message-start
                  (codex-ide-session-current-message-start-marker session)))
        (when (eq (marker-buffer message-start) buffer)
          (with-current-buffer buffer
            (let ((render-start-marker
                   (or (codex-ide--session-metadata-get
                        session
                        :agent-message-stream-render-start-marker)
                       (codex-ide--session-metadata-put
                        session
                        :agent-message-stream-render-start-marker
                        (copy-marker message-start)))))
              (let* ((message-end (codex-ide--agent-message-render-end buffer))
                     (active-boundary
                      (codex-ide--active-input-boundary-marker buffer))
                     (render-end-marker
                      (and active-boundary
                           (= (marker-position active-boundary) message-end)
                           (copy-marker message-end t))))
                (unwind-protect
                    (codex-ide-renderer-render-markdown-streaming
                     (marker-position message-start)
                     message-end
                     render-start-marker)
                  (when render-end-marker
                    (set-marker active-boundary
                                (marker-position render-end-marker))
                    (set-marker render-end-marker nil)))))))))))

(defun codex-ide--insert-input-prompt (&optional session initial-text)
  "Insert a writable `>' prompt for SESSION.
Optionally seed it with INITIAL-TEXT."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
         (let ((inhibit-read-only t)
               (moving (= (point) (point-max)))
               render-state)
           (goto-char (point-max))
           (setq render-state
                 (codex-ide-renderer-insert-input-prompt
                  initial-text
                  (or (codex-ide-session-current-turn-id session)
                      (codex-ide-session-output-prefix-inserted session))))
           (codex-ide--freeze-region
            (marker-position (plist-get render-state :transcript-start))
            (marker-position (plist-get render-state :prompt-start)))
           (codex-ide--delete-input-overlay session)
           (codex-ide--session-metadata-put
            session
            :active-input-boundary-marker
            (plist-get render-state :active-boundary))
           (setf (codex-ide-session-input-prompt-start-marker session)
                 (plist-get render-state :prompt-start))
           (setf (codex-ide-session-input-start-marker session)
                 (plist-get render-state :input-start))
           (codex-ide--reset-prompt-history-navigation session)
           (codex-ide--make-region-writable
            (marker-position (codex-ide-session-input-start-marker session))
            (point))
           (let ((overlay (make-overlay
                           (marker-position
                            (codex-ide-session-input-start-marker session))
                           (point-max)
                           buffer
                           t
                           t)))
             (overlay-put overlay 'face 'codex-ide-user-prompt-face)
             (overlay-put overlay 'field 'codex-ide-active-input)
             (setf (codex-ide-session-input-overlay session) overlay))
           (codex-ide--setup-input-placeholder-hooks)
           (codex-ide--refresh-input-placeholder session)
           (when moving
             (goto-char (point-max)))
           (codex-ide--sync-prompt-minor-mode session)))
        (codex-ide--discard-buffer-undo-history)))))

(defun codex-ide--freeze-active-input-prompt (&optional session context-summary)
  "Freeze SESSION's active input prompt as submitted transcript text.
When CONTEXT-SUMMARY is non-nil, insert it beneath the prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (unless (codex-ide--input-prompt-active-p session)
      (user-error "No editable Codex prompt in this buffer"))
    (with-current-buffer buffer
      (codex-ide--without-undo-recording
       (let ((inhibit-read-only t)
             context-start)
         (codex-ide--delete-running-input-list session)
         (when-let ((start (codex-ide-session-input-prompt-start-marker session)))
           (codex-ide--style-user-prompt-region start (point-max))
           (codex-ide--freeze-region start (point-max))
           (when context-summary
             (setq context-start (point-max))
             (goto-char context-start)
             (codex-ide-renderer-insert-context-summary context-summary)
             (codex-ide--freeze-region context-start (point)))))
       (codex-ide--delete-input-overlay session)
       (codex-ide--session-metadata-put session :active-input-boundary-marker nil)
       (codex-ide--sync-prompt-minor-mode session))
      (codex-ide--discard-buffer-undo-history))))

(defun codex-ide--input-prompt-active-p (&optional session)
  "Return non-nil when SESSION currently has an editable input prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (and session (codex-ide-session-buffer session)))
        (overlay (and session (codex-ide-session-input-overlay session)))
        (marker (and session (codex-ide-session-input-start-marker session))))
    (and (buffer-live-p buffer)
         (overlayp overlay)
         (eq (overlay-buffer overlay) buffer)
         (markerp marker)
         (eq (marker-buffer marker) buffer))))

(defun codex-ide--ensure-input-prompt (&optional session initial-text)
  "Insert an editable prompt for SESSION when one is not already active.
When INITIAL-TEXT is non-nil, seed a newly inserted prompt with it."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (when (and (string= (codex-ide-session-status session) "idle")
             (not (codex-ide-session-current-turn-id session))
             (not (codex-ide-session-output-prefix-inserted session))
             (not (codex-ide--input-prompt-active-p session)))
    (codex-ide--insert-input-prompt session initial-text)))

(defun codex-ide--current-input (&optional session)
  "Return the current editable input text for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session))
        (marker (codex-ide-session-input-start-marker session)))
    (unless (and (buffer-live-p buffer) marker)
      "")
    (with-current-buffer buffer
      (string-trim-right
       (buffer-substring-no-properties marker (point-max))))))

(defun codex-ide--replace-current-input (session text)
  "Replace SESSION's editable input region with TEXT."
  (let ((buffer (codex-ide-session-buffer session))
        (marker (codex-ide-session-input-start-marker session)))
    (unless (and (buffer-live-p buffer) marker)
      (user-error "No editable Codex prompt in this buffer"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char marker)
        (codex-ide-renderer-replace-region marker (point-max) text)
        (goto-char (point-max))
        (codex-ide--refresh-input-placeholder session)))))

(defun codex-ide--browse-prompt-history (direction)
  "Browse prompt history in DIRECTION for the current Codex session.
DIRECTION should be -1 for older history and 1 for newer history."
  (let* ((session (codex-ide--session-for-current-project))
         (history (or (codex-ide--project-persisted-get :prompt-history session)
                      '())))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (user-error "Prompt history is only available in the Codex session buffer"))
    (unless (codex-ide-session-input-start-marker session)
      (user-error "No editable Codex prompt in this buffer"))
    (unless history
      (user-error "No prompt history"))
    (let ((index (codex-ide-session-prompt-history-index session)))
      (when (null index)
        (setf (codex-ide-session-prompt-history-draft session)
              (codex-ide--current-input session)))
      (pcase direction
        (-1
         (cond
          ((null index)
           (setq index 0))
          ((>= index (1- (length history)))
           (user-error "End of prompt history"))
          (t
           (setq index (1+ index)))))
        (1
         (setq index (if (or (null index)
                             (<= index 0))
                         nil
                       (1- index)))))
      (if (null index)
          (progn
            (setf (codex-ide-session-prompt-history-index session) nil)
            (codex-ide--replace-current-input session ""))
        (setf (codex-ide-session-prompt-history-index session) index)
        (codex-ide--replace-current-input session (nth index history))))))

(defun codex-ide--goto-prompt-line (direction)
  "Move point to another user prompt line in DIRECTION.
DIRECTION should be -1 for a previous prompt line and 1 for a next prompt line."
  (let ((session (codex-ide--session-for-current-project)))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (user-error "Prompt-line navigation is only available in the Codex session buffer"))
    (beginning-of-line)
    (when (codex-ide--line-has-prompt-start-p)
      (forward-line direction))
    (let ((found nil))
      (while (and (not found)
                  (not (if (< direction 0) (bobp) (eobp))))
        (setq found (codex-ide--line-has-prompt-start-p))
        (unless found
          (forward-line direction)))
      (unless found
        (user-error (if (< direction 0)
                        "No earlier prompt line"
                      "No later prompt line")))
      (beginning-of-line)
      (cond
       ((when-let ((session (codex-ide--session-for-current-project))
                   (prompt-start (codex-ide-session-input-prompt-start-marker session))
                   (input-start (codex-ide-session-input-start-marker session)))
          (when (and (markerp prompt-start)
                     (markerp input-start)
                     (= (marker-position prompt-start) (point)))
            (goto-char input-start)
            t)))
       ((looking-at-p "> ")
        (forward-char 2))))))

(defun codex-ide--begin-turn-display (&optional session context-summary quiet)
  "Freeze the current prompt and show immediate pending output for SESSION.
When CONTEXT-SUMMARY is non-nil, insert it beneath the submitted prompt.
When QUIET is non-nil, do not refresh SESSION's header line."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
         (let ((inhibit-read-only t)
               context-start
               spacing-start)
           (codex-ide--delete-running-input-list session)
           (when-let ((start (codex-ide-session-input-prompt-start-marker session)))
             (codex-ide--style-user-prompt-region start (point-max))
             (codex-ide--freeze-region start (point-max))
             (when context-summary
               (setq context-start (point-max))
               (goto-char context-start)
               (codex-ide-renderer-insert-context-summary context-summary)
               (codex-ide--freeze-region context-start (point))))
           (codex-ide--delete-input-overlay session)
           (codex-ide--sync-prompt-minor-mode session)
           (when-let ((start (codex-ide-session-input-prompt-start-marker session)))
             (if-let ((turn-id (codex-ide-session-current-turn-id session)))
                 (codex-ide--record-turn-start session turn-id start)
               (codex-ide--set-pending-turn-start-marker
                session
                (copy-marker start nil))))
           (goto-char (point-max))
           (setq spacing-start (point))
           (setq spacing-start
                 (car (codex-ide-renderer-insert-read-only "\n\n")))
           (codex-ide--insert-pending-output-indicator session)
           (setf (codex-ide-session-output-prefix-inserted session) t
                 (codex-ide-session-status session) "running")
           (codex-ide--insert-input-prompt session)
           (unless quiet
             (codex-ide--update-header-line session))))
        (codex-ide--discard-buffer-undo-history)))))

(defun codex-ide--shell-command-string (command)
  "Render COMMAND as a shell-like string."
  (cond
   ((stringp command) command)
   ((or (listp command) (vectorp command))
    (mapconcat (lambda (arg)
                 (if (stringp arg)
                     (shell-quote-argument arg)
                   (format "%s" arg)))
               (append command nil)
               " "))
   (t (format "%s" command))))

(defun codex-ide--command-argv (command)
  "Return COMMAND as an argv list when it can be parsed that way."
  (cond
   ((or (listp command) (vectorp command))
    (mapcar (lambda (arg)
              (if (stringp arg)
                  arg
                (format "%s" arg)))
            (append command nil)))
   ((stringp command)
    (codex-ide--split-shell-words command))))

(defun codex-ide--split-shell-words (command)
  "Split COMMAND into shell-like words.
This handles the simple quoting shapes emitted by command execution items
without interpreting shell metacharacters inside quoted strings."
  (let ((index 0)
        (length (length command))
        quote
        escaping
        in-word
        (word "")
        words)
    (while (< index length)
      (let ((char (aref command index)))
        (cond
         (escaping
          (setq word (concat word (char-to-string char))
                in-word t
                escaping nil))
         ((and quote (eq quote ?\'))
          (if (eq char quote)
              (setq quote nil)
            (setq word (concat word (char-to-string char))
                  in-word t)))
         ((eq char ?\\)
          (setq escaping t
                in-word t))
         (quote
          (if (eq char quote)
              (setq quote nil)
            (setq word (concat word (char-to-string char))
                  in-word t)))
         ((or (eq char ?\') (eq char ?\"))
          (setq quote char
                in-word t))
         ((memq char '(?\s ?\t ?\n))
          (when in-word
            (push word words)
            (setq word ""
                  in-word nil)))
         ((eq char ?|)
          (when in-word
            (push word words)
            (setq word ""
                  in-word nil))
          (push "|" words))
         (t
          (setq word (concat word (char-to-string char))
                in-word t))))
      (setq index (1+ index)))
    (when (or quote escaping)
      (setq words nil
            in-word nil
            word ""))
    (when in-word
      (push word words))
    (nreverse words)))

(defun codex-ide--split-shell-pipeline (command)
  "Split COMMAND on unquoted shell pipeline separators."
  (let ((index 0)
        (length (length command))
        quote
        escaping
        (part-start 0)
        parts)
    (while (< index length)
      (let ((char (aref command index)))
        (cond
         (escaping
          (setq escaping nil))
         ((and quote (eq quote ?\'))
          (when (eq char quote)
            (setq quote nil)))
         ((eq char ?\\)
          (setq escaping t))
         (quote
          (when (eq char quote)
            (setq quote nil)))
         ((or (eq char ?\') (eq char ?\"))
          (setq quote char))
         ((eq char ?|)
          (push (string-trim (substring command part-start index)) parts)
          (setq part-start (1+ index)))))
      (setq index (1+ index)))
    (unless (or quote escaping)
      (push (string-trim (substring command part-start)) parts)
      (nreverse parts))))

(defun codex-ide--shell-wrapper-inner-command (argv)
  "Return the shell script from shell wrapper ARGV, or nil."
  (when (and (>= (length argv) 3)
             (member (file-name-nondirectory (car argv))
                     '("bash" "sh" "zsh"))
             (member (cadr argv) '("-c" "-lc")))
    (nth 2 argv)))

(defun codex-ide--display-command-string (command)
  "Return the user-facing shell command string for COMMAND."
  (or (when-let* ((argv (codex-ide--command-argv command))
                  (inner (codex-ide--shell-wrapper-inner-command argv)))
        inner)
      (codex-ide--shell-command-string command)))

(defun codex-ide--display-command-argv (command)
  "Return argv for COMMAND after removing common shell wrappers."
  (let ((display-command (codex-ide--display-command-string command)))
    (or (codex-ide--split-shell-words display-command)
        (codex-ide--command-argv command))))

(defun codex-ide--sed-print-request (argv)
  "Parse a simple `sed -n' print request from ARGV.
Return (START END FILES), or nil when ARGV does not describe one."
  (when (and (consp argv)
             (string= (file-name-nondirectory (car argv)) "sed"))
    (let ((args (cdr argv))
          quiet
          script
          files
          unsupported)
      (while args
        (let ((arg (pop args)))
          (cond
           ((member arg '("-n" "--quiet" "--silent"))
            (setq quiet t))
           ((string= arg "-e")
            (if (or script (null args))
                (setq unsupported t)
              (setq script (pop args))))
           ((and (string-prefix-p "-e" arg)
                 (> (length arg) 2))
            (if script
                (setq unsupported t)
              (setq script (substring arg 2))))
           ((string-prefix-p "-" arg)
            (setq unsupported t))
           ((not script)
            (setq script arg))
           (t
            (push arg files)))))
      (when (and quiet
                 (not unsupported)
                 (stringp script)
                 (string-match "\\`[[:space:]]*\\([0-9]+\\)\\(?:,[[:space:]]*\\([0-9]+\\)\\)?p[[:space:]]*\\'"
                               script))
        (list (string-to-number (match-string 1 script))
              (if-let ((end (match-string 2 script)))
                  (string-to-number end)
                (string-to-number (match-string 1 script)))
              (nreverse files))))))

(defun codex-ide--nl-command-file (argv)
  "Return the file read by a simple `nl' command ARGV, or nil."
  (when (and (consp argv)
             (string= (file-name-nondirectory (car argv)) "nl"))
    (car (last (cl-remove-if (lambda (arg)
                               (string-prefix-p "-" arg))
                             (cdr argv))))))

(defun codex-ide--read-lines-summary (file start end)
  "Format a summary for reading FILE between START and END."
  (if (= start end)
      (format "Read %s (line %d)" file start)
    (format "Read %s (lines %d to %d)" file start end)))

(defun codex-ide--command-read-summary (command)
  "Return a semantic read summary for COMMAND, or nil."
  (let* ((display-command (codex-ide--display-command-string command))
         (argv (codex-ide--display-command-argv command))
         (sed-request (codex-ide--sed-print-request argv)))
    (cond
     ((and sed-request
           (= (length (nth 2 sed-request)) 1))
      (codex-ide--read-lines-summary
       (car (nth 2 sed-request))
       (nth 0 sed-request)
       (nth 1 sed-request)))
     ((and (stringp display-command)
           (string-match-p "|" display-command))
      (let ((parts (codex-ide--split-shell-pipeline display-command)))
        (when (= (length parts) 2)
          (let* ((left (codex-ide--split-shell-words (car parts)))
                 (right (codex-ide--split-shell-words (cadr parts)))
                 (file (codex-ide--nl-command-file left))
                 (request (codex-ide--sed-print-request right)))
            (when (and file request (null (nth 2 request)))
              (codex-ide--read-lines-summary
               file
               (nth 0 request)
               (nth 1 request))))))))))

(defconst codex-ide--rg-options-with-values
  '("-A" "-B" "-C" "-E" "-M" "-e" "-f" "-g" "-m" "-t" "-T"
    "--after-context" "--before-context" "--colors" "--context"
    "--context-separator" "--encoding" "--engine" "--field-context-separator"
    "--field-match-separator" "--file" "--files-from" "--glob"
    "--glob-case-insensitive" "--iglob"
    "--max-columns" "--max-count" "--max-depth" "--max-filesize"
    "--path-separator"
    "--pre" "--pre-glob" "--regexp" "--replace" "--sort"
    "--threads" "--type" "--type-add" "--type-clear" "--type-not")
  "Ripgrep options that consume the following argv element.")

(defun codex-ide--rg-search-request (argv)
  "Parse a simple ripgrep search from ARGV.
Return (PATTERN PATHS), or nil when ARGV does not describe a search."
  (when (and (consp argv)
             (member (file-name-nondirectory (car argv)) '("rg" "ripgrep")))
    (let ((args (cdr argv))
          pattern
          paths
          literal-args)
      (while args
        (let ((arg (pop args)))
          (cond
           (literal-args
            (if pattern
                (push arg paths)
              (setq pattern arg)))
           ((string= arg "--")
            (setq literal-args t))
           ((or (string= arg "-e")
                (string= arg "--regexp"))
            (when args
              (setq pattern (pop args))))
           ((string-prefix-p "--regexp=" arg)
            (setq pattern (substring arg (length "--regexp="))))
           ((member arg codex-ide--rg-options-with-values)
            (when args
              (pop args)))
           ((and (string-prefix-p "--" arg)
                 (string-match-p "=" arg)))
           ((string-prefix-p "-" arg))
           ((not pattern)
            (setq pattern arg))
           (t
            (push arg paths)))))
      (when (and (stringp pattern)
                 (not (string-empty-p pattern)))
        (list pattern (nreverse paths))))))

(defun codex-ide--search-summary (pattern paths)
  "Format a semantic search summary for PATTERN across PATHS."
  (format "Searched %s for %s"
          (codex-ide--search-locations-summary paths)
          (codex-ide--quote-summary-string pattern)))

(defun codex-ide--quote-summary-string (value)
  "Return VALUE quoted for a summary line."
  (format "\"%s\""
          (replace-regexp-in-string "\"" "\\\\\"" (or value "") t t)))

(defun codex-ide--search-locations-summary (paths)
  "Return a human-readable location summary for PATHS."
  (let ((paths (mapcar (lambda (path)
                         (if (string= path ".")
                             "current directory"
                           path))
                       paths)))
    (cond
     ((null paths) "current directory")
     ((null (cdr paths)) (car paths))
     ((<= (length paths) 3)
      (concat (string-join (butlast paths) ", ")
              " and "
              (car (last paths))))
     (t
      (format "%d locations" (length paths))))))

(defun codex-ide--count-search-output-hits (output)
  "Return a best-effort ripgrep hit count from OUTPUT."
  (when (stringp output)
    (let* ((lines (seq-filter
                   (lambda (line) (not (string-empty-p line)))
                   (split-string output "\n")))
           (numbered-lines
            (seq-filter
             (lambda (line)
               (string-match-p "\\(?:\\`\\|:\\)[0-9]+:" line))
             lines)))
      (length (or numbered-lines lines)))))

(defun codex-ide--format-hit-count (count)
  "Return a short summary for COUNT search hits."
  (format "found %d hit%s" count (if (= count 1) "" "s")))

(defun codex-ide--command-output-trimmed-end (output)
  "Return the end index of OUTPUT after trimming trailing whitespace."
  (let ((end (length output)))
    (while (and (> end 0)
                (memq (aref output (1- end))
                      '(?\s ?\t ?\n ?\r ?\f ?\v)))
      (setq end (1- end)))
    end))

(defun codex-ide--command-output-count-newlines (output end)
  "Return the number of newline characters in OUTPUT before END."
  (let ((count 0)
        (pos 0))
    (while (< pos end)
      (when (= (aref output pos) ?\n)
        (setq count (1+ count)))
      (setq pos (1+ pos)))
    count))

(defun codex-ide--command-output-line-count (output)
  "Return the display line count for command OUTPUT."
  (cond
   ((or (null output) (string-empty-p output)) 0)
   ((= (codex-ide--command-output-trimmed-end output) 0) 1)
   (t
    (1+ (codex-ide--command-output-count-newlines
         output
         (codex-ide--command-output-trimmed-end output))))))

(defun codex-ide--command-output-start-after-lines (output line-count)
  "Return the index after LINE-COUNT newline-terminated lines in OUTPUT."
  (let ((len (length output))
        (seen 0)
        (pos 0))
    (while (and (< pos len)
                (< seen line-count))
      (when (= (aref output pos) ?\n)
        (setq seen (1+ seen)))
      (setq pos (1+ pos)))
    pos))

(defun codex-ide--command-output-render-range (output)
  "Return the raw OUTPUT range to render into the transcript as (START . END)."
  (let ((start 0)
        (end (length output)))
    (when (integerp codex-ide-renderer-command-output-max-rendered-lines)
      (let* ((line-count (codex-ide--command-output-line-count output))
             (hidden-lines
              (max 0
                   (- line-count
                      (max 0 codex-ide-renderer-command-output-max-rendered-lines)))))
        (setq start
              (max start
                   (codex-ide--command-output-start-after-lines
                    output
                    hidden-lines)))))
    (when (integerp codex-ide-renderer-command-output-max-rendered-chars)
      (setq start
            (max start
                 (- end
                    (max 0 codex-ide-renderer-command-output-max-rendered-chars)))))
    (cons (min start end) end)))

(defun codex-ide--command-output-truncation-notice ()
  "Return the transcript notice inserted after truncated command output."
  "    ... transcript output truncated; showing latest output.\n")

(defun codex-ide--format-command-output-text (output &optional truncated)
  "Return prefixed display text for raw command OUTPUT."
  (when (and (stringp output) (not (string-empty-p output)))
    (let* ((ends-with-newline (string-suffix-p "\n" output))
           (body (if ends-with-newline
                     (substring output 0 -1)
                   output))
           (lines (split-string body "\n")))
      (concat
       (when truncated
         (codex-ide--command-output-truncation-notice))
       (mapconcat (lambda (line) (concat "    " line)) lines "\n")
       (if ends-with-newline "\n" "")))))

(defun codex-ide--json-encode-string-or-nil (value)
  "Return VALUE JSON-encoded when possible, or nil on encoding failure."
  (when value
    (condition-case nil
        (json-encode value)
      (error nil))))

(defun codex-ide--mcp-result-text (item)
  "Return a transcript-ready result string for MCP tool call ITEM."
  (let ((result
         (or (alist-get 'result item)
             (alist-get 'output item)
             (alist-get 'content item)
             (alist-get 'response item))))
    (cond
     ((stringp result) result)
     ((null result) nil)
     ((vectorp result)
      (or (let ((parts nil))
            (mapc
             (lambda (entry)
               (let ((text (alist-get 'text entry)))
                 (when (and (stringp text)
                            (not (string-empty-p text)))
                   (push text parts))))
             result)
            (when parts
              (string-join (nreverse parts) "\n\n")))
          (codex-ide--json-encode-string-or-nil result)))
     ((listp result)
      (or (alist-get 'text result)
          (codex-ide--json-encode-string-or-nil result)))
     (t
      (codex-ide--json-encode-string-or-nil result)))))

(defun codex-ide--item-result-header-prefix-text (overlay)
  "Return the non-action header text for item result OVERLAY."
  (let* ((line-count (overlay-get overlay :line-count))
         (visible-line-count (overlay-get overlay :visible-line-count))
         (truncated (overlay-get overlay :truncated))
         (line-label (if (= line-count 1) "line" "lines"))
         (complete (overlay-get overlay :complete))
         (label (or (overlay-get overlay :label) "output")))
    (format "  └ %s: %d %s%s%s "
            label
            line-count
            line-label
            (if truncated
                (format ", showing last %d" visible-line-count)
              "")
            (if complete "" ", streaming"))))

(defun codex-ide--item-result-open-function (overlay)
  "Return the open function for item result OVERLAY."
  (or (overlay-get overlay :open-function)
      #'codex-ide--open-item-result-overlay))

(defun codex-ide--item-result-text (overlay)
  "Return the full result text for item result OVERLAY."
  (let* ((session (overlay-get overlay :session))
         (item-id (overlay-get overlay :item-id))
         (state (and session item-id
                     (codex-ide--item-state session item-id))))
    (or (plist-get state :item-result-text)
        (plist-get state :output-text)
        (overlay-get overlay :item-result-fallback-text)
        (overlay-get overlay :output-fallback-text)
        "")))

(defun codex-ide--item-result-buffer-name (overlay)
  "Return the buffer name for full item result OVERLAY."
  (let* ((session (overlay-get overlay :session))
         (item-id (overlay-get overlay :item-id))
         (item-type (overlay-get overlay :item-type))
         (directory (and session (codex-ide-session-directory session)))
         (project (and directory
                       (file-name-nondirectory
                        (directory-file-name directory)))))
    (format (if (equal item-type "commandExecution")
                "*codex-output[%s:%s]*"
              "*codex-item-result[%s:%s]*")
            (or project "session")
            (or item-id "item"))))

(defun codex-ide--item-result-header-text (overlay)
  "Return the optional header text for full item result OVERLAY."
  (let* ((session (overlay-get overlay :session))
         (item-id (overlay-get overlay :item-id))
         (state (and session item-id
                     (codex-ide--item-state session item-id)))
         (item (plist-get state :item))
         (item-type (and (listp item) (alist-get 'type item))))
    (pcase item-type
      ("commandExecution"
       (when-let ((command (alist-get 'command item)))
         (concat "$ " (codex-ide--display-command-string command))))
      ("mcpToolCall"
       (format "%s/%s"
               (or (alist-get 'server item) "mcp")
               (or (alist-get 'tool item) "tool")))
      (_ nil))))

(defun codex-ide--open-item-result-overlay (overlay)
  "Open full item result for OVERLAY in a separate buffer."
  (unless (overlayp overlay)
    (user-error "No item result at point"))
  (let* ((output (codex-ide--item-result-text overlay))
         (header (codex-ide--item-result-header-text overlay))
         (buffer (get-buffer-create
                  (codex-ide--item-result-buffer-name overlay))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when header
          (insert header "\n\n"))
        (insert output)
        (unless (or (string-empty-p output)
                    (string-suffix-p "\n" output))
          (insert "\n"))
        (goto-char (point-min))
        (special-mode)
        (setq-local buffer-undo-list t)
        (when (bound-and-true-p visual-line-mode)
          (visual-line-mode -1))
        (when (bound-and-true-p font-lock-mode)
          (font-lock-mode -1))))
    (pop-to-buffer buffer)))

(defun codex-ide--toggle-item-result-overlay (overlay)
  "Toggle item result OVERLAY.
Return non-nil when OVERLAY was toggled."
  (when (and (overlayp overlay)
             (buffer-live-p (overlay-buffer overlay)))
    (let ((folded (not (overlay-get overlay :folded)))
          (codex-ide--preserve-transcript-window-follow-anchor nil))
      (overlay-put overlay :folded folded)
      (overlay-put overlay 'invisible (and folded t))
      (codex-ide--set-item-result-header overlay)
      (codex-ide--set-item-result-body
       overlay
       (or (overlay-get overlay :display-text) ""))
      t)))

(defun codex-ide--command-output-header-prefix-text (overlay)
  "Return the non-action header text for command output OVERLAY."
  (codex-ide--item-result-header-prefix-text overlay))

(defun codex-ide--command-output-text (overlay)
  "Return the full output text for command output OVERLAY."
  (codex-ide--item-result-text overlay))

(defun codex-ide--command-output-buffer-name (overlay)
  "Return the buffer name for full command output OVERLAY."
  (codex-ide--item-result-buffer-name overlay))

(defun codex-ide--command-output-command-text (overlay)
  "Return the display command associated with command output OVERLAY."
  (let* ((session (overlay-get overlay :session))
         (item-id (overlay-get overlay :item-id))
         (state (and session item-id
                     (codex-ide--item-state session item-id)))
         (item (plist-get state :item))
         (command (and (listp item) (alist-get 'command item))))
    (and command
         (codex-ide--display-command-string command))))

(defun codex-ide--open-command-output-overlay (overlay)
  "Open full command output for OVERLAY in a separate buffer."
  (codex-ide--open-item-result-overlay overlay))

(defun codex-ide--toggle-command-output-overlay (overlay)
  "Toggle command output OVERLAY.
Return non-nil when OVERLAY was toggled."
  (codex-ide--toggle-item-result-overlay overlay))

(defun codex-ide-open-item-result-at-point (&optional pos)
  "Open full item result for the result block at POS.
Return non-nil when an item result block was found."
  (interactive)
  (if-let ((overlay (codex-ide--item-result-overlay-at-point pos)))
      (progn
        (funcall (codex-ide--item-result-open-function overlay) overlay)
        t)
    (user-error "No item result at point")))

(defun codex-ide-open-command-output-at-point (&optional pos)
  "Open full command output for the output block at POS.
Return non-nil when a command output block was found."
  (interactive)
  (codex-ide-open-item-result-at-point pos))

(defun codex-ide--set-item-result-header (overlay)
  "Refresh the visible header for item result OVERLAY."
  (let ((buffer (overlay-buffer overlay))
        (header-start (overlay-get overlay :header-start))
        (header-end (overlay-get overlay :header-end))
        (body-start (overlay-get overlay :body-start))
        (body-end (overlay-get overlay :body-end)))
    (when (and (buffer-live-p buffer)
               (markerp header-start)
               (markerp header-end)
               (markerp body-start)
               (markerp body-end))
      (with-current-buffer buffer
        (codex-ide--maybe-save-transcript-position (marker-position header-start)
						   (codex-ide--without-undo-recording
						    (let ((inhibit-read-only t)
							  (restore-point (codex-ide--input-point-marker
									  (codex-ide--session-for-buffer buffer)))
							  (moving (= (point) (point-max)))
							  (body-empty (= (marker-position body-start)
									 (marker-position body-end)))
							  (start (marker-position header-start))
							  (header-prefix-function
							   (or (overlay-get overlay :header-prefix-function)
							       #'codex-ide--item-result-header-prefix-text))
							  (open-function (codex-ide--item-result-open-function overlay))
							  (command-output-p
							   (equal (overlay-get overlay :item-type) "commandExecution")))
						      (goto-char start)
						      (delete-region start (marker-position header-end))
						      (if command-output-p
							  (codex-ide-renderer-insert-command-output-header
							   overlay
							   (funcall header-prefix-function overlay)
							   #'codex-ide--toggle-item-result-overlay
							   open-function
							   :keymap codex-ide-item-result-map
							   :overlay-property codex-ide-item-result-overlay-property)
							(codex-ide-renderer-insert-item-result-header
							 overlay
							 (funcall header-prefix-function overlay)
							 #'codex-ide--toggle-item-result-overlay
							 open-function
							 :keymap codex-ide-item-result-map
							 :overlay-property codex-ide-item-result-overlay-property
							 :toggle-help-echo (overlay-get overlay :toggle-help-echo)
							 :toggle-button-help (overlay-get overlay :toggle-button-help)
							 :open-button-label (overlay-get overlay :open-button-label)
							 :open-button-help (overlay-get overlay :open-button-help)
							 :open-button-keymap (codex-ide-nav-button-keymap)))
						      (set-marker header-start start)
						      (set-marker header-end (point))
						      (set-marker body-start (point))
						      (when body-empty
							(set-marker body-end (point)))
						      (move-overlay overlay
								    (marker-position body-start)
								    (marker-position body-end))
						      (codex-ide--advance-active-boundary-after buffer body-end)
						      (codex-ide--freeze-region (marker-position header-start)
										(marker-position header-end))
						      (if restore-point
							  (codex-ide--restore-input-point-marker restore-point)
							(when moving
							  (goto-char (point-max)))))))))))

(defun codex-ide--set-command-output-header (overlay)
  "Refresh the visible header for command output OVERLAY."
  (codex-ide--set-item-result-header overlay))

(defun codex-ide--set-item-result-body (overlay display-text)
  "Refresh OVERLAY's visible body using DISPLAY-TEXT.
When OVERLAY is folded, remove the body text from the transcript buffer."
  (let ((buffer (overlay-buffer overlay))
        (body-start (overlay-get overlay :body-start))
        (body-end (overlay-get overlay :body-end)))
    (when (and (buffer-live-p buffer)
               (markerp body-start)
               (markerp body-end))
      (with-current-buffer buffer
        (let ((codex-ide--current-agent-item-type
               (or (overlay-get overlay :item-type)
                   "commandExecution")))
          (codex-ide--maybe-save-transcript-position (marker-position body-start)
						     (codex-ide--without-undo-recording
						      (let ((inhibit-read-only t)
							    (restore-point (codex-ide--input-point-marker
									    (codex-ide--session-for-buffer buffer)))
							    (moving (= (point) (point-max)))
							    (body-insert-function
							     (or (overlay-get overlay :body-insert-function)
								 #'codex-ide-renderer-insert-item-result-body))
							    start)
							(delete-region (marker-position body-start)
								       (marker-position body-end))
							(goto-char (marker-position body-start))
							(setq start (point))
							(unless (overlay-get overlay :folded)
							  (funcall body-insert-function
								   display-text
								   :keymap codex-ide-item-result-map
								   :overlay overlay
								   :overlay-property codex-ide-item-result-overlay-property
								   :properties (overlay-get overlay :body-properties))
							  (codex-ide--freeze-region start (point)))
							(set-marker body-end (point))
							(move-overlay overlay
								      (marker-position body-start)
								      (marker-position body-end))
							(codex-ide--advance-active-boundary-after buffer body-end)
							(if restore-point
							    (codex-ide--restore-input-point-marker restore-point)
							  (when moving
							    (goto-char (point-max))))))))))))

(defun codex-ide--set-command-output-body (overlay display-text)
  "Refresh OVERLAY's visible body using DISPLAY-TEXT.
When OVERLAY is folded, remove the body text from the transcript buffer."
  (codex-ide--set-item-result-body overlay display-text))

(defun codex-ide--ensure-item-result-block (session item-id)
  "Return the item result overlay for ITEM-ID in SESSION, creating it."
  (let* ((state (codex-ide--item-state session item-id))
         (existing (or (plist-get state :item-result-overlay)
                       (plist-get state :command-output-overlay))))
    (if (and (overlayp existing) (buffer-live-p (overlay-buffer existing)))
        existing
      (let ((buffer (codex-ide-session-buffer session))
            overlay)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((codex-ide--current-agent-item-type
                   (or (plist-get state :type) "commandExecution")))
              (codex-ide--without-undo-recording
               (let* ((inhibit-read-only t)
                      (restore-point (codex-ide--input-point-marker session))
                      (moving (and (= (point) (point-max)) (not restore-point)))
                      (anchor (or (plist-get state :item-result-anchor-marker)
                                  (plist-get state :command-output-anchor-marker)))
                      (active-boundary (codex-ide--active-input-boundary-marker buffer))
                      (insertion-position
                       (if (and (markerp anchor)
                                (eq (marker-buffer anchor) buffer))
                           (marker-position anchor)
                         (codex-ide--transcript-insertion-position buffer)))
                      (advance-active-boundary
                       (and active-boundary
                            (= insertion-position (marker-position active-boundary))))
                      (initial-folded
                       (if (plist-member state :item-result-initial-folded)
                           (plist-get state :item-result-initial-folded)
                         codex-ide-renderer-command-output-fold-on-start))
                      header-start
                      header-end
                      body-start
                      body-end
                      (command-output-p (equal (plist-get state :type)
                                               "commandExecution")))
                 (codex-ide--maybe-save-transcript-position insertion-position
							    (goto-char insertion-position)
							    (setq header-start (copy-marker (point)))
							    (setq overlay (make-overlay (point) (point) buffer nil nil))
							    (overlay-put overlay 'face 'codex-ide-command-output-face)
							    (overlay-put overlay codex-ide-item-result-overlay-property overlay)
							    (overlay-put overlay codex-ide-command-output-overlay-property overlay)
							    (overlay-put overlay :session session)
							    (overlay-put overlay :item-id item-id)
							    (overlay-put overlay :item-type (plist-get state :type))
							    (overlay-put overlay :label (or (plist-get state :item-result-label)
											    "output"))
							    (overlay-put overlay :header-prefix-function
									 (plist-get state :item-result-header-prefix-function))
							    (overlay-put overlay :body-insert-function
									 (plist-get state :item-result-body-insert-function))
							    (overlay-put overlay :open-function
									 (plist-get state :item-result-open-function))
							    (overlay-put overlay :open-button-label
									 (plist-get state :item-result-open-button-label))
							    (overlay-put overlay :open-button-help
									 (plist-get state :item-result-open-button-help))
							    (overlay-put overlay :toggle-help-echo
									 (plist-get state :item-result-toggle-help-echo))
							    (overlay-put overlay :toggle-button-help
									 (plist-get state :item-result-toggle-button-help))
							    (overlay-put overlay :buffer-name
									 (plist-get state :item-result-buffer-name))
							    (overlay-put overlay :diff-stats
									 (plist-get state :item-result-stats))
							    (overlay-put overlay :header-start header-start)
							    (overlay-put overlay :display-text "")
							    (overlay-put overlay :line-count 0)
							    (overlay-put overlay :visible-line-count 0)
							    (overlay-put overlay :truncated nil)
							    (overlay-put overlay :folded initial-folded)
							    (overlay-put overlay :complete nil)
							    (overlay-put overlay 'invisible (and initial-folded t))
							    (overlay-put overlay :body-properties nil)
							    (if command-output-p
								(codex-ide-renderer-insert-command-output-header
								 overlay
								 (funcall
								  (or (overlay-get overlay :header-prefix-function)
								      #'codex-ide--item-result-header-prefix-text)
								  overlay)
								 #'codex-ide--toggle-item-result-overlay
								 (codex-ide--item-result-open-function overlay)
								 :keymap codex-ide-item-result-map
								 :overlay-property codex-ide-item-result-overlay-property)
							      (codex-ide-renderer-insert-item-result-header
							       overlay
							       (funcall
								(or (overlay-get overlay :header-prefix-function)
								    #'codex-ide--item-result-header-prefix-text)
								overlay)
							       #'codex-ide--toggle-item-result-overlay
							       (codex-ide--item-result-open-function overlay)
							       :keymap codex-ide-item-result-map
							       :overlay-property codex-ide-item-result-overlay-property
							       :toggle-help-echo (overlay-get overlay :toggle-help-echo)
							       :toggle-button-help (overlay-get overlay :toggle-button-help)
							       :open-button-label (overlay-get overlay :open-button-label)
							       :open-button-help (overlay-get overlay :open-button-help)))
							    (setq header-end (copy-marker (point)))
							    (setq body-start (copy-marker (point)))
							    (setq body-end (copy-marker (point)))
							    (overlay-put overlay :header-end header-end)
							    (overlay-put overlay :body-start body-start)
							    (overlay-put overlay :body-end body-end)
							    (codex-ide--freeze-region (marker-position header-start)
										      (marker-position header-end))
							    (codex-ide--advance-append-boundary-after
							     buffer
							     insertion-position
							     (point))
							    (when advance-active-boundary
							      (set-marker active-boundary (point)))
							    (when (markerp anchor)
							      (set-marker anchor nil))
							    (cond
							     (restore-point
							      (codex-ide--restore-input-point-marker restore-point))
							     (moving
							      (goto-char (point-max)))))))))
          (setq state (plist-put state :item-result-overlay overlay))
          (setq state (plist-put state :item-result-anchor-marker nil))
          (when (equal (plist-get state :type) "commandExecution")
            (setq state (plist-put state :command-output-overlay overlay))
            (setq state (plist-put state :command-output-anchor-marker nil)))
          (codex-ide--put-item-state session item-id state)
          overlay)))))

(defun codex-ide--ensure-command-output-block (session item-id)
  "Return the command output overlay for ITEM-ID in SESSION, creating it."
  (codex-ide--ensure-item-result-block session item-id))

(defun codex-ide--append-item-result-text (session item-id text)
  "Append item result TEXT for ITEM-ID in SESSION."
  (when (and (stringp text) (not (string-empty-p text)))
    (when-let ((overlay (codex-ide--ensure-item-result-block session item-id)))
      (let* ((state (codex-ide--item-state session item-id))
             (state-output-text
              (or (plist-get state :item-result-text)
                  (plist-get state :output-text)))
             (previous (or (overlay-get overlay :item-result-fallback-text)
                           (overlay-get overlay :output-fallback-text)
                           ""))
             output-text
             visible-range
             visible-output
             display-text
             truncated)
        (setq output-text (or state-output-text (concat previous text))
              visible-range
              (codex-ide--command-output-render-range output-text)
              visible-output
              (substring output-text
                         (car visible-range)
                         (cdr visible-range))
              truncated (> (car visible-range) 0)
              display-text
              (or (codex-ide--format-command-output-text
                   visible-output
                   truncated)
                  (and truncated
                       (codex-ide--command-output-truncation-notice))
                  ""))
        (overlay-put overlay :item-result-fallback-text output-text)
        (when (equal (plist-get state :type) "commandExecution")
          (overlay-put overlay :output-fallback-text output-text))
        (let* ((line-count (codex-ide--command-output-line-count output-text))
               (visible-line-count
                (codex-ide--command-output-line-count visible-output)))
          (overlay-put overlay :line-count line-count)
          (overlay-put overlay :visible-line-count
                       (if truncated
                           (min visible-line-count line-count)
                         line-count))
          (overlay-put overlay :truncated truncated))
        (when (not (equal display-text
                          (overlay-get overlay :display-text)))
          (overlay-put overlay :display-text display-text)
          (overlay-put overlay
                       :body-properties
                       (codex-ide--current-agent-text-properties)))
        (codex-ide--set-item-result-header overlay)
        (codex-ide--set-item-result-body
         overlay
         (or (overlay-get overlay :display-text) ""))
        (codex-ide--ensure-active-input-prompt-spacing session)))))

(defun codex-ide--append-command-output-text (session item-id text)
  "Append command output TEXT for ITEM-ID in SESSION."
  (codex-ide--append-item-result-text session item-id text))

(defun codex-ide--render-command-output-delta (session item-id delta)
  "Render streamed command output DELTA for ITEM-ID in SESSION."
  (codex-ide--append-command-output-text session item-id delta))

(defun codex-ide--complete-item-result-block (session item-id output)
  "Ensure item result for ITEM-ID is rendered and folded after completion."
  (let* ((state (codex-ide--item-state session item-id))
         (overlay (or (plist-get state :item-result-overlay)
                      (plist-get state :command-output-overlay))))
    (when (and (stringp output)
               (not (string-empty-p output))
               (not (and (overlayp overlay)
                         (buffer-live-p (overlay-buffer overlay)))))
      (codex-ide--append-item-result-text session item-id output)
      (setq state (codex-ide--item-state session item-id)
            overlay (or (plist-get state :item-result-overlay)
                        (plist-get state :command-output-overlay))))
    (when (and (overlayp overlay)
               (buffer-live-p (overlay-buffer overlay)))
      (overlay-put overlay :complete t)
      (overlay-put overlay :folded t)
      (overlay-put overlay 'invisible t)
      (codex-ide--set-item-result-header overlay)
      (codex-ide--set-item-result-body
       overlay
       (or (overlay-get overlay :display-text) "")))))

(defun codex-ide--complete-command-output-block (session item-id output)
  "Ensure command output for ITEM-ID is rendered and folded after completion."
  (codex-ide--complete-item-result-block session item-id output))

(defun codex-ide--item-result-overlay-at-point (&optional pos)
  "Return the item result overlay at POS, or nil."
  (let* ((pos (or pos (point)))
         (overlay (get-char-property pos codex-ide-item-result-overlay-property)))
    (cond
     ((overlayp overlay) overlay)
     ((and (> pos (point-min))
           (overlayp (get-char-property
                      (1- pos)
                      codex-ide-item-result-overlay-property)))
      (get-char-property (1- pos) codex-ide-item-result-overlay-property))
     (t nil))))

(defun codex-ide--command-output-overlay-at-point (&optional pos)
  "Return the command output overlay at POS, or nil."
  (codex-ide--item-result-overlay-at-point pos))

(defun codex-ide-toggle-item-result-at-point (&optional pos)
  "Toggle an item result block at POS.
Return non-nil when an item result block was found."
  (interactive)
  (when-let ((overlay (codex-ide--item-result-overlay-at-point pos)))
    (codex-ide--toggle-item-result-overlay overlay)))

(defun codex-ide-toggle-command-output-at-point (&optional pos)
  "Toggle a command output block at POS.
Return non-nil when a command output block was found."
  (interactive)
  (codex-ide-toggle-item-result-at-point pos))

(defun codex-ide--command-search-summary (command)
  "Return a semantic search summary for COMMAND, or nil."
  (when-let ((request (codex-ide--rg-search-request
                       (codex-ide--display-command-argv command))))
    (codex-ide--search-summary (car request) (cadr request))))

(defun codex-ide--command-summary (command)
  "Return the user-facing summary for shell COMMAND."
  (or (codex-ide--command-read-summary command)
      (codex-ide--command-search-summary command)
      "Ran command"))

(defun codex-ide--item-detail-line (text)
  "Format TEXT as an indented detail line."
  (format "  └ %s\n" text))

(defun codex-ide--web-search-action-value (key &rest items)
  "Return the first non-nil web search action KEY found in ITEMS."
  (seq-some
   (lambda (item)
     (when-let ((action (and (listp item) (alist-get 'action item))))
       (alist-get key action)))
   items))

(defun codex-ide--web-search-queries (&rest items)
  "Return a de-duplicated list of non-empty web search queries from ITEMS."
  (cl-remove-duplicates
   (delq nil
         (apply
          #'append
          (mapcar
           (lambda (item)
             (when (listp item)
               (let ((action (alist-get 'action item)))
                 (append (and (stringp (alist-get 'query action))
                              (not (string-empty-p (alist-get 'query action)))
                              (list (alist-get 'query action)))
                         (seq-filter
                          (lambda (query)
                            (and (stringp query)
                                 (not (string-empty-p query))))
                          (alist-get 'queries action))
                         (and (stringp (alist-get 'query item))
                              (not (string-empty-p (alist-get 'query item)))
                              (list (alist-get 'query item)))))))
           items)))
   :test #'equal))

(defun codex-ide--render-web-search-details (session item &optional fallback-item force-query-lines)
  "Render transcript detail lines for web search ITEM in SESSION.
FALLBACK-ITEM provides fields missing from ITEM.  When FORCE-QUERY-LINES is
non-nil, render query detail lines even when only a single query is present."
  (let* ((buffer (codex-ide-session-buffer session))
         (action-type (or (codex-ide--web-search-action-value 'type item)
                          (codex-ide--web-search-action-value 'type fallback-item)))
         (pattern (or (codex-ide--web-search-action-value 'pattern item)
                      (codex-ide--web-search-action-value 'pattern fallback-item)))
         (queries (codex-ide--web-search-queries item fallback-item)))
    (cond
     ((and (equal action-type "findInPage") pattern)
      (codex-ide--append-agent-text
       buffer
       (codex-ide--item-detail-line (format "pattern: %s" pattern))
       'codex-ide-item-detail-face))
     ((and queries
           (or force-query-lines
               (> (length queries) 1)))
      (dolist (query queries)
        (codex-ide--append-agent-text
         buffer
         (codex-ide--item-detail-line query)
         'codex-ide-item-detail-face))))))

(defun codex-ide--append-shell-command-detail (buffer command)
  "Append COMMAND as an indented, shell-highlighted detail line to BUFFER."
  (when (and (stringp command)
             (not (string-empty-p command))
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (codex-ide--without-undo-recording
       (let* ((inhibit-read-only t)
              (moving (= (point) (point-max)))
              (original-point (copy-marker (point) t))
              (active-boundary (codex-ide--active-input-boundary-marker buffer))
              (insertion-position (codex-ide--transcript-insertion-position buffer))
              (advance-active-boundary
               (and active-boundary
                    (= insertion-position (marker-position active-boundary))))
              end)
         (codex-ide--maybe-save-transcript-position insertion-position
						    (goto-char insertion-position)
						    (setq end
							  (cdr
							   (codex-ide-renderer-insert-shell-command-detail
							    command
							    (codex-ide--current-agent-text-properties))))
						    (codex-ide--advance-append-boundary-after buffer insertion-position end)
						    (when advance-active-boundary
						      (set-marker active-boundary end))
						    (if moving
							(goto-char (point-max))
						      (goto-char original-point))
						    (set-marker original-point nil)))))))

(defun codex-ide--item-detail-block (text)
  "Format TEXT as a block of indented detail lines."
  (mapconcat (lambda (line)
               (codex-ide--item-detail-line
                (if (string-empty-p line) "" line)))
             (split-string text "\n")
             ""))

(defun codex-ide--file-change-diff-face (line)
  "Return the face to use for file-change diff LINE."
  (cond
   ((string-prefix-p "@@" line) 'codex-ide-file-diff-hunk-face)
   ((or (string-prefix-p "diff --git" line)
        (string-prefix-p "--- " line)
        (string-prefix-p "+++ " line)
        (string-prefix-p "index " line))
    'codex-ide-file-diff-header-face)
   ((string-prefix-p "+" line) 'codex-ide-file-diff-added-face)
   ((string-prefix-p "-" line) 'codex-ide-file-diff-removed-face)
   (t 'codex-ide-file-diff-context-face)))

(defun codex-ide--diff-line-count (text)
  "Return the raw line count for TEXT."
  (if (string-empty-p text)
      0
    (length (split-string text "\n"))))

(defun codex-ide--file-change-diff-stats (diff-text)
  "Return a plist summarizing DIFF-TEXT."
  (let ((added 0)
        (removed 0)
        filename)
    (dolist (line (split-string diff-text "\n"))
      (cond
       ((and (not filename)
             (string-match
              (rx line-start "diff --git " (? "a/")
                  (group (+ (not (any " \n")))))
              line))
        (setq filename (match-string 1 line)))
       ((string-match (rx line-start "+++" (+ space) (? "b/")
                          (group (+ (not (any " \n")))))
                      line)
        (setq filename (or filename (match-string 1 line))))
       ((and (string-prefix-p "+" line)
             (not (string-prefix-p "+++" line)))
        (setq added (1+ added)))
       ((and (string-prefix-p "-" line)
             (not (string-prefix-p "---" line)))
        (setq removed (1+ removed)))))
    (list :filename (or filename "changes")
          :added added
          :removed removed
          :line-count (codex-ide--diff-line-count diff-text))))

(defun codex-ide--file-change-diff-header-prefix-text (overlay)
  "Return summary header text for file-change diff OVERLAY."
  (let* ((stats (or (overlay-get overlay :diff-stats) '()))
         (filename (or (plist-get stats :filename) "changes"))
         (added (or (plist-get stats :added) 0))
         (removed (or (plist-get stats :removed) 0))
         (line-count (or (plist-get stats :line-count) 0))
         (line-label (if (= line-count 1) "line" "lines")))
    (format "  └ diff: %s (+%d/-%d, %d %s) "
            filename added removed line-count line-label)))

(defun codex-ide--file-change-diff-folded-p (diff-text)
  "Return non-nil when DIFF-TEXT should start folded inline."
  (and (integerp codex-ide-diff-inline-fold-threshold)
       (> (codex-ide--diff-line-count diff-text)
          codex-ide-diff-inline-fold-threshold)))

(defun codex-ide--interactive-request-display-p (session-buffer)
  "Return non-nil when SESSION-BUFFER should be surfaced for approvals."
  (or codex-ide-buffer-display-when-approval-required
      (get-buffer-window session-buffer 0)))

(defun codex-ide--maybe-auto-open-file-change-diff
    (diff-text session-buffer &optional context)
  "Auto-open DIFF-TEXT for SESSION-BUFFER when configured for CONTEXT."
  (when (and (memq codex-ide-diff-auto-display-policy
                   '(always approval-only))
             (or (eq codex-ide-diff-auto-display-policy 'always)
                 (and (eq context 'approval)
                      (codex-ide--interactive-request-display-p session-buffer)))
             (stringp diff-text)
             (not (string-empty-p diff-text)))
    ;; Opening the standalone diff may select another window and switch the
    ;; current buffer. Keep approval/transcript rendering anchored where it was.
    (save-current-buffer
      (codex-ide-diff-open-buffer
       diff-text
       (codex-ide-diff-buffer-name-for-session session-buffer)))))

(cl-defun codex-ide--insert-file-change-diff-body
    (display-text &key keymap overlay overlay-property properties)
  "Insert DISPLAY-TEXT as a styled file-change diff body."
  (let ((start (point)))
    (dolist (line (split-string display-text "\n"))
      (codex-ide-renderer-insert-read-only
       (concat line "\n")
       (codex-ide--file-change-diff-face line)
       (append
        (list 'keymap keymap
              'help-echo "RET toggles this diff"
              overlay-property overlay)
        properties)))
    (cons start (point))))

(defun codex-ide--open-file-change-diff-overlay (overlay)
  "Open the dedicated diff buffer for file-change OVERLAY."
  (codex-ide-diff-open-buffer
   (codex-ide--item-result-text overlay)
   (overlay-get overlay :buffer-name)))

(defun codex-ide--render-file-change-diff-text
    (session item-id text &optional context)
  "Render file-change diff TEXT for SESSION ITEM-ID.
CONTEXT is either nil for ordinary transcript rendering or `approval'."
  (when (and (stringp text)
             (not (string-empty-p text)))
    (let ((trimmed (string-trim-right text)))
      (unless (string-empty-p trimmed)
        (let* ((buffer (codex-ide-session-buffer session))
               (state (copy-tree (or (codex-ide--item-state session item-id) '())))
               (anchor (plist-get state :item-result-anchor-marker))
               (stats (codex-ide--file-change-diff-stats trimmed)))
          (setq state (plist-put state :item-result-text trimmed))
          (setq state (plist-put state :item-result-label "diff"))
          (setq state (plist-put state :item-result-initial-folded
                                 (codex-ide--file-change-diff-folded-p trimmed)))
          (setq state (plist-put state :item-result-header-prefix-function
                                 #'codex-ide--file-change-diff-header-prefix-text))
          (setq state (plist-put state :item-result-body-insert-function
                                 #'codex-ide--insert-file-change-diff-body))
          (setq state (plist-put state :item-result-open-function
                                 #'codex-ide--open-file-change-diff-overlay))
          (setq state (plist-put state :item-result-open-button-label "open diff"))
          (setq state (plist-put state :item-result-open-button-help
                                 "Open this Codex diff in a dedicated diff buffer"))
          (setq state (plist-put state :item-result-toggle-help-echo
                                 "RET toggles this diff"))
          (setq state (plist-put state :item-result-toggle-button-help
                                 "Toggle this diff"))
          (setq state (plist-put state :item-result-buffer-name
                                 (codex-ide-diff-buffer-name-for-session buffer)))
          (setq state (plist-put state :item-result-stats stats))
          (unless (and (markerp anchor)
                       (eq (marker-buffer anchor) buffer))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (goto-char (codex-ide--transcript-insertion-position buffer))
                (setq state
                      (plist-put state :item-result-anchor-marker
                                 (copy-marker (point)))))))
          (codex-ide--put-item-state session item-id state)
          (when-let ((overlay (codex-ide--ensure-item-result-block session item-id)))
            (overlay-put overlay :diff-stats stats)
            (overlay-put overlay :buffer-name
                         (codex-ide-diff-buffer-name-for-session buffer))
            (overlay-put overlay :item-result-fallback-text trimmed)
            (overlay-put overlay :display-text trimmed)
            (overlay-put overlay :line-count (plist-get stats :line-count))
            (overlay-put overlay :visible-line-count (plist-get stats :line-count))
            (overlay-put overlay :truncated nil)
            (overlay-put overlay :body-properties
                         (codex-ide--current-agent-text-properties))
            (overlay-put overlay :complete t)
            (codex-ide--set-item-result-header overlay)
            (codex-ide--set-item-result-body overlay trimmed)
            (codex-ide--maybe-auto-open-file-change-diff trimmed buffer context)))))))

(defun codex-ide--summarize-item-start (item)
  "Build a one-line summary for ITEM start notifications."
  (let ((item-type (alist-get 'type item)))
    (pcase item-type
      ("commandExecution"
       (codex-ide--command-summary (alist-get 'command item)))
      ("webSearch"
       (let* ((action (alist-get 'action item))
              (action-type (alist-get 'type action))
              (queries (codex-ide--web-search-queries item))
              (query-text (string-join queries " | ")))
         (pcase action-type
           ("openPage"
            (format "Opened page %s" (or (alist-get 'url action) "unknown page")))
           ("findInPage"
            (format "Searched in page %s for %s"
                    (or (alist-get 'url action) "unknown page")
                    (or (alist-get 'pattern action) "")))
           (_
            (if (string-empty-p query-text)
                "Searched the web"
              (format "Searched the web for %s" query-text))))))
      ("mcpToolCall"
       (format "Called %s/%s"
               (or (alist-get 'server item) "mcp")
               (or (alist-get 'tool item) "tool")))
      ("dynamicToolCall"
       (format "Called tool %s" (or (alist-get 'tool item) "tool")))
      ("collabToolCall"
       (format "Delegated with %s" (or (alist-get 'tool item) "collab tool")))
      ("fileChange"
       (let ((count (length (or (alist-get 'changes item) '()))))
         (format "Prepared %d file change%s" count (if (= count 1) "" "s"))))
      ("contextCompaction"
       "Compacted conversation context")
      ("imageView"
       (format "Viewed image %s" (or (alist-get 'path item) "")))
      ("enteredReviewMode"
       "Entered review mode")
      ("exitedReviewMode"
       "Exited review mode")
      (_ nil))))

(defun codex-ide--render-item-start-details (session item)
  "Render detail lines for ITEM in SESSION."
  (let ((buffer (codex-ide-session-buffer session))
        (item-type (alist-get 'type item)))
    (pcase item-type
      ("commandExecution"
       (unless (or (codex-ide--command-read-summary (alist-get 'command item))
                   (codex-ide--command-search-summary (alist-get 'command item)))
         (codex-ide--append-shell-command-detail
          buffer
          (codex-ide--display-command-string (alist-get 'command item))))
       (when-let ((cwd (alist-get 'cwd item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "cwd: %s" (abbreviate-file-name cwd)))
          'codex-ide-item-detail-face)))
      ("webSearch"
       (codex-ide--render-web-search-details session item))
      ("mcpToolCall"
       (when-let ((arguments (alist-get 'arguments item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "args: %s" (json-encode arguments)))
          'codex-ide-item-detail-face)))
      ("dynamicToolCall"
       (when-let ((arguments (alist-get 'arguments item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "args: %s" (json-encode arguments)))
          'codex-ide-item-detail-face)))
      ("fileChange"
       (dolist (change (or (alist-get 'changes item) '()))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "%s %s"
                   (or (alist-get 'kind change) "change")
                   (or (alist-get 'path change) "unknown")))
          'codex-ide-item-detail-face)))
      ("imageView"
       (when-let ((path (alist-get 'path item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line path)
          'codex-ide-item-detail-face))))))

(defun codex-ide--render-item-start (&optional session item)
  "Render a newly started ITEM for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((buffer (codex-ide-session-buffer session))
         (item-id (alist-get 'id item))
         (item-type (alist-get 'type item))
         (summary (codex-ide--summarize-item-start item))
         (existing-state (copy-sequence
                          (or (codex-ide--item-state session item-id) '()))))
    (let ((codex-ide--current-agent-item-type item-type))
      (when summary
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--clear-pending-output-indicator session)
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* %s\n" summary)
         'codex-ide-item-summary-face)
        (codex-ide--render-item-start-details session item)
        (let ((state existing-state))
          (setq state (plist-put state :type item-type))
          (setq state (plist-put state :item item))
          (setq state (plist-put state :summary summary))
          (setq state
                (plist-put
                 state
                 :search-request
                 (and (equal item-type "commandExecution")
                      (codex-ide--rg-search-request
                       (codex-ide--display-command-argv
                        (alist-get 'command item))))))
          (setq state (plist-put state :details-rendered t))
          (when (member item-type '("commandExecution" "mcpToolCall" "fileChange"))
            ;; Keep delayed item result output anchored directly after the item
            ;; block; later transcript inserts should not move this placeholder
            ;; forward.
            (setq state
                  (plist-put state
                             :item-result-anchor-marker
                             (with-current-buffer buffer
                               (copy-marker
                                (codex-ide--transcript-insertion-position
                                 buffer))))))
          (setq state
                (plist-put state
                           :item-result-label
                           (pcase item-type
                             ("mcpToolCall" "result")
                             ("fileChange" "diff")
                             (_ "output"))))
          (when (equal item-type "commandExecution")
            (setq state
                  (plist-put state
                             :command-output-anchor-marker
                             (plist-get state :item-result-anchor-marker))))
          (setq state (plist-put state :saw-output nil))
          (codex-ide--put-item-state session item-id state))
        (when-let ((pending-output
                    (plist-get (codex-ide--item-state session item-id)
                               :pending-output-text)))
          (codex-ide--render-command-output-delta session item-id pending-output)
          (codex-ide--put-item-state
           session
           item-id
           (plist-put (codex-ide--item-state session item-id)
                      :pending-output-text nil))))
      (when (and (not summary)
                 (equal item-type "reasoning"))
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session nil t))
        (codex-ide--replace-pending-output-indicator
         session
         "Reasoning...\n")))))

(defun codex-ide--render-plan-delta (&optional session params)
  "Render a plan delta PARAMS for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((delta (or (alist-get 'delta params) ""))
        (buffer (codex-ide-session-buffer session)))
    (let ((codex-ide--current-agent-item-type "plan"))
      (unless (string-empty-p delta)
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--clear-pending-output-indicator session)
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* Plan: %s\n" delta)
         'font-lock-doc-face)))))

(defun codex-ide--reasoning-summary-entry (state summary-index)
  "Return reasoning summary entry from STATE for SUMMARY-INDEX."
  (alist-get summary-index (plist-get state :reasoning-summaries) nil nil #'equal))

(defun codex-ide--put-reasoning-summary-entry (state summary-index entry)
  "Store reasoning summary ENTRY in STATE for SUMMARY-INDEX."
  (let* ((summaries (copy-tree (plist-get state :reasoning-summaries)))
         (existing (assoc summary-index summaries)))
    (if existing
        (setcdr existing entry)
      (push (cons summary-index entry) summaries))
    (plist-put state :reasoning-summaries summaries)))

(defun codex-ide--render-reasoning-summary-entry
    (buffer text start-marker end-marker)
  "Render reasoning summary TEXT in BUFFER between START-MARKER and END-MARKER."
  (with-current-buffer buffer
    (let ((active-boundary (codex-ide--active-input-boundary-marker buffer))
          (restore-point (codex-ide--input-point-marker
                          (codex-ide--session-for-buffer buffer)))
          (moving (= (point) (point-max)))
          (start (marker-position start-marker)))
      (codex-ide--maybe-save-transcript-position start
						 (codex-ide--without-undo-recording
						  (let ((inhibit-read-only t))
						    (delete-region start (marker-position end-marker))
						    (goto-char start)
						    (insert (propertize (format "* Reasoning: %s\n" text)
									'face 'shadow
									'font-lock-face 'shadow
									'rear-nonsticky t
									'front-sticky t))
						    (add-text-properties start (point)
									 (codex-ide--current-agent-text-properties))
						    (codex-ide--freeze-region start (point))
						    (set-marker end-marker (point))
						    (when (and active-boundary
							       (= (marker-position active-boundary) start))
						      (set-marker active-boundary (point)))
						    (if restore-point
							(codex-ide--restore-input-point-marker restore-point)
						      (when moving
							(goto-char (point-max))))))))))

(defun codex-ide--render-reasoning-delta (&optional session params)
  "Render a reasoning summary delta PARAMS for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((delta (or (alist-get 'delta params)
                    (alist-get 'text params)
                    ""))
         (item-id (alist-get 'itemId params))
         (summary-index (or (alist-get 'summaryIndex params) 0))
         (buffer (codex-ide-session-buffer session)))
    (let ((codex-ide--current-agent-item-type "reasoning"))
      (unless (string-empty-p delta)
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--clear-pending-output-indicator session)
        (if (not item-id)
            (progn
              (codex-ide--ensure-output-spacing buffer)
              (codex-ide--append-agent-text
               buffer
               (format "* Reasoning: %s\n" delta)
               'shadow))
          (let* ((state (copy-tree (or (codex-ide--item-state session item-id) '())))
                 (entry (copy-tree
                         (or (codex-ide--reasoning-summary-entry state summary-index)
                             '())))
                 (start-marker (plist-get entry :start-marker))
                 (end-marker (plist-get entry :end-marker))
                 (text (concat (or (plist-get entry :text) "") delta)))
            (unless (and (markerp start-marker)
                         (markerp end-marker)
                         (eq (marker-buffer start-marker) buffer)
                         (eq (marker-buffer end-marker) buffer))
              (codex-ide--ensure-output-spacing buffer)
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (goto-char (codex-ide--transcript-insertion-position buffer))
                  (setq start-marker (copy-marker (point)))
                  (setq end-marker (copy-marker (point) t)))))
            (codex-ide--render-reasoning-summary-entry
             buffer text start-marker end-marker)
            (setq entry (plist-put entry :text text))
            (setq entry (plist-put entry :start-marker start-marker))
            (setq entry (plist-put entry :end-marker end-marker))
            (codex-ide--put-item-state
             session
             item-id
             (codex-ide--put-reasoning-summary-entry state summary-index entry))))))))

(defun codex-ide--render-item-completion (&optional session item)
  "Render any completion-only details for ITEM in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((item-id (alist-get 'id item))
         (buffer (codex-ide-session-buffer session))
         (state (codex-ide--item-state session item-id))
         (item-type (alist-get 'type item))
         (status (alist-get 'status item)))
    (let ((codex-ide--current-agent-item-type item-type))
      (pcase item-type
        ("agentMessage"
         (codex-ide--render-current-agent-message-markdown session item-id t))
        ("commandExecution"
         (let* ((search-request (plist-get state :search-request))
                (output-text (or (plist-get state :output-text)
                                 (alist-get 'aggregatedOutput item)))
                (exit-code (alist-get 'exitCode item)))
           (codex-ide--complete-command-output-block session item-id output-text)
           (cond
            (search-request
             (when-let ((hit-count (or (codex-ide--count-search-output-hits
                                        output-text)
                                       (and (equal exit-code 1) 0))))
               (codex-ide--append-agent-text
                buffer
                (codex-ide--item-detail-line
                 (codex-ide--format-hit-count hit-count))
                'codex-ide-item-detail-face))
             (when (and (equal status "failed")
                        (not (equal exit-code 1)))
               (codex-ide--append-agent-text
                buffer
                (codex-ide--item-detail-line
                 (format "failed%s"
                         (if exit-code
                             (format " with exit code %s" exit-code)
                           "")))
                'error)))
            ((equal status "failed")
             (codex-ide--append-agent-text
              buffer
              (codex-ide--item-detail-line
               (format "failed%s"
                       (if exit-code
                           (format " with exit code %s" exit-code)
			 "")))
              'error))
            ((equal status "declined")
             (codex-ide--append-agent-text
              buffer
              (codex-ide--item-detail-line "declined")
              'warning)))))
        ("mcpToolCall"
         (when-let ((result-text (codex-ide--mcp-result-text item)))
           (let ((state (or (codex-ide--item-state session item-id) '())))
             (codex-ide--put-item-state
              session
              item-id
              (plist-put state :item-result-text result-text)))
           (codex-ide--complete-item-result-block session item-id result-text))
         (when-let ((error-info (alist-get 'error item)))
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line
             (format "error: %s"
                     (or (alist-get 'message error-info) error-info)))
            'error)))
        ("dynamicToolCall"
         (when (eq (alist-get 'success item) :json-false)
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line "tool call failed")
            'error)))
        ("webSearch"
         (codex-ide--render-web-search-details
          session
          item
          (plist-get state :item)
          t))
        ("fileChange"
         (let ((diff-text (codex-ide--file-change-diff-text item))
               (streamed-diff (plist-get state :diff-text))
               (approval-rendered-items
                (codex-ide--session-metadata-get
                 session
                 :approval-file-change-diff-rendered-items)))
           (unless (or (plist-get state :approval-diff-rendered)
                       (and approval-rendered-items
                            (gethash item-id approval-rendered-items)))
             (codex-ide--render-file-change-diff-text
              session
              item-id
              (if (and (stringp diff-text)
                       (not (string-empty-p diff-text)))
                  diff-text
                streamed-diff)))))
        ("exitedReviewMode"
         (when-let ((review (alist-get 'review item)))
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-block review)
            'codex-ide-item-detail-face)))))
    (codex-ide--clear-item-state session item-id)))

(defun codex-ide--ensure-agent-message-prefix (&optional session item-id)
  "Ensure the assistant message prefix has been inserted for ITEM-ID in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (codex-ide-session-buffer session)))
    (unless (and (equal item-id (codex-ide-session-current-message-item-id session))
                 (codex-ide-session-current-message-prefix-inserted session))
      (unless (codex-ide-session-output-prefix-inserted session)
        (codex-ide--begin-turn-display session))
      (codex-ide--clear-pending-output-indicator session)
      (codex-ide--ensure-output-spacing buffer)
      (codex-ide--append-output-separator buffer)
      (codex-ide--append-agent-text buffer "\n")
      (setf (codex-ide-session-current-message-start-marker session)
            (with-current-buffer buffer
              (copy-marker (codex-ide--agent-message-render-end buffer))))
      (codex-ide--session-metadata-put
       session
       :agent-message-stream-render-start-marker
       (copy-marker (codex-ide-session-current-message-start-marker session)))
      (setf (codex-ide-session-current-message-item-id session) item-id
            (codex-ide-session-current-message-prefix-inserted session) t))))

(defun codex-ide--render-current-agent-message-markdown
    (&optional session item-id allow-trailing-tables)
  "Render the current assistant message for SESSION.
When ITEM-ID is non-nil, render only when it matches SESSION's current message."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (or (null item-id)
                   (equal item-id
                          (codex-ide-session-current-message-item-id session))))
      (when-let ((message-start
                  (codex-ide-session-current-message-start-marker session)))
        (when (eq (marker-buffer message-start) buffer)
          (with-current-buffer buffer
            (let* ((stream-marker
                    (codex-ide--session-metadata-get
                     session
                     :agent-message-stream-render-start-marker))
                   (render-start
                    (if (and (markerp stream-marker)
                             (eq (marker-buffer stream-marker) buffer))
                        (marker-position stream-marker)
                      (marker-position message-start)))
                   (message-end (codex-ide--agent-message-render-end buffer)))
              (when (< render-start message-end)
                (codex-ide--maybe-render-markdown-region
                 render-start
                 message-end
                 allow-trailing-tables))))
          (codex-ide--session-metadata-put
           session
           :agent-message-stream-render-start-marker
           nil))))))

(defun codex-ide--render-session-error (session values &optional prefix face)
  "Render session error VALUES for SESSION with PREFIX using FACE."
  (let* ((detail (apply #'codex-ide--extract-error-text values))
         (classification (apply #'codex-ide--classify-session-error values))
         (summary (codex-ide--format-session-error-summary classification prefix))
         (guidance (plist-get classification :guidance))
         (buffer (codex-ide-session-buffer session)))
    (codex-ide-log-message session "%s" summary)
    (unless (string-empty-p detail)
      (codex-ide-log-message session "  %s" detail))
    (when guidance
      (codex-ide-log-message session "%s" guidance))
    (setf (codex-ide-session-status session) "error")
    (codex-ide--update-header-line session)
    (codex-ide--clear-pending-output-indicator session)
    (codex-ide--append-to-buffer buffer (format "\n%s\n" summary) (or face 'error))
    (unless (string-empty-p detail)
      (let ((codex-ide--current-agent-item-type "error"))
        (codex-ide--append-agent-text
         buffer
         (codex-ide--item-detail-line detail)
         'codex-ide-item-detail-face)))
    (when guidance
      (codex-ide--append-to-buffer buffer (format "%s\n" guidance) (or face 'error)))
    classification))

(defun codex-ide--reopen-input-after-submit-error (&optional session prompt err)
  "Show ERR for SESSION and reopen a prompt seeded with PROMPT."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (setf (codex-ide-session-current-turn-id session) nil
        (codex-ide-session-current-message-item-id session) nil
        (codex-ide-session-current-message-prefix-inserted session) nil
        (codex-ide-session-current-message-start-marker session) nil
        (codex-ide-session-output-prefix-inserted session) nil
        (codex-ide-session-item-states session) (make-hash-table :test 'equal)
        (codex-ide-session-status session) "idle")
  (codex-ide--clear-pending-output-indicator session)
  (codex-ide--update-header-line session)
  (codex-ide--append-to-buffer
   (codex-ide-session-buffer session)
   (format "\n[Submit failed] %s\n\n" (error-message-string err))
   'error)
  (codex-ide--insert-input-prompt session prompt))

(defun codex-ide--finish-turn (&optional session closing-note)
  "Reset SESSION after a turn and reopen the prompt.
When CLOSING-NOTE is non-nil, append it before restoring the prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session))
        (turn-id (codex-ide-session-current-turn-id session))
        (queued-prompt (codex-ide--session-metadata-get
                        session
                        :queued-prompts))
        (active-prompt (codex-ide--input-prompt-active-p session)))
    (codex-ide--clear-pending-output-indicator session)
    (when closing-note
      (codex-ide--append-to-buffer buffer (format "\n%s\n" closing-note) 'warning))
    (when active-prompt
      (codex-ide--ensure-active-input-prompt-spacing session))
    (setf (codex-ide-session-current-turn-id session) nil
          (codex-ide-session-current-message-item-id session) nil
          (codex-ide-session-current-message-prefix-inserted session) nil
          (codex-ide-session-current-message-start-marker session) nil
          (codex-ide-session-output-prefix-inserted session) nil
          (codex-ide-session-item-states session) (make-hash-table :test 'equal)
          (codex-ide-session-interrupt-requested session) nil)
    (codex-ide--set-session-status session "idle" 'turn-completed)
    (codex-ide--update-header-line session)
    (codex-ide--run-session-event
     'turn-completed
     session
     :turn-id turn-id
     :closing-note closing-note)
    (cond
     ((and active-prompt queued-prompt)
      (codex-ide--refresh-running-input-display session))
     (active-prompt
      (codex-ide--delete-running-input-list session))
     (queued-prompt
      nil)
     (t
      (codex-ide--append-to-buffer buffer "\n\n")
      (codex-ide--insert-input-prompt session)))))

(defun codex-ide--thread-read-display-user-text (text)
  "Normalize stored user TEXT for transcript display."
  (when (stringp text)
    (let ((display-text (string-trim (codex-ide--strip-emacs-context-prefix text))))
      (unless (string-empty-p display-text)
        display-text))))

(defun codex-ide--append-restored-user-message (session text)
  "Append restored user TEXT to SESSION like a submitted prompt."
  (let ((buffer (codex-ide-session-buffer session))
        (display-text (codex-ide--thread-read-display-user-text text)))
    (when (and (buffer-live-p buffer)
               (stringp display-text)
               (not (string-empty-p display-text)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (codex-ide-renderer-insert-restored-user-message display-text)))
      t)))

(defun codex-ide--append-restored-agent-message (session item)
  "Append restored agent ITEM to SESSION through the live agent render path."
  (let* ((buffer (codex-ide-session-buffer session))
         (item-id (or (alist-get 'id item) "restored-agent-message"))
         (text (codex-ide--thread-read--message-text item)))
    (when (and (buffer-live-p buffer)
               (stringp text)
               (not (string-empty-p (string-trim text))))
      (let ((codex-ide--current-agent-item-type "agentMessage"))
        (codex-ide--ensure-agent-message-prefix session item-id)
        (codex-ide--append-agent-text buffer text)
        (codex-ide--render-item-completion session item))
      t)))

(defun codex-ide--stored-item-with-id (item fallback-id)
  "Return stored ITEM with FALLBACK-ID when it lacks an id."
  (let ((copy (copy-tree item)))
    (if (alist-get 'id copy)
        copy
      (append copy `((id . ,fallback-id))))))

(defun codex-ide--normalized-stored-render-item (item)
  "Return ITEM normalized for replay through live rendering primitives."
  (let ((copy (copy-tree item)))
    (when (and (equal (alist-get 'type copy) "commandExecution")
               (not (alist-get 'aggregatedOutput copy))
               (alist-get 'output copy))
      (push (cons 'aggregatedOutput (alist-get 'output copy)) copy))
    copy))

(defun codex-ide--merge-restored-turn-items (turn rollout-items)
  "Return TURN with ROLLOUT-ITEMS merged into its item list."
  (if (not rollout-items)
      turn
    (let ((copy (copy-tree turn))
          (items (append (codex-ide--thread-read-items turn) nil))
          (merged nil)
          (inserted nil)
          (rollout-has-assistant
           (seq-some
            (lambda (item)
              (eq (codex-ide--thread-read--item-kind item) 'assistant))
            rollout-items)))
      (if rollout-has-assistant
          (setq merged
                (append
                 (seq-filter
                  (lambda (item)
                    (eq (codex-ide--thread-read--item-kind item) 'user))
                  items)
                 rollout-items))
        (dolist (item items)
          (when (and (not inserted)
                     (eq (codex-ide--thread-read--item-kind item) 'assistant))
            (setq merged (append (reverse rollout-items) merged))
            (setq inserted t))
          (push item merged))
        (unless inserted
          (setq merged (append (reverse rollout-items) merged)))
        (setq merged (nreverse merged)))
      (setf (alist-get 'items copy) merged)
      copy)))

(defun codex-ide--thread-read-with-rollout-render-items (thread-read)
  "Return THREAD-READ augmented with renderable rollout storage items."
  (let* ((thread (alist-get 'thread thread-read))
         (path (alist-get 'path thread))
         (rollout-turns (codex-ide-rollout-turn-render-items path)))
    (if (not rollout-turns)
        thread-read
      (let* ((copy (copy-tree thread-read))
             (copy-thread (alist-get 'thread copy))
             (turns (append (codex-ide--thread-read-turns copy) nil))
             (rollout-turns
              (nthcdr (max 0 (- (length rollout-turns) (length turns)))
                      rollout-turns))
             (merged-turns
              (cl-loop for turn in turns
                       for rollout-items in rollout-turns
                       collect (codex-ide--merge-restored-turn-items
                                turn
                                rollout-items))))
        (if (alist-get 'turns copy)
            (setf (alist-get 'turns copy) merged-turns)
          (setf (alist-get 'turns copy-thread) merged-turns))
        copy))))

(defun codex-ide--replay-stored-render-item (session item)
  "Replay stored non-message ITEM into SESSION using live item render primitives."
  (let* ((item (codex-ide--normalized-stored-render-item item))
         (item-type (alist-get 'type item))
         (item-id (alist-get 'id item)))
    (when (codex-ide--summarize-item-start item)
      (when (equal item-type "fileChange")
        (codex-ide--put-current-turn-file-change session item-id item))
      (codex-ide--render-item-start session item)
      (codex-ide--render-item-completion session item)
      t)))

(defun codex-ide--replay-thread-read-turn (session turn)
  "Replay stored TURN into SESSION.
Return non-nil when any transcript content was restored."
  (let ((items (append (codex-ide--thread-read-items turn) nil))
        (turn-id (or (alist-get 'id turn) "turn"))
        (index 0)
        (restored nil))
    (with-current-buffer (codex-ide-session-buffer session)
      (let ((marker (copy-marker
                     (codex-ide--transcript-insertion-position
                      (current-buffer))
                     nil)))
        (codex-ide--record-turn-start session turn-id marker)))
    (dolist (raw-item items restored)
      (let* ((index (prog1 index
                      (setq index (1+ index))))
             (item (codex-ide--stored-item-with-id
                    raw-item
                    (format "restored-%s-item-%d" turn-id index))))
        (pcase (codex-ide--thread-read--item-kind item)
          ('user
           (setq restored
                 (or (codex-ide--append-restored-user-message
                      session
                      (codex-ide--thread-read--message-text item))
                     restored))
           (setf (codex-ide-session-output-prefix-inserted session) t
                 (codex-ide-session-current-message-item-id session) nil
                 (codex-ide-session-current-message-prefix-inserted session) nil
                 (codex-ide-session-current-message-start-marker session) nil))
          ('assistant
           (setq restored
                 (or (codex-ide--append-restored-agent-message session item)
                     restored)))
          (_
           (setq restored
                 (or (codex-ide--replay-stored-render-item session item)
                     restored))))))))

(defun codex-ide--restore-thread-read-transcript (&optional session thread-read)
  "Replay a stored transcript from THREAD-READ into SESSION.
Signal an error when THREAD-READ lacks replayable transcript items."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (setq thread-read (codex-ide--thread-read-with-rollout-render-items thread-read))
  (codex-ide--session-metadata-put session :turn-start-index nil)
  (codex-ide--set-pending-turn-start-marker session nil)
  (let* ((limit (max 0 codex-ide-resume-summary-turn-limit))
         (turns (append (codex-ide--thread-read-turns thread-read) nil))
         (recent-turns (cond
                        ((<= limit 0) nil)
                        ((> (length turns) limit) (last turns limit))
                        (t turns)))
         (restored nil))
    (unless recent-turns
      (error "Stored thread has no replayable turns"))
    (dolist (turn recent-turns restored)
      (setq restored
            (or (codex-ide--replay-thread-read-turn session turn)
                restored)))
    (unless restored
      (error
       (concat
        "Stored thread transcript could not be replayed. "
        "Expected replayable userMessage/agentMessage turn items.")))
    (when restored
      (codex-ide--set-restored-thread-read session thread-read)
      (codex-ide--append-to-buffer (codex-ide-session-buffer session) "\n")
      (codex-ide--append-restored-transcript-separator
       (codex-ide-session-buffer session)))
    (setf (codex-ide-session-current-turn-id session) nil
          (codex-ide-session-current-message-item-id session) nil
          (codex-ide-session-current-message-prefix-inserted session) nil
          (codex-ide-session-current-message-start-marker session) nil
          (codex-ide-session-output-prefix-inserted session) nil
          (codex-ide-session-item-states session) (make-hash-table :test 'equal))
    (codex-ide--set-current-turn-diff-entry session nil)
    restored))

(defun codex-ide--reset-session-buffer (session)
  "Reset SESSION's transcript buffer to an empty session header."
  (let ((buffer (codex-ide-session-buffer session))
        (working-dir (codex-ide-session-directory session)))
    (with-current-buffer buffer
      (setq-local default-directory working-dir)
      (setq-local codex-ide--session session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (codex-ide-renderer-insert-session-header working-dir)))
    (setf (codex-ide-session-current-turn-id session) nil
          (codex-ide-session-current-message-item-id session) nil
          (codex-ide-session-current-message-prefix-inserted session) nil
          (codex-ide-session-current-message-start-marker session) nil
          (codex-ide-session-output-prefix-inserted session) nil
          (codex-ide-session-item-states session) (make-hash-table :test 'equal)
          (codex-ide-session-input-overlay session) nil
          (codex-ide-session-input-start-marker session) nil
          (codex-ide-session-input-prompt-start-marker session) nil
          (codex-ide-session-prompt-history-index session) nil
          (codex-ide-session-prompt-history-draft session) nil
          (codex-ide-session-interrupt-requested session) nil
          (codex-ide-session-status session) "idle"))
  (codex-ide--session-metadata-put session :turn-start-index nil)
  (codex-ide--set-pending-turn-start-marker session nil))

(defun codex-ide--approval-decision (prompt choices)
  "Prompt the user with PROMPT and return one of CHOICES."
  (cdr (assoc (completing-read prompt choices nil t) choices)))

(defun codex-ide--pending-approvals (&optional session)
  "Return the pending approval table for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (or (codex-ide--session-metadata-get session :pending-approvals)
      (codex-ide--session-metadata-put
       session
       :pending-approvals
       (make-hash-table :test 'equal))))

(defun codex-ide--approval-display-value (value)
  "Return a compact display string for approval VALUE."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%S" value))))

(defun codex-ide--approval-result (kind value params)
  "Build the JSON-RPC result for approval KIND with VALUE and PARAMS."
  (pcase kind
    ('elicitation value)
    ('permissions
     (if (eq value 'decline)
         '((permissions . []))
       `((permissions . ,(or (alist-get 'permissions params) '()))
         (scope . ,(symbol-name value)))))
    (_
     `((decision . ,value)))))

(defun codex-ide--mark-approval-resolved (approval label)
  "Update APPROVAL's transcript block to show resolved LABEL."
  (let ((buffer (marker-buffer (plist-get approval :status-marker)))
        (status-marker (plist-get approval :status-marker))
        (start-marker (plist-get approval :start-marker))
        (end-marker (plist-get approval :end-marker)))
    (when (and (buffer-live-p buffer)
               (markerp status-marker)
               (markerp start-marker)
               (markerp end-marker))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (status-pos (marker-position status-marker))
              (block-start (marker-position start-marker))
              (block-end (marker-position end-marker)))
          (when status-pos
            (save-excursion
              (goto-char status-pos)
              (codex-ide-renderer-insert-approval-resolution label)
              (when (and (markerp end-marker)
                         (eq (marker-buffer end-marker) buffer))
                (set-marker end-marker (point)))))
          (when (and block-start block-end)
            (remove-text-properties
             block-start block-end
             '(action nil mouse-face nil help-echo nil follow-link nil
		      keymap nil button nil category nil)))
          (when (and block-start block-end)
            (codex-ide--freeze-region block-start (marker-position end-marker))
            (codex-ide--advance-active-boundary-after buffer end-marker)))))))

(defun codex-ide--resolve-buffer-approval (session id value label)
  "Resolve pending approval ID for SESSION as VALUE with display LABEL."
  (let* ((approvals (codex-ide--pending-approvals session))
         (approval (gethash id approvals)))
    (if (not approval)
        (message "Codex approval already resolved")
      (remhash id approvals)
      (codex-ide-log-message
       session
       "%s approval resolved as %s"
       (capitalize (symbol-name (plist-get approval :kind)))
       (codex-ide--approval-display-value value))
      (codex-ide--mark-approval-resolved approval label)
      (codex-ide--set-session-status
       session
       (if (codex-ide-session-current-turn-id session) "running" "idle")
       'approval-resolved)
      (codex-ide--update-header-line session)
      (codex-ide--jsonrpc-send-response
       session
       id
       (codex-ide--approval-result
        (plist-get approval :kind)
        value
        (plist-get approval :params))))))

(defun codex-ide--insert-approval-choice-button (session id label value)
  "Insert an approval button for SESSION request ID with LABEL and VALUE."
  (codex-ide-renderer-insert-action-button
   label
   (lambda ()
     (codex-ide--resolve-buffer-approval session id value label))
   (format "Resolve Codex approval as %s" label)
   (codex-ide-nav-button-keymap)))

(defun codex-ide--approval-file-change-diff-text (session params)
  "Return diff text for file-change approval PARAMS in SESSION."
  (let* ((item-id (alist-get 'itemId params))
         (state (and item-id (codex-ide--item-state session item-id)))
         (streamed-diff (and state (plist-get state :diff-text))))
    (or (seq-some
         (lambda (candidate)
           (when (listp candidate)
             (let ((text (codex-ide--file-change-diff-text candidate)))
               (when (and (stringp text)
                          (not (string-empty-p text)))
                 text))))
         (list params
               (alist-get 'item params)
               (alist-get 'fileChange params)
               (alist-get 'fileChangeItem params)
               (and state (plist-get state :item))))
        (when (and (stringp streamed-diff)
                   (not (string-empty-p streamed-diff)))
          streamed-diff))))

(defun codex-ide--mark-approval-file-change-diff-rendered (session params)
  "Mark the file-change item in PARAMS as having rendered its approval diff."
  (when-let ((item-id (alist-get 'itemId params)))
    (let ((rendered-items
           (or (codex-ide--session-metadata-get
                session
                :approval-file-change-diff-rendered-items)
               (codex-ide--session-metadata-put
                session
                :approval-file-change-diff-rendered-items
                (make-hash-table :test 'equal)))))
      (puthash item-id t rendered-items))
    (when-let ((state (codex-ide--item-state session item-id)))
      (codex-ide--put-item-state
       session
       item-id
       (plist-put state :approval-diff-rendered t)))))

(defun codex-ide--notify-interactive-request (session message-text)
  "Notify the user that SESSION requires attention with MESSAGE-TEXT."
  (let ((buffer (codex-ide-session-buffer session)))
    (message message-text (buffer-name buffer))
    (when (codex-ide--interactive-request-display-p buffer)
      (codex-ide--show-session-buffer session))))

(cl-defun codex-ide--render-interactive-request
    (session id kind params &key title notify-message render-body metadata)
  "Render an inline interactive request block for SESSION request ID."
  (let ((buffer (codex-ide-session-buffer session))
        (render-state nil)
        active-boundary
        start-marker
        status-marker
        end-marker)
    (codex-ide--clear-pending-output-indicator session)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (restore-point (codex-ide--input-point-marker session))
            (moving (= (point) (point-max))))
        (codex-ide--ensure-output-spacing buffer)
        (setq active-boundary (codex-ide--active-input-boundary-marker buffer))
        (goto-char (codex-ide--transcript-insertion-position buffer))
        (pcase-let ((`(,start ,status ,end ,state)
                     (codex-ide-renderer-insert-interactive-request-shell
                      title render-body)))
          (setq start-marker (copy-marker start)
                status-marker (copy-marker status)
                end-marker (copy-marker end t)
                render-state state))
        (codex-ide--freeze-region (marker-position start-marker)
                                  (marker-position end-marker))
        (when (and active-boundary
                   (<= (marker-position active-boundary)
                       (marker-position start-marker)))
          (set-marker active-boundary (point)))
        (dolist (range (plist-get render-state :writable-ranges))
          (codex-ide--make-region-writable (marker-position (car range))
                                           (marker-position (cdr range))))
        (if restore-point
            (codex-ide--restore-input-point-marker restore-point)
          (when moving
            (goto-char (point-max))))))
    (puthash id
             (append
              (list :kind kind
                    :params params
                    :start-marker start-marker
                    :status-marker status-marker
                    :end-marker end-marker)
              metadata
              (plist-get render-state :metadata))
             (codex-ide--pending-approvals session))
    (codex-ide--set-session-status session "approval" 'approval-requested)
    (codex-ide--update-header-line session)
    (codex-ide--run-session-event
     'approval-requested
     session
     :id id
     :kind kind
     :params params)
    (codex-ide--notify-interactive-request session notify-message)))

(cl-defun codex-ide--render-buffer-approval
    (session id kind &key title details choices params)
  "Render an inline approval block for SESSION request ID."
  (codex-ide--render-interactive-request
   session
   id
   kind
   params
   :title title
   :notify-message "Codex approval required in %s"
   :render-body
   (lambda ()
     (dolist (detail details)
       (if-let ((diff-text (and (eq (plist-get detail :kind) 'diff)
                                (plist-get detail :text)))
                (item-id (alist-get 'itemId params)))
           (let ((state (copy-tree (or (codex-ide--item-state session item-id) '()))))
             (codex-ide-renderer-insert-approval-label "Proposed changes:")
             (codex-ide-renderer-insert-read-only "\n\n")
             (setq state
                   (plist-put state :item-result-anchor-marker
                              (copy-marker (point))))
             (codex-ide--put-item-state session item-id state)
             (codex-ide--render-file-change-diff-text
              session item-id diff-text 'approval)
             (codex-ide-renderer-insert-read-only "\n"))
         (codex-ide-renderer-insert-approval-detail detail))
       (unless (eq (plist-get detail :kind) 'diff)
         (codex-ide-renderer-insert-read-only "\n")))
     (dolist (choice choices)
       (codex-ide--insert-approval-choice-button
        session id (car choice) (cdr choice))
       (codex-ide-renderer-insert-read-only "\n"))
     (codex-ide-renderer-insert-read-only "\n")
     nil)))

(defun codex-ide--schedule-interactive-request (session body on-quit on-error)
  "Run BODY for SESSION on the next tick with shared quit/error handling."
  (run-at-time
   0 nil
   (lambda ()
     (condition-case err
         (funcall body)
       (quit
        (funcall on-quit))
       (error
        (funcall on-error err))))))

(defun codex-ide--elicitation-choice-options (field)
  "Return button options for elicitation FIELD."
  (pcase (plist-get field :type)
    ("boolean"
     (append
      '(("true" . t)
        ("false" . :json-false))
      (unless (plist-get field :requiredp)
        '(("skip" . :codex-ide-mcp-elicitation-omit)))))
    (_
     (let ((choices nil)
           (values (plist-get field :enum))
           (names (plist-get field :enum-names)))
       (cl-mapc
        (lambda (value label)
          (push (cons (or label (format "%s" value)) value) choices))
        values
        (append names (make-list (max 0 (- (length values) (length names))) nil)))
       (setq choices (nreverse choices))
       (unless (plist-get field :requiredp)
         (setq choices (append choices '(("skip" . :codex-ide-mcp-elicitation-omit)))))
       choices))))

(defun codex-ide--elicitation-choice-label (field value)
  "Return a display label for elicitation FIELD VALUE."
  (cond
   ((or (null value)
        (eq value :codex-ide-mcp-elicitation-omit))
    (if (plist-get field :requiredp) "<unset>" "skip"))
   ((equal (plist-get field :type) "boolean")
    (if (eq value t) "true" "false"))
   (t
    (or (car (rassoc value (codex-ide--elicitation-choice-options field)))
        (format "%s" value)))))

(defun codex-ide--set-elicitation-choice-display (field label)
  "Replace FIELD's current choice display text with LABEL."
  (let ((start-marker (plist-get field :display-start-marker))
        (end-marker (plist-get field :display-end-marker)))
    (when (and (markerp start-marker)
               (markerp end-marker)
               (marker-buffer start-marker))
      (with-current-buffer (marker-buffer start-marker)
        (let ((inhibit-read-only t))
          (save-excursion
            (codex-ide-renderer-replace-marker-region
             start-marker end-marker label)))))))

(defun codex-ide--set-elicitation-choice-value (_session _id field label value)
  "Set FIELD for SESSION elicitation ID to VALUE and display LABEL."
  (setcar (plist-get field :value-cell) value)
  (codex-ide--set-elicitation-choice-display field label))

(defun codex-ide--elicitation-field-raw-value (field)
  "Return the current raw value for elicitation FIELD."
  (pcase (plist-get field :input-kind)
    ('choice (car (plist-get field :value-cell)))
    (_
     (let* ((start (marker-position (plist-get field :start-marker)))
            (end (marker-position (plist-get field :end-marker)))
            (text (buffer-substring-no-properties start end)))
       (string-remove-suffix "\n" text)))))

(defun codex-ide--submit-buffer-elicitation (session id)
  "Validate and submit elicitation response for SESSION request ID."
  (let* ((approval (gethash id (codex-ide--pending-approvals session)))
         (fields (plist-get approval :fields))
         (content nil))
    (unless approval
      (user-error "Codex elicitation already resolved"))
    (condition-case err
        (progn
          (dolist (field fields)
            (let ((value (codex-ide-mcp-elicitation-parse-field-value
                          field
                          (with-current-buffer (codex-ide-session-buffer session)
                            (codex-ide--elicitation-field-raw-value field)))))
              (unless (eq value :codex-ide-mcp-elicitation-omit)
                (push (cons (plist-get field :name) value) content))))
          (codex-ide--resolve-buffer-approval
           session
           id
           `((action . "accept")
             (content . ,(nreverse content)))
           "submit"))
      (error
       (message "Codex elicitation error: %s" (error-message-string err))))))

(defun codex-ide--insert-elicitation-field (session id field writable-ranges)
  "Insert one elicitation FIELD and return updated state."
  (let* ((choices (codex-ide--elicitation-choice-options field))
         (default (plist-get field :default))
         (choice-field-p (or (plist-get field :enum)
                             (equal (plist-get field :type) "boolean")))
         (initial (if (or (member default (mapcar #'cdr choices))
                          (and (null default)
                               (not (plist-get field :requiredp))))
                      default
                    nil))
         (value-cell (and choice-field-p (list initial)))
         rendered-field)
    (let ((result
           (codex-ide-renderer-insert-elicitation-field
            (codex-ide-mcp-elicitation-format-field-prompt field)
            (if choice-field-p 'choice 'text)
            (if choice-field-p
                (codex-ide--elicitation-choice-label field initial)
              (plist-get field :default))
            choices
            writable-ranges
            (lambda (label value)
              (codex-ide--set-elicitation-choice-value
               session id rendered-field label value))
            (codex-ide-nav-button-keymap))))
      (setq rendered-field
            (append field
                    (if choice-field-p
                        (list :input-kind 'choice
                              :value-cell value-cell
                              :display-start-marker
                              (plist-get result :display-start-marker)
                              :display-end-marker
                              (plist-get result :display-end-marker))
                      (list :input-kind 'text
                            :start-marker (plist-get result :start-marker)
                            :end-marker (plist-get result :end-marker)))))
      (list rendered-field
            (plist-get result :writable-ranges)))))

(defun codex-ide--render-buffer-elicitation (session id params)
  "Render PARAMS as an inline elicitation block for SESSION request ID."
  (let* ((request (codex-ide-mcp-elicitation-normalize-request params))
         (mode (or (alist-get 'mode request) "form"))
         (message (string-trim (or (alist-get 'message request) "")))
         (schema (alist-get 'requestedSchema request))
         (fields (and schema
                      (codex-ide-mcp-elicitation-field-specs schema)))
         (writable-ranges nil)
         rendered-fields)
    (codex-ide--render-interactive-request
     session
     id
     'elicitation
     request
     :title "[Input required]"
     :notify-message "Codex input required in %s"
     :render-body
     (lambda ()
       (codex-ide-renderer-insert-approval-label "Request: ")
       (codex-ide-renderer-insert-read-only
        (concat (codex-ide-mcp-elicitation-format-request request) "\n"))
       (unless (string-empty-p message)
         (codex-ide-renderer-insert-approval-label "Message: ")
         (codex-ide-renderer-insert-read-only (concat message "\n")))
       (when-let ((url (alist-get 'url request)))
         (codex-ide-renderer-insert-approval-label "URL: ")
         (codex-ide-renderer-insert-read-only (concat url "\n")))
       (codex-ide-renderer-insert-read-only "\n")
       (dolist (field fields)
         (pcase-let ((`(,rendered-field ,new-ranges)
                      (codex-ide--insert-elicitation-field
                       session id field writable-ranges)))
           (push rendered-field rendered-fields)
           (setq writable-ranges new-ranges)))
       (setq rendered-fields (nreverse rendered-fields))
       (pcase mode
         ("url"
          (codex-ide-renderer-insert-action-button
           "open and continue"
           (lambda ()
             (browse-url (alist-get 'url request))
             (codex-ide--resolve-buffer-approval
              session id '((action . "accept")) "open and continue"))
           nil
           (codex-ide-nav-button-keymap))
          (codex-ide-renderer-insert-read-only "\n")
          (codex-ide-renderer-insert-action-button
           "continue"
           (lambda ()
             (codex-ide--resolve-buffer-approval
              session id '((action . "accept")) "continue"))
           nil
           (codex-ide-nav-button-keymap))
          (codex-ide-renderer-insert-read-only "\n"))
         (_
          (codex-ide-renderer-insert-action-button
           "submit"
           (lambda ()
             (codex-ide--submit-buffer-elicitation session id))
           nil
           (codex-ide-nav-button-keymap))
          (codex-ide-renderer-insert-read-only "\n")))
       (codex-ide-renderer-insert-action-button
        "decline"
        (lambda ()
          (codex-ide--resolve-buffer-approval
           session id '((action . "decline")) "decline"))
        nil
        (codex-ide-nav-button-keymap))
       (codex-ide-renderer-insert-read-only "\n")
       (codex-ide-renderer-insert-action-button
        "cancel"
        (lambda ()
          (codex-ide--resolve-buffer-approval
           session id '((action . "cancel")) "cancel"))
        nil
        (codex-ide-nav-button-keymap))
       (codex-ide-renderer-insert-read-only "\n\n")
       (list :writable-ranges writable-ranges
             :metadata (list :fields rendered-fields))))))

(defun codex-ide--command-approval-choices (params)
  "Build completion choices for a command approval request from PARAMS."
  (let ((amendment (alist-get 'proposedExecpolicyAmendment params)))
    (append
     '(("accept" . "accept")
       ("accept for session" . "acceptForSession"))
     (when amendment
       `((,(format "accept and allow prefix (%s)"
                   (mapconcat #'identity amendment " "))
          . ,`((acceptWithExecpolicyAmendment
                . ((execpolicy_amendment . ,amendment)))))))
     '(("decline" . "decline")
       ("cancel turn" . "cancel")))))

(defun codex-ide--handle-command-approval (&optional session id params)
  "Handle a command approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (codex-ide--schedule-interactive-request
   session
   (lambda ()
     (let* ((command (codex-ide--display-command-string
                      (or (alist-get 'command params) "unknown command")))
            (choices (codex-ide--command-approval-choices params)))
       (codex-ide--render-buffer-approval
        session
        id
        'command
        :title "[Approval required]"
        :details (delq nil
                       (list
                        (list :kind 'command :text command)
                        (when-let ((reason (alist-get 'reason params)))
                          (list :label "Reason" :text reason))))
        :choices choices
        :params params)))
   (lambda ()
     (codex-ide-log-message session "Command approval prompt quit; canceling turn")
     (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))
   (lambda (err)
     (codex-ide-log-message
      session
      "Command approval failed: %s"
      (error-message-string err))
     (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))))

(defun codex-ide--handle-file-change-approval (&optional session id params)
  "Handle a file-change approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (codex-ide--schedule-interactive-request
   session
   (lambda ()
     (let* ((reason (or (alist-get 'reason params) "approve file changes"))
            (choices '(("accept" . "accept")
                       ("accept for session" . "acceptForSession")
                       ("decline" . "decline")
                       ("cancel turn" . "cancel"))))
       (codex-ide--render-buffer-approval
        session
        id
        'file-change
        :title "[Approval required]"
        :details (delq nil
                       (list
                        (list :label "Approve file changes" :text reason)
                        (when-let ((diff-text
                                    (codex-ide--approval-file-change-diff-text
                                     session
                                     params)))
                          (codex-ide--mark-approval-file-change-diff-rendered
                           session
                           params)
                          (list :kind 'diff :text diff-text))))
        :choices choices
        :params params)))
   (lambda ()
     (codex-ide-log-message session "File-change approval prompt quit; canceling turn")
     (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))
   (lambda (err)
     (codex-ide-log-message
      session
      "File-change approval failed: %s"
      (error-message-string err))
     (codex-ide--jsonrpc-send-response session id '((decision . "cancel"))))))

(defun codex-ide--handle-permissions-approval (&optional session id params)
  "Handle a permissions approval request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (codex-ide--schedule-interactive-request
   session
   (lambda ()
     (let* ((permissions (or (alist-get 'permissions params) '()))
            (choices '(("grant for turn" . turn)
                       ("grant for session" . session)
                       ("decline" . decline))))
       (codex-ide--render-buffer-approval
        session
        id
        'permissions
        :title "[Approval required]"
        :details (append
                  (when-let ((reason (alist-get 'reason params)))
                    (list (list :label "Reason" :text reason)))
                  (when permissions
                    (list (list :label "Permissions"
                                :text (format "%S" permissions)))))
        :choices choices
        :params params)))
   (lambda ()
     (codex-ide-log-message session "Permissions approval prompt quit; declining")
     (codex-ide--jsonrpc-send-response session id '((permissions . []))))
   (lambda (err)
     (codex-ide-log-message
      session
      "Permissions approval failed: %s"
      (error-message-string err))
     (codex-ide--jsonrpc-send-response session id '((permissions . []))))))

(defun codex-ide--handle-elicitation-request (&optional session id params)
  "Handle an MCP elicitation request for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (codex-ide--schedule-interactive-request
   session
   (lambda ()
     (if (and (fboundp 'codex-ide-mcp-bridge-request-exempt-from-approval-p)
              (codex-ide-mcp-bridge-request-exempt-from-approval-p params))
         (let ((result '((action . "accept"))))
           (codex-ide-log-message
            session
            "Elicitation request resolved as %s"
            (alist-get 'action result))
           (codex-ide--jsonrpc-send-response session id result))
       (codex-ide--render-buffer-elicitation session id params)))
   (lambda ()
     (codex-ide-log-message session "Elicitation request quit; canceling")
     (codex-ide--jsonrpc-send-response session id '((action . "cancel"))))
   (lambda (err)
     (codex-ide-log-message
      session
      "Elicitation request failed: %s"
      (error-message-string err))
     (codex-ide--jsonrpc-send-error session id -32603
                                    (error-message-string err)))))

(defun codex-ide--handle-server-request (&optional session message)
  "Handle a server-initiated request MESSAGE for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message))
        (params (alist-get 'params message)))
    (codex-ide-log-message session "Received server request %s (id=%s)" method id)
    (pcase method
      ((or "elicitation/create"
           "mcpServer/elicitation/request")
       (codex-ide--handle-elicitation-request session id params))
      ("item/commandExecution/requestApproval"
       (codex-ide--handle-command-approval session id params))
      ("item/fileChange/requestApproval"
       (codex-ide--handle-file-change-approval session id params))
      ("item/permissions/requestApproval"
       (codex-ide--handle-permissions-approval session id params))
      (_
       (codex-ide-log-message session "Unsupported server request %s" method)
       (codex-ide--append-to-buffer
        (codex-ide-session-buffer session)
        (format "\n[Codex requested unsupported method %s]\n" method)
        'warning)
       (codex-ide--jsonrpc-send-error session id -32601
                                      (format "Unsupported method: %s" method))))))

(defun codex-ide--append-notification-additional-details (session details)
  "Append notification DETAILS to SESSION."
  (when details
    (codex-ide--append-to-buffer
     (codex-ide-session-buffer session)
     (concat "\n"
             (mapconcat (lambda (detail) (format "  └ %s" detail)) details "\n")
             "\n")
     'shadow)))

(defun codex-ide--handle-retryable-notification-error (session info)
  "Render a retry notice from INFO for SESSION."
  (let* ((message (or (alist-get 'message info) "Retrying"))
         (notice (format "[Codex retrying] %s" message))
         (details (codex-ide--notification-error-additional-details info)))
    (codex-ide-log-message session "Retryable Codex error: %s" message)
    (unless (equal notice (codex-ide--session-metadata-get session :last-retry-notice))
      (codex-ide--session-metadata-put session :last-retry-notice notice)
      (codex-ide--append-to-buffer
       (codex-ide-session-buffer session)
       (concat "\n" notice "\n"
               (mapconcat (lambda (detail) (format "  └ %s" detail)) details "\n")
               "\n")
       'shadow))))

(defun codex-ide--handle-notification (&optional session message)
  "Handle a notification MESSAGE for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((method (alist-get 'method message))
        (params (alist-get 'params message))
        (buffer (codex-ide-session-buffer session)))
    (codex-ide-log-message session "Received notification %s" method)
    (pcase method
      ("thread/started"
       (codex-ide--remember-reasoning-effort session params)
       (codex-ide--remember-model-name session params)
       (when-let ((thread-id (alist-get 'id (alist-get 'thread params))))
         (setf (codex-ide-session-thread-id session) thread-id)
         (codex-ide--mark-session-thread-attached session)
         (codex-ide--run-session-event
          'thread-attached
          session
          :thread-id thread-id
          :action "Started"))
       (codex-ide-log-message
        session
        "Thread started: %s"
        (codex-ide-session-thread-id session))
       (codex-ide--set-session-status session "idle" 'thread-started)
       (codex-ide--update-header-line session))
      ("thread/status/changed"
       (let* ((thread (alist-get 'thread params))
              (status (or (alist-get 'status params)
                          (alist-get 'status thread)))
              (normalized-status (codex-ide--normalize-session-status status)))
         (when normalized-status
           (codex-ide--set-session-status
            session
            normalized-status
            'thread-status-changed)))
       (codex-ide-log-message
        session
        "Thread status changed to %s"
        (codex-ide-session-status session))
       (codex-ide--update-header-line session))
      ("thread/tokenUsage/updated"
       (let ((token-usage (alist-get 'tokenUsage params)))
         (codex-ide--session-metadata-put session :token-usage token-usage)
         (codex-ide-log-message
          session
          "Token usage updated: total=%s window=%s"
          (alist-get 'totalTokens (alist-get 'total token-usage))
          (alist-get 'modelContextWindow token-usage))
         (codex-ide--update-header-line session)))
      ("account/rateLimits/updated"
       (let ((rate-limits (alist-get 'rateLimits params)))
         (codex-ide--session-metadata-put session :rate-limits rate-limits)
         (codex-ide-log-message
          session
          "Rate limits updated: used=%s%% plan=%s"
          (alist-get 'usedPercent (alist-get 'primary rate-limits))
          (or (alist-get 'planType rate-limits) "unknown"))
         (codex-ide--update-header-line session)))
      ("turn/started"
       (codex-ide--remember-reasoning-effort session params)
       (codex-ide--remember-or-request-model-name session params)
       (let ((turn-id (or (alist-get 'id (alist-get 'turn params))
                          (alist-get 'turnId params))))
         (setf (codex-ide-session-current-turn-id session) turn-id
               (codex-ide-session-current-message-item-id session) nil
               (codex-ide-session-current-message-prefix-inserted session) nil
               (codex-ide-session-current-message-start-marker session) nil
               (codex-ide-session-item-states session) (make-hash-table :test 'equal))
         (codex-ide--record-pending-turn-start session turn-id)
         (codex-ide--mark-current-turn-diff-started session turn-id))
       (codex-ide--set-session-status session "running" 'turn-started)
       (codex-ide--session-metadata-put
        session
        :approval-file-change-diff-rendered-items
        nil)
       (codex-ide-log-message
        session
        "Turn started: %s"
        (codex-ide-session-current-turn-id session))
       (codex-ide--run-session-event
        'turn-started
        session
        :turn-id (codex-ide-session-current-turn-id session))
       (codex-ide--update-header-line session)
       (unless (codex-ide-session-output-prefix-inserted session)
         (codex-ide--begin-turn-display session)))
      ("item/started"
       (when-let ((item (alist-get 'item params)))
         (when (codex-ide--remember-or-request-model-name session item)
           (codex-ide--update-header-line session))
         (codex-ide-log-message
          session
          "Item started: %s (%s)"
          (alist-get 'id item)
          (alist-get 'type item))
         (when (equal (alist-get 'type item) "fileChange")
           (codex-ide--put-current-turn-file-change
            session
            (alist-get 'id item)
            item))
         (codex-ide--render-item-start session item)))
      ("item/agentMessage/delta"
       (let ((item-id (alist-get 'itemId params))
             (delta (or (alist-get 'delta params) "")))
         (let ((codex-ide--current-agent-item-type "agentMessage"))
           (when (and codex-ide-log-stream-deltas
                      (not (string-empty-p delta)))
             (codex-ide-log-message
              session
              "Agent delta for item %s (%d chars)"
              item-id
              (length delta)))
           (codex-ide--ensure-agent-message-prefix session item-id)
           (codex-ide--append-agent-text buffer delta)
           (when codex-ide-renderer-render-markdown-during-streaming
             (codex-ide--render-current-agent-message-markdown-streaming
              session
              item-id)))))
      ("item/commandExecution/outputDelta"
       (let ((item-id (alist-get 'itemId params))
             (delta (or (alist-get 'delta params) "")))
         (when codex-ide-log-stream-deltas
           (codex-ide-log-message
            session
            "Command output delta for item %s (%d chars)"
            item-id
            (length delta)))
         (unless (string-empty-p delta)
           (let* ((state (or (codex-ide--item-state session item-id) '()))
                  (state (plist-put
                          state
                          :output-text
                          (concat (or (plist-get state :output-text) "")
                                  delta))))
             (if (or (plist-get state :summary)
                     (plist-get state :command-output-overlay))
                 (progn
                   (codex-ide--put-item-state session item-id state)
                   (codex-ide--render-command-output-delta session item-id delta))
               (codex-ide--put-item-state
                session
                item-id
                (plist-put
                 state
                 :pending-output-text
                 (concat (or (plist-get state :pending-output-text) "")
                         delta))))))))
      ("item/fileChange/outputDelta"
       (let ((item-id (alist-get 'itemId params))
             (delta (or (alist-get 'delta params) "")))
         (when codex-ide-log-stream-deltas
           (codex-ide-log-message
            session
            "File-change delta for item %s (%d chars)"
            item-id
            (length delta)))
         (when-let ((state (codex-ide--item-state session item-id)))
           (codex-ide--put-item-state
            session
            item-id
            (plist-put state :diff-text
                       (concat (or (plist-get state :diff-text) "") delta))))
         (unless (string-empty-p delta)
           (codex-ide--put-current-turn-file-change session item-id nil delta))))
      ("item/plan/delta"
       (when codex-ide-log-stream-deltas
         (codex-ide-log-message
          session
          "Plan delta (%d chars)"
          (length (or (alist-get 'delta params) ""))))
       (codex-ide--render-plan-delta session params))
      ("item/reasoning/summaryTextDelta"
       (when codex-ide-log-stream-deltas
         (codex-ide-log-message
          session
          "Reasoning summary delta (%d chars)"
          (length (or (alist-get 'delta params)
                      (alist-get 'text params)
                      ""))))
       (codex-ide--render-reasoning-delta session params))
      ("item/completed"
       (when-let ((item (alist-get 'item params)))
         (when (codex-ide--remember-or-request-model-name session item)
           (codex-ide--update-header-line session))
         (codex-ide-log-message
          session
          "Item completed: %s (%s, status=%s)"
          (alist-get 'id item)
          (alist-get 'type item)
          (alist-get 'status item))
         (when (equal (alist-get 'type item) "fileChange")
           (codex-ide--put-current-turn-file-change
            session
            (alist-get 'id item)
            item))
         (codex-ide--render-item-completion session item)))
      ("turn/completed"
       (let ((interrupted (codex-ide-session-interrupt-requested session))
             (turn-id (codex-ide-session-current-turn-id session)))
         (codex-ide-log-message
          session
          "Turn completed: %s"
          turn-id)
         (if turn-id
             (progn
               (codex-ide--mark-current-turn-diff-completed session)
               (when interrupted
                 (codex-ide-log-message session "Turn completed after interrupt request"))
               (codex-ide--finish-turn
                session
                (when interrupted "[Agent interrupted]"))
               (codex-ide--maybe-submit-queued-prompt session))
           (codex-ide-log-message
            session
            "Ignoring duplicate turn/completed notification for an already-closed turn"))))
      ("error"
       (let* ((codex-ide--current-agent-item-type "error")
              (info (codex-ide--notification-error-info params))
              (message (codex-ide--notification-error-message info))
              (details (codex-ide--notification-error-additional-details info))
              (detail (codex-ide--notification-error-display-detail info))
              (classification
               (codex-ide--classify-session-error
                detail
                (alist-get 'http-status info))))
         (codex-ide-log-message session "Error notification: %S" params)
         (if (alist-get 'will-retry info)
             (codex-ide--handle-retryable-notification-error session info)
           (progn
             (codex-ide--render-session-error
              session
              (list message (alist-get 'http-status info))
              "Codex notification")
             (codex-ide--append-notification-additional-details session details)
             (codex-ide--recover-from-session-error session classification)))))
      ((or "notifications/elicitation/complete"
           "mcpServer/elicitation/complete")
       (codex-ide-log-message
        session
        "Elicitation completed: %s"
        (alist-get 'elicitationId params))
       (codex-ide--append-to-buffer
        buffer
        (format "\n[%s]\n"
                (codex-ide-mcp-elicitation-format-completion params))
        'shadow))
      (_ nil))))

(defun codex-ide--queued-prompts (session)
  "Return SESSION's queued prompt entries."
  (or (codex-ide--session-metadata-get session :queued-prompts)
      '()))

(defun codex-ide--set-queued-prompts (session prompts)
  "Set SESSION's queued prompt entries to PROMPTS."
  (codex-ide--session-metadata-put session :queued-prompts prompts))

(defun codex-ide--queued-prompt-p (session)
  "Return non-nil when SESSION has at least one queued prompt."
  (consp (codex-ide--queued-prompts session)))

(defun codex-ide--queued-prompt-entry (prompt payload)
  "Return a queued prompt entry for PROMPT and PAYLOAD."
  (list :prompt prompt :payload payload))

(defun codex-ide--clear-queued-prompts (session)
  "Clear SESSION's queued prompt metadata."
  (codex-ide--set-queued-prompts session nil)
  (codex-ide--session-metadata-put session :queued-prompt nil)
  (codex-ide--session-metadata-put session :queued-prompt-payload nil)
  (codex-ide--session-metadata-put session :queued-prompt-start-marker nil))

(defun codex-ide--prompt-for-submission (session prompt)
  "Return prompt text for SESSION using explicit PROMPT or the active input."
  (or prompt
      (if (eq (current-buffer) (codex-ide-session-buffer session))
          (codex-ide--current-input session)
        (read-string "Codex prompt: "))))

(defun codex-ide--send-turn-start (session thread-id payload)
  "Send a `turn/start` request for SESSION THREAD-ID using PAYLOAD."
  (when-let ((effort (codex-ide-config-effective-value 'reasoning-effort session)))
    (codex-ide--session-metadata-put
     session
     :reasoning-effort
     effort))
  (codex-ide--request-sync
   session
   "turn/start"
   `((threadId . ,thread-id)
     ,@(when-let ((approval-policy
                   (codex-ide-config-effective-value 'approval-policy session)))
         `((approvalPolicy . ,approval-policy)))
     ,@(when-let ((sandbox-policy
                   (codex-ide--turn-start-sandbox-policy session)))
         `((sandboxPolicy . ,sandbox-policy)))
     ,@(when-let ((model (codex-ide-config-effective-value 'model session)))
         `((model . ,model)))
     ,@(when-let ((effort (codex-ide-config-effective-value
                           'reasoning-effort
                           session)))
         `((effort . ,effort)))
     ,@(when-let ((personality
                   (codex-ide-config-effective-value 'personality session)))
         `((personality . ,personality)))
     (input . ,(alist-get 'input payload)))))

(defun codex-ide--after-turn-start-submitted (session payload)
  "Update SESSION state after successfully submitting PAYLOAD."
  (when-let ((model (codex-ide-config-effective-value 'model session)))
    (codex-ide--set-session-model-name session model)
    (codex-ide--update-header-line session))
  (codex-ide--mark-session-prompt-submitted session)
  (when (alist-get 'included-session-context payload)
    (codex-ide--session-metadata-put session :session-context-sent t)))

(defun codex-ide--submit-queued-prompt (session)
  "Submit SESSION's next queued prompt as a new turn."
  (let* ((queue (codex-ide--queued-prompts session))
         (entry (car queue))
         (prompt (plist-get entry :prompt))
         (payload (plist-get entry :payload))
         (thread-id (codex-ide-session-thread-id session))
         (draft (and (codex-ide--input-prompt-active-p session)
                     (codex-ide--current-input session))))
    (unless (and prompt payload)
      (error "No queued Codex prompt"))
    (codex-ide--set-queued-prompts session (cdr queue))
    (codex-ide-log-message
     session
     "Submitting queued prompt to thread %s (%d chars)"
     thread-id
     (length prompt))
    (codex-ide--delete-running-input-list session)
    (if (codex-ide--input-prompt-active-p session)
        (codex-ide--replace-current-input session prompt)
      (codex-ide--insert-input-prompt session prompt))
    (codex-ide--begin-turn-display session (alist-get 'context-summary payload))
    (when (and draft (not (string-empty-p draft)))
      (codex-ide--replace-current-input session draft))
    (codex-ide--refresh-running-input-display session)
    (redisplay)
    (condition-case err
        (progn
          (codex-ide--send-turn-start session thread-id payload)
          (codex-ide--after-turn-start-submitted session payload))
      (error
       (codex-ide-log-message session "Queued prompt submission failed: %s"
                              (error-message-string err))
       (codex-ide--reopen-input-after-submit-error session prompt err)
       (signal (car err) (cdr err))))))

(defun codex-ide--maybe-submit-queued-prompt (session)
  "Submit SESSION's queued prompt if one exists."
  (when (codex-ide--queued-prompt-p session)
    (codex-ide--submit-queued-prompt session)))

;;;###autoload
(defun codex-ide-prompt ()
  "Prompt for a Codex message in the minibuffer and submit it from the Codex buffer."
  (interactive)
  (let ((origin-buffer (current-buffer))
        (session (codex-ide--ensure-session-for-current-project)))
    (let* ((buffer (codex-ide-session-buffer session))
           (prompt (read-from-minibuffer
                    (format "Send prompt (%s): " (buffer-name buffer)))))
      (unless (string-empty-p prompt)
        (let ((window (codex-ide-display-buffer
                       buffer
                       codex-ide--display-buffer-other-window-pop-up-action)))
          (with-selected-window window
            (with-current-buffer buffer
              (if (codex-ide-session-input-overlay session)
                  (codex-ide--replace-current-input session prompt)
                (codex-ide--insert-input-prompt session prompt))
              (let ((codex-ide--prompt-origin-buffer origin-buffer))
                (codex-ide--submit-prompt)))))))))

;;;###autoload
(defun codex-ide-previous-prompt-history ()
  "Replace the current prompt with the previous prompt from history."
  (interactive)
  (codex-ide--browse-prompt-history -1))

;;;###autoload
(defun codex-ide-next-prompt-history ()
  "Replace the current prompt with the next prompt from history."
  (interactive)
  (codex-ide--browse-prompt-history 1))

;;;###autoload
(defun codex-ide-previous-prompt-line ()
  "Jump to the previous user prompt line in the session buffer."
  (interactive)
  (codex-ide--goto-prompt-line -1))

;;;###autoload
(defun codex-ide-next-prompt-line ()
  "Jump to the next user prompt line in the session buffer."
  (interactive)
  (codex-ide--goto-prompt-line 1))

(defun codex-ide--ensure-submittable-prompt (prompt)
  "Signal a user error unless PROMPT has content."
  (when (string-empty-p prompt)
    (user-error "Prompt is empty")))

(defun codex-ide--running-prompt-payload (session prompt)
  "Build turn payload for PROMPT from SESSION's buffer."
  (with-current-buffer (codex-ide-session-buffer session)
    (codex-ide--compose-turn-payload prompt)))

(defun codex-ide--prepare-running-prompt (session prompt)
  "Record and freeze PROMPT for SESSION while a turn is running."
  (codex-ide--ensure-submittable-prompt prompt)
  (codex-ide--push-prompt-history session prompt)
  (codex-ide--register-submitted-turn-prompt session prompt)
  (let ((payload (codex-ide--running-prompt-payload session prompt)))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (codex-ide--insert-input-prompt session prompt))
    (codex-ide--freeze-active-input-prompt
     session
     (alist-get 'context-summary payload))
    payload))

(defun codex-ide--steer-prompt (&optional prompt)
  "Submit PROMPT as steering input for the active Codex turn."
  (let* ((session (codex-ide--session-for-current-project))
         (thread-id (codex-ide-session-thread-id session))
         (turn-id (codex-ide-session-current-turn-id session))
         (prompt-to-send (codex-ide--prompt-for-submission session prompt))
         payload)
    (unless turn-id
      (user-error "No active Codex turn to steer"))
    (unless thread-id
      (user-error "Codex session has no active thread"))
    (setq payload (codex-ide--prepare-running-prompt session prompt-to-send))
    (codex-ide-log-message
     session
     "Steering turn %s (%d chars)"
     turn-id
     (length prompt-to-send))
    (condition-case err
        (progn
          (codex-ide--request-sync
           session
           "turn/steer"
           `((threadId . ,thread-id)
             (expectedTurnId . ,turn-id)
             (input . ,(alist-get 'input payload))))
          (codex-ide--mark-session-prompt-submitted session)
          (when (alist-get 'included-session-context payload)
            (codex-ide--session-metadata-put session :session-context-sent t))
          (codex-ide--refresh-running-input-display session)
          (message "Sent steering input to Codex"))
      (error
       (codex-ide-log-message session "Steering prompt failed: %s"
                              (error-message-string err))
       (codex-ide--reopen-input-after-submit-error session prompt-to-send err)
       (signal (car err) (cdr err))))))

(defun codex-ide--queue-prompt (&optional prompt)
  "Queue PROMPT to run after the active Codex turn finishes."
  (let* ((session (codex-ide--session-for-current-project))
         (thread-id (codex-ide-session-thread-id session))
         (turn-id (codex-ide-session-current-turn-id session))
         (prompt-to-send (codex-ide--prompt-for-submission session prompt))
         payload)
    (unless turn-id
      (user-error "No active Codex turn to queue behind"))
    (unless thread-id
      (user-error "Codex session has no active thread"))
    (codex-ide--ensure-submittable-prompt prompt-to-send)
    (codex-ide--push-prompt-history session prompt-to-send)
    (setq payload (codex-ide--running-prompt-payload session prompt-to-send))
    (codex-ide--set-queued-prompts
     session
     (append (codex-ide--queued-prompts session)
             (list (codex-ide--queued-prompt-entry prompt-to-send payload))))
    (when (alist-get 'included-session-context payload)
      (codex-ide--session-metadata-put session :session-context-sent t))
    (when (eq (current-buffer) (codex-ide-session-buffer session))
      (codex-ide--replace-current-input session ""))
    (codex-ide--refresh-running-input-display session)
    (codex-ide-log-message
     session
     "Queued prompt after turn %s (%d chars)"
     turn-id
     (length prompt-to-send))
    (message "Queued prompt for the next Codex turn")))

(defun codex-ide--submit-prompt (&optional prompt)
  "Submit PROMPT to the current Codex session."
  (interactive)
  (let* ((session (codex-ide--session-for-current-project))
         (thread-id (codex-ide-session-thread-id session))
         (prompt-to-send (codex-ide--prompt-for-submission session prompt))
         payload)
    (if (codex-ide-session-current-turn-id session)
        (pcase codex-ide-running-submit-action
          ('queue (codex-ide--queue-prompt prompt-to-send))
          (_ (codex-ide--steer-prompt prompt-to-send)))
      (unless thread-id
        (user-error "Codex session has no active thread"))
      (codex-ide--ensure-submittable-prompt prompt-to-send)
      (codex-ide--push-prompt-history session prompt-to-send)
      (codex-ide--register-submitted-turn-prompt session prompt-to-send)
      (codex-ide-log-message
       session
       "Sending prompt to thread %s (%d chars)"
       thread-id
       (length prompt-to-send))
      (unless (eq (current-buffer) (codex-ide-session-buffer session))
        (codex-ide--insert-input-prompt session prompt-to-send))
      (setq payload
            (with-current-buffer (codex-ide-session-buffer session)
              (codex-ide--compose-turn-payload prompt-to-send)))
      (codex-ide--begin-turn-display session (alist-get 'context-summary payload))
      (redisplay)
      (condition-case err
          (progn
            (codex-ide--send-turn-start session thread-id payload)
            (codex-ide--after-turn-start-submitted session payload))
        (error
         (codex-ide-log-message session "Prompt submission failed: %s" (error-message-string err))
         (codex-ide--reopen-input-after-submit-error session prompt-to-send err)
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun codex-ide-submit ()
  "Submit the current in-buffer prompt to Codex."
  (interactive)
  (codex-ide--submit-prompt))

;;;###autoload
(defun codex-ide-steer ()
  "Submit the current prompt as steering input to the active Codex turn."
  (interactive)
  (codex-ide--steer-prompt))

;;;###autoload
(defun codex-ide-queue ()
  "Queue the current prompt as the next Codex turn."
  (interactive)
  (codex-ide--queue-prompt))

(defun codex-ide-transcript-append-to-buffer (buffer text &optional face properties)
  "Append TEXT to BUFFER as transcript text."
  (codex-ide--append-to-buffer buffer text face properties))

(defun codex-ide-transcript-append-agent-text (buffer text &optional face properties)
  "Append agent-originated TEXT to BUFFER."
  (codex-ide--append-agent-text buffer text face properties))

(defun codex-ide-transcript-update-header-line (&optional session)
  "Refresh the header line for SESSION."
  (codex-ide--update-header-line session))

(defun codex-ide-transcript-render-item-start (&optional session item)
  "Render a newly started ITEM for SESSION."
  (codex-ide--render-item-start session item))

(defun codex-ide-transcript-render-plan-delta (&optional session params)
  "Render a plan delta PARAMS for SESSION."
  (codex-ide--render-plan-delta session params))

(defun codex-ide-transcript-render-reasoning-delta (&optional session params)
  "Render a reasoning summary delta PARAMS for SESSION."
  (codex-ide--render-reasoning-delta session params))

(defun codex-ide-transcript-render-item-completion (&optional session item)
  "Render any completion-only details for ITEM in SESSION."
  (codex-ide--render-item-completion session item))

(defun codex-ide-transcript-ensure-agent-message-prefix (&optional session item-id)
  "Ensure the assistant message prefix has been inserted for ITEM-ID in SESSION."
  (codex-ide--ensure-agent-message-prefix session item-id))

(defun codex-ide-transcript-render-current-agent-message-markdown
    (&optional session item-id allow-trailing-tables)
  "Render the current assistant message for SESSION."
  (codex-ide--render-current-agent-message-markdown
   session
   item-id
   allow-trailing-tables))

(defun codex-ide-transcript-render-current-agent-message-markdown-streaming
    (&optional session item-id)
  "Incrementally render stream-safe markdown for SESSION's current message."
  (codex-ide--render-current-agent-message-markdown-streaming session item-id))

(defun codex-ide-transcript-render-session-error (session values &optional prefix face)
  "Render session error VALUES for SESSION with PREFIX using FACE."
  (codex-ide--render-session-error session values prefix face))

(defun codex-ide-transcript-finish-turn (&optional session closing-note)
  "Reset SESSION after a turn and reopen the prompt."
  (codex-ide--finish-turn session closing-note))

(defun codex-ide-transcript-restore-thread-read-transcript (&optional session thread-read)
  "Replay a stored transcript from THREAD-READ into SESSION."
  (codex-ide--restore-thread-read-transcript session thread-read))

(provide 'codex-ide-transcript)

;;; codex-ide-transcript.el ends here
