;;; codex-ide-session-mode.el --- Session buffer modes for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns the Emacs major/minor modes used by live Codex session
;; buffers.
;;
;; Its job is intentionally narrow:
;;
;; - Define the major mode used by transcript buffers.
;; - Define the prompt-only minor mode and its keymap.
;; - Define navigation integration for transcript buttons and active input.
;; - Keep prompt-editing mode synchronized with the current point location.
;;
;; It does not own session lifecycle, prompt submission, or transcript mutation.
;; Those higher-level concerns live in the session and transcript controller
;; modules.  This separation keeps mode setup reloadable and minimizes the
;; amount of stateful logic tied directly to Emacs mode activation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'codex-ide-approvals-data)
(require 'codex-ide-core)
(require 'codex-ide-diff-data)
(require 'codex-ide-mention)
(require 'codex-ide-nav)
(require 'codex-ide-renderer)
(require 'codex-ide-slash-command)
(require 'imenu)

(autoload 'codex-ide-session-diff-transcript-point-changed
  "codex-ide-diff-view" nil nil)
(autoload 'codex-ide-session-diff-open
  "codex-ide-diff-view" nil t)
(autoload 'codex-ide-apply-config-preset
  "codex-ide-transient" nil t)
(autoload 'codex-ide-monitor-promote-rail-session-1
  "codex-ide-monitor" nil t)
(autoload 'codex-ide-monitor-promote-rail-session-2
  "codex-ide-monitor" nil t)
(autoload 'codex-ide-monitor-promote-rail-session-3
  "codex-ide-monitor" nil t)

(defvar codex-ide-session-enable-visual-line-mode)
(defvar corfu-mode)
(defvar corfu-on-exact-match)

(defconst codex-ide-session-mode--transcript-detail-kind-property
  'codex-ide-transcript-detail-kind
  "Text property identifying semantic transcript detail regions.")

(defconst codex-ide-session-mode--transcript-item-detail-kind
  'item-detail
  "Detail kind used for compact-hideable transcript rows.")

(defconst codex-ide-session-mode--transcript-compact-hidden
  'codex-ide-transcript-compact-hidden
  "Invisible property value used for compact transcript detail rows.")

(defvar codex-ide-session-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    map)
  "Keymap for `codex-ide-session-mode'.")

(defvar codex-ide-session-prompt-minor-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-prompt-minor-mode'.")

(defvar codex-ide-session-slash-command-minor-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-slash-command-minor-mode'.")

(defvar codex-ide-session-mention-minor-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-mention-minor-mode'.")

(defvar codex-ide-session-approval-minor-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-approval-minor-mode'.")

(define-key codex-ide-session-mode-map (kbd "C-c C-c") #'codex-ide-interrupt)
(define-key codex-ide-session-mode-map (kbd "C-c RET") #'codex-ide-submit)
(define-key codex-ide-session-mode-map (kbd "C-c C-d") #'codex-ide-session-diff-open)
(define-key codex-ide-session-mode-map (kbd "C-c C-k") #'codex-ide-interrupt)
(define-key codex-ide-session-mode-map (kbd "C-c C-p") #'codex-ide-apply-config-preset)
(define-key codex-ide-session-mode-map (kbd "C-c C-v") #'codex-ide-session-transcript-toggle-detail-level)
(define-key codex-ide-session-mode-map (kbd "C-c 1") #'codex-ide-monitor-promote-rail-session-1)
(define-key codex-ide-session-mode-map (kbd "C-c 2") #'codex-ide-monitor-promote-rail-session-2)
(define-key codex-ide-session-mode-map (kbd "C-c 3") #'codex-ide-monitor-promote-rail-session-3)
(define-key codex-ide-session-mode-map (kbd "C-M-p") #'codex-ide-previous-prompt-line)
(define-key codex-ide-session-mode-map (kbd "C-M-n") #'codex-ide-next-prompt-line)
(define-key codex-ide-session-mode-map (kbd "TAB") #'codex-ide-session-mode-nav-forward)
(define-key codex-ide-session-mode-map (kbd "<backtab>") #'codex-ide-session-mode-nav-backward)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "M-p") #'codex-ide-previous-prompt-history)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "M-n") #'codex-ide-next-prompt-history)
(define-key codex-ide-session-slash-command-minor-mode-map
            (kbd "RET")
            #'codex-ide-slash-command-complete-or-submit)
(define-key codex-ide-session-mention-minor-mode-map
            (kbd "RET")
            #'codex-ide-mention-complete-or-newline)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "DEL") #'codex-ide-delete-backward-or-remove-attached-image)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "<backspace>") #'codex-ide-delete-backward-or-remove-attached-image)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "<delete>") #'codex-ide-delete-forward-or-remove-attached-image)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "C-d") #'codex-ide-delete-forward-or-remove-attached-image)
(dotimes (index 9)
  (define-key codex-ide-session-approval-minor-mode-map
              (number-to-string (1+ index))
              #'codex-ide-session-approval-dispatch))
(define-key codex-ide-session-approval-minor-mode-map
            [remap self-insert-command]
            #'codex-ide-session-approval-blocked-input)
(define-key codex-ide-session-approval-minor-mode-map
            [remap codex-ide-submit]
            #'codex-ide-session-approval-blocked-input)
(define-key codex-ide-session-approval-minor-mode-map
            [remap codex-ide-interrupt]
            #'codex-ide-session-approval-blocked-input)
(define-key codex-ide-session-approval-minor-mode-map
            [remap codex-ide-previous-prompt-history]
            #'codex-ide-session-approval-blocked-input)
(define-key codex-ide-session-approval-minor-mode-map
            [remap codex-ide-next-prompt-history]
            #'codex-ide-session-approval-blocked-input)

(defvar-local codex-ide-session-mode--last-point nil
  "Last observed point used for transcript tail-follow navigation tracking.")

(defvar-local codex-ide-session-mode--last-window-start nil
  "Last observed `window-start' for transcript tail-follow navigation tracking.")

(defvar codex-ide-session-mode--theme-refresh-buffers nil
  "Live buffers currently using `codex-ide-session-mode' theme refresh hooks.")

(defvar-local codex-ide-session-mode--slash-command-auto-completing nil
  "Non-nil while slash command auto-completion is invoking completion.")

(defvar-local codex-ide-session-mode--mention-auto-completing nil
  "Non-nil while mention auto-completion is invoking completion.")

(defcustom codex-ide-session-transcript-default-detail-level 'standard
  "Default detail level for Codex session transcript buffers."
  :type '(choice (const :tag "Standard" standard)
                 (const :tag "Compact" compact))
  :group 'codex-ide)

(defvar-local codex-ide-session-transcript-detail-level
    codex-ide-session-transcript-default-detail-level
  "Current detail level for this Codex session transcript buffer.
The value is either `standard' or `compact'.")

(define-minor-mode codex-ide-session-prompt-minor-mode
  "Minor mode enabled only while point is in the active Codex prompt."
  :lighter " Prompt"
  :keymap codex-ide-session-prompt-minor-mode-map)

(define-minor-mode codex-ide-session-slash-command-minor-mode
  "Minor mode enabled while editing a Codex slash command prompt."
  :lighter " Slash"
  :keymap codex-ide-session-slash-command-minor-mode-map)

(define-minor-mode codex-ide-session-mention-minor-mode
  "Minor mode enabled while editing a Codex prompt mention."
  :lighter " Mention"
  :keymap codex-ide-session-mention-minor-mode-map)

(define-minor-mode codex-ide-session-approval-minor-mode
  "Minor mode enabled while a Codex approval is pending."
  :lighter " Approval"
  :keymap codex-ide-session-approval-minor-mode-map)

(defun codex-ide-session-mode--active-approval (&optional session)
  "Return SESSION's active approval record, if any."
  (seq-find
   (lambda (approval)
     (not (eq (plist-get approval :kind) 'elicitation)))
   (and session (codex-ide-approvals-data-active-list session))))

(defun codex-ide-session-approval--actions (&optional session)
  "Return active keyboard approval actions for SESSION."
  (when-let* ((approval (codex-ide-session-mode--active-approval session)))
    (codex-ide-approvals-data-view-get approval :key-actions)))

(defun codex-ide-session-approval-blocked-input ()
  "Notify that normal input is blocked by a pending Codex approval."
  (interactive)
  (message "Resolve or cancel the pending Codex approval first"))

(defun codex-ide-session-approval-dispatch ()
  "Dispatch the numeric approval action for the pressed key."
  (interactive)
  (let* ((event (event-basic-type last-command-event))
         (session (and (boundp 'codex-ide--session) codex-ide--session))
         (digit (and (characterp event)
                     (>= event ?1)
                     (<= event ?9)
                     (- event ?0)))
         (action (and digit
                      (nth (1- digit)
                           (codex-ide-session-approval--actions session)))))
    (if-let* ((function (and digit (plist-get action :function))))
        (funcall function)
      (codex-ide-session-approval-blocked-input))))

(defun codex-ide-session-mode--approval-pending-p (&optional session)
  "Return non-nil when SESSION has pending approval work."
  (and (codex-ide-session-mode--active-approval session) t))

(defun codex-ide-session-mode-sync-approval-minor-mode (&optional session)
  "Enable or disable `codex-ide-session-approval-minor-mode' for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (and session (derived-mode-p 'codex-ide-session-mode))
    (let ((pending (codex-ide-session-mode--approval-pending-p session)))
      (unless (eq pending codex-ide-session-approval-minor-mode)
        (codex-ide-session-approval-minor-mode (if pending 1 -1))))))

(defun codex-ide--point-in-active-prompt-p (&optional session pos)
  "Return non-nil when POS is inside SESSION's active prompt region."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (setq pos (or pos (point)))
  (when-let* ((overlay (and session (codex-ide-session-input-overlay session))))
    (let ((start (overlay-start overlay))
          (end (overlay-end overlay)))
      (and start
           end
           (<= start pos)
           (<= pos end)))))

(defun codex-ide-session-mode--input-end-position (&optional session)
  "Return SESSION's editable input end position, if available."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let* ((marker (and session
                           (codex-ide--session-metadata-get
                            session
                            :input-end-marker))))
    (let ((buffer (and session (codex-ide-session-buffer session))))
      (when (and (buffer-live-p buffer)
                 (markerp marker)
                 (eq (marker-buffer marker) buffer))
        (marker-position marker)))))

(defun codex-ide--sync-prompt-minor-mode (&optional session)
  "Enable or disable `codex-ide-session-prompt-minor-mode' for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (and session (derived-mode-p 'codex-ide-session-mode))
    (when-let* ((input-end (codex-ide-session-mode--input-end-position session)))
      (when (and (codex-ide--point-in-active-prompt-p session)
                 (> (point) input-end))
        (goto-char input-end)))
    (let ((inside (and (not (codex-ide-session-mode--approval-pending-p session))
                       (codex-ide--point-in-active-prompt-p session))))
      (unless (eq inside codex-ide-session-prompt-minor-mode)
        (codex-ide-session-prompt-minor-mode (if inside 1 -1))))))

(defun codex-ide-session-mode--slash-command-input-p (&optional session)
  "Return non-nil when SESSION's active prompt begins with a slash."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when-let* ((input-start (and session
                                (codex-ide-session-input-start-marker session)))
              (input-end (codex-ide-session-mode--input-end-position session))
              (buffer (and session (codex-ide-session-buffer session))))
    (and (buffer-live-p buffer)
         (markerp input-start)
         (eq (marker-buffer input-start) buffer)
         (< (marker-position input-start) input-end)
         (with-current-buffer buffer
           (eq (char-after input-start) ?/)))))

(defun codex-ide-session-mode-sync-slash-command-minor-mode (&optional session)
  "Enable or disable slash command prompt editing mode for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (derived-mode-p 'codex-ide-session-mode)
    (let ((active (and session
                       codex-ide-session-prompt-minor-mode
                       (codex-ide-session-mode--slash-command-input-p session))))
      (unless (eq active codex-ide-session-slash-command-minor-mode)
        (codex-ide-session-slash-command-minor-mode (if active 1 -1))))))

(defun codex-ide-session-mode--mention-input-p (&optional session)
  "Return non-nil when point is in a mention completion context."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (and session
       (codex-ide-mention-active-completion-p session)))

(defun codex-ide-session-mode-sync-mention-minor-mode (&optional session)
  "Enable or disable prompt mention editing mode for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (derived-mode-p 'codex-ide-session-mode)
    (let ((active (and session
                       codex-ide-session-prompt-minor-mode
                       (codex-ide-session-mode--mention-input-p session))))
      (unless (eq active codex-ide-session-mention-minor-mode)
        (codex-ide-session-mention-minor-mode (if active 1 -1))))))

(defun codex-ide-session-mode--show-slash-command-completions ()
  "Show slash command completions using the active completion frontend."
  (let ((codex-ide-slash-command--suppress-completion-submit t))
    (if (bound-and-true-p corfu-mode)
        (when-let* ((capf (codex-ide-slash-command-completion-at-point)))
          (let ((completion-extra-properties (nthcdr 3 capf))
                (corfu-on-exact-match 'show))
            (completion-in-region
             (nth 0 capf)
             (nth 1 capf)
             (codex-ide-session-mode--preserve-sole-completion-prefix
              (nth 2 capf)))))
      (completion-help-at-point))))

(defun codex-ide-session-mode--show-mention-completions ()
  "Show mention completions using the active completion frontend."
  (if (bound-and-true-p corfu-mode)
      (when-let* ((capf (codex-ide-mention-completion-at-point)))
        (let ((completion-extra-properties (nthcdr 3 capf))
              (corfu-on-exact-match 'show))
          (completion-in-region
           (nth 0 capf)
           (nth 1 capf)
           (codex-ide-session-mode--preserve-sole-completion-prefix
            (nth 2 capf)))))
    (completion-help-at-point)))

(defun codex-ide-session-mode--preserve-sole-completion-prefix (table)
  "Return completion TABLE with sole-match prefix expansion disabled.
The wrapped table preserves normal candidate listing, metadata, and exact-match
checks.  It only changes `try-completion' so automatic popup display does not
replace a partial slash command with the sole matching command name."
  (lambda (string pred action)
    (if action
        (complete-with-action action table string pred)
      (let ((result (complete-with-action action table string pred)))
        (if (and (or (stringp result)
                     (and (consp result)
                          (stringp (car result))))
                 (not (string= (if (consp result) (car result) result)
                               string))
                 (let ((matches (all-completions string table pred)))
                   (and (consp matches)
                        (null (cdr matches)))))
            (if (consp result)
                (cons string (min (or (cdr result) (length string))
                                  (length string)))
              string)
          result)))))

(defun codex-ide-session-mode--maybe-complete-slash-command ()
  "Automatically show completion while typing a prompt slash command."
  (codex-ide-session-mode-sync-slash-command-minor-mode)
  (when (and codex-ide-session-slash-command-minor-mode
             (not codex-ide-session-mode--slash-command-auto-completing)
             (not (bound-and-true-p completion-in-region-mode))
             (codex-ide-slash-command-completion-at-point))
    (let ((codex-ide-session-mode--slash-command-auto-completing t))
      (codex-ide-session-mode--show-slash-command-completions))))

(defun codex-ide-session-mode--maybe-complete-mention ()
  "Automatically show completion while typing a prompt mention."
  (codex-ide-session-mode-sync-mention-minor-mode)
  (when (and codex-ide-session-mention-minor-mode
             (not codex-ide-session-mode--mention-auto-completing)
             (not (bound-and-true-p completion-in-region-mode))
             (codex-ide-mention-completion-at-point))
    (let ((codex-ide-session-mode--mention-auto-completing t))
      (codex-ide-session-mode--show-mention-completions))))

(defun codex-ide-session-mode--focal-points ()
  "Return focal points for the current session buffer."
  (let ((session (and (boundp 'codex-ide--session) codex-ide--session)))
    (append (codex-ide-session-mode--collect-prompt-starts session)
            (codex-ide-nav-collect-buttons)
            (and session
                 (codex-ide-nav-collect-session-input session)))))

(defun codex-ide-session-mode--collect-prompt-starts (&optional session)
  "Return submitted prompt starts for the current session buffer.

SESSION's active editable prompt is excluded because input navigation should
land at the editable input start rather than on the read-only prompt prefix."
  (let ((active-prompt-start
         (when-let* ((marker (and session
                                  (codex-ide-session-input-prompt-start-marker
                                   session))))
           (when (and (markerp marker)
                      (eq (marker-buffer marker) (current-buffer)))
             (marker-position marker))))
        points)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (codex-ide-renderer-line-has-prompt-start-p)
          (let ((start (line-beginning-position))
                (end (line-end-position))
                (input-start
                 (save-excursion
                   (goto-char (line-beginning-position))
                   (if (looking-at-p "> ")
                       (+ (line-beginning-position) 2)
                     (line-beginning-position)))))
            (unless (and active-prompt-start
                         (= start active-prompt-start))
              (push (list :pos input-start
                          :start start
                          :end (max (1+ start) end)
                          :kind 'prompt-start)
                    points))))
        (forward-line 1)))
    (nreverse points)))

(defun codex-ide-session-mode--prompt-end-position (start)
  "Return the best known end position for the prompt beginning at START."
  (let* ((session (and (boundp 'codex-ide--session) codex-ide--session))
         (input-start (and session
                           (codex-ide-session-input-prompt-start-marker
                            session)))
         (active-p (and (markerp input-start)
                        (eq (marker-buffer input-start) (current-buffer))
                        (= (marker-position input-start) start)))
         (face-end (next-single-char-property-change
                    start 'face nil (point-max))))
    (cond
     ((and active-p
           (codex-ide-session-mode--input-end-position session)))
     ((> face-end (+ start 2))
      face-end)
     (t
      (save-excursion
        (goto-char start)
        (line-end-position))))))

(defun codex-ide-session-mode--imenu-label (text)
  "Return TEXT normalized for `imenu' display."
  (replace-regexp-in-string
   "[ \t]+"
   " "
   (replace-regexp-in-string "[[:space:]]*[\n\r]+[[:space:]]*" "↵"
                             (string-trim text))))

(defun codex-ide-session-mode--prompt-preview (start end fallback-number)
  "Return an `imenu' preview for prompt START..END.
Use FALLBACK-NUMBER when the prompt body is empty."
  (let* ((text (buffer-substring-no-properties start end))
         (body (codex-ide-session-mode--imenu-label
                (string-remove-prefix "> " text))))
    (if (string-empty-p body)
        (format "Prompt %d" fallback-number)
      body)))

(defun codex-ide-session-mode--imenu-create-index ()
  "Return a prompt-only `imenu' index for the current session buffer."
  (save-excursion
    (let ((count 0)
          entries)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (codex-ide-renderer-line-has-prompt-start-p)
          (let* ((start (line-beginning-position))
                 (input-start
                  (save-excursion
                    (goto-char start)
                    (if (looking-at-p "> ")
                        (+ start 2)
                      start)))
                 (end (codex-ide-session-mode--prompt-end-position start)))
            (setq count (1+ count))
            (push (cons (codex-ide-session-mode--prompt-preview
                         start
                         end
                         count)
                        (copy-marker input-start))
                  entries)))
        (forward-line 1))
      (nreverse entries))))

;;;###autoload
(defun codex-ide-session-mode-nav-forward ()
  "Move point to the next focal point in a Codex session buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-session-mode)
    (user-error "Not in a Codex session buffer"))
  (codex-ide-nav-forward))

;;;###autoload
(defun codex-ide-session-mode-nav-backward ()
  "Move point to the previous focal point in a Codex session buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-session-mode)
    (user-error "Not in a Codex session buffer"))
  (codex-ide-nav-backward))

(defun codex-ide--disable-session-font-lock ()
  "Disable buffer font-lock machinery for Codex transcript buffers."
  (when (fboundp 'jit-lock-mode)
    (jit-lock-mode nil))
  (when (fboundp 'font-lock-mode)
    (font-lock-mode -1)))

(defun codex-ide-session-mode--tail-follow-rejoined-p (&optional session)
  "Return non-nil when point has explicitly rejoined the live transcript tail.

Rejoining means point is at `point-max' or back inside SESSION's active prompt."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (or (= (point) (point-max))
      (codex-ide--point-in-active-prompt-p session)))

(defun codex-ide-session-mode--interactive-request-preserve-start (&optional session)
  "Return the earliest pending interactive-request start position for SESSION.

When non-nil, positions at or after the returned buffer location are treated as
part of the live interactive request zone and should preserve existing tail
follow state."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (let (start)
    (dolist (approval (and session
                           (codex-ide-approvals-data-active-list session)))
      (let ((marker (codex-ide-approvals-data-view-get approval :start-marker)))
        (when (and (markerp marker)
                   (eq (marker-buffer marker) (current-buffer)))
          (setq start (if start
                          (min start (marker-position marker))
                        (marker-position marker))))))
    start))

(defun codex-ide-session-mode--tail-follow-preserve-p (&optional session pos)
  "Return non-nil when POS should preserve the current tail-follow state.

This covers inline approval and elicitation regions rendered near the live tail,
so users can navigate within those controls without opting out of follow mode."
  (setq pos (or pos (point)))
  (when-let* ((start (codex-ide-session-mode--interactive-request-preserve-start
                      session)))
    (>= pos start)))

(defun codex-ide-session-mode--track-tail-follow-navigation ()
  "Track whether the selected transcript window has opted out of tail following."
  (when-let* ((window (and (derived-mode-p 'codex-ide-session-mode)
                           (eq (window-buffer (selected-window)) (current-buffer))
                           (selected-window))))
    (let ((session (and (boundp 'codex-ide--session) codex-ide--session))
          (point-pos (point))
          (window-start-pos (window-start window)))
      (unless (or (null codex-ide-session-mode--last-point)
                  (null codex-ide-session-mode--last-window-start))
        (when (or (/= point-pos codex-ide-session-mode--last-point)
                  (/= window-start-pos codex-ide-session-mode--last-window-start))
          (if (codex-ide-session-mode--tail-follow-rejoined-p session)
              (set-window-parameter window 'codex-ide-tail-follow-suspended nil)
            (unless (codex-ide-session-mode--tail-follow-preserve-p session point-pos)
              (set-window-parameter window 'codex-ide-tail-follow-suspended t)))))
      (setq codex-ide-session-mode--last-point point-pos
            codex-ide-session-mode--last-window-start window-start-pos))))

(defun codex-ide-session-mode--notify-diff-point-changed ()
  "Notify the canonical session diff buffer about transcript point movement."
  (when (derived-mode-p 'codex-ide-session-mode)
    (when-let* ((session (and (boundp 'codex-ide--session)
                              codex-ide--session)))
      (codex-ide-session-diff-transcript-point-changed
       session
       (codex-ide-diff-data-turn-id-at-point
        session
        (point)
        (current-buffer))))))

(defun codex-ide-session-mode--handle-theme-change (&rest _args)
  "Refresh session renderer faces after a theme change."
  (setq codex-ide-session-mode--theme-refresh-buffers
        (cl-remove-if-not #'buffer-live-p
                          codex-ide-session-mode--theme-refresh-buffers))
  (when codex-ide-session-mode--theme-refresh-buffers
    (codex-ide-renderer-schedule-theme-refresh)))

(defun codex-ide-session-mode--setup-theme-refresh ()
  "Subscribe the current session buffer to theme change events."
  (cl-pushnew (current-buffer) codex-ide-session-mode--theme-refresh-buffers)
  (codex-ide-renderer-schedule-theme-refresh)
  (add-hook 'enable-theme-functions #'codex-ide-session-mode--handle-theme-change)
  (add-hook 'disable-theme-functions #'codex-ide-session-mode--handle-theme-change)
  (add-hook 'kill-buffer-hook #'codex-ide-session-mode--teardown-theme-refresh nil t)
  (add-hook 'change-major-mode-hook #'codex-ide-session-mode--teardown-theme-refresh nil t))

(defun codex-ide-session-mode--teardown-theme-refresh ()
  "Remove theme change subscriptions for the current session buffer."
  (setq codex-ide-session-mode--theme-refresh-buffers
        (delq (current-buffer)
              (cl-remove-if-not #'buffer-live-p
                                codex-ide-session-mode--theme-refresh-buffers)))
  (unless codex-ide-session-mode--theme-refresh-buffers
    (remove-hook 'enable-theme-functions #'codex-ide-session-mode--handle-theme-change)
    (remove-hook 'disable-theme-functions #'codex-ide-session-mode--handle-theme-change))
  (remove-hook 'kill-buffer-hook #'codex-ide-session-mode--teardown-theme-refresh t)
  (remove-hook 'change-major-mode-hook #'codex-ide-session-mode--teardown-theme-refresh t))

(defun codex-ide-session-mode--teardown-table-resize ()
  "Remove table resize subscriptions for the current session buffer."
  (codex-ide-renderer-teardown-markdown-table-resize)
  (remove-hook 'kill-buffer-hook #'codex-ide-session-mode--teardown-table-resize t)
  (remove-hook 'change-major-mode-hook
               #'codex-ide-session-mode--teardown-table-resize
               t))

(defun codex-ide-session-mode--valid-detail-level-p (level)
  "Return non-nil when LEVEL is a valid transcript detail level."
  (memq level '(standard compact)))

(defun codex-ide-session-mode--read-detail-level ()
  "Read a transcript detail level from the minibuffer."
  (intern
   (completing-read
    "Transcript detail level: "
    '("standard" "compact")
    nil
    t
    nil
    nil
    (symbol-name codex-ide-session-transcript-detail-level))))

(defun codex-ide-session-mode--add-compact-invisibility (value)
  "Return invisible VALUE with compact transcript invisibility added."
  (cond
   ((null value) codex-ide-session-mode--transcript-compact-hidden)
   ((eq value t) value)
   ((eq value codex-ide-session-mode--transcript-compact-hidden) value)
   ((listp value)
    (if (memq codex-ide-session-mode--transcript-compact-hidden value)
        value
      (cons codex-ide-session-mode--transcript-compact-hidden value)))
   (t
    (list codex-ide-session-mode--transcript-compact-hidden value))))

(defun codex-ide-session-mode--remove-compact-invisibility (value)
  "Return invisible VALUE with compact transcript invisibility removed."
  (cond
   ((eq value codex-ide-session-mode--transcript-compact-hidden) nil)
   ((and (listp value)
         (memq codex-ide-session-mode--transcript-compact-hidden value))
    (let ((remaining (delq codex-ide-session-mode--transcript-compact-hidden
                           (copy-sequence value))))
      (cond
       ((null remaining) nil)
       ((null (cdr remaining)) (car remaining))
       (t remaining))))
   (t value)))

(defun codex-ide-session-mode--compact-detail-region-p (pos)
  "Return non-nil when POS is in a compact-hideable transcript detail region."
  (eq (get-text-property pos codex-ide-session-mode--transcript-detail-kind-property)
      codex-ide-session-mode--transcript-item-detail-kind))

(defun codex-ide-session-mode--apply-detail-level-to-region (start end compact)
  "Apply transcript detail visibility between START and END.
When COMPACT is non-nil, hide item detail regions.  Otherwise reveal them."
  (let ((pos start))
    (while (< pos end)
      (let ((next
             (min
              (or (next-single-property-change
                   pos
                   codex-ide-session-mode--transcript-detail-kind-property
                   nil
                   end)
                  end)
              (or (next-single-property-change pos 'invisible nil end)
                  end))))
        (when (codex-ide-session-mode--compact-detail-region-p pos)
          (let* ((current (get-text-property pos 'invisible))
                 (updated
                  (if compact
                      (codex-ide-session-mode--add-compact-invisibility current)
                    (codex-ide-session-mode--remove-compact-invisibility current))))
            (unless (equal current updated)
              (put-text-property pos next 'invisible updated))))
        (setq pos next)))))

(defun codex-ide-session-mode--capture-window-state ()
  "Capture current buffer window positions."
  (mapcar
   (lambda (window)
     (list :window window
           :start (copy-marker (window-start window))
           :point (copy-marker (window-point window))))
   (get-buffer-window-list (current-buffer) nil t)))

(defun codex-ide-session-mode--restore-window-state (states)
  "Restore current buffer window STATES."
  (dolist (state states)
    (let ((window (plist-get state :window))
          (start (plist-get state :start))
          (point (plist-get state :point)))
      (unwind-protect
          (when (and (window-live-p window)
                     (eq (window-buffer window) (current-buffer))
                     (markerp start)
                     (markerp point)
                     (marker-buffer start)
                     (marker-buffer point))
            (set-window-start window (marker-position start) t)
            (set-window-point window (marker-position point)))
        (when (markerp start)
          (set-marker start nil))
        (when (markerp point)
          (set-marker point nil))))))

(defun codex-ide-session-mode-refresh-transcript-detail-visibility ()
  "Refresh transcript detail visibility for the current buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-session-mode)
    (user-error "Not in a Codex session buffer"))
  (let ((compact (eq codex-ide-session-transcript-detail-level 'compact))
        (window-states (codex-ide-session-mode--capture-window-state)))
    (unwind-protect
        (let ((inhibit-read-only t))
          (if compact
              (add-to-invisibility-spec codex-ide-session-mode--transcript-compact-hidden)
            (remove-from-invisibility-spec codex-ide-session-mode--transcript-compact-hidden))
          (codex-ide-session-mode--apply-detail-level-to-region
           (point-min)
           (point-max)
           compact))
      (codex-ide-session-mode--restore-window-state window-states))))

;;;###autoload
(defun codex-ide-session-transcript-set-detail-level (level)
  "Set the current session transcript detail LEVEL.
Interactively, prompt for LEVEL.  LEVEL must be `standard' or `compact'."
  (interactive (list (codex-ide-session-mode--read-detail-level)))
  (unless (derived-mode-p 'codex-ide-session-mode)
    (user-error "Not in a Codex session buffer"))
  (unless (codex-ide-session-mode--valid-detail-level-p level)
    (user-error "Invalid transcript detail level: %s" level))
  (setq-local codex-ide-session-transcript-detail-level level)
  (codex-ide-session-mode-refresh-transcript-detail-visibility)
  (message "Codex transcript detail: %s" level))

;;;###autoload
(defun codex-ide-session-transcript-toggle-detail-level ()
  "Toggle the current session transcript between standard and compact detail."
  (interactive)
  (codex-ide-session-transcript-set-detail-level
   (if (eq codex-ide-session-transcript-detail-level 'compact)
       'standard
     'compact)))

;;;###autoload
(define-derived-mode codex-ide-session-mode text-mode "Codex-IDE"
  "Major mode for Codex app-server session buffers.

* \\<codex-ide-session-mode-map>\\[codex-ide-submit] submits the active prompt.

* \\[codex-ide-interrupt] interrupts the current turn.

* \\[codex-ide-session-diff-open] opens the session diff buffer.

* \\[codex-ide-apply-config-preset] prompts for and applies a config preset.

* \\[codex-ide-session-transcript-toggle-detail-level] toggles standard/compact transcript detail.

* \\[codex-ide-previous-prompt-line] and \\[codex-ide-next-prompt-line] move between prompt lines.

* \\[codex-ide-session-mode-nav-forward] and \\[codex-ide-session-mode-nav-backward] move between transcript focal points, including
  buttons, submitted prompts, and the active prompt.

When point is in the active prompt, `codex-ide-session-prompt-minor-mode'
adds these bindings:

* \\<codex-ide-session-prompt-minor-mode-map>\\[codex-ide-previous-prompt-history] and \\[codex-ide-next-prompt-history] move through prompt history.

When the active prompt begins with a slash,
`codex-ide-session-slash-command-minor-mode' adds this binding:

* \\<codex-ide-session-slash-command-minor-mode-map>\\[codex-ide-slash-command-complete-or-submit] completes or submits the slash command.

When point is in a prompt mention,
`codex-ide-session-mention-minor-mode' adds this binding:

* \\<codex-ide-session-mention-minor-mode-map>\\[codex-ide-mention-complete-or-newline] completes a skill mention or inserts a newline."
  (codex-ide--disable-session-font-lock)
  (setq-local truncate-lines nil)
  (when codex-ide-session-enable-visual-line-mode
    (visual-line-mode 1))
  (setq-local mode-line-process
              '((:eval (codex-ide-renderer-mode-line-status codex-ide--session))))
  (setq-local codex-ide-nav-focal-point-functions
              '(codex-ide-session-mode--focal-points))
  (setq-local imenu-create-index-function
              #'codex-ide-session-mode--imenu-create-index)
  (setq-local codex-ide-session-transcript-detail-level
              codex-ide-session-transcript-default-detail-level)
  (codex-ide-session-mode-refresh-transcript-detail-visibility)
  (add-hook 'completion-at-point-functions
            #'codex-ide-slash-command-completion-at-point
            nil
            t)
  (add-hook 'completion-at-point-functions
            #'codex-ide-mention-completion-at-point
            nil
            t)
  (add-hook 'post-self-insert-hook
            #'codex-ide-session-mode--maybe-complete-slash-command
            nil
            t)
  (add-hook 'post-self-insert-hook
            #'codex-ide-session-mode--maybe-complete-mention
            nil
            t)
  (setq-local codex-ide-session-mode--last-point (point))
  (setq-local codex-ide-session-mode--last-window-start nil)
  (codex-ide-session-mode--teardown-theme-refresh)
  (codex-ide-session-mode--setup-theme-refresh)
  (codex-ide-session-mode--teardown-table-resize)
  (codex-ide-renderer-setup-markdown-table-resize)
  (add-hook 'kill-buffer-hook #'codex-ide-session-mode--teardown-table-resize nil t)
  (add-hook 'change-major-mode-hook
            #'codex-ide-session-mode--teardown-table-resize
            nil
            t)
  (add-hook 'post-command-hook #'codex-ide--sync-prompt-minor-mode nil t)
  (add-hook 'post-command-hook
            #'codex-ide-session-mode-sync-slash-command-minor-mode
            nil
            t)
  (add-hook 'post-command-hook
            #'codex-ide-session-mode-sync-mention-minor-mode
            nil
            t)
  (add-hook 'post-command-hook
            #'codex-ide-session-mode-sync-approval-minor-mode
            nil
            t)
  (add-hook 'post-command-hook
            #'codex-ide-session-mode--track-tail-follow-navigation
            nil
            t)
  (add-hook 'post-command-hook
            #'codex-ide-session-mode--notify-diff-point-changed
            nil
            t))

(provide 'codex-ide-session-mode)

;;; codex-ide-session-mode.el ends here
