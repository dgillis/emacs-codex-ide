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
(require 'codex-ide-core)
(require 'codex-ide-nav)
(require 'codex-ide-renderer)
(require 'imenu)

(defvar codex-ide-session-enable-visual-line-mode)

(defvar codex-ide-session-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    map)
  "Keymap for `codex-ide-session-mode'.")

(defvar codex-ide-session-prompt-minor-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-prompt-minor-mode'.")

(define-key codex-ide-session-mode-map (kbd "C-c C-c") #'codex-ide-interrupt)
(define-key codex-ide-session-mode-map (kbd "C-c RET") #'codex-ide-submit)
(define-key codex-ide-session-mode-map (kbd "C-c C-k") #'codex-ide-interrupt)
(define-key codex-ide-session-mode-map (kbd "C-M-p") #'codex-ide-previous-prompt-line)
(define-key codex-ide-session-mode-map (kbd "C-M-n") #'codex-ide-next-prompt-line)
(define-key codex-ide-session-mode-map (kbd "TAB") #'codex-ide-session-mode-nav-forward)
(define-key codex-ide-session-mode-map (kbd "<backtab>") #'codex-ide-session-mode-nav-backward)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "M-p") #'codex-ide-previous-prompt-history)
(define-key codex-ide-session-prompt-minor-mode-map (kbd "M-n") #'codex-ide-next-prompt-history)

(defvar-local codex-ide-session-mode--last-point nil
  "Last observed point used for transcript tail-follow navigation tracking.")

(defvar-local codex-ide-session-mode--last-window-start nil
  "Last observed `window-start' for transcript tail-follow navigation tracking.")

(defvar codex-ide-session-mode--theme-refresh-buffers nil
  "Live buffers currently using `codex-ide-session-mode' theme refresh hooks.")

(define-minor-mode codex-ide-session-prompt-minor-mode
  "Minor mode enabled only while point is in the active Codex prompt."
  :lighter " Prompt"
  :keymap codex-ide-session-prompt-minor-mode-map)

(defun codex-ide--point-in-active-prompt-p (&optional session pos)
  "Return non-nil when POS is inside SESSION's active prompt region."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (setq pos (or pos (point)))
  (when-let ((overlay (and session (codex-ide-session-input-overlay session))))
    (let ((start (overlay-start overlay))
          (end (overlay-end overlay)))
      (and start
           end
           (<= start pos)
           (<= pos end)))))

(defun codex-ide-session-mode--input-end-position (&optional session)
  "Return SESSION's editable input end position, if available."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((marker (and session
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
    (when-let ((input-end (codex-ide-session-mode--input-end-position session)))
      (when (and (codex-ide--point-in-active-prompt-p session)
                 (> (point) input-end))
        (goto-char input-end)))
    (let ((inside (codex-ide--point-in-active-prompt-p session)))
      (unless (eq inside codex-ide-session-prompt-minor-mode)
        (codex-ide-session-prompt-minor-mode (if inside 1 -1))))))

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
         (when-let ((marker (and session
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
  (let ((approvals (and session
                        (codex-ide--session-metadata-get session :pending-approvals)))
        start)
    (when (hash-table-p approvals)
      (maphash
       (lambda (_id approval)
         (let ((marker (plist-get approval :start-marker)))
           (when (and (markerp marker)
                      (eq (marker-buffer marker) (current-buffer)))
             (setq start (if start
                             (min start (marker-position marker))
                           (marker-position marker))))))
       approvals))
    start))

(defun codex-ide-session-mode--tail-follow-preserve-p (&optional session pos)
  "Return non-nil when POS should preserve the current tail-follow state.

This covers inline approval and elicitation regions rendered near the live tail,
so users can navigate within those controls without opting out of follow mode."
  (setq pos (or pos (point)))
  (when-let ((start (codex-ide-session-mode--interactive-request-preserve-start
                     session)))
    (>= pos start)))

(defun codex-ide-session-mode--track-tail-follow-navigation ()
  "Track whether the selected transcript window has opted out of tail following."
  (when-let ((window (and (derived-mode-p 'codex-ide-session-mode)
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

(defun codex-ide-session-mode--handle-theme-change (&rest _args)
  "Refresh session renderer faces after a theme change."
  (setq codex-ide-session-mode--theme-refresh-buffers
        (cl-remove-if-not #'buffer-live-p
                          codex-ide-session-mode--theme-refresh-buffers))
  (when codex-ide-session-mode--theme-refresh-buffers
    (codex-ide-renderer-refresh-theme-faces)))

(defun codex-ide-session-mode--setup-theme-refresh ()
  "Subscribe the current session buffer to theme change events."
  (cl-pushnew (current-buffer) codex-ide-session-mode--theme-refresh-buffers)
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

;;;###autoload
(define-derived-mode codex-ide-session-mode text-mode "Codex-IDE"
  "Major mode for Codex app-server session buffers."
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
            #'codex-ide-session-mode--track-tail-follow-navigation
            nil
            t))

(provide 'codex-ide-session-mode)

;;; codex-ide-session-mode.el ends here
