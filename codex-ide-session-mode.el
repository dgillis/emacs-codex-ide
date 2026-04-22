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

(require 'codex-ide-core)
(require 'codex-ide-nav)
(require 'codex-ide-renderer)

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

(defun codex-ide--sync-prompt-minor-mode (&optional session)
  "Enable or disable `codex-ide-session-prompt-minor-mode' for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (and session (derived-mode-p 'codex-ide-session-mode))
    (let ((inside (codex-ide--point-in-active-prompt-p session)))
      (unless (eq inside codex-ide-session-prompt-minor-mode)
        (codex-ide-session-prompt-minor-mode (if inside 1 -1))))))

(defun codex-ide-session-mode--focal-points ()
  "Return focal points for the current session buffer."
  (let ((session (and (boundp 'codex-ide--session) codex-ide--session)))
    (append (codex-ide-nav-collect-buttons)
            (and session
                 (codex-ide-nav-collect-session-input session)))))

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
  (setq-local codex-ide-session-mode--last-point (point))
  (setq-local codex-ide-session-mode--last-window-start nil)
  (add-hook 'post-command-hook #'codex-ide--sync-prompt-minor-mode nil t)
  (add-hook 'post-command-hook
            #'codex-ide-session-mode--track-tail-follow-navigation
            nil
            t))

(provide 'codex-ide-session-mode)

;;; codex-ide-session-mode.el ends here
