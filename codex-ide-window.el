;;; codex-ide-window.el --- Buffer display policy for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns window and buffer display decisions for codex-ide.
;;
;; It answers questions like:
;;
;; - When a Codex buffer should reuse an existing window versus create a new
;;   one.
;; - How a newly created session should honor the configured split direction.
;; - How to apply focus and dedication rules consistently across callers.
;;
;; Separating these rules from session and transcript control keeps the higher
;; level code focused on lifecycle and transcript concerns instead of scattering
;; `display-buffer', split, and focus policy throughout the codebase.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar codex-ide-focus-on-open)
(defvar codex-ide-new-session-split)
(defvar codex-ide-display-buffer-options)

(declare-function codex-ide--remember-buffer-context-before-switch "codex-ide-core"
                  (&optional buffer))

(defun codex-ide--display-buffer-in-window (buffer window)
  "Display BUFFER in WINDOW, preserving dedication when possible."
  (when window
    (let ((was-dedicated (window-dedicated-p window)))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window was-dedicated)
      (when codex-ide-focus-on-open
        (select-window window))
      window)))

(defun codex-ide-display-buffer (buffer)
  "Display BUFFER according to `codex-ide-display-buffer-options'."
  (codex-ide--remember-buffer-context-before-switch)
  (let* ((options (or codex-ide-display-buffer-options
                      '(:reuse-buffer-window :reuse-mode-window :other-window)))
         (window
          (seq-some
           (lambda (option)
             (pcase option
               (:reuse-buffer-window
                (get-buffer-window buffer 0))
               (:reuse-mode-window
                (seq-find
                 (lambda (candidate)
                   (with-current-buffer (window-buffer candidate)
                     (derived-mode-p 'codex-ide-session-mode)))
                 (window-list)))
               (:other-window
                (when (not (one-window-p t))
                  (when-let ((candidate (ignore-errors (other-window-for-scrolling))))
                    (unless (window-minibuffer-p candidate)
                      candidate))))
               (:new-window
                (split-window-sensibly))
               (_ nil)))
           options)))
    (setq window (or window (selected-window)))
    (codex-ide--display-buffer-in-window buffer window)))

(defun codex-ide--split-window-for-new-session (window)
  "Return a new window split from WINDOW for a newly created session."
  (pcase codex-ide-new-session-split
    ('vertical
     (split-window window nil 'right))
    ('horizontal
     (split-window window nil 'below))
    (_
     nil)))

(defun codex-ide--display-new-session-buffer (buffer)
  "Display newly created session BUFFER honoring split preferences."
  (let ((window (or (and codex-ide-new-session-split
                         (codex-ide--split-window-for-new-session (selected-window)))
                    (codex-ide-display-buffer buffer))))
    (codex-ide--display-buffer-in-window buffer window)))

(provide 'codex-ide-window)

;;; codex-ide-window.el ends here
