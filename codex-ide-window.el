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

(defvar codex-ide-new-session-split)

(declare-function codex-ide--remember-buffer-context-before-switch "codex-ide-core"
                  (&optional buffer))

(defvar codex-ide-select-window-on-open t
  "Whether `codex-ide-display-buffer' should select the shown window.")

(defconst codex-ide-display-buffer-pop-up-action
  '(display-buffer-reuse-window display-buffer-pop-up-window)
  "Display action used when Codex should surface a buffer if needed.")

(defconst codex-ide--display-buffer-other-window-pop-up-action
  '(display-buffer-reuse-window
    display-buffer-use-some-window
    display-buffer-pop-up-window
    (inhibit-same-window . t))
  "Display action used when Codex should surface a buffer in another window.")

(defun codex-ide-display-buffer (buffer &optional action)
  "Display BUFFER via `display-buffer' and return the selected window.
When ACTION is non-nil, pass it through as the DISPLAY-BUFFER action."
  (codex-ide--remember-buffer-context-before-switch)
  (let ((window (display-buffer buffer action)))
    (when (and window codex-ide-select-window-on-open)
      (select-window window))
    window))

(defun codex-ide--display-buffer-new-session-split (buffer alist)
  "Display BUFFER in a new split for a newly created session.
ALIST is ignored and accepted for `display-buffer' compatibility."
  (let ((window (selected-window)))
    (when window
      (set-window-buffer
       (pcase codex-ide-new-session-split
         ('vertical
          (split-window window nil 'right))
         ('horizontal
          (split-window window nil 'below))
         (_
          nil))
       buffer))))

(defun codex-ide--display-new-session-buffer (buffer)
  "Display newly created session BUFFER honoring split preferences."
  (codex-ide-display-buffer buffer
                            (if codex-ide-new-session-split
                                '(codex-ide--display-buffer-new-session-split)
                              codex-ide-display-buffer-pop-up-action)))

(provide 'codex-ide-window)

;;; codex-ide-window.el ends here
