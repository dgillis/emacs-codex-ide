;;; codex-ide-math-preview.el --- LaTeX previews in Codex transcripts -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This optional module renders LaTeX fragments in completed assistant messages.
;; It uses the external `texfrag' package when available, but does not make
;; `texfrag' or AUCTeX a package dependency of codex-ide.  Preview work is
;; debounced and never runs for streaming deltas.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'codex-ide-renderer)
(require 'codex-ide-transcript)

(declare-function texfrag-clearout-region "texfrag" (beg end))
(declare-function texfrag-cleanup "texfrag" ())
(declare-function texfrag-mode "texfrag" (&optional arg))
(declare-function texfrag-region "texfrag" (&optional beg end))
(declare-function texfrag-search-forward-fragment "texfrag" (&optional bound))

(defvar texfrag-comments-only)
(defvar texfrag-equation-filter)
(defvar texfrag-frag-alist)
(defvar texfrag-LaTeX-frag-alist)
(defvar texfrag-markdown-preview-image-links)
(defvar texfrag-mode)
(defvar texfrag-running)
(defvar texfrag-scale)
(defvar texfrag-subdir)

(defconst codex-ide-math-preview--texfrag-variables
  '(texfrag-comments-only
    texfrag-equation-filter
    texfrag-frag-alist
    texfrag-markdown-preview-image-links
    texfrag-scale
    texfrag-subdir)
  "Texfrag variables configured buffer-locally by math preview mode.")

(defgroup codex-ide-math-preview nil
  "LaTeX previews in Codex session buffers."
  :group 'codex-ide
  :prefix "codex-ide-math-preview-")

(defcustom codex-ide-math-preview-auto-enable nil
  "Whether new Codex session buffers should enable math previews.

This option requires the optional `texfrag' package and its AUCTeX preview
prerequisites.  When those packages are unavailable, session creation
continues without math previews."
  :type 'boolean
  :group 'codex-ide-math-preview)

(defcustom codex-ide-math-preview-idle-delay 0.25
  "Idle seconds to wait before previewing completed assistant messages."
  :type 'number
  :group 'codex-ide-math-preview)

(defcustom codex-ide-math-preview-scale 1.0
  "Additional scale applied to generated math preview images."
  :type 'number
  :group 'codex-ide-math-preview)

(defvar-local codex-ide-math-preview--dirty-start-marker nil
  "Start marker for assistant text awaiting math preview rendering.")

(defvar-local codex-ide-math-preview--dirty-end-marker nil
  "End marker for assistant text awaiting math preview rendering.")

(defvar-local codex-ide-math-preview--render-timer nil
  "Idle timer for pending math preview work.")

(defvar-local codex-ide-math-preview--owns-texfrag-mode nil
  "Whether math preview mode enabled `texfrag-mode' in this buffer.")

(defvar-local codex-ide-math-preview--texfrag-keymap nil
  "Empty overriding keymap installed for `texfrag-mode'.")

(defvar-local codex-ide-math-preview--texfrag-state nil
  "Saved local state for variables configured by math preview mode.")

(defvar-local codex-ide-math-preview--temporary-directory nil
  "Temporary directory containing this buffer's `texfrag' artifacts.")

(defun codex-ide-math-preview--escaped-p (position)
  "Return non-nil when the character at POSITION is backslash-escaped."
  (let ((cursor (1- position))
        (backslashes 0))
    (while (and (>= cursor (point-min))
                (eq (char-after cursor) ?\\))
      (setq backslashes (1+ backslashes)
            cursor (1- cursor)))
    (cl-oddp backslashes)))

(defun codex-ide-math-preview--eligible-position-p (position delimiter)
  "Return non-nil when a fragment at POSITION with DELIMITER is previewable."
  (and (equal
        (get-text-property position codex-ide-agent-item-type-property)
        "agentMessage")
       (not (get-text-property position 'codex-ide-markdown-code-content))
       (not (get-text-property position 'codex-ide-markdown-inline-code))
       (not (get-text-property position 'codex-ide-markdown-table-original))
       (not (and (string-prefix-p "$" delimiter)
                 (codex-ide-math-preview--escaped-p position)))))

(defun codex-ide-math-preview--texfrag-fragment-p ()
  "Return non-nil when the current `texfrag' match is previewable."
  (codex-ide-math-preview--eligible-position-p
   (match-beginning 0)
   (match-string-no-properties 0)))

(defun codex-ide-math-preview--texfrag-fragment-alist ()
  "Return `texfrag-LaTeX-frag-alist' filtered for Codex transcript text."
  (mapcar
   (lambda (entry)
     (cons (list (car entry) #'codex-ide-math-preview--texfrag-fragment-p)
           (cdr entry)))
   texfrag-LaTeX-frag-alist))

(defun codex-ide-math-preview--suppress-texfrag-keymap ()
  "Prevent `texfrag-mode' bindings from shadowing Codex session bindings."
  (codex-ide-math-preview--restore-texfrag-keymap)
  (setq codex-ide-math-preview--texfrag-keymap (make-sparse-keymap))
  (push (cons 'texfrag-mode codex-ide-math-preview--texfrag-keymap)
        minor-mode-overriding-map-alist))

(defun codex-ide-math-preview--restore-texfrag-keymap ()
  "Remove the `texfrag-mode' keymap override installed by this mode."
  (when codex-ide-math-preview--texfrag-keymap
    (setq minor-mode-overriding-map-alist
          (cl-remove-if
           (lambda (entry)
             (and (eq (car-safe entry) 'texfrag-mode)
                  (eq (cdr-safe entry)
                      codex-ide-math-preview--texfrag-keymap)))
           minor-mode-overriding-map-alist))
    (setq codex-ide-math-preview--texfrag-keymap nil)))

(defun codex-ide-math-preview--save-texfrag-state ()
  "Remember configured `texfrag' variables before changing them."
  (unless codex-ide-math-preview--texfrag-state
    (setq codex-ide-math-preview--texfrag-state
          (mapcar
           (lambda (variable)
             (list variable
                   (local-variable-p variable)
                   (and (boundp variable)
                        (symbol-value variable))))
           codex-ide-math-preview--texfrag-variables))))

(defun codex-ide-math-preview--restore-texfrag-state ()
  "Restore `texfrag' variables changed by math preview mode."
  (dolist (state codex-ide-math-preview--texfrag-state)
    (pcase-let ((`(,variable ,local-p ,value) state))
      (if local-p
          (set variable value)
        (kill-local-variable variable))))
  (setq codex-ide-math-preview--texfrag-state nil))

(defun codex-ide-math-preview--delete-temporary-directory ()
  "Delete preview artifacts created for the current buffer."
  (when (and (stringp codex-ide-math-preview--temporary-directory)
             (file-directory-p
              codex-ide-math-preview--temporary-directory))
    (delete-directory codex-ide-math-preview--temporary-directory t))
  (setq codex-ide-math-preview--temporary-directory nil))

(defun codex-ide-math-preview--cancel-timer ()
  "Cancel pending math preview work in the current buffer."
  (when (timerp codex-ide-math-preview--render-timer)
    (cancel-timer codex-ide-math-preview--render-timer))
  (setq codex-ide-math-preview--render-timer nil))

(defun codex-ide-math-preview--clear-dirty-region ()
  "Release markers tracking pending math preview work."
  (when (markerp codex-ide-math-preview--dirty-start-marker)
    (set-marker codex-ide-math-preview--dirty-start-marker nil))
  (when (markerp codex-ide-math-preview--dirty-end-marker)
    (set-marker codex-ide-math-preview--dirty-end-marker nil))
  (setq codex-ide-math-preview--dirty-start-marker nil
        codex-ide-math-preview--dirty-end-marker nil))

(defun codex-ide-math-preview--schedule-timer ()
  "Schedule pending math preview work for the current buffer."
  (codex-ide-math-preview--cancel-timer)
  (setq codex-ide-math-preview--render-timer
        (run-with-idle-timer
         (max 0 codex-ide-math-preview-idle-delay)
         nil
         #'codex-ide-math-preview--render-pending
         (current-buffer))))

(defun codex-ide-math-preview--note-region (start end)
  "Add START through END to pending math preview work."
  (when (< start end)
    (if (and (markerp codex-ide-math-preview--dirty-start-marker)
             (marker-buffer codex-ide-math-preview--dirty-start-marker))
        (set-marker
         codex-ide-math-preview--dirty-start-marker
         (min start
              (marker-position codex-ide-math-preview--dirty-start-marker)))
      (setq codex-ide-math-preview--dirty-start-marker
            (copy-marker start)))
    (if (and (markerp codex-ide-math-preview--dirty-end-marker)
             (marker-buffer codex-ide-math-preview--dirty-end-marker))
        (set-marker
         codex-ide-math-preview--dirty-end-marker
         (max end
              (marker-position codex-ide-math-preview--dirty-end-marker)))
      (setq codex-ide-math-preview--dirty-end-marker
            (copy-marker end t)))
    (codex-ide-math-preview--schedule-timer)))

(defun codex-ide-math-preview-request (buffer start end)
  "Request math previews for BUFFER between START and END."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when codex-ide-math-preview-mode
        (codex-ide-math-preview--note-region
         (max (point-min) (min start (point-max)))
         (max (point-min) (min end (point-max))))))))

(defun codex-ide-math-preview--region-has-fragment-p (start end)
  "Return non-nil when START through END contains a previewable fragment."
  (save-excursion
    (goto-char start)
    (condition-case nil
        (and (texfrag-search-forward-fragment end) t)
      (user-error nil))))

(defun codex-ide-math-preview--render-region (start end)
  "Render math previews between START and END with `texfrag'."
  (setq-local texfrag-scale codex-ide-math-preview-scale)
  (if (codex-ide-math-preview--region-has-fragment-p start end)
      (texfrag-region start end)
    (texfrag-clearout-region start end)))

(defun codex-ide-math-preview--render-pending (buffer)
  "Render queued math previews in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq codex-ide-math-preview--render-timer nil)
      (when (and codex-ide-math-preview-mode
                 (markerp codex-ide-math-preview--dirty-start-marker)
                 (marker-buffer codex-ide-math-preview--dirty-start-marker)
                 (markerp codex-ide-math-preview--dirty-end-marker)
                 (marker-buffer codex-ide-math-preview--dirty-end-marker))
        (if (bound-and-true-p texfrag-running)
            (codex-ide-math-preview--schedule-timer)
          (let ((start (marker-position
                        codex-ide-math-preview--dirty-start-marker))
                (end (marker-position
                      codex-ide-math-preview--dirty-end-marker)))
            (codex-ide-math-preview--clear-dirty-region)
            (condition-case err
                (codex-ide-math-preview--render-region start end)
              (error
               (message "codex-ide math preview failed: %s"
                        (error-message-string err))))))))))

(defun codex-ide-math-preview--configure-texfrag ()
  "Configure `texfrag' for Codex transcript buffers."
  (codex-ide-math-preview--save-texfrag-state)
  (setq-local texfrag-comments-only nil
              texfrag-equation-filter #'identity
              texfrag-frag-alist
              (codex-ide-math-preview--texfrag-fragment-alist)
              texfrag-markdown-preview-image-links nil
              texfrag-scale codex-ide-math-preview-scale)
  (when codex-ide-math-preview--owns-texfrag-mode
    (unless (and (stringp codex-ide-math-preview--temporary-directory)
                 (file-directory-p
                  codex-ide-math-preview--temporary-directory))
      (setq codex-ide-math-preview--temporary-directory
            (make-temp-file "codex-ide-texfrag-" t)))
    (setq-local texfrag-subdir
                codex-ide-math-preview--temporary-directory)))

(defun codex-ide-math-preview--enable ()
  "Enable the `texfrag' backend in the current Codex session buffer."
  (unless (derived-mode-p 'codex-ide-session-mode)
    (user-error "Math previews are only available in Codex session buffers"))
  (unless (require 'texfrag nil t)
    (user-error
     "Math previews require the optional texfrag package and AUCTeX"))
  (unless (bound-and-true-p texfrag-mode)
    (setq codex-ide-math-preview--owns-texfrag-mode t))
  (codex-ide-math-preview--configure-texfrag)
  (when codex-ide-math-preview--owns-texfrag-mode
    (texfrag-mode 1))
  (add-hook 'kill-buffer-hook
            #'codex-ide-math-preview--delete-temporary-directory
            t
            t)
  (codex-ide-math-preview--suppress-texfrag-keymap))

(defun codex-ide-math-preview--disable ()
  "Disable math previews in the current buffer and release their state."
  (codex-ide-math-preview--cancel-timer)
  (codex-ide-math-preview--clear-dirty-region)
  (unwind-protect
      (progn
        (when (and (featurep 'texfrag)
                   (fboundp 'texfrag-clearout-region))
          (texfrag-clearout-region (point-min) (point-max)))
        (codex-ide-math-preview--restore-texfrag-keymap)
        (when codex-ide-math-preview--owns-texfrag-mode
          (unwind-protect
              (when (bound-and-true-p texfrag-mode)
                (texfrag-mode -1))
            (when (fboundp 'texfrag-cleanup)
              (texfrag-cleanup)))))
    (remove-hook 'kill-buffer-hook
                 #'codex-ide-math-preview--delete-temporary-directory
                 t)
    (codex-ide-math-preview--delete-temporary-directory)
    (codex-ide-math-preview--restore-texfrag-state)
    (setq codex-ide-math-preview--owns-texfrag-mode nil)))

;;;###autoload
(define-minor-mode codex-ide-math-preview-mode
  "Preview LaTeX fragments in completed Codex assistant messages."
  :lighter " Math"
  (if codex-ide-math-preview-mode
      (condition-case err
          (codex-ide-math-preview--enable)
        (error
         (ignore-errors (codex-ide-math-preview--disable))
         (setq codex-ide-math-preview-mode nil)
         (signal (car err) (cdr err))))
    (codex-ide-math-preview--disable)))

;;;###autoload
(defun codex-ide-math-preview-refresh (&optional start end)
  "Refresh math previews between START and END.

Interactively, use the active region or the entire current buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless codex-ide-math-preview-mode
    (user-error "codex-ide math preview mode is not enabled"))
  (codex-ide-math-preview--note-region
   (or start (point-min))
   (or end (point-max))))

;;;###autoload
(defun codex-ide-math-preview-clear (&optional start end)
  "Clear math previews between START and END.

Interactively, use the active region or the entire current buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (unless (and (featurep 'texfrag)
               (fboundp 'texfrag-clearout-region))
    (user-error "The texfrag backend is not loaded"))
  (codex-ide-math-preview--cancel-timer)
  (codex-ide-math-preview--clear-dirty-region)
  (texfrag-clearout-region
   (or start (point-min))
   (or end (point-max))))

(defun codex-ide-math-preview--maybe-enable ()
  "Enable math previews when `codex-ide-math-preview-auto-enable' is non-nil."
  (when codex-ide-math-preview-auto-enable
    (condition-case err
        (codex-ide-math-preview-mode 1)
      (error
       (message "codex-ide math preview unavailable: %s"
                (error-message-string err))))))

(defun codex-ide-math-preview--after-agent-message-rendered
    (buffer start end)
  "Schedule math previews for finalized assistant text in BUFFER from START to END."
  (codex-ide-math-preview-request buffer start end))

(add-hook 'codex-ide-session-mode-hook
          #'codex-ide-math-preview--maybe-enable)
(add-hook 'codex-ide-transcript-agent-message-rendered-hook
          #'codex-ide-math-preview--after-agent-message-rendered)

(provide 'codex-ide-math-preview)

;;; codex-ide-math-preview.el ends here
