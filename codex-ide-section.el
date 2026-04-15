;;; codex-ide-section.el --- Local collapsible sections for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Small section helper used by `codex-ide-status-mode'.

;;; Code:

(require 'cl-lib)

(cl-defstruct (codex-ide-section
               (:constructor codex-ide-section--create))
  type
  value
  keymap
  parent
  children
  heading-start
  heading-end
  body-start
  end
  hidden
  overlay
  indicator-overlay)

(defvar-local codex-ide-section--root-sections nil
  "Top-level sections in the current buffer.")

(defvar-local codex-ide-section--section-stack nil
  "Stack of sections being rendered in the current buffer.")

(define-fringe-bitmap 'codex-ide-section-fringe-bitmap-closed
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'codex-ide-section-fringe-bitmap-open
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

(defvar codex-ide-section-heading-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<double-mouse-1>") #'codex-ide-section-mouse-toggle-section)
    (define-key map (kbd "<double-mouse-2>") #'codex-ide-section-mouse-toggle-section)
    map)
  "Keymap active on all Codex section headings.")

(defvar codex-ide-section-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "<left-fringe> <mouse-1>") #'codex-ide-section-mouse-toggle-section)
    (define-key map (kbd "<left-fringe> <mouse-2>") #'codex-ide-section-mouse-toggle-section)
    (define-key map (kbd "TAB") #'codex-ide-section-toggle-at-point)
    map)
  "Parent keymap for modes derived from `codex-ide-section-mode'.")

(define-derived-mode codex-ide-section-mode special-mode "Codex-Sections"
  "Parent major mode for buffers with Codex expandable sections."
  (setq-local truncate-lines t)
  (setq-local buffer-invisibility-spec '(t))
  (setq-local line-move-visual t)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky))

(defun codex-ide-section-reset ()
  "Clear section state in the current buffer."
  (remove-overlays (point-min) (point-max) 'codex-ide-section-hidden t)
  (remove-overlays (point-min) (point-max) 'codex-ide-section-indicator t)
  (setq codex-ide-section--root-sections nil
        codex-ide-section--section-stack nil))

(defun codex-ide-section-at-point (&optional pos)
  "Return the section at POS or point."
  (setq pos (or pos (point)))
  (or (get-text-property pos 'codex-ide-section)
      (and (> pos (point-min))
           (get-text-property (1- pos) 'codex-ide-section))))

(defun codex-ide-section--indicator-before-string (section)
  "Return the before-string used to indicate SECTION visibility."
  (if (display-graphic-p)
      (propertize
       " "
       'display `(left-fringe
                  ,(if (codex-ide-section-hidden section)
                       'codex-ide-section-fringe-bitmap-closed
                     'codex-ide-section-fringe-bitmap-open)
                  fringe))
    (propertize
     (if (codex-ide-section-hidden section) "> " "v ")
     'face 'shadow)))

(defun codex-ide-section--set-heading-properties (section start end)
  "Tag SECTION heading text from START to END."
  (let ((map (if-let ((section-map (codex-ide-section-keymap section)))
                 (make-composed-keymap
                  (list section-map codex-ide-section-heading-map))
               codex-ide-section-heading-map)))
  (add-text-properties
   start end
   `(codex-ide-section ,section
                       keymap ,map
                       rear-nonsticky (codex-ide-section
                                       keymap
                                       help-echo)
                       help-echo "TAB: toggle section"))))

(defun codex-ide-section--update-indicator (section)
  "Refresh SECTION's visible indicator."
  (let ((overlay (or (codex-ide-section-indicator-overlay section)
                     (make-overlay (codex-ide-section-heading-start section)
                                   (codex-ide-section-heading-end section)
                                   nil t t))))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'codex-ide-section-indicator t)
    (overlay-put overlay 'before-string
                 (codex-ide-section--indicator-before-string section))
    (setf (codex-ide-section-indicator-overlay section) overlay)
    (codex-ide-section--set-heading-properties
     section
     (codex-ide-section-heading-start section)
     (codex-ide-section-heading-end section))))

(defun codex-ide-section-show (section)
  "Show SECTION body."
  (when-let ((overlay (codex-ide-section-overlay section)))
    (delete-overlay overlay)
    (setf (codex-ide-section-overlay section) nil))
  (setf (codex-ide-section-hidden section) nil)
  (codex-ide-section--update-indicator section)
  section)

(defun codex-ide-section-hide (section)
  "Hide SECTION body."
  (unless (codex-ide-section-overlay section)
    (let ((overlay (make-overlay (codex-ide-section-body-start section)
                                 (codex-ide-section-end section)
                                 nil nil nil)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'isearch-open-invisible #'delete-overlay)
      (overlay-put overlay 'codex-ide-section-hidden t)
      (setf (codex-ide-section-overlay section) overlay)))
  (setf (codex-ide-section-hidden section) t)
  (codex-ide-section--update-indicator section)
  section)

(defun codex-ide-section-toggle (section)
  "Toggle SECTION visibility."
  (if (codex-ide-section-hidden section)
      (codex-ide-section-show section)
    (codex-ide-section-hide section)))

(defun codex-ide-section-toggle-at-point ()
  "Toggle the section at point."
  (interactive)
  (if-let ((section (codex-ide-section-at-point)))
      (codex-ide-section-toggle section)
    (user-error "No section at point")))

(defun codex-ide-section-mouse-toggle-section (event)
  "Toggle the section clicked in EVENT."
  (interactive "e")
  (let* ((pos (event-start event))
         (section (codex-ide-section-at-point (posn-point pos))))
    (when section
      (goto-char (codex-ide-section-heading-start section))
      (codex-ide-section-toggle section))))

(defun codex-ide-section-insert (type value title body-fn &optional hidden keymap)
  "Insert a section with TYPE, VALUE, TITLE, and BODY-FN.
BODY-FN is called with the new section object inserted as current parent.
When HIDDEN is non-nil, initially hide the section body.
When KEYMAP is non-nil, compose it with `codex-ide-section-heading-map'."
  (let ((inhibit-read-only t))
    (let* ((parent (car codex-ide-section--section-stack))
           (section (codex-ide-section--create
                     :type type
                     :value value
                     :keymap keymap
                     :parent parent
                     :children nil
                     :hidden nil))
           (heading-start (point))
           (heading-end nil))
      (if parent
          (setf (codex-ide-section-children parent)
                (append (codex-ide-section-children parent) (list section)))
        (setq codex-ide-section--root-sections
              (append codex-ide-section--root-sections (list section))))
      (insert title)
      (insert "\n")
      (setq heading-end (point))
      (setf (codex-ide-section-heading-start section) heading-start
            (codex-ide-section-heading-end section) heading-end
            (codex-ide-section-body-start section) (point))
      (codex-ide-section--set-heading-properties section heading-start heading-end)
      (codex-ide-section--update-indicator section)
      (let ((codex-ide-section--section-stack
             (cons section codex-ide-section--section-stack)))
        (funcall body-fn section))
      (setf (codex-ide-section-end section) (point))
      (when hidden
        (codex-ide-section-hide section))
      section)))

(provide 'codex-ide-section)

;;; codex-ide-section.el ends here
