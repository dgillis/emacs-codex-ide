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
  interactive-heading
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

(defvar-local codex-ide-section--highlight-overlay nil
  "Overlay used to highlight the current line within the active section.")

(defvar-local codex-ide-section--highlighted-section nil
  "Section containing the current line highlight.")

(defface codex-ide-section-highlight
  '((t :inherit highlight :extend t))
  "Face used to highlight the current section heading."
  :group 'codex-ide)

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
    map)
  "Parent keymap for modes derived from `codex-ide-section-mode'.")

(define-key codex-ide-section-mode-map (kbd "<left-fringe> <mouse-1>") #'codex-ide-section-mouse-toggle-section)
(define-key codex-ide-section-mode-map (kbd "<left-fringe> <mouse-2>") #'codex-ide-section-mouse-toggle-section)
(define-key codex-ide-section-mode-map (kbd "t") #'codex-ide-section-toggle-at-point)
(define-key codex-ide-section-mode-map (kbd "T") #'codex-ide-section-toggle-siblings-at-point)
;; Binding "(" for consistency with direds binding for toggling expanded directory detail.
(define-key codex-ide-section-mode-map (kbd "(") #'codex-ide-section-toggle-siblings-at-point)
(define-key codex-ide-section-mode-map (kbd "TAB") #'codex-ide-section-toggle-at-point)
(define-key codex-ide-section-mode-map (kbd "^") #'codex-ide-section-up)
(define-key codex-ide-section-mode-map (kbd "p") #'codex-ide-section-backward)
(define-key codex-ide-section-mode-map (kbd "n") #'codex-ide-section-forward)
(define-key codex-ide-section-mode-map (kbd "M-p") #'codex-ide-section-backward-sibling)
(define-key codex-ide-section-mode-map (kbd "M-n") #'codex-ide-section-forward-sibling)

(define-derived-mode codex-ide-section-mode special-mode "Codex-Sections"
  "Parent major mode for buffers with Codex expandable sections."
  (setq-local truncate-lines t)
  (setq-local buffer-invisibility-spec '(t))
  (setq-local line-move-visual t)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'codex-ide-section-post-command-hook nil t))

(defun codex-ide-section-reset ()
  "Clear section state in the current buffer."
  (remove-overlays (point-min) (point-max) 'codex-ide-section-hidden t)
  (remove-overlays (point-min) (point-max) 'codex-ide-section-indicator t)
  (when-let ((overlay codex-ide-section--highlight-overlay))
    (delete-overlay overlay))
  (setq codex-ide-section--root-sections nil
        codex-ide-section--section-stack nil
        codex-ide-section--highlight-overlay nil
        codex-ide-section--highlighted-section nil))

(defun codex-ide-section-at-point (&optional pos)
  "Return the section at POS or point."
  (setq pos (or pos (point)))
  (or (get-text-property pos 'codex-ide-section)
      (and (> pos (point-min))
           (get-text-property (1- pos) 'codex-ide-section))))

(defun codex-ide-section-heading-at-point (&optional pos)
  "Return the section whose heading contains POS or point."
  (setq pos (or pos (point)))
  (when-let ((section (codex-ide-section-at-point pos)))
    (when (and (<= (codex-ide-section-heading-start section) pos)
               (< pos (codex-ide-section-heading-end section)))
      section)))

(defun codex-ide-section-containing-point (&optional pos)
  "Return the deepest section containing POS or point."
  (setq pos (or pos (point)))
  (cl-labels ((find-in (sections)
                (cl-find-if
                 #'identity
                 (mapcar
                  (lambda (section)
                    (when (and (<= (codex-ide-section-heading-start section) pos)
                               (< pos (codex-ide-section-end section)))
                      (or (find-in (codex-ide-section-children section))
                          section)))
                  sections))))
    (find-in codex-ide-section--root-sections)))

(defun codex-ide-section--current ()
  "Return the current section for navigation."
  (or (codex-ide-section-at-point)
      (codex-ide-section-containing-point)
      (user-error "No section at point")))

(defun codex-ide-section--visible-p (section)
  "Return non-nil when SECTION's heading is visible."
  (not (invisible-p (codex-ide-section-heading-start section))))

(defun codex-ide-section--all-sections ()
  "Return all sections in depth-first order."
  (let (sections)
    (cl-labels ((walk (section)
                  (push section sections)
                  (dolist (child (codex-ide-section-children section))
                    (walk child))))
      (dolist (section codex-ide-section--root-sections)
        (walk section)))
    (nreverse sections)))

(defun codex-ide-section--move-to (section)
  "Move point to SECTION's heading start and return SECTION."
  (goto-char (codex-ide-section-heading-start section))
  section)

(defun codex-ide-section--siblings (section)
  "Return SECTION's siblings in display order."
  (if-let ((parent (codex-ide-section-parent section)))
      (codex-ide-section-children parent)
    codex-ide-section--root-sections))

(defun codex-ide-section-up ()
  "Move point to the parent section heading."
  (interactive)
  (if-let ((parent (codex-ide-section-parent (codex-ide-section--current))))
      (codex-ide-section--move-to parent)
    (user-error "No parent section")))

(defun codex-ide-section-forward ()
  "Move point to the next visible section heading."
  (interactive)
  (let* ((current (codex-ide-section--current))
         (sections (seq-filter #'codex-ide-section--visible-p
                               (codex-ide-section--all-sections)))
         (tail (memq current sections)))
    (if-let ((next (cadr tail)))
        (codex-ide-section--move-to next)
      (user-error "No next section"))))

(defun codex-ide-section-backward ()
  "Move point to the previous visible section heading."
  (interactive)
  (let* ((current (codex-ide-section--current))
         (sections (seq-filter #'codex-ide-section--visible-p
                               (codex-ide-section--all-sections)))
         (tail (memq current sections))
         (previous (car (last (butlast sections (length tail))))))
    (if previous
        (codex-ide-section--move-to previous)
      (user-error "No previous section"))))

(defun codex-ide-section-forward-sibling ()
  "Move point to the next visible sibling section heading."
  (interactive)
  (let* ((current (codex-ide-section--current))
         (siblings (seq-filter #'codex-ide-section--visible-p
                               (codex-ide-section--siblings current)))
         (tail (memq current siblings)))
    (if-let ((next (cadr tail)))
        (codex-ide-section--move-to next)
      (user-error "No next sibling section"))))

(defun codex-ide-section-backward-sibling ()
  "Move point to the previous visible sibling section heading."
  (interactive)
  (let* ((current (codex-ide-section--current))
         (siblings (seq-filter #'codex-ide-section--visible-p
                               (codex-ide-section--siblings current)))
         (tail (memq current siblings))
         (previous (car (last (butlast siblings (length tail))))))
    (if previous
        (codex-ide-section--move-to previous)
      (user-error "No previous sibling section"))))

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
  (when (codex-ide-section-interactive-heading section)
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
                           help-echo "t: toggle section")))))

(defun codex-ide-section--update-indicator (section)
  "Refresh SECTION's visible indicator."
  (if (codex-ide-section-interactive-heading section)
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
         (codex-ide-section-heading-end section)))
    (when-let ((overlay (codex-ide-section-indicator-overlay section)))
      (delete-overlay overlay)
      (setf (codex-ide-section-indicator-overlay section) nil))))

(defun codex-ide-section--delete-highlight-overlay ()
  "Delete the current section highlight overlay."
  (when-let ((overlay codex-ide-section--highlight-overlay))
    (delete-overlay overlay))
  (setq codex-ide-section--highlight-overlay nil))

(defun codex-ide-section--current-line-bounds ()
  "Return `(START . END)' covering the current line."
  (cons (line-beginning-position)
        (min (point-max) (1+ (line-end-position)))))

(defun codex-ide-section-update-highlight (&optional force)
  "Update the highlighted line in the current buffer.
When FORCE is non-nil, repaint even if the highlighted section and line did not change."
  (let* ((section (or (codex-ide-section-heading-at-point)
                      (codex-ide-section-containing-point)))
         (line-bounds (and section (codex-ide-section--current-line-bounds)))
         (line-start (car-safe line-bounds))
         (line-end (cdr-safe line-bounds))
         (overlay codex-ide-section--highlight-overlay))
    (when (or force
              (not (eq section codex-ide-section--highlighted-section))
              (and section
                   (or (not overlay)
                       (/= (overlay-start overlay) line-start)
                       (/= (overlay-end overlay) line-end))))
      (codex-ide-section--delete-highlight-overlay)
      (setq codex-ide-section--highlighted-section section)
      (when section
        (setq overlay (make-overlay line-start line-end nil t t))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'priority '(nil . 1))
        (overlay-put overlay 'face 'codex-ide-section-highlight)
        (setq codex-ide-section--highlight-overlay overlay)))))

(defun codex-ide-section-post-command-hook ()
  "Track point movement and highlight the current line."
  (codex-ide-section-update-highlight))

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
    (let* ((body-start (codex-ide-section-body-start section))
           (overlay (make-overlay body-start
                                 (codex-ide-section-end section)
                                 nil nil nil)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'cursor-intangible t)
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

(defun codex-ide-section-toggle-siblings-at-point ()
  "Toggle all sibling sections for the heading at point.
If every sibling is collapsed, expand them all.  Otherwise, collapse them all."
  (interactive)
  (let* ((section (codex-ide-section-heading-at-point))
         (siblings (if section
                       (codex-ide-section--siblings section)
                     codex-ide-section--root-sections))
         (action (if (seq-every-p #'codex-ide-section-hidden siblings)
                     #'codex-ide-section-show
                   #'codex-ide-section-hide)))
    (unless siblings
      (user-error "No section at point"))
    (mapc action siblings)))

(defun codex-ide-section-mouse-toggle-section (event)
  "Toggle the section clicked in EVENT."
  (interactive "e")
  (let* ((pos (event-start event))
         (section (codex-ide-section-at-point (posn-point pos))))
    (when section
      (goto-char (codex-ide-section-heading-start section))
      (codex-ide-section-toggle section))))

(defun codex-ide-section-insert
    (type value title body-fn &optional hidden keymap properties)
  "Insert a section with TYPE, VALUE, TITLE, and BODY-FN.
BODY-FN is called with the new section object inserted as current parent.
When HIDDEN is non-nil, initially hide the section body.
When KEYMAP is non-nil, compose it with `codex-ide-section-heading-map'.
PROPERTIES is a plist of section options.  Supported keys:
`:interactive-heading' controls whether the heading is toggleable."
  (let ((inhibit-read-only t))
    (let* ((parent (car codex-ide-section--section-stack))
           (interactive-heading
            (if (plist-member properties :interactive-heading)
                (plist-get properties :interactive-heading)
              t))
           (section (codex-ide-section--create
                     :type type
                     :value value
                     :keymap keymap
                     :interactive-heading interactive-heading
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
