;;; codex-ide-nav.el --- Focal point navigation for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared navigation helpers for Codex buffers that define mode-specific
;; "focal points" such as section headings, buttons, or the active prompt.

;;; Code:

(require 'button)
(require 'seq)
(require 'codex-ide-core)
(require 'codex-ide-section)

(defvar-local codex-ide-nav-focal-point-functions nil
  "List of functions returning focal points for the current buffer.

Each function should return a list of plists describing focusable locations.
Supported plist keys are:

- `:pos'   canonical position to move point to
- `:start' start of the focal region, defaults to `:pos'
- `:end'   end of the focal region, defaults to `:start' + 1
- `:kind'  optional focal-point kind symbol
- `:object' optional backing object")

(defvar codex-ide-nav-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "TAB") #'codex-ide-nav-button-forward)
    (define-key map (kbd "<backtab>") #'codex-ide-nav-button-backward)
    map)
  "Button keymap used on Codex-owned buttons.

This composes normal button behavior with Codex focal-point navigation.")

;;;###autoload
(defun codex-ide-nav-button-forward ()
  "Move to the next focal point from a Codex-owned button."
  (interactive)
  (codex-ide-nav-forward))

;;;###autoload
(defun codex-ide-nav-button-backward ()
  "Move to the previous focal point from a Codex-owned button."
  (interactive)
  (codex-ide-nav-backward))

(defun codex-ide-nav-button-keymap ()
  "Return the appropriate keymap for a newly created Codex button."
  codex-ide-nav-button-map)

(defun codex-ide-nav-collect-sections ()
  "Return visible Codex section headings in the current buffer."
  (mapcar
   (lambda (section)
     (list :pos (codex-ide-section-heading-start section)
           :start (codex-ide-section-heading-start section)
           :end (codex-ide-section-heading-end section)
           :kind 'section
           :object section))
   (seq-filter
    #'codex-ide-section--visible-p
    (codex-ide-section--all-sections))))

(defun codex-ide-nav-collect-buttons (&optional start end)
  "Return buttons between START and END in the current buffer."
  (setq start (or start (point-min))
        end (or end (point-max)))
  (let ((pos start)
        points)
    (while (and (< pos end)
                (setq pos (next-button pos)))
      (if (>= pos end)
          (setq pos end)
        (when-let ((button (button-at pos)))
          (push (list :pos (button-start button)
                      :start (button-start button)
                      :end (button-end button)
                      :kind 'button
                      :object button)
                points)
          (setq pos (max (1+ pos) (button-end button))))))
    (nreverse points)))

(defun codex-ide-nav-collect-session-input (session)
  "Return the active prompt focal point for SESSION, when present."
  (when-let* ((overlay (codex-ide-session-input-overlay session))
              (start (codex-ide-session-input-start-marker session))
              (buffer (overlay-buffer overlay))
              (end (overlay-end overlay)))
    (when (and (eq buffer (current-buffer))
               (markerp start)
               (eq (marker-buffer start) buffer))
      (list (list :pos (marker-position start)
                  :start (marker-position start)
                  :end end
                  :kind 'prompt
                  :object session)))))

(defun codex-ide-nav--normalize-focal-point (point)
  "Return focal POINT with normalized `:start' and `:end' fields."
  (when-let ((pos (plist-get point :pos)))
    (let* ((start (or (plist-get point :start) pos))
           (end (max start (or (plist-get point :end) (1+ start)))))
      (when (and (integer-or-marker-p start)
                 (integer-or-marker-p end)
                 (not (invisible-p start)))
        (list :pos pos
              :start start
              :end end
              :kind (plist-get point :kind)
              :object (plist-get point :object))))))

(defun codex-ide-nav--focal-point-lessp (left right)
  "Return non-nil when focal LEFT sorts before RIGHT."
  (< (plist-get left :start) (plist-get right :start)))

(defun codex-ide-nav--focal-points ()
  "Return normalized focal points for the current buffer."
  (let ((points nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (fn codex-ide-nav-focal-point-functions)
      (dolist (point (funcall fn))
        (when-let ((normalized (codex-ide-nav--normalize-focal-point point)))
          (let ((key (cons (plist-get normalized :kind)
                           (plist-get normalized :start))))
            (unless (gethash key seen)
              (puthash key t seen)
              (push normalized points))))))
    (sort points #'codex-ide-nav--focal-point-lessp)))

(defun codex-ide-nav--focal-point-at-point (points pos)
  "Return the focal point from POINTS containing POS, if any."
  (seq-find
   (lambda (point)
     (let ((start (plist-get point :start))
           (end (plist-get point :end)))
       (and (<= start pos)
            (< pos end))))
   points))

(defun codex-ide-nav--move (direction)
  "Move to the next focal point in DIRECTION.

DIRECTION should be 1 for forward or -1 for backward."
  (unless (memq direction '(-1 1))
    (error "Unsupported Codex navigation direction: %s" direction))
  (let* ((points (codex-ide-nav--focal-points))
         (current (codex-ide-nav--focal-point-at-point points (point)))
         (origin (if current
                     (plist-get current :start)
                   (point)))
         target)
    (unless points
      (user-error "No focal points in this buffer"))
    (if (> direction 0)
        (setq target
              (seq-find (lambda (point)
                          (> (plist-get point :start) origin))
                        points))
      (dolist (point points)
        (when (< (plist-get point :start) origin)
          (setq target point))))
    (unless target
      (user-error (if (> direction 0)
                      "No next focal point"
                    "No previous focal point")))
    (goto-char (plist-get target :pos))
    target))

;;;###autoload
(defun codex-ide-nav-forward ()
  "Move point to the next focal point in the current buffer."
  (interactive)
  (codex-ide-nav--move 1))

;;;###autoload
(defun codex-ide-nav-backward ()
  "Move point to the previous focal point in the current buffer."
  (interactive)
  (codex-ide-nav--move -1))

(provide 'codex-ide-nav)

;;; codex-ide-nav.el ends here
