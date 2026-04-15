;;; codex-ide-section-tests.el --- Tests for codex-ide sections -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-section'.

;;; Code:

(require 'ert)
(require 'seq)
(require 'codex-ide-section)

(defun codex-ide-section-test--indicator-overlay-at-point ()
  "Return the section indicator overlay at point."
  (seq-find
   (lambda (overlay)
     (overlay-get overlay 'codex-ide-section-indicator))
   (overlays-at (point))))

(ert-deftest codex-ide-section-toggle-at-point-hides-and-shows-body ()
  (with-temp-buffer
    (special-mode)
    (setq-local buffer-invisibility-spec '(t))
    (codex-ide-section-reset)
    (codex-ide-section-insert
     'root nil "Section"
     (lambda (_section)
       (insert "body line\n")))
    (goto-char (point-min))
    (should (codex-ide-section-at-point))
    (should-not (invisible-p (save-excursion
                               (forward-line 1)
                               (point))))
    (codex-ide-section-toggle-at-point)
    (should (invisible-p (save-excursion
                           (forward-line 1)
                           (point))))
    (should
     (equal (overlay-get (codex-ide-section-test--indicator-overlay-at-point)
                         'before-string)
            (propertize "> " 'face 'shadow)))
    (codex-ide-section-toggle-at-point)
    (should-not (invisible-p (save-excursion
                               (forward-line 1)
                               (point))))
    (should
     (equal (overlay-get (codex-ide-section-test--indicator-overlay-at-point)
                         'before-string)
            (propertize "v " 'face 'shadow)))))

(ert-deftest codex-ide-section-insert-tracks-nested-sections ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (codex-ide-section-insert
     'parent 'alpha "Parent"
     (lambda (section)
       (should (eq (codex-ide-section-value section) 'alpha))
       (codex-ide-section-insert
        'child 'beta "Child"
        (lambda (_child)
          (insert "details\n"))
        t)))
    (should (= (length codex-ide-section--root-sections) 1))
    (let* ((parent (car codex-ide-section--root-sections))
           (child (car (codex-ide-section-children parent))))
      (should (eq (codex-ide-section-parent child) parent))
      (should (codex-ide-section-hidden child))
      (should (overlayp (codex-ide-section-overlay child))))))

(ert-deftest codex-ide-section-mode-installs-heading-keymap ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (codex-ide-section-insert
     'root nil "Section"
     (lambda (_section)
       (insert "body line\n")))
    (goto-char (point-min))
    (let ((map (get-text-property (point) 'keymap)))
      (should map)
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "TAB"))
                  #'codex-ide-section-toggle-at-point))
      (should (eq (lookup-key map (kbd "<double-mouse-1>"))
                  #'codex-ide-section-mouse-toggle-section)))))

(provide 'codex-ide-section-tests)

;;; codex-ide-section-tests.el ends here
