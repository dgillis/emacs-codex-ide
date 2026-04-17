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
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "t"))
                  #'codex-ide-section-toggle-at-point))
      (should-not (eq (lookup-key codex-ide-section-mode-map (kbd "TAB"))
                      #'codex-ide-section-toggle-at-point))
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "^"))
                  #'codex-ide-section-up))
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "p"))
                  #'codex-ide-section-backward))
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "n"))
                  #'codex-ide-section-forward))
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "M-p"))
                  #'codex-ide-section-backward-sibling))
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "M-n"))
                  #'codex-ide-section-forward-sibling))
      (should (eq (lookup-key map (kbd "<double-mouse-1>"))
                  #'codex-ide-section-mouse-toggle-section)))))

(ert-deftest codex-ide-section-navigation-commands-follow-section-structure ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (codex-ide-section-insert
     'root-a 'root-a "Root A"
     (lambda (_section)
       (insert "root-a body\n")
       (codex-ide-section-insert
        'child-a 'child-a "Child A"
        (lambda (_child)
          (insert "child-a body\n")))
       (codex-ide-section-insert
        'child-b 'child-b "Child B"
        (lambda (_child)
          (insert "child-b body\n")))))
    (codex-ide-section-insert
     'root-b 'root-b "Root B"
     (lambda (_section)
       (insert "root-b body\n")))
    (goto-char (point-min))
    (search-forward "child-a body")
    (goto-char (match-beginning 0))
    (should (eq (codex-ide-section-value (codex-ide-section-containing-point))
                'child-a))
    (codex-ide-section-up)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'root-a))
    (codex-ide-section-forward)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'child-a))
    (codex-ide-section-forward)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'child-b))
    (codex-ide-section-forward)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'root-b))
    (codex-ide-section-backward)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'child-b))
    (codex-ide-section-backward-sibling)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'child-a))
    (should-error (codex-ide-section-backward-sibling) :type 'user-error)
    (codex-ide-section-forward-sibling)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'child-b))
    (should-error (codex-ide-section-forward-sibling) :type 'user-error)
    (codex-ide-section-up)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'root-a))
    (codex-ide-section-forward-sibling)
    (should (eq (codex-ide-section-value (codex-ide-section-at-point))
                'root-b))
    (should-error (codex-ide-section-forward) :type 'user-error)
    (should-error (codex-ide-section-up) :type 'user-error)))

(provide 'codex-ide-section-tests)

;;; codex-ide-section-tests.el ends here
