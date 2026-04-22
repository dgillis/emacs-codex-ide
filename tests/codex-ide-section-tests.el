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

(defun codex-ide-section-test--highlight-overlay ()
  "Return the current section highlight overlay in the buffer."
  (seq-find
   (lambda (overlay)
     (eq (overlay-get overlay 'face) 'codex-ide-section-highlight))
   (overlays-in (point-min) (point-max))))

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
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "T"))
                  #'codex-ide-section-toggle-siblings-at-point))
      (should (eq (lookup-key codex-ide-section-mode-map (kbd "TAB"))
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

(ert-deftest codex-ide-section-can-render-noninteractive-heading ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (let ((section
           (codex-ide-section-insert
            'root nil "Section"
            (lambda (_section)
              (insert "body line\n"))
            nil
            nil
            '(:interactive-heading nil))))
      (goto-char (point-min))
      (should-not (get-text-property (point) 'codex-ide-section))
      (should-not (codex-ide-section-indicator-overlay section))
      (should-not (seq-find
                   (lambda (overlay)
                     (overlay-get overlay 'codex-ide-section-indicator))
                   (overlays-in (point-min) (point-max)))))))

(ert-deftest codex-ide-section-toggle-siblings-at-point-expands-or-collapses-all ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (codex-ide-section-insert
     'root 'root "Root"
     (lambda (_section)
       (insert "root body\n")
       (codex-ide-section-insert
        'child-a 'child-a "Child A"
        (lambda (_child)
          (insert "child-a body\n"))
        t)
       (codex-ide-section-insert
        'child-b 'child-b "Child B"
        (lambda (_child)
          (insert "child-b body\n"))
        t)))
    (let* ((root (car codex-ide-section--root-sections))
           (siblings (codex-ide-section-children root))
           (child-a (car siblings))
           (child-b (cadr siblings)))
      (goto-char (codex-ide-section-heading-start child-a))
      (should (seq-every-p #'codex-ide-section-hidden siblings))
      (codex-ide-section-toggle-siblings-at-point)
      (should-not (codex-ide-section-hidden child-a))
      (should-not (codex-ide-section-hidden child-b))
      (codex-ide-section-toggle-siblings-at-point)
      (should (codex-ide-section-hidden child-a))
      (should (codex-ide-section-hidden child-b)))))

(ert-deftest codex-ide-section-toggle-siblings-at-point-uses-top-level-siblings-outside-heading ()
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
          (insert "child-a body\n")))))
    (codex-ide-section-insert
     'root-b 'root-b "Root B"
     (lambda (_section)
       (insert "root-b body\n")))
    (let ((root-a (car codex-ide-section--root-sections))
          (root-b (cadr codex-ide-section--root-sections)))
      (goto-char (point-min))
      (search-forward "child-a body")
      (goto-char (match-beginning 0))
      (should-not (codex-ide-section-hidden root-a))
      (should-not (codex-ide-section-hidden root-b))
      (codex-ide-section-toggle-siblings-at-point)
      (should (codex-ide-section-hidden root-a))
      (should (codex-ide-section-hidden root-b))
      (codex-ide-section-toggle-siblings-at-point)
      (should-not (codex-ide-section-hidden root-a))
      (should-not (codex-ide-section-hidden root-b)))))

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

(ert-deftest codex-ide-section-hide-starts-at-body ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (let ((section
           (codex-ide-section-insert
            'root-a 'root-a "Root A"
            (lambda (_section)
              (insert "root-a body\n"))
            t)))
      (let ((overlay (codex-ide-section-overlay section)))
        (should overlay)
        (should (= (overlay-start overlay)
                   (codex-ide-section-body-start section)))
        (should (overlay-get overlay 'cursor-intangible))
        (should-not (overlay-get overlay 'after-string))))))

(ert-deftest codex-ide-section-move-end-of-line-stays-on-collapsed-heading ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (let* ((first
            (codex-ide-section-insert
             'first 'first "First"
             (lambda (_section)
               (insert "first body\n"))
             t))
           (second
            (codex-ide-section-insert
             'second 'second "Second"
             (lambda (_section)
               (insert "second body\n"))
             t)))
      (goto-char (codex-ide-section-heading-start first))
      (move-end-of-line 1)
      (should (= (point) (1- (codex-ide-section-heading-end first))))
      (should (< (point) (codex-ide-section-heading-start second))))))

(ert-deftest codex-ide-section-post-command-hook-highlights-current-line ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (let (root child)
      (setq root
            (codex-ide-section-insert
             'root 'root "Root"
             (lambda (_section)
               (insert "root body\n")
               (setq child
                     (codex-ide-section-insert
                      'child 'child "Child"
                      (lambda (_child)
                        (insert "child body 1\nchild body 2\n")))))))
      (goto-char (codex-ide-section-body-start child))
      (codex-ide-section-post-command-hook)
      (let ((overlay (codex-ide-section-test--highlight-overlay))
            (expected-start (line-beginning-position))
            (expected-end (min (point-max) (1+ (line-end-position)))))
        (should overlay)
        (should (eq codex-ide-section--highlighted-section child))
        (should (= (overlay-start overlay) expected-start))
        (should (= (overlay-end overlay) expected-end)))
      (forward-line 1)
      (codex-ide-section-post-command-hook)
      (let ((overlay (codex-ide-section-test--highlight-overlay))
            (expected-start (line-beginning-position))
            (expected-end (min (point-max) (1+ (line-end-position)))))
        (should overlay)
        (should (eq codex-ide-section--highlighted-section child))
        (should (= (overlay-start overlay) expected-start))
        (should (= (overlay-end overlay) expected-end)))
      (goto-char (codex-ide-section-heading-start root))
      (codex-ide-section-post-command-hook)
      (let ((overlay (codex-ide-section-test--highlight-overlay))
            (expected-start (line-beginning-position))
            (expected-end (min (point-max) (1+ (line-end-position)))))
        (should overlay)
        (should (eq codex-ide-section--highlighted-section root))
        (should (= (overlay-start overlay) expected-start))
        (should (= (overlay-end overlay) expected-end))))))

(ert-deftest codex-ide-section-reset-clears-highlight-state ()
  (with-temp-buffer
    (codex-ide-section-mode)
    (codex-ide-section-reset)
    (codex-ide-section-insert
     'root 'root "Root"
     (lambda (_section)
       (insert "root body\n")))
    (goto-char (point-min))
    (codex-ide-section-post-command-hook)
    (should (codex-ide-section-test--highlight-overlay))
    (should codex-ide-section--highlighted-section)
    (codex-ide-section-reset)
    (should-not (codex-ide-section-test--highlight-overlay))
    (should-not codex-ide-section--highlighted-section)))

(provide 'codex-ide-section-tests)

;;; codex-ide-section-tests.el ends here
