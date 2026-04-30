;;; codex-ide-diff-view-tests.el --- Tests for codex-ide diff views -*- lexical-binding: t; -*-

;;; Commentary:

;; Diff viewer coverage.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)
(require 'codex-ide)

(ert-deftest codex-ide-diff-open-buffer-displays-diff-mode-buffer ()
  (let ((display-call nil)
        diff-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (setq display-call (list buffer action))
                     nil)))
          (setq diff-buffer
                (codex-ide-diff-open-buffer
                 (string-join
                  '("diff --git a/foo.txt b/foo.txt"
                    "--- a/foo.txt"
                    "+++ b/foo.txt"
                    "@@ -1 +1 @@"
                    "-old"
                    "+new")
                  "\n")))
          (should (buffer-live-p diff-buffer))
          (should (equal (car display-call) diff-buffer))
          (with-current-buffer diff-buffer
            (should (derived-mode-p 'diff-mode))
            (should buffer-read-only)
            (should (string-match-p
                     (regexp-quote "diff --git a/foo.txt b/foo.txt")
                     (buffer-string)))
            (should (string-suffix-p "\n" (buffer-string)))
            (should (string-match-p "foo\\.txt" (buffer-name)))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer)))))

(ert-deftest codex-ide-diff-open-buffer-reuses-explicit-buffer-name ()
  (let ((display-calls nil)
        (buffer-name "*codex[my-project]*-diff")
        first-buffer
        second-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (push (list buffer action) display-calls)
                     nil)))
          (setq first-buffer
                (codex-ide-diff-open-buffer
                 "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                 buffer-name))
          (setq second-buffer
                (codex-ide-diff-open-buffer
                 "diff --git a/bar.txt b/bar.txt\n@@ -1 +1 @@\n-older\n+newer"
                 buffer-name))
          (should (eq first-buffer second-buffer))
          (should (equal (buffer-name first-buffer) buffer-name))
          (with-current-buffer first-buffer
            (should (string-match-p "bar\\.txt" (buffer-string)))
            (should-not (string-match-p "foo\\.txt" (buffer-string)))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest codex-ide-diff-buffer-name-for-session-appends-suffix ()
  (should (equal (codex-ide-diff-buffer-name-for-session "*codex[my-project]*")
                 "*codex[my-project]*-diff")))

(ert-deftest codex-ide-diff-open-buffer-errors-without-diff-text ()
  (should-error (codex-ide-diff-open-buffer nil) :type 'user-error)
  (should-error (codex-ide-diff-open-buffer "  \n") :type 'user-error))

(ert-deftest codex-ide-diff-open-combined-turn-buffer-uses-dedicated-buffer-name ()
  (let* ((session-buffer (generate-new-buffer "*codex[test]*"))
         (session (make-instance 'codex-ide-session :buffer session-buffer))
         (opened nil))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                   (lambda () session))
                  ((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                   (lambda (&optional resolved-session turn-id)
                     (should (eq resolved-session session))
                     (should-not turn-id)
                     "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"))
                  ((symbol-function 'codex-ide-diff-open-buffer)
                   (lambda (diff-text buffer-name)
                     (setq opened (list diff-text buffer-name))
                     nil)))
          (codex-ide-diff-open-combined-turn-buffer)
          (should (equal opened
                         '("diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                           "*codex[test]*-turn-diff"))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-diff-open-combined-turn-buffer-interactive-uses-point-turn ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-point]*"))
         (session (make-instance 'codex-ide-session :buffer session-buffer))
         (opened nil))
    (unwind-protect
        (with-current-buffer session-buffer
          (insert "> first\nresult\n\n> second\nresult\n")
          (goto-char (point-min))
          (let ((first-marker (copy-marker (point) nil)))
            (search-forward "> second")
            (let ((second-marker (copy-marker (match-beginning 0) nil)))
              (codex-ide--record-turn-start session "turn-1" first-marker)
              (codex-ide--record-turn-start session "turn-2" second-marker)
              (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                         (lambda () session))
                        ((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                         (lambda (&optional resolved-session turn-id)
                           (should (eq resolved-session session))
                           (should (equal turn-id "turn-2"))
                           "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"))
                        ((symbol-function 'codex-ide-diff-open-buffer)
                         (lambda (diff-text buffer-name)
                           (setq opened (list diff-text buffer-name))
                           nil)))
                (call-interactively #'codex-ide-diff-open-combined-turn-buffer)
                (should (equal opened
                               '("diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                                 "*codex[test-point]*-turn-diff")))))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

;;; codex-ide-diff-view-tests.el ends here
