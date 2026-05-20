;;; codex-ide-diff-model-tests.el --- Tests for codex-ide diff model -*- lexical-binding: t; -*-

;;; Commentary:

;; Parsed diff model coverage.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'codex-ide)

(ert-deftest codex-ide-diff-model-body-only-side-uses-dev-null-markers ()
  (let* ((added-diff
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- /dev/null"
             "+++ b/foo.txt"
             "first")
           "\n"))
         (removed-diff
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ /dev/null"
             "first")
           "\n"))
         (added-file (car (codex-ide-diff-model-parse-files added-diff)))
         (removed-file (car (codex-ide-diff-model-parse-files removed-diff))))
    (should (eq (codex-ide-diff-model-body-only-side added-file)
                'added))
    (should (eq (codex-ide-diff-model-body-only-side removed-file)
                'removed))))

(ert-deftest codex-ide-diff-model-body-only-side-falls-back-to-file-existence ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-model-" t)))
         (file-path (expand-file-name "foo.txt" root))
         (diff-text
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "first")
           "\n"))
         (file (car (codex-ide-diff-model-parse-files diff-text))))
    (unwind-protect
        (progn
          (should (eq (codex-ide-diff-model-body-only-side file root)
                      'removed))
          (with-temp-file file-path
            (insert "first\n"))
          (should (eq (codex-ide-diff-model-body-only-side file root)
                      'added)))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-model-file-stats-counts-body-only-side ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-model-" t)))
         (diff-text
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "first"
             "second")
           "\n"))
         (file (car (codex-ide-diff-model-parse-files diff-text))))
    (unwind-protect
        (progn
          (should (equal (codex-ide-diff-model-file-stats file root)
                         '(:path "foo.txt"
				 :added 0
				 :removed 2
				 :changed 2)))
          (with-temp-file (expand-file-name "foo.txt" root)
            (insert "first\nsecond\n"))
          (should (equal (codex-ide-diff-model-file-stats file root)
                         '(:path "foo.txt"
				 :added 2
				 :removed 0
				 :changed 2))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-model-file-stats-counts-grouped-body-only-side ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-model-" t)))
         (diff-text
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "first"
             "second"
             "diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "@@ -2 +2,2 @@"
             " second"
             "+newer")
           "\n"))
         (file (car (codex-ide-diff-model-group-files-by-path
                     (codex-ide-diff-model-parse-files diff-text)))))
    (unwind-protect
        (progn
          (should (equal (codex-ide-diff-model-file-stats file root)
                         '(:path "foo.txt"
                                 :added 1
                                 :removed 2
                                 :changed 3)))
          (with-temp-file (expand-file-name "foo.txt" root)
            (insert "first\nsecond\nnewer\n"))
          (should (equal (codex-ide-diff-model-file-stats file root)
                         '(:path "foo.txt"
                                 :added 3
                                 :removed 0
                                 :changed 3))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-model-source-location-tracks-body-only-lines ()
  (let ((diff-text
         (string-join
          '("diff --git a/foo.txt b/foo.txt"
            "--- a/foo.txt"
            "+++ b/foo.txt"
            "first"
            "second")
          "\n")))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 3)
                   '(:path "foo.txt" :line 1)))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 4)
                   '(:path "foo.txt" :line 2)))))

(provide 'codex-ide-diff-model-tests)

;;; codex-ide-diff-model-tests.el ends here
