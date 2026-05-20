;;; codex-ide-diff-view-tests.el --- Tests for codex-ide diff views -*- lexical-binding: t; -*-

;;; Commentary:

;; Diff viewer coverage.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)
(require 'codex-ide)

(ert-deftest codex-ide-diff-mode-docstring-lists-key-bindings ()
  (let ((doc (documentation #'codex-ide-diff-mode t)))
    (dolist (binding '("* \\<codex-ide-diff-mode-map>\\[codex-ide-diff-toggle-file-at-point]"
                       "* \\[codex-ide-diff-collapse-all-files]"
                       "* \\[codex-ide-diff-expand-all-files]"
                       "* \\[codex-ide-diff-goto-source-at-point]"))
      (should (string-match-p (regexp-quote binding) doc)))))

(ert-deftest codex-ide-session-diff-mode-docstring-lists-key-bindings ()
  (let ((doc (documentation #'codex-ide-session-diff-mode t)))
    (dolist (binding '("* \\<codex-ide-session-diff-mode-map>\\[codex-ide-session-diff-follow-live]"
                       "* \\[codex-ide-session-diff-follow-transcript]"
                       "* \\[codex-ide-session-diff-pin-current-turn]"
                       "* \\[codex-ide-session-diff-refresh]"))
      (should (string-match-p (regexp-quote binding) doc)))))

(ert-deftest codex-ide-diff-open-buffer-displays-codex-diff-mode-buffer ()
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
            (should (eq major-mode 'codex-ide-diff-mode))
            (should (derived-mode-p 'codex-ide-section-mode))
            (should buffer-read-only)
            (should-not (string-match-p
                         (regexp-quote "diff --git a/foo.txt b/foo.txt")
                         (buffer-string)))
            (should (string-match-p
                     (regexp-quote "@@ -1 +1 @@")
                     (buffer-string)))
            (should (string-match-p
                     (regexp-quote "+new")
                     (buffer-string)))
            (should (string-match-p
                     (regexp-quote "diff --git a/foo.txt b/foo.txt")
                     codex-ide-diff--display-text))
            (should (string-suffix-p "\n" (buffer-string)))
            (should (string-match-p "foo\\.txt" (buffer-name)))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer)))))

(ert-deftest codex-ide-diff-open-buffer-binds-return-to-source-jump ()
  (let ((display-call nil)
        diff-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (setq display-call (list buffer action))
                     nil)))
          (setq diff-buffer
                (codex-ide-diff-open-buffer
                 "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                 nil
                 default-directory))
          (should (equal (car display-call) diff-buffer))
          (with-current-buffer diff-buffer
            (should (eq (key-binding (kbd "RET"))
                        #'codex-ide-diff-goto-source-at-point))
            (should (eq (key-binding (kbd "<return>"))
                        #'codex-ide-diff-goto-source-at-point))
            (should (equal (expand-file-name default-directory)
                           (file-name-as-directory
                            (expand-file-name default-directory))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer)))))

(ert-deftest codex-ide-diff-open-buffer-shortens-project-local-absolute-headers ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-view-" t)))
         (file (expand-file-name "lib/foo.txt" root))
         (diff-text (string-join
                     (list (format "diff --git a/%s b/%s" file file)
                           (format "--- a/%s" file)
                           (format "+++ b/%s" file)
                           "@@ -1 +1 @@"
                           "-old"
                           "+new")
                     "\n"))
         (display-call nil)
         diff-buffer
         visited-buffer)
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "new\n"))
          (cl-letf (((symbol-function 'codex-ide-display-buffer)
                     (lambda (buffer &optional action)
                       (setq display-call (list buffer action))
                       nil)))
            (setq diff-buffer
                  (codex-ide-diff-open-buffer diff-text nil root)))
          (should (equal (car display-call) diff-buffer))
          (with-current-buffer diff-buffer
            (should-not (string-match-p
                         (regexp-quote "diff --git a/lib/foo.txt b/lib/foo.txt")
                         (buffer-string)))
            (should-not (string-match-p
                         (regexp-quote file)
                         (buffer-string)))
            (goto-char (point-min))
            (search-forward "+new")
            (codex-ide-diff-goto-source-at-point)
            (setq visited-buffer (current-buffer))
            (should (equal (buffer-file-name visited-buffer) file))
            (should (= (line-number-at-pos) 1))))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-mode-binds-file-folding-commands ()
  (with-temp-buffer
    (codex-ide-diff-mode)
    (should (eq (key-binding (kbd "C-c TAB"))
                #'codex-ide-diff-toggle-file-at-point))
    (should (eq (key-binding (kbd "C-c C-a"))
                #'codex-ide-diff-collapse-all-files))
    (should (eq (key-binding (kbd "C-c C-e"))
                #'codex-ide-diff-expand-all-files))))

(ert-deftest codex-ide-diff-collapse-and-expand-all-files ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new"
              "diff --git a/bar.txt b/bar.txt"
              "--- a/bar.txt"
              "+++ b/bar.txt"
              "@@ -1 +1 @@"
              "-older"
              "+newer")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (should (= (codex-ide-diff-collapse-all-files) 2))
    (should (seq-every-p #'codex-ide-section-hidden
                         (codex-ide-diff--file-sections)))
    (codex-ide-diff-expand-all-files)
    (should-not (seq-some #'codex-ide-section-hidden
                          (codex-ide-diff--file-sections)))))

(ert-deftest codex-ide-diff-toggle-file-at-point-folds-single-file ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new"
              "diff --git a/bar.txt b/bar.txt"
              "--- a/bar.txt"
              "+++ b/bar.txt"
              "@@ -1 +1 @@"
              "-older"
              "+newer")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (goto-char (point-min))
    (search-forward "foo.txt +1 -1")
    (beginning-of-line)
    (codex-ide-diff-toggle-file-at-point)
    (let ((sections (codex-ide-diff--file-sections)))
      (should (= (length sections) 2))
      (should (codex-ide-section-hidden (car sections)))
      (should-not (codex-ide-section-hidden (cadr sections))))
    (codex-ide-diff-toggle-file-at-point)
    (should-not (seq-some #'codex-ide-section-hidden
                          (codex-ide-diff--file-sections)))))

(ert-deftest codex-ide-diff-collapse-all-files-supports-headerless-normalized-diff ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new"
              "--- a/bar.txt"
              "+++ b/bar.txt"
              "@@ -1 +1 @@"
              "-older"
              "+newer")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (should (= (codex-ide-diff-collapse-all-files) 2))))

(ert-deftest codex-ide-diff-render-groups-repeated-file-sections ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new"
              "diff --git a/bar.txt b/bar.txt"
              "--- a/bar.txt"
              "+++ b/bar.txt"
              "@@ -1 +1 @@"
              "-before"
              "+after"
              "diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -10 +10 @@"
              "-older"
              "+newer")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (let ((sections (codex-ide-diff--file-sections)))
      (should (= (length sections) 2))
      (should (string-match-p
               (regexp-quote "foo.txt +2 -2")
               (buffer-substring-no-properties
                (codex-ide-section-heading-start (car sections))
                (codex-ide-section-heading-end (car sections)))))
      (should (= (length (codex-ide-section-children (car sections))) 2))
      (should (string-match-p
               (regexp-quote "bar.txt +1 -1")
               (buffer-substring-no-properties
                (codex-ide-section-heading-start (cadr sections))
		(codex-ide-section-heading-end (cadr sections))))))))

(ert-deftest codex-ide-diff-render-inserts-collapsed-summary-section ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1,2 +1,3 @@"
              " keep"
              "-old"
              "+new"
              "+extra"
              "diff --git a/bar.txt b/bar.txt"
              "--- a/bar.txt"
              "+++ b/bar.txt"
              "@@ -10 +10 @@"
              "-before"
              "+after")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (let ((summary (car codex-ide-section--root-sections)))
      (should (eq (codex-ide-section-type summary) 'summary))
      (should-not (codex-ide-section-hidden summary))
      (should (string-match-p
               (regexp-quote "2 files changed, 3 insertions(+), 2 deletions(-)")
               (buffer-substring-no-properties
                (codex-ide-section-heading-start summary)
                (codex-ide-section-heading-end summary)))))
    (should (string-match-p
             (rx line-start "foo.txt" (+ space) "|" (+ space) "3" (+ space) "++-")
             (buffer-string)))
    (should (string-match-p
             (rx line-start "bar.txt" (+ space) "|" (+ space) "2" (+ space) "+-")
             (buffer-string)))
    (should (string-match-p
             (rx line-start "bar.txt" (+ space) "|" (+ space) "2" (+ space) "+-"
                 "\n\n"
                 "foo.txt +2 -1")
             (buffer-substring-no-properties (point-min) (point-max))))
    (should (= (length (codex-ide-diff--file-sections)) 2))))

(ert-deftest codex-ide-diff-render-section-headings-are-bold ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (cl-labels ((face-at (section)
                  (get-text-property
                   (codex-ide-section-heading-start section)
                   'face))
                (face-includes-p (face target)
                  (if (listp face)
                      (memq target face)
                    (eq face target))))
      (let* ((summary (car codex-ide-section--root-sections))
             (file (car (codex-ide-diff--file-sections)))
             (hunk (car (codex-ide-section-children file)))
             (hunk-face (face-at hunk)))
        (should (face-includes-p (face-at summary) 'bold))
        (should (face-includes-p (face-at file) 'bold))
        (should (face-includes-p hunk-face 'bold))
        (should (= (get-text-property
                    (codex-ide-section-heading-start hunk)
                    'codex-ide-diff-line-index)
                   3))))))

(ert-deftest codex-ide-diff-render-file-heading-shows-nonzero-line-stats ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1,2 +1,3 @@"
              " keep"
              "-old"
              "+new"
              "+extra"
              "diff --git a/remove-only.txt b/remove-only.txt"
              "--- a/remove-only.txt"
              "+++ b/remove-only.txt"
              "@@ -1,2 +1 @@"
              "-gone"
              " keep"
              "diff --git a/add-only.txt b/add-only.txt"
              "--- a/add-only.txt"
              "+++ b/add-only.txt"
              "@@ -1 +1,2 @@"
              " keep"
              "+added")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (let* ((sections (codex-ide-diff--file-sections))
           (foo (car sections))
           (remove-only (cadr sections))
           (add-only (caddr sections))
           (foo-heading
            (buffer-substring
             (codex-ide-section-heading-start foo)
             (codex-ide-section-heading-end foo)))
           (remove-heading
            (buffer-substring-no-properties
             (codex-ide-section-heading-start remove-only)
             (codex-ide-section-heading-end remove-only)))
           (add-heading
            (buffer-substring-no-properties
             (codex-ide-section-heading-start add-only)
             (codex-ide-section-heading-end add-only))))
      (should (string-match-p
               (regexp-quote "foo.txt +2 -1")
               (substring-no-properties foo-heading)))
      (should (string-match-p
               (regexp-quote "remove-only.txt -1")
               remove-heading))
      (should-not (string-match-p
                   (regexp-quote "remove-only.txt +")
                   remove-heading))
      (should (string-match-p
               (regexp-quote "add-only.txt +1")
               add-heading))
      (should-not (string-match-p
                   (regexp-quote "add-only.txt +1 -")
                   add-heading))
      (should (memq 'codex-ide-diff-added-snippet-face
                    (get-text-property
                     (string-match-p (regexp-quote "+2") foo-heading)
                     'face
                     foo-heading)))
      (should (memq 'codex-ide-diff-removed-snippet-face
                    (get-text-property
                     (string-match-p (regexp-quote "-1") foo-heading)
                     'face
                     foo-heading))))))

(ert-deftest codex-ide-diff-render-body-only-new-file-lines-as-added ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-view-" t)))
         (file-path (expand-file-name "foo.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "first\nsecond\n"))
          (with-temp-buffer
            (let ((diff-text
                   (string-join
                    '("diff --git a/foo.txt b/foo.txt"
                      "--- a/foo.txt"
                      "+++ b/foo.txt"
                      "first"
                      "second")
                    "\n")))
              (codex-ide-diff-mode)
              (codex-ide-diff--render-text diff-text diff-text root))
            (let* ((file (car (codex-ide-diff--file-sections)))
                   (heading
                    (buffer-substring
                     (codex-ide-section-heading-start file)
                     (codex-ide-section-heading-end file))))
              (let ((stat-index
                     (string-match-p (regexp-quote "+2")
                                     (substring-no-properties heading))))
                (should stat-index)
                (should (memq 'codex-ide-diff-added-snippet-face
                              (get-text-property stat-index 'face heading))))
              (goto-char (point-min))
              (search-forward "first")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'codex-ide-file-diff-added-face))
              (search-forward "second")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'codex-ide-file-diff-added-face)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-render-grouped-body-only-new-file-lines-as-added ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-view-" t)))
         (file-path (expand-file-name "foo.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "first\nsecond\nnewer\n"))
          (with-temp-buffer
            (let ((diff-text
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
                    "\n")))
              (codex-ide-diff-mode)
              (codex-ide-diff--render-text diff-text diff-text root))
            (let* ((file (car (codex-ide-diff--file-sections)))
                   (heading
                    (buffer-substring
                     (codex-ide-section-heading-start file)
                     (codex-ide-section-heading-end file))))
              (should (string-match-p
                       (regexp-quote "foo.txt +3")
                       (substring-no-properties heading)))
              (goto-char (point-min))
              (search-forward "first")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'codex-ide-file-diff-added-face))
              (search-forward "second")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'codex-ide-file-diff-added-face))
              (search-forward "+newer")
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'codex-ide-file-diff-added-face)))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-render-body-only-deleted-file-lines-as-removed ()
  (let ((root (file-name-as-directory
               (make-temp-file "codex-ide-diff-view-" t))))
    (unwind-protect
        (with-temp-buffer
          (let ((diff-text
                 (string-join
                  '("diff --git a/foo.txt b/foo.txt"
                    "--- a/foo.txt"
                    "+++ b/foo.txt"
                    "first"
                    "second")
                  "\n")))
            (codex-ide-diff-mode)
            (codex-ide-diff--render-text diff-text diff-text root))
          (let* ((file (car (codex-ide-diff--file-sections)))
                 (heading
                  (buffer-substring
                   (codex-ide-section-heading-start file)
                   (codex-ide-section-heading-end file))))
            (let ((stat-index
                   (string-match-p (regexp-quote "-2")
                                   (substring-no-properties heading))))
              (should stat-index)
              (should (memq 'codex-ide-diff-removed-snippet-face
                            (get-text-property stat-index 'face heading))))
            (goto-char (point-min))
            (search-forward "first")
            (should (eq (get-text-property (match-beginning 0) 'face)
                        'codex-ide-file-diff-removed-face))
            (search-forward "second")
            (should (eq (get-text-property (match-beginning 0) 'face)
                        'codex-ide-file-diff-removed-face))))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest codex-ide-diff-render-summary-groups-repeated-file-stats ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new"
              "diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -10 +10,2 @@"
              "+extra")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (let ((summary (car codex-ide-section--root-sections)))
      (should (string-match-p
               (regexp-quote "1 file changed, 2 insertions(+), 1 deletion(-)")
               (buffer-substring-no-properties
                (codex-ide-section-heading-start summary)
                (codex-ide-section-heading-end summary)))))
    (should (string-match-p
             (rx line-start "foo.txt" (+ space) "|" (+ space) "3" (+ space) "++-")
             (buffer-string)))))

(ert-deftest codex-ide-diff-render-passes-initial-state-to-new-file-fold-predicate ()
  (with-temp-buffer
    (let ((first-diff
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new")
            "\n"))
          (second-diff
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+newer"
              "diff --git a/bar.txt b/bar.txt"
              "--- a/bar.txt"
              "+++ b/bar.txt"
              "@@ -3 +3 @@"
              "-before"
              "+after")
            "\n"))
          captured-state)
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text first-diff first-diff default-directory)
      (codex-ide-section-hide (car (codex-ide-diff--file-sections)))
      (let ((codex-ide-diff-new-file-section-fold-predicate
             (lambda (initial-state)
               (setq captured-state initial-state)
               t)))
        (codex-ide-diff--render-text second-diff second-diff default-directory))
      (should
       (cl-some
        (lambda (entry)
          (and (equal (car entry) '((file "foo.txt")))
               (cdr entry)))
        (alist-get 'hidden captured-state)))
      (let ((bar (cl-find-if
                  (lambda (section)
                    (equal (plist-get (codex-ide-section-value section) :path)
                           "bar.txt"))
                  (codex-ide-diff--file-sections))))
        (should bar)
        (should (codex-ide-section-hidden bar))))))

(ert-deftest codex-ide-diff-render-hides-ordinary-file-headers ()
  (with-temp-buffer
    (let ((diff-text
           (string-join
            '("diff --git a/foo.txt b/foo.txt"
              "index 1234567..89abcde 100644"
              "--- a/foo.txt"
              "+++ b/foo.txt"
              "@@ -1 +1 @@"
              "-old"
              "+new")
            "\n")))
      (codex-ide-diff-mode)
      (codex-ide-diff--render-text diff-text diff-text default-directory))
    (should-not (string-match-p
                 (regexp-quote "diff --git a/foo.txt b/foo.txt")
                 (buffer-string)))
    (should-not (string-match-p
                 (regexp-quote "index 1234567..89abcde 100644")
                 (buffer-string)))
    (should-not (string-match-p
                 (regexp-quote "--- a/foo.txt")
                 (buffer-string)))
    (should-not (string-match-p
                 (regexp-quote "+++ b/foo.txt")
                 (buffer-string)))
    (should (string-match-p
             (regexp-quote "@@ -1 +1 @@")
             (buffer-string)))
    (should (string-match-p
             (regexp-quote "+new")
             (buffer-string)))))

(ert-deftest codex-ide-diff-source-location-tracks-hunk-new-lines ()
  (let ((diff-text
         (string-join
          '("diff --git a/foo.txt b/bar.txt"
            "--- a/foo.txt"
            "+++ b/bar.txt"
            "@@ -1,3 +10,4 @@"
            " context"
            "-old"
            "+new"
            " after")
          "\n")))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 4)
                   '(:path "bar.txt" :line 10)))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 5)
                   '(:path "bar.txt" :line 11)))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 6)
                   '(:path "bar.txt" :line 11)))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 7)
                   '(:path "bar.txt" :line 12)))))

(ert-deftest codex-ide-diff-source-location-resolves-normalized-headerless-patch ()
  (let* ((item `((type . "fileChange")
                 (changes . (((path . "foo.txt")
                              (diff . ,(string-join
                                        '("@@ -3,2 +3,3 @@"
                                          " context"
                                          "+new")
                                        "\n")))))))
         (diff-text (codex-ide--file-change-diff-text item)))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 4)
                   '(:path "foo.txt" :line 3)))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 5)
                   '(:path "foo.txt" :line 4)))))

(ert-deftest codex-ide-diff-source-location-resolves-body-only-new-file ()
  (let ((diff-text
         (string-join
          '("diff --git a/foo.txt b/foo.txt"
            "--- /dev/null"
            "+++ b/foo.txt"
            "first"
            "second")
          "\n")))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 3)
                   '(:path "foo.txt" :line 1)))
    (should (equal (codex-ide-diff-model-source-location-for-line diff-text 4)
                   '(:path "foo.txt" :line 2)))))

(ert-deftest codex-ide-diff-goto-source-resolves-project-relative-header ()
  (let* ((root (file-name-as-directory
                (make-temp-file "codex-ide-diff-view-" t)))
         (file (expand-file-name "lib/foo.txt" root))
         (diff-text (string-join
                     '("diff --git a/lib/foo.txt b/lib/foo.txt"
                       "--- a/lib/foo.txt"
                       "+++ b/lib/foo.txt"
                       "@@ -1 +1 @@"
                       "-old"
                       "+new")
                     "\n"))
         visited-buffer)
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "new\n"))
          (codex-ide-diff-goto-source diff-text 5 root)
          (setq visited-buffer (current-buffer))
          (should (equal (buffer-file-name visited-buffer) file))
          (should (= (line-number-at-pos) 1)))
      (when (buffer-live-p visited-buffer)
        (kill-buffer visited-buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

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

(ert-deftest codex-ide-session-diff-buffer-name-for-session-appends-session-suffix ()
  (should (equal (codex-ide-session-diff-buffer-name-for-session
                  "*codex[my-project]*")
                 "*codex[my-project]*-session-diff")))

(ert-deftest codex-ide-session-diff-mode-sets-header-line ()
  (with-temp-buffer
    (codex-ide-session-diff-mode)
    (should header-line-format)))

(ert-deftest codex-ide-session-diff-header-line-shows-source-turn-and-colored-stat ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-header-line]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (turn-id "019e25d6-2b37-7550-a989-922b69c9c56a")
         (display-text
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "@@ -1 +1 @@"
             "-old"
             "+new")
           "\n"))
         (header nil))
    (unwind-protect
        (with-temp-buffer
          (codex-ide-session-diff-mode)
          (setq-local codex-ide-session-diff--header-line
                      (codex-ide-session-diff--format-header-line
                       session
                       'pinned
                       turn-id
                       display-text))
          (setq header codex-ide-session-diff--header-line)
          (should (equal (substring-no-properties header)
                         " Pinned diff | turn 922b69c9c56a | 1 file +1 -1"))
          (let ((added-index (string-match-p "\\+1" header))
                (removed-index (string-match-p "-1\\'" header)))
            (should (member 'codex-ide-file-diff-added-face
                            (ensure-list
                             (get-text-property added-index 'face header))))
            (should (member 'codex-ide-file-diff-removed-face
                            (ensure-list
                             (get-text-property removed-index 'face header))))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-header-line-shows-no-changes ()
  (with-temp-buffer
    (codex-ide-session-diff-mode)
    (setq-local codex-ide-session-diff--header-line
                (codex-ide-session-diff--format-header-line
                 nil
                 'transcript
                 nil
                 "# No prompt at transcript position\n# Press C-h m for help."))
    (should (equal (substring-no-properties
                    codex-ide-session-diff--header-line)
                   " Turn-at-point diff | no turn at point | follows point | no changes"))))

(ert-deftest codex-ide-session-diff-header-line-shows-running-turn ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-running-header-line]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :current-turn-id "019e25c9-5964-7b60-be3b-18b9ecaa82f3"
                                 :directory default-directory))
         (display-text
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "@@ -1 +1 @@"
             "-old"
             "+new")
           "\n")))
    (unwind-protect
        (with-temp-buffer
          (codex-ide-session-diff-mode)
          (setq-local codex-ide-session-diff--header-line
                      (codex-ide-session-diff--format-header-line
                       session
                       'live
                       nil
                       display-text))
          (should (equal (substring-no-properties
                          codex-ide-session-diff--header-line)
                         " Live diff | running turn | 1 file +1 -1")))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-empty-message-points-to-mode-help ()
  (should (equal (codex-ide-session-diff--empty-message
                  'transcript "turn-1" "No prompt at transcript position")
                 (string-join
                  '("# No prompt at transcript position"
                    "# Press C-h m for help.")
                  "\n"))))

(ert-deftest codex-ide-session-diff-open-uses-canonical-mode-and-live-source ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-session-diff]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (display-call nil)
         diff-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                   (lambda () session))
                  ((symbol-function 'codex-ide-display-buffer)
                   (lambda (buffer &optional action)
                     (setq display-call (list buffer action))
                     nil))
                  ((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                   (lambda (resolved-session &optional turn-id)
                     (should (eq resolved-session session))
                     (should-not turn-id)
                     "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new")))
          (setq diff-buffer (codex-ide-session-diff-open))
          (should (buffer-live-p diff-buffer))
          (should (eq (car display-call) diff-buffer))
          (should (equal (buffer-name diff-buffer)
                         "*codex[test-session-diff]*-session-diff"))
          (with-current-buffer diff-buffer
            (should (eq major-mode 'codex-ide-session-diff-mode))
            (should (derived-mode-p 'codex-ide-diff-mode))
            (should (derived-mode-p 'codex-ide-section-mode))
            (should (eq codex-ide-session-diff--session session))
            (should (eq codex-ide-session-diff-source 'live))
            (should (equal (substring-no-properties
                            codex-ide-session-diff--header-line)
                           " Live diff | latest turn | 1 file +1 -1"))
            (should (string-match-p "foo\\.txt" (buffer-string)))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-open-kills-diff-with-session-buffer ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-session-diff-kill]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         diff-buffer)
    (unwind-protect
        (progn
          (with-current-buffer session-buffer
            (setq-local codex-ide--session session))
          (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                     (lambda () session))
                    ((symbol-function 'codex-ide-display-buffer)
                     (lambda (_buffer &optional _action) nil))
                    ((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (_resolved-session &optional _turn-id)
                       "diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new")))
            (setq diff-buffer (codex-ide-session-diff-open)))
          (should (buffer-live-p diff-buffer))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer session-buffer))
          (should-not (buffer-live-p diff-buffer)))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-refreshes-live-buffer ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-live-refresh]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         (diff-text "diff --git a/one.txt b/one.txt\n@@ -1 +1 @@\n-old\n+new"))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'live))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (should-not turn-id)
                       diff-text)))
            (codex-ide-session-diff-note-session-updated session)
            (with-current-buffer diff-buffer
              (should (string-match-p "one\\.txt" (buffer-string))))
            (setq diff-text
                  "diff --git a/two.txt b/two.txt\n@@ -1 +1 @@\n-old\n+new")
            (codex-ide-session-diff-note-session-updated session)
            (with-current-buffer diff-buffer
              (should (string-match-p "two\\.txt" (buffer-string)))
              (should-not (string-match-p "one\\.txt" (buffer-string))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-preserves-folded-sections ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-live-folds]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         (diff-text
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "@@ -1 +1 @@"
             "-old"
             "+new"
             "diff --git a/bar.txt b/bar.txt"
             "--- a/bar.txt"
             "+++ b/bar.txt"
             "@@ -3 +3 @@"
             "-before"
             "+after")
           "\n")))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'live))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (should-not turn-id)
                       diff-text)))
            (codex-ide-session-diff-note-session-updated session)
            (with-current-buffer diff-buffer
              (let* ((foo (cl-find-if
                           (lambda (section)
                             (equal (plist-get (codex-ide-section-value section) :path)
                                    "foo.txt"))
                           (codex-ide-diff--file-sections)))
                     (bar (cl-find-if
                           (lambda (section)
                             (equal (plist-get (codex-ide-section-value section) :path)
                                    "bar.txt"))
                           (codex-ide-diff--file-sections)))
                     (bar-hunk (car (codex-ide-section-children bar))))
                (codex-ide-section-hide foo)
                (codex-ide-section-hide bar-hunk)))
            (setq diff-text
                  (string-join
                   '("diff --git a/foo.txt b/foo.txt"
                     "--- a/foo.txt"
                     "+++ b/foo.txt"
                     "@@ -1 +1 @@"
                     "-old"
                     "+newer"
                     "diff --git a/bar.txt b/bar.txt"
                     "--- a/bar.txt"
                     "+++ b/bar.txt"
                     "@@ -3 +3 @@"
                     "-before"
                     "+after"
                     "diff --git a/baz.txt b/baz.txt"
                     "--- a/baz.txt"
                     "+++ b/baz.txt"
                     "@@ -5 +5 @@"
                     "-then"
                     "+now")
                   "\n"))
            (codex-ide-session-diff-note-session-updated session)
            (with-current-buffer diff-buffer
              (let* ((files (codex-ide-diff--file-sections))
                     (foo (cl-find-if
                           (lambda (section)
                             (equal (plist-get (codex-ide-section-value section) :path)
                                    "foo.txt"))
                           files))
                     (bar (cl-find-if
                           (lambda (section)
                             (equal (plist-get (codex-ide-section-value section) :path)
                                    "bar.txt"))
                           files))
                     (baz (cl-find-if
                           (lambda (section)
                             (equal (plist-get (codex-ide-section-value section) :path)
                                    "baz.txt"))
                           files))
                     (bar-hunk (car (codex-ide-section-children bar))))
                (should (codex-ide-section-hidden foo))
                (should (codex-ide-section-hidden bar-hunk))
                (should-not (codex-ide-section-hidden bar))
                (should (codex-ide-section-hidden baz))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-leaves-new-files-open-when-all-files-open ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-live-new-open]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         (diff-text
          (string-join
           '("diff --git a/foo.txt b/foo.txt"
             "--- a/foo.txt"
             "+++ b/foo.txt"
             "@@ -1 +1 @@"
             "-old"
             "+new")
           "\n")))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'live))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (should-not turn-id)
                       diff-text)))
            (codex-ide-session-diff-note-session-updated session)
            (setq diff-text
                  (string-join
                   '("diff --git a/foo.txt b/foo.txt"
                     "--- a/foo.txt"
                     "+++ b/foo.txt"
                     "@@ -1 +1 @@"
                     "-old"
                     "+newer"
                     "diff --git a/bar.txt b/bar.txt"
                     "--- a/bar.txt"
                     "+++ b/bar.txt"
                     "@@ -3 +3 @@"
                     "-before"
                     "+after")
                   "\n"))
            (codex-ide-session-diff-note-session-updated session)
            (with-current-buffer diff-buffer
              (let* ((bar (cl-find-if
                           (lambda (section)
                             (equal (plist-get (codex-ide-section-value section) :path)
                                    "bar.txt"))
                           (codex-ide-diff--file-sections))))
                (should bar)
                (should-not (codex-ide-section-hidden bar))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-ignores-pinned-buffer ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-pinned-refresh]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer))))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'pinned)
            (let ((inhibit-read-only t))
              (insert "original\n")))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (&rest _)
                       (error "Pinned buffers should not live-refresh"))))
            (codex-ide-session-diff-note-session-updated session))
          (with-current-buffer diff-buffer
            (should (equal (buffer-string) "original\n"))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-note-session-updated-refreshes-transcript-current-turn ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-transcript-live]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :current-turn-id "turn-1"
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         requested-turns)
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'transcript)
            (setq-local codex-ide-session-diff--turn-id "turn-1"))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (push turn-id requested-turns)
                       "diff --git a/current.txt b/current.txt\n@@ -1 +1 @@\n-old\n+new")))
            (codex-ide-session-diff-note-session-updated session)
            (should (equal requested-turns '("turn-1")))
            (with-current-buffer diff-buffer
              (should (string-match-p "current\\.txt" (buffer-string))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-transcript-point-changed-refreshes-new-turn ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-transcript-refresh]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer)))
         requested-turns)
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'transcript)
            (setq-local codex-ide-session-diff--turn-id "turn-1"))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (resolved-session &optional turn-id)
                       (should (eq resolved-session session))
                       (push turn-id requested-turns)
                       (format "diff --git a/%s.txt b/%s.txt\n@@ -1 +1 @@\n-old\n+new"
                               turn-id
                               turn-id))))
            (codex-ide-session-diff-transcript-point-changed session "turn-1")
            (should-not requested-turns)
            (codex-ide-session-diff-transcript-point-changed session "turn-2")
            (should (equal requested-turns '("turn-2")))
            (with-current-buffer diff-buffer
              (should (equal codex-ide-session-diff--turn-id "turn-2"))
              (should (string-match-p "turn-2\\.txt" (buffer-string))))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

(ert-deftest codex-ide-session-diff-transcript-source-without-turn-shows-empty-state ()
  (let* ((session-buffer (generate-new-buffer "*codex[test-transcript-empty]*"))
         (session (make-instance 'codex-ide-session
                                 :buffer session-buffer
                                 :directory default-directory))
         (diff-buffer (get-buffer-create
                       (codex-ide-session-diff-buffer-name-for-session
                        session-buffer))))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (codex-ide-session-diff-mode)
            (setq-local codex-ide-session-diff--session session)
            (setq-local codex-ide-session-diff-source 'transcript)
            (setq-local codex-ide-session-diff--turn-id nil))
          (cl-letf (((symbol-function 'codex-ide-diff-data-combined-turn-diff-text)
                     (lambda (&rest _)
                       (error "Transcript mode should not fall back to latest turn"))))
            (codex-ide-session-diff-refresh diff-buffer))
          (with-current-buffer diff-buffer
            (should (string-match-p
                     "No prompt at transcript position"
                     (buffer-string)))
            (goto-char (point-min))
            (should (eq (get-text-property (point) 'face)
                        'font-lock-comment-face))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

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
                   (lambda (diff-text buffer-name &optional _directory)
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
                         (lambda (diff-text buffer-name &optional _directory)
                           (setq opened (list diff-text buffer-name))
                           nil)))
                (call-interactively #'codex-ide-diff-open-combined-turn-buffer)
                (should (equal opened
                               '("diff --git a/foo.txt b/foo.txt\n@@ -1 +1 @@\n-old\n+new"
                                 "*codex[test-point]*-turn-diff")))))))
      (when (buffer-live-p session-buffer)
        (kill-buffer session-buffer)))))

;;; codex-ide-diff-view-tests.el ends here
