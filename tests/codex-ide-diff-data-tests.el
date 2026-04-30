;;; codex-ide-diff-data-tests.el --- Tests for codex-ide diff data -*- lexical-binding: t; -*-

;;; Commentary:

;; Diff normalization and lookup coverage.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'codex-ide)

(ert-deftest codex-ide-combine-diff-texts-trims-and-joins-blocks ()
  (should
   (equal (codex-ide--combine-diff-texts
           '("diff --git a/foo b/foo\n@@ -1 +1 @@\n-old\n+new\n"
             nil
             "   \n"
             "diff --git a/bar b/bar\n@@ -1 +1 @@\n-before\n+after  "))
          (concat
           "diff --git a/foo b/foo\n@@ -1 +1 @@\n-old\n+new"
           "\n\n"
           "diff --git a/bar b/bar\n@@ -1 +1 @@\n-before\n+after"))))

(ert-deftest codex-ide-combine-diff-texts-returns-nil-when-empty ()
  (should-not (codex-ide--combine-diff-texts nil))
  (should-not (codex-ide--combine-diff-texts '(" \n" nil))))

(ert-deftest codex-ide-file-change-diff-text-uses-apply-patch-file-header ()
  (let* ((patch-text (string-join
                      '("*** Begin Patch"
                        "*** Update File: foo.txt"
                        "@@"
                        "-old"
                        "+new"
                        "*** End Patch")
                      "\n"))
         (expected-diff (string-join
                         '("--- a/foo.txt"
                           "+++ b/foo.txt"
                           "@@"
                           "-old"
                           "+new")
                         "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . "patch")
                              (diff . ,patch-text)))))))
    (should (equal (codex-ide--file-change-diff-text item)
                   expected-diff))
    (should-not (string-match-p
                 (rx line-start "*** " (or "Begin" "End" "Update") (* nonl))
                 (codex-ide--file-change-diff-text item)))))

(ert-deftest codex-ide-file-change-diff-text-normalizes-top-level-apply-patch ()
  (let* ((patch-text (string-join
                      '("*** Begin Patch"
                        "*** Add File: foo.txt"
                        "@@"
                        "+new"
                        "*** End Patch")
                      "\n"))
         (item `((type . "fileChange")
                 (diff . ,patch-text))))
    (should (equal (codex-ide--file-change-diff-text item)
                   (string-join
                    '("--- /dev/null"
                      "+++ b/foo.txt"
                      "@@"
                      "+new")
                    "\n")))))

(ert-deftest codex-ide-file-change-diff-text-normalizes-multi-file-apply-patch ()
  (let* ((patch-text (string-join
                      '("*** Begin Patch"
                        "*** Update File: foo.txt"
                        "@@"
                        "-old"
                        "+new"
                        "*** Delete File: bar.txt"
                        "@@"
                        "-gone"
                        "*** End Patch")
                      "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . "patch")
                              (diff . ,patch-text)))))))
    (should (equal (codex-ide--file-change-diff-text item)
                   (string-join
                    '("--- a/foo.txt"
                      "+++ b/foo.txt"
                      "@@"
                      "-old"
                      "+new"
                      ""
                      "--- a/bar.txt"
                      "+++ /dev/null"
                      "@@"
                      "-gone")
                    "\n")))))

(ert-deftest codex-ide-file-change-diff-text-keeps-git-diff-header ()
  (let* ((diff-text (string-join
                     '("diff --git a/foo.txt b/foo.txt"
                       "--- a/foo.txt"
                       "+++ b/foo.txt"
                       "@@ -1 +1 @@"
                       "-old"
                       "+new")
                     "\n"))
         (item `((type . "fileChange")
                 (changes . (((path . "foo.txt")
                              (diff . ,diff-text)))))))
    (should (equal (codex-ide--file-change-diff-text item)
                   diff-text))))

(ert-deftest codex-ide-turn-file-change-diff-texts-normalizes-historical-items ()
  (let* ((diff-1 (string-join
                  '("diff --git a/foo.txt b/foo.txt"
                    "--- a/foo.txt"
                    "+++ b/foo.txt"
                    "@@ -1 +1 @@"
                    "-old"
                    "+new")
                  "\n"))
         (diff-2 (string-join
                  '("diff --git a/bar.txt b/bar.txt"
                    "--- a/bar.txt"
                    "+++ b/bar.txt"
                    "@@ -2 +2 @@"
                    "-before"
                    "+after")
                  "\n"))
         (file-change-1 `((type . "fileChange")
                          (id . "file-change-1")
                          (changes . (((path . "foo.txt")
                                       (diff . ,diff-1))))))
         (file-change-2 `((type . "fileChange")
                          (id . "file-change-2")
                          (changes . (((path . "bar.txt")
                                       (diff . ,diff-2))))))
         (expected-1 (codex-ide--file-change-diff-text file-change-1))
         (expected-2 (codex-ide--file-change-diff-text file-change-2))
         (turn `((id . "turn-latest")
                 (items . (,file-change-1 ,file-change-2)))))
    (should
     (equal (codex-ide--turn-file-change-diff-texts turn)
            (list expected-1 expected-2)))))

;;; codex-ide-diff-data-tests.el ends here
