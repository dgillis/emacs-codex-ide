;;; codex-ide-renderer-tests.el --- Tests for codex-ide renderer -*- lexical-binding: t; -*-

;;; Commentary:

;; Renderer-specific coverage.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'codex-ide)

(ert-deftest codex-ide-renderer-renders-indented-fenced-code-blocks ()
  (with-temp-buffer
    (insert "Each PR should target:\n\n    ```text\n    dgillis/emacs-codex-ide:main\n    ```\n")
    (codex-ide--render-markdown-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "```text")
    (should (equal (get-text-property (match-beginning 0) 'display) ""))
    (search-forward "dgillis/emacs-codex-ide:main")
    (let ((code-pos (match-beginning 0)))
      (should (get-text-property code-pos 'codex-ide-markdown))
      (should (memq 'fixed-pitch
                    (ensure-list (get-text-property code-pos 'face)))))
    (search-forward "```")
    (should (equal (get-text-property (match-beginning 0) 'display) ""))))

(ert-deftest codex-ide-renderer-renders-javascript-fenced-code-blocks ()
  (with-temp-buffer
    (insert "```javascript\nconst x = 1;\n```\n")
    (codex-ide--render-markdown-region (point-min) (point-max) t)
    (goto-char (point-min))
    (should (equal (get-text-property (point-min) 'display) ""))
    (search-forward "const x")
    (let ((code-pos (match-beginning 0)))
      (should (get-text-property code-pos 'codex-ide-markdown))
      (should (memq 'fixed-pitch
                    (ensure-list (get-text-property code-pos 'face))))
      (should (memq 'font-lock-keyword-face
                    (ensure-list (get-text-property code-pos 'face)))))
    (goto-char (point-max))
    (forward-line -1)
    (should (equal (get-text-property (point) 'display) ""))))

(ert-deftest codex-ide-renderer-fontifies-completed-fences-while-streaming ()
  (with-temp-buffer
    (insert "```javascript\nconst x = 1;\n")
    (codex-ide--render-markdown-region (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "const x")
    (let ((code-pos (match-beginning 0)))
      (should-not (memq 'font-lock-keyword-face
                        (ensure-list (get-text-property code-pos 'face)))))
    (goto-char (point-max))
    (insert "```\n")
    (codex-ide--render-markdown-region (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "const x")
    (let ((code-pos (match-beginning 0)))
      (should (memq 'fixed-pitch
                    (ensure-list (get-text-property code-pos 'face))))
      (should (memq 'font-lock-keyword-face
                    (ensure-list (get-text-property code-pos 'face)))))))

(ert-deftest codex-ide-renderer-renders-indented-pipe-tables ()
  (with-temp-buffer
    (insert "Indented table inside a list item:\n\n    | Remote | Branch | Purpose |\n    | --- | --- | --- |\n    | upstream | main | PR base |\n    | fork | topic-branch | PR head |\n")
    (codex-ide--render-markdown-region (point-min) (point-max) t)
    (should (string-match-p "^    | Remote   | Branch       | Purpose |$"
                            (buffer-string)))
    (should (string-match-p "^    |----------|--------------|---------|$"
                            (buffer-string)))
    (should (string-match-p "^    | upstream | main         | PR base |$"
                            (buffer-string)))
    (should-not (string-match-p "^    |   | Remote" (buffer-string)))))

(provide 'codex-ide-renderer-tests)

;;; codex-ide-renderer-tests.el ends here
