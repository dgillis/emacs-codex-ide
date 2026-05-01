;;; codex-ide-renderer-tests.el --- Tests for codex-ide renderer -*- lexical-binding: t; -*-

;;; Commentary:

;; Renderer-specific coverage.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)

(defmacro codex-ide-renderer-test-with-agent-message-buffer (&rest body)
  "Run BODY in a temporary current-agent-message buffer.
BODY may refer to the lexical variable `session'."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((codex-ide--session-metadata (make-hash-table :test 'eq))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :current-message-item-id "msg-1"
                      :current-message-prefix-inserted t
                      :item-states (make-hash-table :test 'equal))))
       (setf (codex-ide-session-current-message-start-marker session)
             (copy-marker (point-min)))
       (codex-ide--session-metadata-put
        session
        :agent-message-stream-render-start-marker
        (copy-marker (point-min)))
       ,@body)))

(ert-deftest codex-ide-renderer-renders-indented-fenced-code-blocks ()
  (with-temp-buffer
    (insert "Each PR should target:\n\n    ```text\n    dgillis/emacs-codex-ide:main\n    ```\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
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
    (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
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

(ert-deftest codex-ide-renderer-renders-json-fenced-code-blocks-with-stock-mode ()
  (with-temp-buffer
    (insert "```json\n{\"tool\": true}\n```\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
    (goto-char (point-min))
    (search-forward "tool")
    (let ((code-pos (1- (point))))
      (should (get-text-property code-pos 'codex-ide-markdown))
      (should (memq 'fixed-pitch
                    (ensure-list (get-text-property code-pos 'face))))
      (should (memq 'font-lock-string-face
                    (ensure-list (get-text-property code-pos 'face)))))))

(ert-deftest codex-ide-renderer-renders-leading-underscore-inline-code ()
  (with-temp-buffer
    (insert "prefix `_x_yz` suffix")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "_x_yz")
    (let ((code-pos (match-beginning 0))
          (open-tick-pos (1- (match-beginning 0)))
          (close-tick-pos (match-end 0)))
      (should (eq (get-text-property code-pos 'face) 'font-lock-keyword-face))
      (should (get-text-property code-pos 'codex-ide-markdown))
      (should (equal (get-text-property open-tick-pos 'display) ""))
      (should (equal (get-text-property close-tick-pos 'display) ""))
      (should-not (memq 'italic
                        (ensure-list (get-text-property code-pos 'face)))))))

(ert-deftest codex-ide-renderer-renders-bold-containing-inline-code ()
  (with-temp-buffer
    (insert "**bold with `verbatim` and `_x_yz` inside**\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "bold with `verbatim` and `_x_yz` inside\n"))
    (goto-char (point-min))
    (search-forward "bold")
    (should (memq 'bold
                  (ensure-list (get-text-property (match-beginning 0) 'face))))
    (search-forward "verbatim")
    (let ((code-pos (match-beginning 0)))
      (should (get-text-property code-pos 'codex-ide-markdown))
      (should (memq 'font-lock-keyword-face
                    (ensure-list (get-text-property code-pos 'face))))
      (should (memq 'bold
                    (ensure-list (get-text-property code-pos 'face)))))
    (search-forward "_x_yz")
    (let ((code-pos (match-beginning 0)))
      (should (get-text-property code-pos 'codex-ide-markdown))
      (should (memq 'font-lock-keyword-face
                    (ensure-list (get-text-property code-pos 'face))))
      (should (memq 'bold
                    (ensure-list (get-text-property code-pos 'face))))
      (should-not (memq 'italic
                        (ensure-list (get-text-property code-pos 'face)))))))

(ert-deftest codex-ide-renderer-fontifies-completed-fences-while-streaming ()
  (with-temp-buffer
    (insert "```javascript\nconst x = 1;\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "const x")
    (let ((code-pos (match-beginning 0)))
      (should-not (memq 'font-lock-keyword-face
                        (ensure-list (get-text-property code-pos 'face)))))
    (goto-char (point-max))
    (insert "```\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max) nil)
    (goto-char (point-min))
    (search-forward "const x")
    (let ((code-pos (match-beginning 0)))
      (should (memq 'fixed-pitch
                    (ensure-list (get-text-property code-pos 'face))))
      (should (memq 'font-lock-keyword-face
                    (ensure-list (get-text-property code-pos 'face)))))))

(ert-deftest codex-ide-renderer-link-keymap-binds-other-window-open-commands ()
  (should (eq (lookup-key codex-ide-renderer-link-keymap (kbd "M-<return>"))
              #'codex-ide-renderer-open-file-link-other-window))
  (should (eq (lookup-key codex-ide-renderer-link-keymap (kbd "C-M-j"))
              #'codex-ide-renderer-open-file-link-other-window)))

(ert-deftest codex-ide-renderer-button-keymaps-bind-codex-navigation ()
  (should (eq (lookup-key codex-ide-renderer-link-keymap (kbd "TAB"))
              #'codex-ide-renderer-button-nav-forward))
  (should (eq (lookup-key codex-ide-renderer-link-keymap (kbd "<backtab>"))
              #'codex-ide-renderer-button-nav-backward))
  (should (eq (lookup-key codex-ide-renderer-action-button-keymap (kbd "TAB"))
              #'codex-ide-renderer-button-nav-forward))
  (should (eq (lookup-key codex-ide-renderer-action-button-keymap (kbd "<backtab>"))
              #'codex-ide-renderer-button-nav-backward)))

(ert-deftest codex-ide-renderer-theme-face-specs-follow-default-colors ()
  (cl-letf (((symbol-function 'face-background)
             (lambda (face &optional _frame _inherit)
               (when (eq face 'default)
                 "#101010")))
            ((symbol-function 'face-foreground)
             (lambda (face &optional _frame _inherit)
               (when (eq face 'default)
                 "#f0f0f0"))))
    (should (equal (plist-get (cdr (car (codex-ide-renderer--user-prompt-face-spec)))
                              :background)
                   "#1f1f1f"))
    (should (equal (plist-get (cdr (car (codex-ide-renderer--output-separator-face-spec)))
                              :foreground)
                   "#595959"))
    (should-not (plist-member
                 (cdr (car (codex-ide-renderer--command-output-face-spec)))
                 :background))))

(ert-deftest codex-ide-renderer-refresh-theme-faces-reapplies-session-face-specs ()
  (let (seen)
    (cl-letf (((symbol-function 'face-spec-set)
               (lambda (face spec &optional _spec-type)
                 (push (cons face spec) seen)))
              ((symbol-function 'face-background)
               (lambda (face &optional _frame _inherit)
                 (when (eq face 'default)
                   "#ffffff")))
              ((symbol-function 'face-foreground)
               (lambda (face &optional _frame _inherit)
                 (when (eq face 'default)
                   "#000000"))))
      (codex-ide-renderer-refresh-theme-faces))
    (should (equal seen
                   (list
                    (cons 'codex-ide-result-rail-face
                          '((t :inherit (shadow fringe))))
                    (cons 'codex-ide-command-output-face
                          '((t :inherit fixed-pitch
                               :extend t)))
                    (cons 'codex-ide-output-separator-face
                          '((t :foreground "#c7c7c7")))
                    (cons 'codex-ide-user-prompt-face
                          '((t :inherit default
                               :background "#f2f2f2"
                               :extend t))))))))

(ert-deftest codex-ide-renderer-open-file-link-other-window-uses-other-window-opener ()
  (let ((path (make-temp-file "codex-ide-renderer-link-"))
        (opened-path nil)
        (target-buffer (generate-new-buffer " *codex-ide-renderer-link-target*")))
    (unwind-protect
        (with-temp-buffer
          (add-text-properties
           (progn (insert "link") (point-min))
           (point-max)
           `(codex-ide-path ,path
                            codex-ide-line 2
                            codex-ide-column 3))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (file)
                       (setq opened-path file)
                       (with-current-buffer target-buffer
                         (erase-buffer)
                         (insert "alpha\nbeta\ngamma\n"))
                       (set-buffer target-buffer)
                       target-buffer)))
            (codex-ide-renderer-open-file-link-other-window nil)
            (should (equal opened-path path))
            (should (eq (current-buffer) target-buffer))
            (should (= (line-number-at-pos) 2))
            (should (= (current-column) 2))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer))
      (ignore-errors
        (delete-file path)))))

(ert-deftest codex-ide-renderer-wraps-wide-markdown-tables-with-unicode-box ()
  (with-temp-buffer
    (let ((codex-ide-renderer-markdown-table-max-width 50)
          (codex-ide-renderer-markdown-table-max-cell-width 24)
          (codex-ide-renderer-markdown-table-min-cell-width 8))
      (insert "| Commit | Date | Summary |\n")
      (insert "| --- | --- | --- |\n")
      (insert "| `8a1c3c8` | 2026-04-30 | Added richer incremental markdown rendering that wraps across several visual table rows. |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
      (let ((rendered (buffer-string)))
        (should (string-prefix-p "┌" rendered))
        (should (string-match-p "^├" rendered))
        (should (string-match-p "^└" rendered))
        (should (string-match-p "^│ 8a1c3c8 │ 2026-04-30 │ Added richer" rendered))
        (should (string-match-p "^│         │            │ incremental markdown" rendered))
        (dolist (line (split-string rendered "\n" t))
          (should (<= (string-width line) 50)))))
    (goto-char (point-min))
    (search-forward "8a1c3c8")
    (should (get-text-property (match-beginning 0) 'codex-ide-markdown))
    (should (get-text-property (point-min) 'codex-ide-markdown-table-original))))

(ert-deftest codex-ide-renderer-wrapped-table-preserves-file-link-buttons ()
  (with-temp-buffer
    (let ((codex-ide-renderer-markdown-table-max-width 42)
          (codex-ide-renderer-markdown-table-max-cell-width 20)
          (codex-ide-renderer-markdown-table-min-cell-width 8))
      (insert "| File | Summary |\n")
      (insert "| --- | --- |\n")
      (insert "| [`foo.el`](/tmp/foo.el#L3C2) | This wrapped cell keeps the file link active. |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
      (should (string-prefix-p "┌" (buffer-string)))
      (goto-char (point-min))
      (search-forward "foo.el")
      (let ((pos (match-beginning 0)))
        (should (button-at pos))
        (should (equal (get-text-property pos 'codex-ide-path) "/tmp/foo.el"))
        (should (equal (get-text-property pos 'codex-ide-line) 3))
        (should (equal (get-text-property pos 'codex-ide-column) 2))))))

(ert-deftest codex-ide-renderer-table-width-uses-rendered-link-label ()
  (with-temp-buffer
    (let ((codex-ide-renderer-markdown-table-max-width 20)
          (codex-ide-renderer-markdown-table-max-cell-width nil))
      (insert "| File | Note |\n")
      (insert "| --- | --- |\n")
      (insert "| [`foo.el`](/tmp/some/really/long/path/foo.el#L3C2) | ok |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
      (let ((rendered (buffer-string)))
        (should-not (string-prefix-p "┌" rendered))
        (should (string-match-p "^| foo\\.el | ok" rendered))
        (should-not (string-match-p (regexp-quote "[`foo.el`]") rendered))))
    (goto-char (point-min))
    (search-forward "foo.el")
    (should (button-at (match-beginning 0)))))

(ert-deftest codex-ide-renderer-open-file-link-other-window-is-callable-interactively ()
  (let ((path (make-temp-file "codex-ide-renderer-link-"))
        (opened-path nil)
        (target-buffer (generate-new-buffer " *codex-ide-renderer-link-target*")))
    (unwind-protect
        (with-temp-buffer
          (add-text-properties
           (progn (insert "link") (point-min))
           (point-max)
           `(codex-ide-path ,path
                            codex-ide-line 2
                            codex-ide-column 3))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (file)
                       (setq opened-path file)
                       (with-current-buffer target-buffer
                         (erase-buffer)
                         (insert "alpha\nbeta\ngamma\n"))
                       (set-buffer target-buffer)
                       target-buffer)))
            (call-interactively #'codex-ide-renderer-open-file-link-other-window)
            (should (equal opened-path path))
            (should (eq (current-buffer) target-buffer))
            (should (= (line-number-at-pos) 2))
            (should (= (current-column) 2))))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer))
      (ignore-errors
        (delete-file path)))))

(ert-deftest codex-ide-renderer-streaming-renders-completed-inline-markdown ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "Use `code` here.\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "code")
   (should (get-text-property (1- (point)) 'codex-ide-markdown))
   (should (eq (get-text-property (1- (point)) 'face)
               'font-lock-keyword-face))))

(ert-deftest codex-ide-renderer-streaming-holds-incomplete-inline-markdown ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (let ((scheduled nil)
         (cancelled nil))
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (seconds repeat function buffer)
                  (setq scheduled (list seconds repeat function buffer))
                  'codex-ide-renderer-test-timer))
               ((symbol-function 'timerp)
                (lambda (object)
                  (eq object 'codex-ide-renderer-test-timer)))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (setq cancelled timer))))
       (insert "Use `co")
       (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
       (goto-char (point-min))
       (search-forward "co")
       (should-not (get-text-property (1- (point)) 'codex-ide-markdown))
       (should (get-text-property (1- (point)) 'invisible))
       (should (equal (car scheduled) 3.0))
       (goto-char (point-max))
       (insert "de` here")
       (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
       (goto-char (point-min))
       (search-forward "code")
       (should (get-text-property (1- (point)) 'codex-ide-markdown))
       (should-not (get-text-property (1- (point)) 'invisible))
       (should (eq (get-text-property (1- (point)) 'face)
                   'font-lock-keyword-face))
       (should (eq cancelled 'codex-ide-renderer-test-timer))))))

(ert-deftest codex-ide-renderer-streaming-defers-incomplete-file-links ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (cl-letf (((symbol-function 'run-at-time)
              (lambda (&rest _args)
                'codex-ide-renderer-test-timer))
             ((symbol-function 'timerp)
              (lambda (object)
                (eq object 'codex-ide-renderer-test-timer)))
             ((symbol-function 'cancel-timer)
              #'ignore))
     (insert "Open [`foo.el`](/tmp/foo.el:12")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "foo.el")
     (should (get-text-property (match-beginning 0) 'invisible))
     (goto-char (point-max))
     (insert ")")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "foo.el")
     (should-not (get-text-property (match-beginning 0) 'invisible))
     (should (eq (get-text-property (match-beginning 0) 'face) 'link)))))

(ert-deftest codex-ide-renderer-streaming-renders-completed-inline-code-on-current-line ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (cl-letf (((symbol-function 'run-at-time)
              (lambda (&rest _args)
                (ert-fail "completed inline code should render immediately"))))
     (insert "Use `copy-marker`")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "copy-marker")
     (let ((code-pos (match-beginning 0)))
       (should (get-text-property code-pos 'codex-ide-markdown))
       (should (eq (get-text-property code-pos 'face)
                   'font-lock-keyword-face))
       (should-not (get-text-property code-pos 'invisible))))))

(ert-deftest codex-ide-renderer-streaming-renders-completed-file-link-on-current-line ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (cl-letf (((symbol-function 'run-at-time)
              (lambda (&rest _args)
                (ert-fail "completed file link should render immediately"))))
     (insert "Open [`foo.el`](/tmp/foo.el:12)")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "foo.el")
     (let ((link-pos (match-beginning 0)))
       (should (get-text-property link-pos 'codex-ide-markdown))
       (should (eq (get-text-property link-pos 'face) 'link))
       (should-not (get-text-property link-pos 'invisible))))))

(ert-deftest codex-ide-renderer-streaming-renders-closed-fenced-code-block ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "```javascript\nconst x = 1;\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "```javascript")
   (should (equal (get-text-property (match-beginning 0) 'display) ""))
   (goto-char (point-min))
   (search-forward "const x")
   (let ((code-pos (match-beginning 0)))
     (should (memq 'fixed-pitch
                   (ensure-list (get-text-property code-pos 'face))))
     (should (memq 'font-lock-keyword-face
                   (ensure-list (get-text-property code-pos 'face))))
     (should-not (get-text-property code-pos 'invisible)))
   (goto-char (point-max))
   (insert "```\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "const x")
   (let ((code-pos (match-beginning 0)))
     (should (memq 'fixed-pitch
                   (ensure-list (get-text-property code-pos 'face))))
     (should (memq 'font-lock-keyword-face
                   (ensure-list (get-text-property code-pos 'face)))))))

(ert-deftest codex-ide-renderer-streaming-does-not-defer-fence-line ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (cl-letf (((symbol-function 'run-at-time)
              (lambda (&rest _args)
                (ert-fail "triple-backtick fences should not be delayed"))))
     (insert "```javascript")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "```javascript")
     (should (equal (get-text-property (match-beginning 0) 'display) ""))
     (should-not (get-text-property (match-beginning 0) 'invisible)))))

(ert-deftest codex-ide-renderer-streaming-does-not-render-inline-code-inside-open-fence ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (cl-letf (((symbol-function 'run-at-time)
              (lambda (&rest _args)
                (ert-fail "open fenced code blocks should not use inline delay"))))
     (insert "```text\nliteral `copy-marker`")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "copy-marker")
     (should (get-text-property (match-beginning 0) 'codex-ide-markdown))
     (should (memq 'fixed-pitch
                   (ensure-list
                    (get-text-property (match-beginning 0) 'face))))
     (should-not (memq 'font-lock-keyword-face
                       (ensure-list
                        (get-text-property (match-beginning 0) 'face))))
     (should-not (get-text-property (match-beginning 0) 'invisible)))))

(ert-deftest codex-ide-renderer-streaming-defers-trailing-table-until-following-text ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "| Bob | 3 |")
   (should (get-text-property (match-beginning 0)
                              'codex-ide-markdown-deferred))
   (should (get-text-property (match-beginning 0) 'invisible))
   (goto-char (point-max))
   (insert "\nDone\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (should (string-match-p "^| Bob  |   3 |$" (buffer-string)))
   (goto-char (point-min))
   (search-forward "| Bob  |   3 |")
   (should (get-text-property
            (match-beginning 0)
            'codex-ide-markdown-table-original))))

(ert-deftest codex-ide-renderer-streaming-hides-possible-table-header ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "| Feature | `Example` |\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "Example")
   (should (get-text-property (1- (point)) 'codex-ide-markdown-deferred))
   (should (get-text-property (1- (point)) 'invisible))
   (should-not (get-text-property (1- (point)) 'codex-ide-markdown))
   (goto-char (point-max))
   (insert "| --- | --- |\n| Inline | `copy-marker` |\n\nDone\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (should (string-match-p "^| Inline  | copy-marker |$" (buffer-string)))
   (goto-char (point-min))
   (search-forward "| Inline  | copy-marker |")
   (should (get-text-property
            (match-beginning 0)
            'codex-ide-markdown-table-original))))

(ert-deftest codex-ide-renderer-streaming-wide-table-shrink-keeps-tail-cleanup-in-range ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (let ((codex-ide-renderer-markdown-table-max-width 48)
         (codex-ide-renderer-markdown-table-max-cell-width 22)
         (codex-ide-renderer-markdown-table-min-cell-width 8))
     (insert "| File | Summary |\n")
     (insert "| --- | --- |\n")
     (insert "| [`foo.el`](/tmp/some/really/long/path/foo.el#L3C2) | This is a long streaming table cell that should render to less text than the original markdown source. |\n")
     (should
      (codex-ide--render-current-agent-message-markdown-streaming
       session
       "msg-1"))
     (goto-char (point-min))
     (search-forward "foo.el")
     (should (get-text-property (match-beginning 0)
                                'codex-ide-markdown-deferred))
     (codex-ide--render-current-agent-message-markdown session "msg-1" t)
     (should (string-prefix-p "┌" (buffer-string)))
     (goto-char (point-min))
     (search-forward "foo.el")
     (should (button-at (match-beginning 0))))))

(ert-deftest codex-ide-renderer-table-render-width-can-use-window-sized-override ()
  (with-temp-buffer
    (let ((codex-ide-renderer-markdown-table-max-width nil)
          (codex-ide-renderer-markdown-table-max-cell-width nil)
          (codex-ide-renderer-markdown-table-min-cell-width 6)
          (codex-ide-renderer--markdown-table-max-width-override 38))
      (insert "| File | Summary |\n")
      (insert "| --- | --- |\n")
      (insert "| [`foo.el`](/tmp/foo.el#L3C2) | This is a long table cell that should wrap after width constraining. |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
      (should (string-prefix-p "┌" (buffer-string)))
      (should (equal (get-text-property
                      (point-min)
                      'codex-ide-markdown-table-render-width)
                     38))
      (goto-char (point-min))
      (search-forward "foo.el")
      (should (button-at (match-beginning 0))))))

(ert-deftest codex-ide-renderer-table-width-override-supersedes-static-cap ()
  (let ((codex-ide-renderer-markdown-table-max-width 20)
        (codex-ide-renderer-markdown-table-max-cell-width nil)
        (codex-ide-renderer-markdown-table-min-cell-width 6))
    (should (equal (codex-ide-renderer--markdown-table-constrain-widths
                    '(30)
                    40)
                   '(30)))))

(ert-deftest codex-ide-renderer-rerenders-existing-table-for-new-width ()
  (with-temp-buffer
    (let ((codex-ide-renderer-markdown-table-max-width nil)
          (codex-ide-renderer-markdown-table-max-cell-width nil)
          (codex-ide-renderer-markdown-table-min-cell-width 6)
          (codex-ide-renderer--markdown-table-max-width-override 120))
      (insert "| File | Summary |\n")
      (insert "| --- | --- |\n")
      (insert "| [`foo.el`](/tmp/foo.el#L3C2) | This is a long table cell that should wrap after rerendering. |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
      (should (string-prefix-p "| File" (buffer-string)))
      (codex-ide-renderer-rerender-markdown-tables (point-min) (point-max) 38)
      (should (string-prefix-p "┌" (buffer-string)))
      (should (equal (get-text-property
                      (point-min)
                      'codex-ide-markdown-table-render-width)
                     38))
      (goto-char (point-min))
      (search-forward "foo.el")
      (should (button-at (match-beginning 0))))))

(ert-deftest codex-ide-renderer-table-layout-window-prefers-selected-window ()
  (let ((buffer (get-buffer-create " *codex-ide-renderer-table-selected*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (let ((selected (selected-window)))
            (split-window-right)
            (should (eq (codex-ide-renderer-markdown-table-layout-window buffer)
                        selected))))
      (kill-buffer buffer))))

(ert-deftest codex-ide-renderer-table-window-width-leaves-margin ()
  (let ((buffer (get-buffer-create " *codex-ide-renderer-table-margin*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buffer)
          (let ((codex-ide-renderer-markdown-table-window-margin 4))
            (cl-letf (((symbol-function 'window-body-width)
                       (lambda (_window &optional _pixelwise) 80)))
              (should (equal
                       (codex-ide-renderer-markdown-table-max-width-for-buffer
                        buffer)
                       76)))))
      (kill-buffer buffer))))

(ert-deftest codex-ide-renderer-table-layout-window-uses-smallest-unselected-window ()
  (let ((buffer (get-buffer-create " *codex-ide-renderer-table-smallest*"))
        (other-buffer (get-buffer-create " *codex-ide-renderer-table-other*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) other-buffer)
          (let* ((wide (split-window-right))
                 (narrow (split-window wide nil 'right)))
            (set-window-buffer wide buffer)
            (set-window-buffer narrow buffer)
            (cl-letf (((symbol-function 'window-body-width)
                       (lambda (window &optional _pixelwise)
                         (if (eq window narrow) 40 100))))
              (should (eq (codex-ide-renderer-markdown-table-layout-window buffer)
                          narrow)))))
      (kill-buffer buffer)
      (kill-buffer other-buffer))))

(ert-deftest codex-ide-renderer-table-rerender-is-debounced ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (scheduled nil)
          (cancelled nil)
          (timer-1 'codex-ide-renderer-test-timer-1)
          (timer-2 'codex-ide-renderer-test-timer-2))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (seconds repeat function buffer width)
                   (let ((timer (if scheduled timer-2 timer-1)))
                     (push (list seconds repeat function buffer width timer)
                           scheduled)
                     timer)))
                ((symbol-function 'timerp)
                 (lambda (object)
                   (memq object (list timer-1 timer-2))))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (push timer cancelled))))
        (codex-ide-renderer--schedule-markdown-table-rerender buffer 80)
        (codex-ide-renderer--schedule-markdown-table-rerender buffer 72)
        (should (= (length scheduled) 2))
        (should (equal cancelled (list timer-1)))
        (should (equal codex-ide-renderer--markdown-table-pending-rerender-width
                       72))
        (should (eq codex-ide-renderer--markdown-table-rerender-timer
                    timer-2))))))

(ert-deftest codex-ide-renderer-streaming-releases-pipe-line-when-not-table ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "| Not a `table` row |\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "table")
   (should-not (get-text-property (1- (point)) 'codex-ide-markdown))
   (goto-char (point-max))
   (insert "plain next line\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "table")
   (should (get-text-property (1- (point)) 'codex-ide-markdown))))

(ert-deftest codex-ide-renderer-streaming-hides-trailing-table-during-next-partial-row ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-max))
   (insert "| S")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "| Bob | 3 |")
   (should (get-text-property (match-beginning 0)
                              'codex-ide-markdown-deferred))
   (goto-char (point-max))
   (search-backward "| S")
   (should (get-text-property (point) 'codex-ide-markdown-deferred))
   (goto-char (point-max))
   (insert "ue | 12 |\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "| Sue | 12 |")
   (should (get-text-property (match-beginning 0)
                              'codex-ide-markdown-deferred))))

(ert-deftest codex-ide-renderer-streaming-renders-deferred-table-after-following-text ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-max))
   (insert "| S")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-max))
   (search-backward "| S")
   (should (get-text-property (point) 'codex-ide-markdown-deferred))
   (should (get-text-property (point) 'invisible))
   (goto-char (point-max))
   (insert "ue | 12 |\n\nDone\n")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (search-forward "| Sue  |  12 |")
   (should-not (get-text-property (match-beginning 0)
                                  'codex-ide-markdown-deferred))
   (should-not (get-text-property (match-beginning 0) 'invisible))))

(ert-deftest codex-ide-renderer-streaming-does-not-hide-lone-pipe-line ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (insert "| just a partial thought")
   (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
   (goto-char (point-min))
   (should-not (get-text-property (point) 'codex-ide-markdown-deferred))
   (should-not (get-text-property (point) 'invisible))))

(ert-deftest codex-ide-renderer-streaming-notification-defers-table-until-completion ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (codex-ide--handle-notification
				     session
				     '((method . "turn/started")
				       (params . ((turn . ((id . "turn-1")))))))
				    (codex-ide--handle-notification
				     session
				     '((method . "item/agentMessage/delta")
				       (params . ((itemId . "msg-1")
						  (delta . "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (goto-char (point-min))
				      (search-forward "| Bob | 3 |")
				      (should (get-text-property
					       (match-beginning 0)
					       'codex-ide-markdown-deferred)))
				    (codex-ide--handle-notification
				     session
				     '((method . "item/agentMessage/delta")
				       (params . ((itemId . "msg-1")
						  (delta . "| Sue | 12 |\n")))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (goto-char (point-min))
				      (search-forward "| Sue | 12 |")
				      (should (get-text-property
					       (match-beginning 0)
					       'codex-ide-markdown-deferred)))
				    (codex-ide--handle-notification
				     session
				     '((method . "item/completed")
				       (params . ((item . ((id . "msg-1")
							   (type . "agentMessage")
							   (status . "completed")))))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (string-match-p "^| Bob  |   3 |$" (buffer-string)))
				      (should (string-match-p "^| Sue  |  12 |$" (buffer-string)))
				      (goto-char (point-min))
				      (search-forward "| Sue  |  12 |")
				      (should (get-text-property
					       (match-beginning 0)
					       'codex-ide-markdown-table-original))))))))

(ert-deftest codex-ide-renderer-completion-skips-markdown-over-size-limit ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (let ((codex-ide-renderer-markdown-render-max-chars 10))
     (insert "This longer message has `code` here.\n")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (codex-ide--render-current-agent-message-markdown session "msg-1" t)
     (goto-char (point-min))
     (search-forward "code")
     (should-not (get-text-property (1- (point)) 'codex-ide-markdown))
     (should-not (get-text-property (1- (point)) 'face)))))

(ert-deftest codex-ide-renderer-streaming-size-limit-applies-to-spans ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (let ((codex-ide-renderer-markdown-render-max-chars 25))
     (insert "Use `a`.\n")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-max))
     (insert "This plain filler line is intentionally longer than the limit.\n")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-max))
     (insert "Use `b`.\n")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "a")
     (should (get-text-property (1- (point)) 'codex-ide-markdown))
     (search-forward "b")
     (should (get-text-property (1- (point)) 'codex-ide-markdown)))))

(ert-deftest codex-ide-renderer-completion-preserves-streamed-markdown-over-size-limit ()
  (codex-ide-renderer-test-with-agent-message-buffer
   (let ((codex-ide-renderer-markdown-render-max-chars 25))
     (insert "Use `a`.\n")
     (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
     (goto-char (point-min))
     (search-forward "a")
     (should (get-text-property (1- (point)) 'codex-ide-markdown))
     (goto-char (point-max))
     (insert "This trailing dirty span is intentionally longer than the limit.\n")
     (codex-ide--render-current-agent-message-markdown session "msg-1" t)
     (goto-char (point-min))
     (search-forward "a")
     (should (get-text-property (1- (point)) 'codex-ide-markdown)))))

(ert-deftest codex-ide-renderer-renders-indented-pipe-tables ()
  (with-temp-buffer
    (insert "Indented table inside a list item:\n\n    | Remote | Branch | Purpose |\n    | --- | --- | --- |\n    | upstream | main | PR base |\n    | fork | topic-branch | PR head |\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
    (should (string-match-p "^    | Remote   | Branch       | Purpose |$"
                            (buffer-string)))
    (should (string-match-p "^    |----------|--------------|---------|$"
                            (buffer-string)))
    (should (string-match-p "^    | upstream | main         | PR base |$"
                            (buffer-string)))
    (should-not (string-match-p "^    |   | Remote" (buffer-string)))))

(ert-deftest codex-ide-renderer-replaces-marker-region ()
  (with-temp-buffer
    (insert "Selected: old\n")
    (let ((start (copy-marker 11))
          (end (copy-marker 14 t)))
      (codex-ide-renderer-replace-marker-region start end "new")
      (should (equal (buffer-string) "Selected: new\n"))
      (should (= (marker-position end) 14)))))

(ert-deftest codex-ide-renderer-replaces-region ()
  (with-temp-buffer
    (insert "prefix old suffix")
    (let ((range (codex-ide-renderer-replace-region 8 11 "new")))
      (should (equal (buffer-string) "prefix new suffix"))
      (should (equal range '(8 . 11)))
      (should-not (get-text-property 8 'read-only)))))

(ert-deftest codex-ide-renderer-inserts-read-only-newlines ()
  (with-temp-buffer
    (insert "Agent output")
    (let ((range (codex-ide-renderer-insert-read-only-newlines 2)))
      (should (equal (buffer-string) "Agent output\n\n"))
      (should (equal range '(13 . 15)))
      (should (get-text-property 13 'read-only))
      (should (get-text-property 14 'read-only)))))

(ert-deftest codex-ide-renderer-inserts-input-prompt-with-separator ()
  (with-temp-buffer
    (insert "Agent output")
    (let* ((result (codex-ide-renderer-insert-input-prompt "draft" t))
           (transcript-start (plist-get result :transcript-start))
           (active-boundary (plist-get result :active-boundary))
           (prompt-start (plist-get result :prompt-start))
           (input-start (plist-get result :input-start)))
      (should (equal (buffer-string) "Agent output\n\n> draft"))
      (should (= (marker-position transcript-start) 13))
      (should (= (marker-position active-boundary) 14))
      (should (= (marker-position prompt-start) 15))
      (should (= (marker-position input-start) 17))
      (should (get-text-property (marker-position prompt-start)
                                 'codex-ide-prompt-start)))))

(ert-deftest codex-ide-renderer-input-text-does-not-inherit-prompt-start ()
  (with-temp-buffer
    (let* ((result (codex-ide-renderer-insert-input-prompt "draft"))
           (input-start (marker-position (plist-get result :input-start))))
      (should-not (get-text-property input-start 'codex-ide-prompt-start)))))

(ert-deftest codex-ide-renderer-line-prompt-start-ignores-continuation-lines ()
  (with-temp-buffer
    (insert "> first line\nsecond line\n")
    (add-text-properties (point-min) (point-max)
                         '(codex-ide-prompt-start t))
    (goto-char (point-min))
    (should (codex-ide-renderer-line-has-prompt-start-p))
    (forward-line 1)
    (should-not (codex-ide-renderer-line-has-prompt-start-p))))

(ert-deftest codex-ide-renderer-input-prompt-prefix-is-read-only ()
  (with-temp-buffer
    (let* ((result (codex-ide-renderer-insert-input-prompt "draft"))
           (prompt-start (marker-position (plist-get result :prompt-start)))
           (input-start (marker-position (plist-get result :input-start))))
      (should (get-text-property prompt-start 'read-only))
      (goto-char input-start)
      (should-error (delete-backward-char 1) :type 'text-read-only)
      (should (equal (buffer-string) "> draft")))))

(ert-deftest codex-ide-renderer-inserts-running-input-list ()
  (with-temp-buffer
    (insert "Transcript")
    (let* ((result (codex-ide-renderer-insert-running-input-list
                    "Queued turns:\n  > draft\n"))
           (delete-start (plist-get result :delete-start))
           (boundary (plist-get result :boundary))
           (end (plist-get result :end)))
      (should (equal (buffer-string) "Transcript\n\nQueued turns:\n  > draft\n"))
      (should (markerp delete-start))
      (should (= (marker-position boundary) 12))
      (should (= (marker-position end) (point-max))))))

(ert-deftest codex-ide-renderer-inserts-context-summary ()
  (with-temp-buffer
    (insert "> prompt")
    (let ((range (codex-ide-renderer-insert-context-summary "focus: foo.el:12")))
      (should (equal (buffer-string) "> prompt\nfocus: foo.el:12"))
      (should (= (car range) 9))
      (should (eq (get-text-property 10 'face) 'codex-ide-item-detail-face)))))

(ert-deftest codex-ide-renderer-inserts-session-header ()
  (with-temp-buffer
    (let ((range (codex-ide-renderer-insert-session-header "/tmp/project")))
      (should (equal (buffer-string) "Codex session for /tmp/project\n\n"))
      (should (= (car range) (point-min)))
      (should (get-text-property (point-min) 'read-only)))))

(ert-deftest codex-ide-renderer-inserts-approval-resolution ()
  (with-temp-buffer
    (let ((range (codex-ide-renderer-insert-approval-resolution
                  "accept for session")))
      (should (equal (buffer-string) "Selected: accept for session\n"))
      (should (= (car range) (point-min)))
      (should (eq (get-text-property (point-min) 'face)
                  'codex-ide-approval-label-face)))))

(ert-deftest codex-ide-renderer-inserts-approval-detail-command ()
  (with-temp-buffer
    (codex-ide-renderer-insert-approval-detail
     '(:kind command :text "git status"))
    (should (string-match-p "Run the following command?" (buffer-string)))
    (should (string-match-p "git status" (buffer-string)))
    (goto-char (point-min))
    (search-forward "git status")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'codex-ide-item-summary-face))))

(ert-deftest codex-ide-renderer-inserts-command-output-body ()
  (with-temp-buffer
    (let* ((overlay (make-overlay (point-min) (point-min)))
           (range (codex-ide-renderer-insert-command-output-body
                   "line 1\nline 2\n"
                   :keymap codex-ide-renderer-link-keymap
                   :overlay overlay
                   :overlay-property 'codex-ide-command-output-overlay
                   :properties '(mouse-face highlight))))
      (should (equal (buffer-string) "line 1\nline 2\n"))
      (should (eq (get-text-property (car range) 'face)
                  'codex-ide-command-output-face))
      (should (eq (get-text-property (car range) 'keymap)
                  codex-ide-renderer-link-keymap))
      (should (eq (get-text-property (car range) 'mouse-face) 'highlight))
      (should (eq (get-text-property (car range)
                                     'codex-ide-command-output-overlay)
                  overlay))
      (let* ((rails (overlay-get overlay :result-rail-overlays))
             (rail-string (overlay-get (car rails) 'before-string)))
        (should (= (length rails) 2))
        (should (equal (get-text-property 0 'display rail-string)
                       '(left-fringe codex-ide-result-rail
                                     codex-ide-result-rail-face))))
      (should (get-text-property (car range) 'read-only)))))

(ert-deftest codex-ide-renderer-clears-result-rail-overlays ()
  (with-temp-buffer
    (let ((overlay (make-overlay (point-min) (point-min))))
      (insert "line 1\nline 2\n")
      (codex-ide-renderer-add-result-rail-overlays
       (point-min) (point-max) overlay)
      (let ((rails (overlay-get overlay :result-rail-overlays)))
        (should (= (length rails) 2))
        (codex-ide-renderer-clear-result-rail-overlays overlay)
        (should-not (overlay-get overlay :result-rail-overlays))
        (dolist (rail rails)
          (should-not (overlay-buffer rail)))))))

(ert-deftest codex-ide-renderer-inserts-elicitation-text-field ()
  (with-temp-buffer
    (let* ((result (codex-ide-renderer-insert-elicitation-field
                    "Name" 'text "Ada" nil nil nil nil))
           (start (plist-get result :start-marker))
           (end (plist-get result :end-marker))
           (ranges (plist-get result :writable-ranges)))
      (should (equal (buffer-string)
                     "Name:\n    Ada\n\n"))
      (should (= (marker-position start) 11))
      (should (= (marker-position end) 15))
      (should (= (length ranges) 1)))))

(ert-deftest codex-ide-renderer-inserts-elicitation-choice-field ()
  (with-temp-buffer
    (let ((chosen nil))
      (let ((result (codex-ide-renderer-insert-elicitation-field
                     "Mode"
                     'choice
                     "true"
                     '(("true" . t) ("false" . :json-false))
                     nil
                     (lambda (label value)
                       (setq chosen (cons label value)))
                     nil)))
        (should (string-match-p "Mode:\n    Selected: true\n" (buffer-string)))
        (goto-char (point-min))
        (search-forward "[false]")
        (button-activate (button-at (match-beginning 0)))
        (should (equal chosen '("false" . :json-false)))
        (should (markerp (plist-get result :display-start-marker)))
        (should (markerp (plist-get result :display-end-marker)))))))

(provide 'codex-ide-renderer-tests)

;;; codex-ide-renderer-tests.el ends here
