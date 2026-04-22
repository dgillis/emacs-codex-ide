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
    (insert "Use `co")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (goto-char (point-min))
    (search-forward "co")
    (should-not (get-text-property (1- (point)) 'codex-ide-markdown))
    (should-not (get-text-property (1- (point)) 'face))
    (goto-char (point-max))
    (insert "de` here.\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (goto-char (point-min))
    (search-forward "code")
    (should (get-text-property (1- (point)) 'codex-ide-markdown))
    (should (eq (get-text-property (1- (point)) 'face)
                'font-lock-keyword-face))))

(ert-deftest codex-ide-renderer-streaming-renders-closed-fenced-code-block ()
  (codex-ide-renderer-test-with-agent-message-buffer
    (insert "```javascript\nconst x = 1;\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (goto-char (point-min))
    (search-forward "const x")
    (should-not (memq 'font-lock-keyword-face
                      (ensure-list
                       (get-text-property (match-beginning 0) 'face))))
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

(ert-deftest codex-ide-renderer-streaming-rerenders-table-after-each-row ()
  (codex-ide-renderer-test-with-agent-message-buffer
    (insert "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (should (string-match-p "^| Bob  |   3 |$" (buffer-string)))
    (goto-char (point-min))
    (search-forward "| Bob  |   3 |")
    (should (get-text-property
             (match-beginning 0)
             'codex-ide-markdown-table-original))
    (goto-char (point-max))
    (insert "| Sue | 12 |\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (should (string-match-p "^| Bob  |   3 |$" (buffer-string)))
    (should (string-match-p "^| Sue  |  12 |$" (buffer-string)))
    (goto-char (point-min))
    (search-forward "| Sue  |  12 |")
    (should (get-text-property
             (match-beginning 0)
             'codex-ide-markdown-table-original))))

(ert-deftest codex-ide-renderer-streaming-holds-possible-table-header ()
  (codex-ide-renderer-test-with-agent-message-buffer
    (insert "| Feature | `Example` |\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (goto-char (point-min))
    (search-forward "Example")
    (should-not (get-text-property (1- (point)) 'codex-ide-markdown))
    (goto-char (point-max))
    (insert "| --- | --- |\n| Inline | `copy-marker` |\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (should (string-match-p "^| Inline  | copy-marker |$" (buffer-string)))
    (goto-char (point-min))
    (search-forward "| Inline  | copy-marker |")
    (should (get-text-property
             (match-beginning 0)
             'codex-ide-markdown-table-original))))

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

(ert-deftest codex-ide-renderer-streaming-keeps-table-rendered-during-next-partial-row ()
  (codex-ide-renderer-test-with-agent-message-buffer
    (insert "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (goto-char (point-max))
    (insert "| S")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (should (string-match-p "^| Bob  |   3 |$" (buffer-string)))
    (goto-char (point-min))
    (search-forward "| Bob  |   3 |")
    (should (get-text-property
             (match-beginning 0)
             'codex-ide-markdown-table-original))
    (goto-char (point-max))
    (insert "ue | 12 |\n")
    (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
    (should (string-match-p "^| Sue  |  12 |$" (buffer-string)))))

(ert-deftest codex-ide-renderer-streaming-notification-rerenders-table-after-each-row ()
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
            (should (string-match-p "^| Bob  |   3 |$" (buffer-string)))
            (goto-char (point-min))
            (search-forward "| Bob  |   3 |")
            (should (get-text-property
                     (match-beginning 0)
                     'codex-ide-markdown-table-original)))
          (codex-ide--handle-notification
           session
           '((method . "item/agentMessage/delta")
             (params . ((itemId . "msg-1")
                        (delta . "| Sue | 12 |\n")))))
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
      (should (get-text-property (car range) 'read-only)))))

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
