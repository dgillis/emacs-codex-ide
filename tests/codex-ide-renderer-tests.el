;;; codex-ide-renderer-tests.el --- Tests for codex-ide renderer -*- lexical-binding: t; -*-

;;; Commentary:

;; Renderer-specific coverage.

;;; Code:

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

(ert-deftest codex-ide-renderer-renders-json-fenced-code-blocks-with-stock-mode ()
  (with-temp-buffer
    (insert "```json\n{\"tool\": true}\n```\n")
    (codex-ide--render-markdown-region (point-min) (point-max) t)
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
    (codex-ide--render-markdown-region (point-min) (point-max))
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
    (codex-ide--render-markdown-region (point-min) (point-max))
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
    (let ((codex-ide-markdown-render-max-chars 10))
      (insert "This longer message has `code` here.\n")
      (codex-ide--render-current-agent-message-markdown-streaming session "msg-1")
      (codex-ide--render-current-agent-message-markdown session "msg-1" t)
      (goto-char (point-min))
      (search-forward "code")
      (should-not (get-text-property (1- (point)) 'codex-ide-markdown))
      (should-not (get-text-property (1- (point)) 'face)))))

(ert-deftest codex-ide-renderer-streaming-size-limit-applies-to-spans ()
  (codex-ide-renderer-test-with-agent-message-buffer
    (let ((codex-ide-markdown-render-max-chars 25))
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
    (let ((codex-ide-markdown-render-max-chars 25))
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
