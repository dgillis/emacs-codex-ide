;;; codex-ide-mcp-tests.el --- Tests for codex-ide-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; MCP proxy tests for codex-ide.

;;; Code:

(require 'ert)
(require 'json)
(require 'subr-x)
(require 'codex-ide-test-fixtures)

(ert-deftest codex-ide-mcp-script-starts-with-optional-server-name-flag ()
  (let ((script-path (expand-file-name "bin/codex-ide-mcp-server.py"
                                       codex-ide-test--root-directory))
        (mock-emacsclient (make-temp-file "codex-ide-emacsclient-" nil ".py"))
        (argv-log (make-temp-file "codex-ide-emacsclient-argv-"))
        (input-buffer (generate-new-buffer " *codex-ide-mcp-test-input*"))
        (output-buffer (generate-new-buffer " *codex-ide-mcp-test*")))
    (unwind-protect
        (let (argv)
          (with-temp-file mock-emacsclient
            (insert "#!/usr/bin/env python3\n")
            (insert "import json\n")
            (insert "import sys\n")
            (insert (format "with open(%S, 'w', encoding='utf-8') as handle:\n" argv-log))
            (insert "    json.dump(sys.argv[1:], handle)\n")
            (insert "print(json.dumps(\"[]\"))\n"))
          (set-file-modes mock-emacsclient #o755)
          (with-current-buffer input-buffer
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'string))
              (insert
               (json-encode
                '((jsonrpc . "2.0")
                  (id . 1)
                  (method . "tools/call")
                  (params . ((name . "view_file_buffer")
                             (arguments . ((path . "/tmp/example.el")))))))
               "\n")))
          (should
           (equal
            (with-current-buffer input-buffer
              (call-process-region
               (point-min)
               (point-max)
               "python3"
               nil
               output-buffer
               nil
               script-path
               "--emacsclient"
               mock-emacsclient
               "--server-name"
               "testsrv"))
            0))
          (with-temp-buffer
            (insert-file-contents argv-log)
            (setq argv (json-read)))
          (should (= (length argv) 4))
          (should (equal (aref argv 0) "-s"))
          (should (equal (aref argv 1) "testsrv"))
          (should (equal (aref argv 2) "--eval"))
          (should (string-match-p "codex-ide-mcp-bridge--json-tool-call"
                                  (aref argv 3)))
          (with-current-buffer output-buffer
            (should (string-match-p "\"jsonrpc\":\"2.0\"" (buffer-string)))))
      (when (file-exists-p mock-emacsclient)
        (delete-file mock-emacsclient))
      (when (file-exists-p argv-log)
        (delete-file argv-log))
      (kill-buffer input-buffer)
      (kill-buffer output-buffer))))

(ert-deftest codex-ide-mcp-script-uses-emacsclient-bridge-responses ()
  (let ((script-path (expand-file-name "bin/codex-ide-mcp-server.py"
                                       codex-ide-test--root-directory))
        (mock-emacsclient (make-temp-file "codex-ide-emacsclient-" nil ".py"))
        (input-buffer (generate-new-buffer " *codex-ide-mcp-input*"))
        (output-buffer (generate-new-buffer " *codex-ide-mcp-output*")))
    (unwind-protect
        (progn
          (with-temp-file mock-emacsclient
            (insert "#!/usr/bin/env python3\n")
            (insert "import json\n")
            (insert "import sys\n")
            (insert "expr = sys.argv[-1]\n")
            (insert "response = []\n")
            (insert "if 'view_file_buffer' in expr:\n")
            (insert "    response = {'tool': 'view_file_buffer', 'params': {'path': '/tmp/example.el', 'line': 9, 'column': 2}}\n")
            (insert "elif 'get_all_open_file_buffers' in expr:\n")
            (insert "    response = {'files': [{'buffer': 'example.el', 'file': '/tmp/example.el'}]}\n")
            (insert "elif 'get_diagnostics' in expr:\n")
            (insert "    response = {'buffer': 'example.el', 'diagnostics': [{'severity': 'error', 'message': 'Boom'}]}\n")
            (insert "elif 'lisp_check_parens' in expr:\n")
            (insert "    response = {'path': '/tmp/example.el', 'balanced': False, 'mismatch': True, 'line': 9, 'column': 2, 'point': 123}\n")
            (insert "print(json.dumps(json.dumps(response, separators=(',', ':'))))\n"))
          (set-file-modes mock-emacsclient #o755)
          (with-current-buffer input-buffer
            (dolist (message
                     (list
                      `((jsonrpc . "2.0") (id . 1) (method . "initialize")
                        (params . ((protocolVersion . "2024-11-05")
                                   (capabilities . ,(make-hash-table))
                                   (clientInfo . ((name . "ert") (version . "1"))))))
                      `((jsonrpc . "2.0") (id . 2) (method . "tools/list")
                        (params . ,(make-hash-table)))
                      `((jsonrpc . "2.0") (id . 3) (method . "tools/call")
                        (params . ((name . "view_file_buffer")
                                   (arguments . ((path . "/tmp/example.el")
                                                 (line . 9)
                                                 (column . 2))))))
                      `((jsonrpc . "2.0") (id . 4) (method . "tools/call")
                        (params . ((name . "get_all_open_file_buffers")
                                   (arguments . ()))))
                      `((jsonrpc . "2.0") (id . 5) (method . "tools/call")
                        (params . ((name . "get_diagnostics")
                                   (arguments . ((buffer . "example.el"))))))
                      `((jsonrpc . "2.0") (id . 6) (method . "tools/call")
                        (params . ((name . "lisp_check_parens")
                                   (arguments . ((path . "/tmp/example.el"))))))))
              (let ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-key-type 'string))
                (insert (json-encode message))
                (insert "\n"))))
          (should
           (equal
            (with-current-buffer input-buffer
              (call-process-region
               (point-min)
               (point-max)
               "python3"
               nil
               output-buffer
               nil
               script-path
               "--emacsclient"
               mock-emacsclient
               "--server-name"
               "testsrv"))
            0))
          (with-current-buffer output-buffer
            (let ((responses nil))
              (goto-char (point-min))
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
                  (unless (string-empty-p line)
                    (push (let ((json-object-type 'alist)
                                (json-array-type 'list)
                                (json-key-type 'string))
                            (json-read-from-string line))
                          responses)))
                (forward-line 1))
              (setq responses (nreverse responses))
              (should (= (length responses) 6))
              (should
               (equal (alist-get "protocolVersion"
                                 (alist-get "result" (nth 0 responses) nil nil #'equal)
                                 nil nil #'equal)
                      "2024-11-05"))
              (let ((tools (alist-get "tools"
                                      (alist-get "result" (nth 1 responses) nil nil #'equal)
                                      nil nil #'equal)))
                (should (= (length tools) 9))
                (should
                 (equal (mapcar (lambda (tool)
                                  (alist-get "name" tool nil nil #'equal))
                                tools)
                        '("get_all_open_file_buffers"
                          "get_buffer_info"
                          "get_buffer_text"
                          "get_diagnostics"
                          "get_window_list"
                          "ensure_file_buffer_open"
                          "view_file_buffer"
                          "kill_file_buffer"
                          "lisp_check_parens"))))
              (let* ((open-file-text
                      (alist-get "text"
                                 (car (alist-get "content"
                                                 (alist-get "result" (nth 2 responses) nil nil #'equal)
                                                 nil nil #'equal))
                                 nil nil #'equal))
                     (open-files-text
                      (alist-get "text"
                                 (car (alist-get "content"
                                                 (alist-get "result" (nth 3 responses) nil nil #'equal)
                                                 nil nil #'equal))
                                 nil nil #'equal))
                      (diagnostics-text
                      (alist-get "text"
                                 (car (alist-get "content"
                                                 (alist-get "result" (nth 4 responses) nil nil #'equal)
                                                 nil nil #'equal))
                                 nil nil #'equal))
                     (parens-text
                      (alist-get "text"
                                 (car (alist-get "content"
                                                 (alist-get "result" (nth 5 responses) nil nil #'equal)
                                                 nil nil #'equal))
                                 nil nil #'equal)))
                (should (string-match-p "\"tool\": \"view_file_buffer\"" open-file-text))
                (should (string-match-p "\"path\": \"/tmp/example.el\"" open-file-text))
                (should (string-match-p "\"files\"" open-files-text))
                (should (string-match-p "\"diagnostics\"" diagnostics-text))
                (should (string-match-p "\"Boom\"" diagnostics-text))
                (should (string-match-p "\"balanced\": false" parens-text))
                (should (string-match-p "\"point\": 123" parens-text))))))
      (when (file-exists-p mock-emacsclient)
        (delete-file mock-emacsclient))
      (kill-buffer input-buffer)
      (kill-buffer output-buffer))))

(provide 'codex-ide-mcp-tests)

;;; codex-ide-mcp-tests.el ends here
