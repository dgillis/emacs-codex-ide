;;; codex-ide-mcp-tests.el --- Tests for codex-ide-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; MCP proxy tests for codex-ide.

;;; Code:

(require 'ert)
(require 'json)
(require 'subr-x)
(require 'codex-ide-test-fixtures)

(defun codex-ide-mcp-test--run-script (input)
  "Run the MCP bridge script with INPUT and return (EXIT-CODE . OUTPUT)."
  (let ((script-path (expand-file-name "bin/codex-ide-mcp-server.py"
                                       codex-ide-test--root-directory))
        (input-buffer (generate-new-buffer " *codex-ide-mcp-run-input*"))
        (output-buffer (generate-new-buffer " *codex-ide-mcp-run-output*")))
    (unwind-protect
        (progn
          (with-current-buffer input-buffer
            (insert input))
          (cons
           (with-current-buffer input-buffer
             (call-process-region
              (point-min)
              (point-max)
              "python3"
              nil
              output-buffer
              nil
              script-path))
           (with-current-buffer output-buffer
             (buffer-string))))
      (kill-buffer input-buffer)
      (kill-buffer output-buffer))))

(defun codex-ide-mcp-test--read-response (output)
  "Read the first JSON response from OUTPUT."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string
     (car (split-string output "\n" t)))))

(ert-deftest codex-ide-mcp-script-skips-blank-lines-before-message ()
  (let* ((result (codex-ide-mcp-test--run-script
                  "\n{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}\n"))
         (response (codex-ide-mcp-test--read-response (cdr result))))
    (should (= (car result) 0))
    (should (equal (alist-get "id" response nil nil #'equal) 1))
    (should-not (alist-get "result" response nil nil #'equal))))

(ert-deftest codex-ide-mcp-script-returns-parse-error-for-malformed-json ()
  (let* ((result (codex-ide-mcp-test--run-script "{bad json}\n"))
         (response (codex-ide-mcp-test--read-response (cdr result)))
         (error (alist-get "error" response nil nil #'equal)))
    (should (= (car result) 0))
    (should (= (alist-get "code" error nil nil #'equal) -32700))
    (should (string-match-p "Parse error"
                            (alist-get "message" error nil nil #'equal)))))

(ert-deftest codex-ide-mcp-script-rejects-non-object-json ()
  (let* ((result (codex-ide-mcp-test--run-script "[]\n"))
         (response (codex-ide-mcp-test--read-response (cdr result)))
         (error (alist-get "error" response nil nil #'equal)))
    (should (= (car result) 0))
    (should (= (alist-get "code" error nil nil #'equal) -32600))
    (should (string-match-p "message must be a JSON object"
                            (alist-get "message" error nil nil #'equal)))))

(ert-deftest codex-ide-mcp-script-rejects-non-object-params ()
  (let* ((result (codex-ide-mcp-test--run-script
                  "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":[]}\n"))
         (response (codex-ide-mcp-test--read-response (cdr result)))
         (error (alist-get "error" response nil nil #'equal)))
    (should (= (car result) 0))
    (should (= (alist-get "code" error nil nil #'equal) -32600))
    (should (string-match-p "params must be an object"
                            (alist-get "message" error nil nil #'equal)))))

(ert-deftest codex-ide-mcp-script-rejects-non-object-tool-arguments ()
  (let* ((result (codex-ide-mcp-test--run-script
                  "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"emacs_get_buffer_info\",\"arguments\":[]}}\n"))
         (response (codex-ide-mcp-test--read-response (cdr result)))
         (tool-result (alist-get "result" response nil nil #'equal))
         (content (car (alist-get "content" tool-result nil nil #'equal))))
    (should (= (car result) 0))
    (should (alist-get "isError" tool-result nil nil #'equal))
    (should (string-match-p "Invalid tool arguments"
                            (alist-get "text" content nil nil #'equal)))))

(ert-deftest codex-ide-mcp-script-accepts-content-length-framed-message ()
  (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"ping\"}")
         (result (codex-ide-mcp-test--run-script
                  (format "Content-Length: %d\r\n\r\n%s"
                          (string-bytes body)
                          body)))
         (response (codex-ide-mcp-test--read-response (cdr result))))
    (should (= (car result) 0))
    (should (equal (alist-get "id" response nil nil #'equal) 7))
    (should-not (alist-get "result" response nil nil #'equal))))

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
                  (params . ((name . "emacs_show_file_buffer")
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
          (should (string-match-p "base64-encode-string"
                                  (aref argv 3)))
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
            (insert "import base64\n")
            (insert "import json\n")
            (insert "import sys\n")
            (insert "expr = sys.argv[-1]\n")
            (insert "response = []\n")
            (insert "if 'emacs_show_file_buffer' in expr:\n")
            (insert "    response = {'tool': 'emacs_show_file_buffer', 'params': {'path': '/tmp/example.el', 'line': 9, 'column': 2}}\n")
            (insert "elif 'emacs_get_all_buffers' in expr:\n")
            (insert "    response = {'files': [{'buffer': 'example.el', 'file': '/tmp/example.el'}]}\n")
            (insert "elif 'emacs_get_buffer_diagnostics' in expr:\n")
            (insert "    response = {'buffer': 'example.el', 'diagnostics': [{'severity': 'error', 'message': 'Boom'}]}\n")
            (insert "elif 'emacs_lisp_check_parens' in expr:\n")
            (insert "    response = {'path': '/tmp/example.el', 'balanced': False, 'mismatch': True, 'line': 9, 'column': 2, 'point': 123}\n")
            (insert "payload = json.dumps(response, separators=(',', ':'), ensure_ascii=False).encode('utf-8')\n")
            (insert "print(json.dumps(base64.b64encode(payload).decode('ascii')))\n"))
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
                        (params . ((name . "emacs_show_file_buffer")
                                   (arguments . ((path . "/tmp/example.el")
                                                 (line . 9)
                                                 (column . 2))))))
                      `((jsonrpc . "2.0") (id . 4) (method . "tools/call")
                        (params . ((name . "emacs_get_all_buffers")
                                   (arguments . ()))))
                      `((jsonrpc . "2.0") (id . 5) (method . "tools/call")
                        (params . ((name . "emacs_get_buffer_diagnostics")
                                   (arguments . ((buffer . "example.el"))))))
                      `((jsonrpc . "2.0") (id . 6) (method . "tools/call")
                        (params . ((name . "emacs_lisp_check_parens")
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
                (should (= (length tools) 17))
                (should
                 (equal (mapcar (lambda (tool)
                                  (alist-get "name" tool nil nil #'equal))
                                tools)
                        '("emacs_get_all_buffers"
                          "emacs_get_buffer_info"
                          "emacs_get_buffer_text"
                          "emacs_get_buffer_diagnostics"
                          "emacs_get_current_context"
                          "emacs_get_buffer_slice"
                          "emacs_get_region_text"
                          "emacs_search_buffers"
                          "emacs_get_symbol_at_point"
                          "emacs_describe_symbol"
                          "emacs_get_messages"
                          "emacs_get_minibuffer_state"
                          "emacs_get_all_windows"
                          "emacs_ensure_file_buffer_open"
                          "emacs_show_file_buffer"
                          "emacs_kill_file_buffer"
                          "emacs_lisp_check_parens"))))
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
                (should (string-match-p "\"tool\": \"emacs_show_file_buffer\"" open-file-text))
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

(ert-deftest codex-ide-mcp-script-decodes-base64-bridge-response-with-control-and-unicode-text ()
  (let ((script-path (expand-file-name "bin/codex-ide-mcp-server.py"
                                       codex-ide-test--root-directory))
        (mock-emacsclient (make-temp-file "codex-ide-emacsclient-" nil ".py"))
        (input-buffer (generate-new-buffer " *codex-ide-mcp-control-input*"))
        (output-buffer (generate-new-buffer " *codex-ide-mcp-control-output*")))
    (unwind-protect
        (progn
          (with-temp-file mock-emacsclient
            (insert "#!/usr/bin/env python3\n")
            (insert "import base64\n")
            (insert "import json\n")
            (insert "payload = json.dumps({'buffer': 'example.el', 'text': 'alpha\\u000bbeta\\n└'}, separators=(',', ':'), ensure_ascii=False).encode('utf-8')\n")
            (insert "print(json.dumps(base64.b64encode(payload).decode('ascii')))\n"))
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
                  (params . ((name . "emacs_get_buffer_text")
                             (arguments . ((buffer . "example.el")))))))
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
               mock-emacsclient))
            0))
          (with-current-buffer output-buffer
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (response (json-read-from-string
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
                   (text (alist-get "text"
                                    (car (alist-get "content"
                                                    (alist-get "result" response nil nil #'equal)
                                                    nil nil #'equal))
                                    nil nil #'equal)))
              (should (string-match-p "\"buffer\": \"example.el\"" text))
              (should (string-match-p "\"text\": \"alpha\\\\u000bbeta\\\\n\\\\u2514\"" text)))))
      (when (file-exists-p mock-emacsclient)
        (delete-file mock-emacsclient))
      (kill-buffer input-buffer)
      (kill-buffer output-buffer))))

(provide 'codex-ide-mcp-tests)

;;; codex-ide-mcp-tests.el ends here
