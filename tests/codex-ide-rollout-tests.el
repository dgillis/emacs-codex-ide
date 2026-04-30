;;; codex-ide-rollout-tests.el --- Tests for codex-ide-rollout -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for the rollout JSONL storage adapter.

;;; Code:

(require 'ert)
(require 'json)
(require 'codex-ide-rollout)

(ert-deftest codex-ide-rollout-turn-render-items-preserves-storage-order ()
  (let ((path (make-temp-file "codex-ide-rollout-" nil ".jsonl"))
        (patch-text (string-join
                     '("*** Begin Patch"
                       "*** Update File: foo.txt"
                       "@@"
                       "-old"
                       "+new"
                       "*** End Patch")
                     "\n")))
    (unwind-protect
        (progn
          (with-temp-file path
            (dolist (entry
                     `(((type . "event_msg")
                        (payload . ((type . "task_started"))))
                       ((type . "response_item")
                        (payload . ((type . "message")
                                    (role . "assistant")
                                    (content . (((type . "output_text")
                                                 (text . "First.")))))))
                       ((type . "response_item")
                        (payload . ((type . "function_call")
                                    (name . "exec_command")
                                    (call_id . "call-command-1")
                                    (arguments . ,(json-encode
                                                   '((cmd . "printf hello")))))))
                       ((type . "response_item")
                        (payload . ((type . "function_call_output")
                                    (call_id . "call-command-1")
                                    (output . "hello\n"))))
                       ((type . "response_item")
                        (payload . ((type . "message")
                                    (role . "assistant")
                                    (content . (((type . "output_text")
                                                 (text . "Second.")))))))
                       ((type . "response_item")
                        (payload . ((type . "custom_tool_call")
                                    (name . "apply_patch")
                                    (call_id . "call-patch-1")
                                    (input . ,patch-text))))
                       ((type . "response_item")
                        (payload . ((type . "custom_tool_call_output")
                                    (call_id . "call-patch-1")
                                    (output . "{\"output\":\"Success\"}"))))
                       ((type . "event_msg")
                        (payload . ((type . "task_complete"))))))
              (insert (json-encode entry) "\n")))
          (let* ((turns (codex-ide-rollout-turn-render-items path))
                 (items (car turns)))
            (should (= (length turns) 1))
            (should (equal (mapcar (lambda (item) (alist-get 'type item)) items)
                           '("agentMessage"
                             "commandExecution"
                             "agentMessage"
                             "fileChange")))
            (should (equal (alist-get 'text (nth 0 items)) "First."))
            (should (equal (alist-get 'aggregatedOutput (nth 1 items)) "hello\n"))
            (should (equal (alist-get 'text (nth 2 items)) "Second."))
            (should (equal (alist-get 'diff
                                      (car (alist-get 'changes (nth 3 items))))
                           patch-text))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest codex-ide-rollout-turn-render-items-ignores-unknown-storage-records ()
  (let ((path (make-temp-file "codex-ide-rollout-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file path
            (dolist (entry
                     '(((type . "event_msg")
                        (payload . ((type . "task_started"))))
                       ((type . "response_item")
                        (payload . ((type . "function_call")
                                    (name . "future_call")
                                    (call_id . "call-1")
                                    (arguments . "{\"x\":true}"))))
                       ((type . "response_item")
                        (payload . ((type . "function_call_output")
                                    (call_id . "call-1")
                                    (output . "should not render"))))
                       ((type . "response_item")
                        (payload . ((type . "custom_tool_call")
                                    (name . "future_custom")
                                    (call_id . "call-2")
                                    (input . "opaque"))))
                       ((type . "response_item")
                        (payload . ((type . "message")
                                    (role . "assistant")
                                    (content . (((type . "output_text")
                                                 (text . "Still here.")))))))
                       ((type . "event_msg")
                        (payload . ((type . "task_complete"))))))
              (insert (json-encode entry) "\n")))
          (let* ((turns (codex-ide-rollout-turn-render-items path))
                 (items (car turns)))
            (should (= (length turns) 1))
            (should (equal (length items) 1))
            (should (equal (alist-get 'type (car items)) "agentMessage"))
            (should (equal (alist-get 'text (car items)) "Still here."))))
      (when (file-exists-p path)
        (delete-file path)))))

(provide 'codex-ide-rollout-tests)

;;; codex-ide-rollout-tests.el ends here
