;;; codex-ide-thread-history-tests.el --- Tests for stored thread history helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for stored thread normalization.

;;; Code:

(require 'ert)
(require 'json)
(require 'codex-ide-thread-history)

(ert-deftest codex-ide-thread-history-adds-rollout-render-items ()
  (let ((path (make-temp-file "codex-ide-thread-history-" nil ".jsonl"))
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
                        (payload . ((type . "custom_tool_call")
                                    (name . "apply_patch")
                                    (call_id . "call-patch-1")
                                    (input . ,patch-text))))
                       ((type . "event_msg")
                        (payload . ((type . "task_complete"))))))
              (insert (json-encode entry) "\n")))
          (let* ((thread-read
                  `((thread . ((id . "thread-1")
                               (path . ,path)
                               (turns . [((id . "turn-1")
                                          (items . [((type . "userMessage")
                                                     (text . "change it"))
                                                    ((type . "agentMessage")
                                                     (text . "done"))]))])))))
                 (normalized
                  (codex-ide--thread-read-with-rollout-render-items
                   thread-read))
                 (turn (car (append (codex-ide--thread-read-turns normalized)
                                    nil)))
                 (items (append (codex-ide--thread-read-items turn) nil)))
            (should (equal (mapcar (lambda (item) (alist-get 'type item)) items)
                           '("userMessage" "fileChange" "agentMessage")))
            (should (equal (alist-get 'diff
                                      (car (alist-get 'changes (cadr items))))
                           patch-text))))
      (when (file-exists-p path)
        (delete-file path)))))

(ert-deftest codex-ide-thread-history-preserves-thread-read-without-rollout ()
  (let ((thread-read '((thread . ((id . "thread-1")
                                  (turns . [((id . "turn-1")
                                             (items . []))]))))))
    (should (eq (codex-ide--thread-read-with-rollout-render-items thread-read)
                thread-read))))

(ert-deftest codex-ide-thread-history-passes-rollout-limit ()
  (let ((thread-read '((thread . ((id . "thread-1")
                                  (path . "/tmp/thread.jsonl")
                                  (turns . [((id . "turn-1")
                                             (items . []))])))))
        captured-limit)
    (cl-letf (((symbol-function 'codex-ide-rollout-turn-render-items)
               (lambda (_path &optional limit)
                 (setq captured-limit limit)
                 nil)))
      (codex-ide--thread-read-with-rollout-render-items thread-read 7))
    (should (equal captured-limit 7))))

(ert-deftest codex-ide-thread-history-aligns-limited-rollout-tail ()
  (let* ((thread-read
          '((thread . ((id . "thread-1")
                       (path . "/tmp/thread.jsonl")
                       (turns . [((id . "turn-1")
                                  (items . [((type . "userMessage")
                                             (text . "old prompt"))]))
                                 ((id . "turn-2")
                                  (items . [((type . "userMessage")
                                             (text . "recent prompt 1"))]))
                                 ((id . "turn-3")
                                  (items . [((type . "userMessage")
                                             (text . "recent prompt 2"))]))])))))
         (rollout-turns
          '((((type . "agentMessage")
              (text . "recent answer 1")))
            (((type . "agentMessage")
              (text . "recent answer 2"))))))
    (cl-letf (((symbol-function 'codex-ide-rollout-turn-render-items)
               (lambda (_path &optional _limit)
                 rollout-turns)))
      (let* ((normalized
              (codex-ide--thread-read-with-rollout-render-items thread-read 2))
             (turns (append (codex-ide--thread-read-turns normalized) nil)))
        (should (= (length turns) 2))
        (should (equal (mapcar (lambda (turn) (alist-get 'id turn)) turns)
                       '("turn-2" "turn-3")))
        (should (equal
                 (mapcar
                  (lambda (turn)
                    (mapcar (lambda (item)
                              (alist-get 'text item))
                            (append (codex-ide--thread-read-items turn) nil)))
                  turns)
                 '(("recent prompt 1" "recent answer 1")
                   ("recent prompt 2" "recent answer 2"))))))))

(ert-deftest codex-ide-thread-history-skips-rollout-when-limit-is-zero ()
  (let ((thread-read '((thread . ((id . "thread-1")
                                  (path . "/tmp/thread.jsonl")
                                  (turns . [((id . "turn-1")
                                             (items . []))]))))))
    (cl-letf (((symbol-function 'codex-ide-rollout-turn-render-items)
               (lambda (&rest _)
                 (ert-fail "Rollout should not be parsed for a zero limit"))))
      (should (eq (codex-ide--thread-read-with-rollout-render-items
                   thread-read 0)
                  thread-read)))))

(provide 'codex-ide-thread-history-tests)

;;; codex-ide-thread-history-tests.el ends here
