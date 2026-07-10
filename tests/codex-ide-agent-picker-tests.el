;;; codex-ide-agent-picker-tests.el --- Tests for Codex agent selection -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for querying and selecting threads in the current agent tree.

;;; Code:

(require 'ert)
(require 'codex-ide)

(ert-deftest codex-ide-list-descendant-threads-paginates-agent-sources ()
  (let ((requests nil)
        (codex-ide-thread-list-default-limit 2))
    (cl-letf (((symbol-function 'codex-ide--request-sync)
               (lambda (_session method params)
                 (push (cons method params) requests)
                 (if (alist-get 'cursor params)
                     '((data . [((id . "child-2"))]))
                   '((data . [((id . "child-1"))])
                     (nextCursor . "page-2"))))))
      (should (equal (codex-ide--list-descendant-threads 'session "root-thread")
                     '(((id . "child-1"))
                       ((id . "child-2")))))
      (setq requests (nreverse requests))
      (should (equal (mapcar #'car requests)
                     '("thread/list" "thread/list")))
      (let ((first (cdar requests))
            (second (cdadr requests)))
        (should (equal (alist-get 'ancestorThreadId first) "root-thread"))
        (should (equal (append (alist-get 'sourceKinds first) nil)
                       '("subAgent"
                         "subAgentReview"
                         "subAgentCompact"
                         "subAgentThreadSpawn"
                         "subAgentOther")))
        (should (equal (alist-get 'sortKey first) "created_at"))
        (should (equal (alist-get 'sortDirection first) "asc"))
        (should (= (alist-get 'limit first) 2))
        (should-not (alist-get 'cursor first))
        (should (equal (alist-get 'cursor second) "page-2"))))))

(ert-deftest codex-ide-agent-tree-threads-finds-root-from-current-thread ()
  (let ((read-ids nil)
        (root '((id . "root-thread") (cwd . "/tmp/project/")))
        (child '((id . "child-1") (parentThreadId . "root-thread")))
        (grandchild '((id . "child-2") (parentThreadId . "child-1"))))
    (cl-letf (((symbol-function 'codex-ide--read-thread)
               (lambda (_session thread-id _include-turns)
                 (push thread-id read-ids)
                 `((thread . ,(pcase thread-id
                                ("root-thread" root)
                                ("child-1" child)
                                ("child-2" grandchild))))))
              ((symbol-function 'codex-ide--list-descendant-threads)
               (lambda (_session ancestor-thread-id)
                 (should (equal ancestor-thread-id "root-thread"))
                 (list child grandchild))))
      (should (equal (codex-ide--agent-tree-threads 'session "child-2")
                     (list root child grandchild)))
      (should (equal (nreverse read-ids)
                     '("child-2" "child-1" "root-thread"))))))

(ert-deftest codex-ide-agent-tree-threads-explains-unsupported-filter ()
  (cl-letf (((symbol-function 'codex-ide--agent-root-thread)
             (lambda (_session _thread-id) '((id . "root-thread"))))
            ((symbol-function 'codex-ide--list-descendant-threads)
             (lambda (_session _thread-id)
               (error "Codex app-server request thread/list failed: Invalid request: unknown field `ancestorThreadId`"))))
    (let ((err (should-error
                (codex-ide--agent-tree-threads 'session "root-thread")
                :type 'user-error)))
      (should (string-match-p "update the Codex CLI"
                              (error-message-string err))))))

(ert-deftest codex-ide-agent-tree-threads-preserves-unrelated-request-errors ()
  (cl-letf (((symbol-function 'codex-ide--agent-root-thread)
             (lambda (_session _thread-id) '((id . "root-thread"))))
            ((symbol-function 'codex-ide--list-descendant-threads)
             (lambda (_session _thread-id)
               (error "Codex app-server request thread/list failed: Invalid request: database unavailable"))))
    (condition-case err
        (progn
          (codex-ide--agent-tree-threads 'session "root-thread")
          (ert-fail "Expected the app-server error to be preserved"))
      (user-error
       (ert-fail (format "Unexpected compatibility error: %s"
                         (error-message-string err))))
      (error
       (should (string-match-p "database unavailable"
                               (error-message-string err)))))))

(ert-deftest codex-ide-pick-agent-thread-shows-agent-metadata ()
  (let* ((root '((id . "root-thread")
                 (cwd . "/tmp/project/")
                 (status . ((type . "idle")))))
         (child '((id . "child-12345678")
                  (parentThreadId . "root-thread")
                  (agentNickname . "worker-one")
                  (agentRole . "explorer")
                  (status . ((type . "active")))))
         (selected nil)
         (recorded-extra-properties nil))
    (cl-letf (((symbol-function 'codex-ide--agent-tree-threads)
               (lambda (_session _thread-id) (list root child)))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest _args)
                 (should (equal prompt "Switch to agent: "))
                 (should (equal (mapcar #'car collection)
                                '("Main" "  worker-one")))
                 (setq recorded-extra-properties completion-extra-properties
                       selected "  worker-one")
                 selected)))
      (should (equal (codex-ide--pick-agent-thread 'session "child-12345678")
                     child))
      (let ((affixation
             (car (funcall (plist-get recorded-extra-properties
                                      :affixation-function)
                           (list selected)))))
        (should (equal (nth 2 affixation)
                       " [explorer, active, child-12]"))))))

(ert-deftest codex-ide-pick-agent-thread-errors-without-subagents ()
  (cl-letf (((symbol-function 'codex-ide--agent-tree-threads)
             (lambda (_session _thread-id)
               '(((id . "root-thread"))))))
    (should-error (codex-ide--pick-agent-thread 'session "root-thread")
                  :type 'user-error)))

(ert-deftest codex-ide-agent-picker-opens-selected-thread ()
  (let ((session (make-codex-ide-session
                  :thread-id "child-current"
                  :directory "/tmp/current/"))
        (opened nil)
        (selected '((id . "child-selected")
                    (cwd . "/tmp/selected/"))))
    (cl-letf (((symbol-function 'codex-ide--get-default-session-for-current-buffer)
               (lambda () session))
              ((symbol-function 'codex-ide--pick-agent-thread)
               (lambda (actual-session current-thread-id)
                 (should (eq actual-session session))
                 (should (equal current-thread-id "child-current"))
                 selected))
              ((symbol-function 'codex-ide--show-or-resume-thread)
               (lambda (thread-id directory)
                 (setq opened (list thread-id directory)))))
      (codex-ide-agent-picker)
      (should (equal opened '("child-selected" "/tmp/selected/"))))))

(provide 'codex-ide-agent-picker-tests)

;;; codex-ide-agent-picker-tests.el ends here
