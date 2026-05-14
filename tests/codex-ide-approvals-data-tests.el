;;; codex-ide-approvals-data-tests.el --- Tests for approval data helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for lifecycle-aware approval data records.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide-approvals-data)

(ert-deftest codex-ide-approvals-data-tracks-pending-and-resolved-by-turn ()
  (let ((session (make-codex-ide-session)))
    (codex-ide-approvals-data-add
     session
     1
     'command
     '((command . "git status"))
     :turn-id "turn-1"
     :created-at '(1 0 0 0))
    (codex-ide-approvals-data-add
     session
     2
     'command
     '((command . "pwd"))
     :turn-id "turn-1"
     :created-at '(1 0 1 0))
    (should (= (codex-ide-approvals-data-count session :turn-id "turn-1") 2))
    (should (equal (mapcar
                    (lambda (approval) (plist-get approval :id))
                    (codex-ide-approvals-data-list
                     session
                     :turn-id "turn-1"))
                   '(1 2)))
    (should (= (codex-ide-approvals-data-count
                session
                :status 'pending
                :turn-id "turn-1")
               2))
    (codex-ide-approvals-data-resolve
     session
     1
     'accepted
     :decision "accept"
     :result '((decision . "accept"))
     :resolved-at '(1 0 2 0))
    (should (= (codex-ide-approvals-data-count session :turn-id "turn-1") 2))
    (should (= (codex-ide-approvals-data-count
                session
                :status 'pending
                :turn-id "turn-1")
               1))
    (let ((approval (codex-ide-approvals-data-get session 1)))
      (should (eq (plist-get approval :status) 'accepted))
      (should (equal (plist-get approval :decision) "accept"))
      (should (equal (plist-get approval :result) '((decision . "accept")))))))

(ert-deftest codex-ide-approvals-data-clears-heavy-view-state-on-resolution ()
  (let ((session (make-codex-ide-session))
        (start-marker (make-marker))
        (status-marker (make-marker))
        (end-marker (make-marker)))
    (codex-ide-approvals-data-add
     session
     7
     'elicitation
     '((message . "Need input"))
     :turn-id "turn-2"
     :view (list :start-marker start-marker
                 :status-marker status-marker
                 :end-marker end-marker
                 :fields (list (list :name 'choice
                                     :value-cell (list "a")
                                     :display-start-marker (make-marker)))))
    (should (codex-ide-approvals-data-view-get
             (codex-ide-approvals-data-get session 7)
             :start-marker))
    (codex-ide-approvals-data-resolve
     session
     7
     'declined
     :decision '((action . "decline"))
     :result '((action . "decline"))
     :clear-view t)
    (let ((approval (codex-ide-approvals-data-get session 7)))
      (should (eq (plist-get approval :status) 'declined))
      (should-not (plist-get approval :view)))))

(provide 'codex-ide-approvals-data-tests)

;;; codex-ide-approvals-data-tests.el ends here
