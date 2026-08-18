;;; codex-ide-status-api-tests.el --- Status API tests for codex-ide -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'codex-ide)
(require 'codex-ide-status-api)

(ert-deftest codex-ide-thread-row-normalizes-public-schema ()
  (let* ((directory (codex-ide--normalize-directory "/tmp/status-api"))
         (session (codex-ide-session :directory directory
                                     :thread-id "thread-one"
                                     :status "running"))
         (thread `((id . "thread-one")
                   (name . "Public row")
                   (cwd . ,directory)
                   (createdAt . 10)
                   (updatedAt . 20))))
    (cl-letf (((symbol-function 'codex-ide--session-for-thread-id)
               (lambda (thread-id row-directory)
                 (when (and (equal thread-id "thread-one")
                            (equal row-directory directory))
                   session))))
      (let ((row (codex-ide-thread-row thread)))
        (should (eq (plist-get row :kind) 'thread))
        (should (equal (plist-get row :thread-id) "thread-one"))
        (should (equal (plist-get row :title) "Public row"))
        (should (equal (plist-get row :directory) directory))
        (should (= (plist-get row :created-at) 10))
        (should (= (plist-get row :updated-at) 20))
        (should (equal (plist-get row :technical-status) "running"))
        (should (eq (plist-get row :session) session))))))

(ert-deftest codex-ide-list-thread-rows-uses-project-inventory ()
  (let ((project-called nil)
        (thread '((id . "thread-project")
                  (name . "Project")
                  (cwd . "/tmp/project"))))
    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
               (lambda () nil))
              ((symbol-function 'codex-ide--get-working-directory)
               (lambda () "/tmp/query"))
              ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
               (lambda (_directory) 'query-session))
              ((symbol-function 'codex-ide--thread-list-data)
               (lambda (_session &optional _omit-thread-id)
                 (setq project-called t)
                 (list thread)))
              ((symbol-function 'codex-ide--session-for-thread-id)
               (lambda (&rest _args) nil)))
      (let ((rows (codex-ide-list-thread-rows :directory "/tmp/project")))
        (should project-called)
        (should (equal (plist-get (car rows) :thread-id) "thread-project"))
        (should (equal (plist-get (car rows) :technical-status) "stored"))))))

(ert-deftest codex-ide-status-actions-register-filter-and-unregister ()
  (let ((codex-ide-status-actions nil)
        (handler (lambda (_row) 'handled)))
    (codex-ide-register-status-action
     "Only linked"
     handler
     (lambda (row) (plist-get row :linked)))
    (should-not (codex-ide-status-available-actions '(:linked nil)))
    (should (equal (plist-get
                    (car (codex-ide-status-available-actions '(:linked t)))
                    :function)
                   handler))
    (codex-ide-unregister-status-action "Only linked")
    (should-not codex-ide-status-actions)))

(ert-deftest codex-ide-status-title-text-combines-provider-results ()
  (let ((codex-ide-status-before-title-functions
         (list (lambda (_row) "TODO")
               (lambda (_row) '("linked" "review"))
               (lambda (_row) nil)))
        (codex-ide-status-after-title-functions
         (list (lambda (_row) "backend"))))
    (should (equal (codex-ide-status-before-title-text
                    '(:thread-id "thread"))
                   "TODO  linked  review"))
    (should (equal (codex-ide-status-after-title-text
                    '(:thread-id "thread"))
                   "backend"))))

(provide 'codex-ide-status-api-tests)

;;; codex-ide-status-api-tests.el ends here
