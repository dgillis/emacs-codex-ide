;;; codex-ide-session-thread-list-tests.el --- Tests for session thread list -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-session-thread-list'.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-session-thread-list)

(ert-deftest codex-ide-session-thread-list-creates-background-query-session-and-renders-threads ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (requests nil)
        (buffer-name nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                   (lambda () t))
                  ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
                   (lambda () nil))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda () nil))
                  ((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (push method requests)
                     (pcase method
                       ("initialize" '((ok . t)))
                       ("thread/list"
                        '((data . [((id . "thread-12345678")
                                    (createdAt . 1744038896)
                                    (updatedAt . 1744039999)
                                    (preview . "Investigate\nfailure"))])))
                       (_ (ert-fail (format "Unexpected method %s" method)))))))
          (setq buffer-name
                (format "*Codex Threads: %s*"
                        (file-name-nondirectory
                         (directory-file-name project-dir))))
          (codex-ide-session-thread-list)
          (should (= (length codex-ide--sessions) 1))
          (should (equal (seq-remove (lambda (method)
                                       (equal method "config/read"))
                                     (nreverse requests))
                         '("initialize" "thread/list")))
          (with-current-buffer buffer-name
            (should (derived-mode-p 'codex-ide-session-thread-list-mode))
            (should hl-line-mode)
            (should (eq hl-line-face 'codex-ide-session-list-current-row-face))
            (should (equal (mapcar #'car (append tabulated-list-format nil))
                           '("ID" "Preview" "Status" "Buffer" "Created" "Updated")))
            (let* ((entries (funcall tabulated-list-entries))
                   (first-row (cadr (car entries))))
              (should (eq (get-text-property 0 'face (aref first-row 0))
                          'codex-ide-session-list-id-face))
              (should (eq (get-text-property 0 'face (aref first-row 1))
                          'codex-ide-session-list-primary-face))
              (should (eq (get-text-property 0 'face (aref first-row 2))
                          'codex-ide-session-list-status-face))
              (should (eq (get-text-property 0 'face (aref first-row 4))
                          'codex-ide-session-list-time-face))
              (should (eq (get-text-property 0 'face (aref first-row 5))
                          'codex-ide-session-list-time-face))
              (should (string= (substring-no-properties (aref first-row 3)) "")))
            (should (string-match-p "Investigate↵failure" (buffer-string)))
            (should (string-match-p "thread-1" (buffer-string)))))))))

(ert-deftest codex-ide-session-thread-list-redisplay-refreshes-visible-rows ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (thread-list-call-count 0)
        (buffer-name nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                   (lambda () t))
                  ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
                   (lambda () nil))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda () nil))
                  ((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (pcase method
                       ("initialize" '((ok . t)))
                       ("thread/list"
                        (prog1
                            (if (= thread-list-call-count 0)
                                '((data . [((id . "thread-11111111")
                                            (createdAt . 1744038896)
                                            (updatedAt . 1744039999)
                                            (preview . "First result"))]))
                              '((data . [((id . "thread-22222222")
                                          (createdAt . 1744038896)
                                          (updatedAt . 1744041000)
                                          (preview . "Updated result"))])))
                          (setq thread-list-call-count (1+ thread-list-call-count))))
                       (_ (ert-fail (format "Unexpected method %s" method)))))))
          (setq buffer-name
                (format "*Codex Threads: %s*"
                        (file-name-nondirectory
                         (directory-file-name project-dir))))
          (codex-ide-session-thread-list)
          (with-current-buffer buffer-name
            (should (eq (lookup-key codex-ide-session-thread-list-mode-map (kbd "l"))
                        #'codex-ide-session-thread-list-redisplay))
            (should (string-match-p "First result" (buffer-string)))
            (should-not (string-match-p "Updated result" (buffer-string)))
            (call-interactively #'codex-ide-session-thread-list-redisplay)
            (should (= thread-list-call-count 2))
            (should-not (string-match-p "First result" (buffer-string)))
            (should (string-match-p "Updated result" (buffer-string)))))))))

(ert-deftest codex-ide-session-thread-list-thread-status-uses-renderer-label-for-live-sessions ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (thread '((id . "thread-12345678"))))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((live-session (make-codex-ide-session
                             :directory project-dir
                             :buffer (get-buffer-create " *codex-thread-live*")
                             :thread-id "thread-12345678"
                              :status "running")))
          (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                     (lambda () t))
                    ((symbol-function 'codex-ide--session-for-thread-id)
                     (lambda (thread-id _directory)
                       (when (equal thread-id "thread-12345678")
                         live-session))))
            (should (equal (codex-ide-session-thread-list--thread-status
                            thread
                            project-dir)
                           "Running"))))))))

(ert-deftest codex-ide-session-thread-list-delete-thread-binds-D-and-refreshes ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (deleted-thread-id nil)
        (skip-confirmation nil)
        (thread-deleted nil)
        (buffer-name nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                   (lambda () t))
                  ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
                   (lambda () nil))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda () nil))
                  ((symbol-function 'codex-ide-delete-session-thread)
                   (lambda (thread-id &optional skip-confirm)
                     (setq deleted-thread-id thread-id
                           skip-confirmation skip-confirm
                           thread-deleted t)))
                  ((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (pcase method
                       ("initialize" '((ok . t)))
                       ("thread/list"
                        (if thread-deleted
                            '((data . [((id . "thread-22222222")
                                        (createdAt . 1744038896)
                                        (updatedAt . 1744041000)
                                        (preview . "Remaining thread"))]))
                          '((data . [((id . "thread-11111111")
                                      (createdAt . 1744038896)
                                      (updatedAt . 1744039999)
                                      (preview . "Delete me"))
                                     ((id . "thread-22222222")
                                      (createdAt . 1744038896)
                                      (updatedAt . 1744041000)
                                      (preview . "Remaining thread"))]))))
                       (_ (ert-fail (format "Unexpected method %s" method)))))))
          (setq buffer-name
                (format "*Codex Threads: %s*"
                        (file-name-nondirectory
                         (directory-file-name project-dir))))
          (codex-ide-session-thread-list)
          (with-current-buffer buffer-name
            (should (eq (lookup-key codex-ide-session-thread-list-mode-map (kbd "D"))
                        #'codex-ide-session-thread-list-delete-thread))
            (goto-char (point-min))
            (search-forward "Delete me")
            (beginning-of-line)
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (ert-fail "Unexpected y-or-n-p prompt")))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (prompt)
                         (should (equal prompt
                                        (format "Permanently remove 1 Codex thread from %s? "
                                                (abbreviate-file-name
                                                 (codex-ide--codex-home)))))
                         t)))
              (call-interactively #'codex-ide-session-thread-list-delete-thread))
            (should (equal deleted-thread-id "thread-11111111"))
            (should skip-confirmation)
            (should-not (string-match-p "Delete me" (buffer-string)))
            (should (string-match-p "Remaining thread" (buffer-string)))))))))

(ert-deftest codex-ide-session-thread-list-delete-thread-deletes-region-with-one-confirmation ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (deleted-thread-ids nil)
        (skip-confirmations nil)
        (thread-deleted-count 0)
        (confirmation-count 0)
        (buffer-name nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (cl-letf (((symbol-function 'codex-ide--ensure-cli)
                   (lambda () t))
                  ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
                   (lambda () nil))
                  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
                   (lambda () nil))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (ert-fail "Unexpected y-or-n-p prompt")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (prompt)
                     (setq confirmation-count (1+ confirmation-count))
                     (should (equal prompt
                                    (format "Permanently remove 2 Codex threads from %s? "
                                            (abbreviate-file-name
                                             (codex-ide--codex-home)))))
                     t))
                  ((symbol-function 'codex-ide-delete-session-thread)
                   (lambda (thread-id &optional skip-confirm)
                     (push thread-id deleted-thread-ids)
                     (push skip-confirm skip-confirmations)
                     (setq thread-deleted-count (1+ thread-deleted-count))))
                  ((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (pcase method
                       ("initialize" '((ok . t)))
                       ("thread/list"
                        (if (>= thread-deleted-count 2)
                            '((data . [((id . "thread-33333333")
                                        (createdAt . 1744038896)
                                        (updatedAt . 1744042000)
                                        (preview . "Remaining thread"))]))
                          '((data . [((id . "thread-11111111")
                                      (createdAt . 1744038896)
                                      (updatedAt . 1744039999)
                                      (preview . "Delete me first"))
                                     ((id . "thread-22222222")
                                      (createdAt . 1744038896)
                                      (updatedAt . 1744041000)
                                      (preview . "Delete me second"))
                                     ((id . "thread-33333333")
                                      (createdAt . 1744038896)
                                      (updatedAt . 1744042000)
                                      (preview . "Remaining thread"))]))))
                       (_ (ert-fail (format "Unexpected method %s" method)))))))
          (setq buffer-name
                (format "*Codex Threads: %s*"
                        (file-name-nondirectory
                         (directory-file-name project-dir))))
          (codex-ide-session-thread-list)
          (with-current-buffer buffer-name
            (cl-letf (((symbol-function 'codex-ide-session-list-selected-ids)
                       (lambda ()
                         '("thread-22222222" "thread-11111111"))))
              (call-interactively #'codex-ide-session-thread-list-delete-thread))
            (should (= confirmation-count 1))
            (should (equal deleted-thread-ids
                           '("thread-11111111" "thread-22222222")))
            (should (equal skip-confirmations '(t t)))
            (should-not (string-match-p "Delete me first" (buffer-string)))
            (should-not (string-match-p "Delete me second" (buffer-string)))
            (should (string-match-p "Remaining thread" (buffer-string)))))))))

(provide 'codex-ide-session-thread-list-tests)

;;; codex-ide-session-thread-list-tests.el ends here
