;;; codex-ide-debug-info-tests.el --- Tests for codex-ide-debug-info -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for Codex IDE debug info summaries.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide-debug-info)

(ert-deftest codex-ide-show-debug-info-reports-live-project-state ()
  (let ((project-a (codex-ide-test--make-temp-project))
        (project-b (codex-ide-test--make-temp-project))
        (message-text nil))
    (codex-ide-test-with-fixture project-a
				 (codex-ide-test-with-fake-processes
				  (let ((session-a-1 (codex-ide--create-process-session))
					(session-a-2 (codex-ide--create-process-session))
					session-b
					active-buffer-a
					active-buffer-b)
				    (let ((default-directory (file-name-as-directory project-b)))
				      (setq session-b (codex-ide--create-process-session)))
				    (setq active-buffer-a (find-file-noselect
							   (codex-ide-test--make-project-file project-a "a.txt" "a"))
					  active-buffer-b (find-file-noselect
							   (codex-ide-test--make-project-file project-b "b.txt" "b")))
				    (puthash (codex-ide--normalize-directory project-a)
					     active-buffer-a
					     codex-ide--active-buffer-objects)
				    (puthash (codex-ide--normalize-directory project-b)
					     active-buffer-b
					     codex-ide--active-buffer-objects)
				    (puthash (codex-ide--normalize-directory project-a)
					     `((file . ,(expand-file-name "a.txt" project-a))
					       (display-file . "a.txt")
					       (line . 7)
					       (column . 2)
					       (project-dir . ,(codex-ide--normalize-directory project-a))
					       (custom-field . custom-value))
					     codex-ide--active-buffer-contexts)
				    (puthash (codex-ide--normalize-directory project-b)
					     `((buffer-name . ,(buffer-name active-buffer-b)))
					     codex-ide--active-buffer-contexts)
				    (setf (codex-ide-session-created-at session-a-1) 1.0
					  (codex-ide-session-created-at session-a-2) 2.0
					  (codex-ide-session-created-at session-b) 3.0
					  (codex-ide-session-last-prompt-submitted-at session-a-2) 4.0)
				    (cl-letf (((symbol-function 'message)
					       (lambda (format-string &rest args)
						 (setq message-text (apply #'format format-string args)))))
				      (codex-ide-show-debug-info))
				    (let ((project-a-block
					   (format
					    (concat
					     "Project: %s\n"
					     "  - 2 active sessions\n"
					     "  - Active session: %S\n"
					     "  - Active buffer: %S\n"
					     "  - Active buffer context:\n"
					     "    - file: %s\n"
					     "    - display-file: %s\n"
					     "    - line: %s\n"
					     "    - column: %s\n"
					     "    - project-dir: %s\n"
					     "    - custom-field: %s")
					    (codex-ide--project-name project-a)
					    (buffer-name (codex-ide-session-buffer session-a-2))
					    (buffer-name active-buffer-a)
					    (expand-file-name "a.txt" project-a)
					    "a.txt"
					    "7"
					    "2"
					    (codex-ide--normalize-directory project-a)
					    "custom-value"))
					  (project-b-block
					   (format
					    (concat
					     "Project: %s\n"
					     "  - 1 active session\n"
					     "  - Active session: %S\n"
					     "  - Active buffer: %S\n"
					     "  - Active buffer context:\n"
					     "    - buffer-name: %s")
					    (codex-ide--project-name project-b)
					    (buffer-name (codex-ide-session-buffer session-b))
					    (buffer-name active-buffer-b)
					    (buffer-name active-buffer-b))))
				      (should (equal
					       message-text
					       (concat
						"Codex IDE Debug Info\n\n"
						(string-join
						 (sort (list project-a-block project-b-block) #'string-lessp)
						 "\n\n"))))))))))

(ert-deftest codex-ide-show-debug-info-renders-nil-active-buffer-context ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (message-text nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'message)
					       (lambda (format-string &rest args)
						 (setq message-text (apply #'format format-string args)))))
				      (codex-ide-show-debug-info))
				    (should (equal
					     message-text
					     (format
					      (concat
					       "Codex IDE Debug Info\n\n"
					       "Project: %s\n"
					       "  - 1 active session\n"
					       "  - Active session: %S\n"
					       "  - Active buffer: %S\n"
					       "  - Active buffer context:\n"
					       "    - nil")
					      (codex-ide--project-name project-dir)
					      (buffer-name (codex-ide-session-buffer session))
					      nil))))))))

(ert-deftest codex-ide-show-debug-info-reports-when-no-live-sessions-exist ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (message-text nil))
    (codex-ide-test-with-fixture project-dir
				 (cl-letf (((symbol-function 'message)
					    (lambda (format-string &rest args)
					      (setq message-text (apply #'format format-string args)))))
				   (codex-ide-show-debug-info))
				 (should (equal message-text "Codex IDE Debug Info\n\nNo active sessions")))))

(provide 'codex-ide-debug-info-tests)

;;; codex-ide-debug-info-tests.el ends here
