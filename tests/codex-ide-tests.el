;;; codex-ide-tests.el --- Tests for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Core codex-ide tests plus the suite entrypoint for split test modules.

;;; Code:

(require 'ert)
(require 'json)
(require 'package)
(require 'project)
(require 'seq)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-context-tests)
(require 'codex-ide-delete-session-thread-tests)
(require 'codex-ide-rollout-tests)
(require 'codex-ide-session-buffer-list-tests)

(defun codex-ide-test--prompt-prefix-at-line ()
  "Return the visible prompt prefix string at the current line."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (beginning-of-line))
    (buffer-substring-no-properties
     (point)
     (min (+ (point) 2) (line-end-position)))))

(defun codex-ide-test--line-has-prompt-start ()
  "Return non-nil when the current line is marked as a prompt line."
  (save-excursion
    (beginning-of-line)
    (codex-ide--line-has-prompt-start-p)))

(defun codex-ide-test--prompt-line-count ()
  "Return the number of prompt lines in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (< (point) (point-max))
        (when (codex-ide--line-has-prompt-start-p)
          (setq count (1+ count)))
        (forward-line 1))
      count)))

(defun codex-ide-test--input-placeholder-text (session)
  "Return SESSION's visible input placeholder text, or nil."
  (when-let* ((overlay (codex-ide--session-metadata-get
                        session
                        :input-placeholder-overlay))
              (_ (overlay-buffer overlay))
              (text (overlay-get overlay 'after-string)))
    (substring-no-properties text)))

(defun codex-ide-test--input-placeholder-overlay-live-p (session)
  "Return non-nil when SESSION's placeholder overlay is attached."
  (when-let ((overlay (codex-ide--session-metadata-get
                       session
                       :input-placeholder-overlay)))
    (and (overlayp overlay)
         (overlay-buffer overlay)
         (overlay-start overlay)
         (overlay-end overlay))))

(ert-deftest codex-ide-app-server-command-includes-bridge-and-extra-flags ()
  (let ((codex-ide-cli-path "/tmp/codex")
        (codex-ide-cli-extra-flags "--model test-model --debug")
        (bridge-args '("-c" "mcp_servers.emacs.command=\"python3\"")))
    (cl-letf (((symbol-function 'codex-ide-mcp-bridge-mcp-config-args)
               (lambda () bridge-args)))
      (should
       (equal (codex-ide--app-server-command)
              '("/tmp/codex"
                "app-server"
                "--listen"
                "stdio://"
                "-c"
                "mcp_servers.emacs.command=\"python3\""
                "--model"
                "test-model"
                "--debug"))))))

(ert-deftest codex-ide-app-server-process-environment-adds-color-defaults ()
  (let ((env (codex-ide--app-server-process-environment
              '("TERM=dumb"
                "PATH=/bin"))))
    (should (equal (codex-ide--environment-variable-value "TERM" env)
                   "xterm-256color"))
    (should (equal (codex-ide--environment-variable-value "COLORTERM" env)
                   "truecolor"))
    (should (equal (codex-ide--environment-variable-value "CLICOLOR" env)
                   "1"))))

(ert-deftest codex-ide-app-server-process-environment-respects-no-color ()
  (let ((env (codex-ide--app-server-process-environment
              '("NO_COLOR=1"
                "TERM=dumb"
                "PATH=/bin"))))
    (should (equal (codex-ide--environment-variable-value "TERM" env)
                   "dumb"))
    (should-not (codex-ide--environment-variable-value "COLORTERM" env))
    (should-not (codex-ide--environment-variable-value "CLICOLOR" env))))

(ert-deftest codex-ide-toggle-logging-enabled-flips-state ()
  (let ((codex-ide-logging-enabled nil))
    (codex-ide-toggle-logging-enabled)
    (should codex-ide-logging-enabled)
    (codex-ide-toggle-logging-enabled)
    (should-not codex-ide-logging-enabled)))

(ert-deftest codex-ide-create-process-session-builds-buffers-and-registers-session ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (should (string= (codex-ide-session-directory session)
						     (directory-file-name (file-truename project-dir))))
				    (should (codex-ide-test-process-p (codex-ide-session-process session)))
				    (should (memq session codex-ide--sessions))
				    (should (eq session
						(codex-ide--last-active-session-for-directory project-dir)))
				    (should (numberp (codex-ide-session-created-at session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (derived-mode-p 'codex-ide-session-mode))
				      (should visual-line-mode)
				      (should (string-match-p "Codex session for" (buffer-string))))
				    (with-current-buffer (codex-ide-test--log-buffer session)
				      (should (derived-mode-p 'codex-ide-log-mode))
				      (should (equal (buffer-name)
						     (format "*%s[%s]-log*"
							     codex-ide-buffer-name-prefix
							     (file-name-nondirectory
							      (directory-file-name project-dir)))))
				      (should (string-match-p "Codex log for" (buffer-string))))
				    (should
				     (equal (plist-get (codex-ide-test-process-plist
							(codex-ide-session-process session))
						       'codex-session)
					    session)))))))

(ert-deftest codex-ide-create-query-session-registers-headless-session ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-query-session)))
				    (should (string= (codex-ide-session-directory session)
						     (directory-file-name (file-truename project-dir))))
				    (should (codex-ide-session-query-only session))
				    (should (codex-ide-test-process-p (codex-ide-session-process session)))
				    (should (memq session codex-ide--sessions))
				    (should-not (codex-ide-session-buffer session))
				    (should (buffer-live-p (codex-ide-test--log-buffer session)))
				    (should-not (eq session
						    (codex-ide--last-active-session-for-directory project-dir)))
				    (with-current-buffer (codex-ide-test--log-buffer session)
				      (should (derived-mode-p 'codex-ide-log-mode))
				      (should (equal (buffer-name)
						     (format "*%s-log[%s]-query*"
							     codex-ide-buffer-name-prefix
							     (file-name-nondirectory
							      (directory-file-name project-dir)))))))))))

(ert-deftest codex-ide-create-process-session-skips-log-buffer-when-logging-disabled ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((codex-ide-logging-enabled nil))
				    (let ((session (codex-ide--create-process-session)))
				      (should (string= (codex-ide-session-directory session)
						       (directory-file-name (file-truename project-dir))))
				      (should-not (get-buffer (codex-ide-test--log-buffer-name session)))))))))

(ert-deftest codex-ide-create-process-session-emits-created-event ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (events nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((codex-ide-session-event-hook
					 (list (lambda (event session payload)
						 (push (list event session payload) events)))))
				    (let ((session (codex-ide--create-process-session)))
				      (should (= (length events) 1))
				      (pcase-let ((`(,event ,emitted-session ,payload) (car events)))
					(should (eq event 'created))
					(should (eq emitted-session session))
					(should (equal (plist-get payload :directory)
						       (codex-ide-session-directory session)))
					(should (eq (plist-get payload :buffer)
						    (codex-ide-session-buffer session)))
					(should (equal (plist-get payload :status) "starting")))))))))

(ert-deftest codex-ide-set-session-status-emits-only-on-change ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (events nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((codex-ide-session-event-hook
					 (list (lambda (event session payload)
						 (push (list event session payload) events)))))
				    (let ((session (codex-ide--create-process-session)))
				      (setq events nil)
				      (codex-ide--set-session-status session "idle" 'test-transition)
				      (codex-ide--set-session-status session "idle" 'test-transition)
				      (should (= (length events) 1))
				      (pcase-let ((`(,event ,emitted-session ,payload) (car events)))
					(should (eq event 'status-changed))
					(should (eq emitted-session session))
					(should (equal (plist-get payload :old-status) "starting"))
					(should (equal (plist-get payload :status) "idle"))
					(should (eq (plist-get payload :reason) 'test-transition)))))))))

(ert-deftest codex-ide-create-process-session-errors-gracefully-when-executable-is-missing ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (cl-letf (((symbol-function 'make-pipe-process)
					    (lambda (&rest plist)
					      (codex-ide-test-process-create
					       :live t
					       :plist (list :make-pipe-process-spec plist)
					       :sent-strings nil)))
					   ((symbol-function 'make-process)
					    (lambda (&rest _)
					      (signal 'file-missing
						      '("Searching for program"
							"exec: codex: executable file not found in $PATH"))))
					   ((symbol-function 'process-live-p)
					    (lambda (process)
					      (and (codex-ide-test-process-p process)
						   (codex-ide-test-process-live process))))
					   ((symbol-function 'delete-process)
					    (lambda (process)
					      (setf (codex-ide-test-process-live process) nil)
					      nil)))
				   (should-error
				    (codex-ide--create-process-session)
				    :type 'user-error)))))

(ert-deftest codex-ide-ensure-session-for-current-project-prompts-to-start ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (prompt nil)
        (started nil)
        (session nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (cl-letf (((symbol-function 'y-or-n-p)
					     (lambda (message)
					       (setq prompt message)
					       t))
					    ((symbol-function 'codex-ide--start-session)
					     (lambda (kind)
					       (setq started kind
						     session (codex-ide--create-process-session))
					       session)))
				    (should (eq (codex-ide--ensure-session-for-current-project) session))
				    (should (eq started 'new))
				    (should (equal prompt "No Codex session for this workspace. Start one? ")))))))

(ert-deftest codex-ide-switch-to-buffer-displays-ensured-session ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (shown nil))
    (codex-ide-test-with-fixture project-dir
				 (delete-other-windows)
				 (let ((origin-window (selected-window))
				       (origin-buffer (current-buffer)))
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (session-window (split-window-right)))
				      (set-window-buffer session-window (codex-ide-session-buffer session))
				      (select-window origin-window)
				      (set-window-buffer origin-window origin-buffer)
				      (cl-letf (((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda ()
						   session)))
					(should (eq (codex-ide-switch-to-buffer) session))
					(setq shown (window-buffer (selected-window)))
					(should (eq (selected-window) session-window))
					(should (eq shown (codex-ide-session-buffer session)))
					(should (eq (window-buffer session-window)
						    (codex-ide-session-buffer session))))))))))

(ert-deftest codex-ide-switch-to-buffer-shows-session-in-selected-window-by-default ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (save-window-excursion
				   (delete-other-windows)
				   (let ((origin-window (selected-window))
					 (origin-buffer (current-buffer)))
				     (codex-ide-test-with-fake-processes
				      (let* ((session (codex-ide--create-process-session))
					     (session-buffer (codex-ide-session-buffer session)))
					(set-window-buffer origin-window origin-buffer)
					(cl-letf (((symbol-function 'codex-ide--ensure-session-for-current-project)
						   (lambda ()
						     session)))
					  (should (eq (codex-ide-switch-to-buffer) session))
					  (should (= (length (window-list nil 'no-minibuf)) 1))
					  (should (eq (selected-window) origin-window))
					  (should (eq (window-buffer (selected-window))
						      session-buffer))))))))))

(ert-deftest codex-ide-display-buffer-calls-display-buffer ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (remembered nil)
        (captured-buffer nil)
        (captured-action nil))
    (codex-ide-test-with-fixture project-dir
				 (let ((target-buffer (get-buffer-create " *codex-display-delegate*"))
				       (returned-window (selected-window)))
				   (cl-letf (((symbol-function 'codex-ide--remember-buffer-context-before-switch)
					      (lambda (&optional buffer)
						(setq remembered (or buffer (current-buffer)))))
					     ((symbol-function 'display-buffer)
					      (lambda (buffer action)
						(setq captured-buffer buffer
						      captured-action action)
						returned-window)))
				     (let ((origin-buffer (current-buffer))
					   (codex-ide-select-window-on-open nil))
				       (should (eq (codex-ide-display-buffer target-buffer) returned-window))
				       (should (eq remembered origin-buffer))
				       (should (eq captured-buffer target-buffer))
				       (should-not captured-action)))))))

(ert-deftest codex-ide-display-buffer-selects-returned-window-by-default ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (save-window-excursion
				   (delete-other-windows)
				   (let ((target-buffer (get-buffer-create " *codex-display-focus*")))
				     (let ((origin-window (selected-window))
					   (target-window (split-window-right)))
				       (set-window-buffer target-window target-buffer)
				       (select-window origin-window)
				       (cl-letf (((symbol-function 'display-buffer)
						  (lambda (_buffer _action)
						    target-window)))
					 (should (eq (codex-ide-display-buffer target-buffer) target-window))
					 (should (eq (selected-window) target-window))
					 (should (eq (window-buffer target-window) target-buffer)))))))))

(ert-deftest codex-ide-display-buffer-respects-display-buffer-alist ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (save-window-excursion
				   (delete-other-windows)
				   (let ((target-buffer (get-buffer-create " *codex-display-alist*"))
					 (origin-window (selected-window))
					 (display-buffer-alist
					  '(("\\` \\*codex-display-alist\\*\\'"
					     (display-buffer-pop-up-window)))))
				     (let ((window (codex-ide-display-buffer
						    target-buffer
						    codex-ide-display-buffer-pop-up-action)))
				       (should (window-live-p window))
				       (should-not (eq window origin-window))
				       (should (eq (window-buffer window) target-buffer))
				       (should (= (length (window-list nil 'no-minibuf)) 2))))))))

(ert-deftest codex-ide-display-buffer-pop-up-action-uses-action-function-list ()
  (should (equal codex-ide-display-buffer-pop-up-action
                 '((display-buffer-reuse-window
                    display-buffer-same-window)))))

(ert-deftest codex-ide-display-new-session-buffer-uses-vertical-split ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (save-window-excursion
				   (delete-other-windows)
				   (let ((target-buffer (get-buffer-create " *codex-new-session-vertical*"))
					 (origin-window (selected-window))
					 (codex-ide-new-session-split 'vertical)
					 (codex-ide-select-window-on-open nil))
				     (let* ((origin-left (nth 0 (window-edges origin-window)))
					    (window (codex-ide--display-new-session-buffer target-buffer))
					    (window-left (nth 0 (window-edges window))))
				       (should (window-live-p window))
				       (should-not (eq window origin-window))
				       (should (> window-left origin-left))
				       (should (eq (window-buffer window) target-buffer))
				       (should (eq (selected-window) origin-window))))))))

(ert-deftest codex-ide-display-new-session-buffer-uses-horizontal-split ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (save-window-excursion
				   (delete-other-windows)
				   (let ((target-buffer (get-buffer-create " *codex-new-session-horizontal*"))
					 (origin-window (selected-window))
					 (codex-ide-new-session-split 'horizontal)
					 (codex-ide-select-window-on-open nil))
				     (let* ((origin-top (nth 1 (window-edges origin-window)))
					    (window (codex-ide--display-new-session-buffer target-buffer))
					    (window-top (nth 1 (window-edges window))))
				       (should (window-live-p window))
				       (should-not (eq window origin-window))
				       (should (> window-top origin-top))
				       (should (eq (window-buffer window) target-buffer))
				       (should (eq (selected-window) origin-window))))))))

(ert-deftest codex-ide-last-active-session-for-directory-uses-activity-timestamps ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((first (codex-ide--create-process-session))
					second)
				    (setq second (codex-ide--create-process-session))
				    (setf (codex-ide-session-created-at first) 1.0
					  (codex-ide-session-created-at second) 2.0)
				    (should (equal (codex-ide--sessions-for-directory project-dir t)
						   (list second first)))
				    (should (eq (codex-ide--last-active-session-for-directory project-dir)
						second))
				    (setf (codex-ide-session-last-thread-attached-at first) 3.0)
				    (should (eq (codex-ide--last-active-session-for-directory project-dir)
						first))
				    (setf (codex-ide-session-last-prompt-submitted-at second) 4.0)
				    (should (eq (codex-ide--last-active-session-for-directory project-dir)
						second))
				    (delete-process (codex-ide-session-process second))
				    (codex-ide--cleanup-dead-sessions)
				    (should (equal (codex-ide--sessions-for-directory project-dir t)
						   (list first)))
				    (should (eq (codex-ide--last-active-session-for-directory project-dir)
						first)))))))

(ert-deftest codex-ide-last-active-buffer-helpers-use-live-session-activity ()
  (let ((project-a (codex-ide-test--make-temp-project))
        (project-b (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-a
				 (codex-ide-test-with-fake-processes
				  (let ((session-a (codex-ide--create-process-session))
					session-b)
				    (let ((default-directory (file-name-as-directory project-b)))
				      (setq session-b (codex-ide--create-process-session)))
				    (setf (codex-ide-session-created-at session-a) 1.0
					  (codex-ide-session-created-at session-b) 2.0
					  (codex-ide-session-last-prompt-submitted-at session-a) 3.0)
				    (should (eq (codex-ide--get-last-active-buffer-for-project project-a)
						(codex-ide-session-buffer session-a)))
				    (should (eq (codex-ide--get-last-active-buffer-for-project project-b)
						(codex-ide-session-buffer session-b)))
				    (should (eq (codex-ide--get-last-active-buffer-all-projects)
						(codex-ide-session-buffer session-a)))
				    (kill-buffer (codex-ide-session-buffer session-a))
				    (should-not
				     (codex-ide--get-last-active-buffer-for-project project-a)))))))

(ert-deftest codex-ide-create-process-session-adds-suffixes-for-additional-workspace-sessions ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((first (codex-ide--create-process-session))
					(second (codex-ide--create-process-session))
					(third (codex-ide--create-process-session)))
				    (should (equal (buffer-name (codex-ide-session-buffer first))
						   (format "*%s[%s]*"
							   codex-ide-buffer-name-prefix
							   (file-name-nondirectory
							    (directory-file-name project-dir)))))
				    (should (equal (buffer-name (codex-ide-session-buffer second))
						   (format "*%s[%s]<1>*"
							   codex-ide-buffer-name-prefix
							   (file-name-nondirectory
							    (directory-file-name project-dir)))))
				    (should (equal (buffer-name (codex-ide-session-buffer third))
						   (format "*%s[%s]<2>*"
							   codex-ide-buffer-name-prefix
							   (file-name-nondirectory
							    (directory-file-name project-dir)))))
				    (should (equal (codex-ide-test--log-buffer-name second)
						   (format "*%s[%s]<1>-log*"
							   codex-ide-buffer-name-prefix
							   (file-name-nondirectory
							    (directory-file-name project-dir)))))
				    (should (equal (mapcar #'codex-ide-session-name-suffix
							   (reverse codex-ide--sessions))
						   '(nil 1 2))))))))

(ert-deftest codex-ide-session-mode-enables-visual-line-mode-by-default ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (should visual-line-mode)))

(ert-deftest codex-ide-session-mode-allows-opting-out-of-visual-line-mode ()
  (let ((codex-ide-session-enable-visual-line-mode nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (should-not visual-line-mode))))

(ert-deftest codex-ide-session-mode-disables-font-lock-jit-lock ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (should-not font-lock-mode)
    (should-not jit-lock-functions)
    (font-lock-mode 1)
    (should-not font-lock-mode)
    (should-not jit-lock-functions)))

(ert-deftest codex-ide-log-mode-disables-undo ()
  (with-temp-buffer
    (buffer-enable-undo)
    (codex-ide-log-mode)
    (should (eq buffer-undo-list t))))

(ert-deftest codex-ide-append-to-buffer-does-not-record-undo ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (codex-ide--append-to-buffer (current-buffer) (make-string 4096 ?x))
    (should (= (buffer-size) 4096))
    (should-not buffer-undo-list)))

(ert-deftest codex-ide-renderer-append-to-buffer-inserts-at-position-and-restores-point ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (insert "alpha omega")
    (let ((restore-point (copy-marker (point-max))))
      (setq buffer-undo-list nil)
      (codex-ide-renderer-append-to-buffer
       " beta"
       :insertion-point 6
       :restore-point restore-point)
      (should (equal (buffer-string) "alpha beta omega"))
      (should (= (point) (point-max)))
      (should-not (marker-buffer restore-point))
      (should-not buffer-undo-list))))

(ert-deftest codex-ide-renderer-append-to-buffer-runs-after-insert-hook ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (insert "hello")
    (let (called)
      (codex-ide-renderer-append-to-buffer
       " world"
       :insertion-point (point-max)
       :after-insert
       (lambda (start end insertion-point)
         (setq called (list start end insertion-point
                            (buffer-substring-no-properties start end)))))
      (should (equal (buffer-string) "hello world"))
      (should (equal called '(6 12 6 " world"))))))

(ert-deftest codex-ide-render-markdown-region-does-not-record-undo ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (buffer-enable-undo)
    (insert "See [`foo.el`](/tmp/foo.el#L3C2) and `code`.\n")
    (setq buffer-undo-list nil)
    (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
    (should-not buffer-undo-list)))

(ert-deftest codex-ide-discard-buffer-undo-history-clears-undo-tree-history ()
  (with-temp-buffer
    (buffer-enable-undo)
    (setq buffer-undo-list '((1 . 2)))
    (let ((called nil))
      (cl-letf (((symbol-function 'undo-tree-clear-history)
                 (lambda () (setq called t))))
        (codex-ide--discard-buffer-undo-history))
      (should-not buffer-undo-list)
      (should called))))

(ert-deftest codex-ide-session-prompt-keeps-typed-undo ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (should-not buffer-undo-list)
      (goto-char (codex-ide--input-end-position session))
      (insert "hello")
      (undo-boundary)
      (should buffer-undo-list)
      (primitive-undo 1 (cdr buffer-undo-list))
      (should (equal (buffer-string) "\n> \n\n"))
      (should (equal (codex-ide-test--prompt-prefix-at-line) "> ")))))

(ert-deftest codex-ide-input-prompt-shows-idle-placeholder-when-empty-and-unfocused ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (goto-char (point-min))
      (codex-ide--refresh-input-placeholder session)
      (should (codex-ide-test--input-placeholder-overlay-live-p session))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Tell Codex what to do..."))
      (should (get-text-property
               0
               'cursor
               (overlay-get
                (codex-ide--session-metadata-get
                 session
                 :input-placeholder-overlay)
                'after-string)))
      (should (equal (buffer-string) "\n> \n\n"))
      (should (equal (codex-ide--current-input session) "")))))

(ert-deftest codex-ide-input-prompt-keeps-placeholder-on-focus-and-hides-on-typing ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (switch-to-buffer (current-buffer))
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (goto-char (point-min))
      (codex-ide--refresh-input-placeholder session)
      (should (codex-ide-test--input-placeholder-overlay-live-p session))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Tell Codex what to do..."))
      (goto-char (marker-position (codex-ide-session-input-start-marker session)))
      (codex-ide--refresh-input-placeholder session)
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Tell Codex what to do..."))
      (insert "h")
      (goto-char (point-min))
      (codex-ide--refresh-input-placeholder session)
      (should-not (codex-ide-test--input-placeholder-text session))
      (should (equal (buffer-string) "\n> h\n\n"))
      (should (equal (codex-ide--current-input session) "h")))))

(ert-deftest codex-ide-running-input-prompt-shows-steering-placeholder ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "running"
                    :current-turn-id "turn-1")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (goto-char (point-min))
      (codex-ide--refresh-input-placeholder session)
      (should (codex-ide-test--input-placeholder-overlay-live-p session))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Running..."))
      (should (string-suffix-p "\n> \n\n" (buffer-string)))
      (should (equal (codex-ide--current-input session) "")))))

(ert-deftest codex-ide-thread-status-active-shows-working-placeholder ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (codex-ide--handle-notification
       session
       '((method . "thread/status/changed")
         (params . ((status . ((type . "active")))))))
      (should (equal (codex-ide-session-status session) "running"))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Working...")))))

(ert-deftest codex-ide-busy-input-prompt-uses-status-placeholder ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "approval"
                    :current-turn-id "turn-1")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (codex-ide--refresh-input-placeholder session)
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Seeking approval...")))))

(ert-deftest codex-ide-busy-input-prompt-status-placeholder-can-be-customized ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((codex-ide-status-placeholder-text-alist
           '(("running" . "Still running...")))
          (session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "running"
                    :current-turn-id "turn-1")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (codex-ide--refresh-input-placeholder session)
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Still running...")))))

(ert-deftest codex-ide-finish-turn-refreshes-existing-prompt-placeholder ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "running"
                    :current-turn-id "turn-1"
                    :output-prefix-inserted t
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (codex-ide--refresh-input-placeholder session)
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Running..."))
      (codex-ide--finish-turn session)
      (should-not (codex-ide-session-current-turn-id session))
      (should-not (codex-ide-session-output-prefix-inserted session))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Tell Codex what to do...")))))

(ert-deftest codex-ide-insert-input-prompt-clears-stale-undo-history ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (insert "stale undo entry")
    (undo-boundary)
    (should buffer-undo-list)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (should-not buffer-undo-list))))

(ert-deftest codex-ide-begin-turn-display-clears-submitted-prompt-undo ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (insert "submitted prompt")
      (undo-boundary)
      (should buffer-undo-list)
      (codex-ide--begin-turn-display session)
      (should-not buffer-undo-list)
      (should (string-match-p "submitted prompt\n\n\n\n> "
                              (buffer-string)))
      (should-not (string-match-p "Working\\.\\.\\." (buffer-string)))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Working..."))
      (should (codex-ide--input-prompt-active-p session)))))

(ert-deftest codex-ide-running-input-stays-below-streamed-items ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-current-input session "steer me")
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . "echo hi")
         (cwd . "/tmp")))
      (should (string-match-p
               (rx "submitted prompt"
                   (* anything)
                   "* Ran command")
               (buffer-string)))
      (should (equal (codex-ide--current-input session) "steer me"))
      (goto-char (marker-position
                  (codex-ide-session-input-prompt-start-marker session)))
      (should (equal (codex-ide-test--prompt-prefix-at-line) "> ")))))

(ert-deftest codex-ide-running-input-stays-below-streamed-agent-deltas ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-current-input session "steer draft")
      (codex-ide--set-queued-prompts session '("queued prompt"))
      (codex-ide--refresh-running-input-display session)
      (codex-ide--handle-notification
       session
       '((method . "item/agentMessage/delta")
         (params . ((itemId . "msg-1")
                    (delta . "Assistant update.")))))
      (let ((text (buffer-string)))
        (should (string-match-p
                 (rx "Assistant update."
                     (* anything)
                     "Queued turns:"
                     "\n  1. queued prompt"
                     "\n\n\n> steer draft\n\n" string-end)
                 text))
        (should-not (string-match-p
                     (rx "Assistant update." (* anything) "> steer draft"
                         (* anything) "Queued turns:")
                     text))))))

(ert-deftest codex-ide-running-output-spacing-preserves-input-point ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-current-input session "steer draft")
      (goto-char (point-max))
      (codex-ide--ensure-output-spacing (current-buffer))
      (should (= (point) (codex-ide--input-end-position session)))
      (should (equal (codex-ide--current-input session) "steer draft")))))

(ert-deftest codex-ide-streaming-append-keeps-following-window-point-at-input-end ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-streaming-input-point*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (erase-buffer)
            (codex-ide-session-mode)
            (let ((session (make-codex-ide-session
                            :buffer buffer
                            :status "idle")))
              (setq-local codex-ide--session session)
              (codex-ide--insert-input-prompt session "steer draft")
              (goto-char (point-max))
              (set-window-point (selected-window) (point-max))
              (codex-ide--append-to-buffer buffer "streamed output\n")
              (should (= (window-point)
                         (codex-ide--input-end-position session)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-streaming-append-preserves-scrolled-transcript-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-transcript-window*")))
      (unwind-protect
          (let* ((top-window (selected-window))
                 (bottom-window (split-window-below)))
            (ignore-errors
              (window-resize top-window
                             (- 8 (window-total-height top-window))))
            (with-current-buffer buffer
              (erase-buffer)
              (dotimes (line 400)
                (insert (format "line %02d\n" line))))
            (set-window-buffer top-window buffer)
            (set-window-buffer bottom-window buffer)
            (set-window-start top-window (point-min) t)
            (set-window-point top-window (point-min))
            (with-selected-window bottom-window
              (goto-char (point-max))
              (recenter -1))
            (redisplay t)
            (cl-letf (((symbol-function 'codex-ide--transcript-window-follows-anchor-p)
                       (lambda (window _anchor)
                         (eq window bottom-window))))
              (let ((top-start (window-start top-window))
                    (top-point (window-point top-window)))
                (with-current-buffer buffer
                  (codex-ide--append-to-buffer buffer "streamed tail\n"))
                (should (= (window-start top-window) top-start))
                (should (= (window-point top-window) top-point))
                (should (>= (window-end bottom-window t)
                            (with-current-buffer buffer
                              (point-max)))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-streaming-follow-tail-when-buffer-end-is-visible ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-transcript-end-visible*")))
      (unwind-protect
          (let ((window (selected-window)))
            (with-current-buffer buffer
              (erase-buffer)
              (dotimes (line 80)
                (insert (format "line %02d\n" line)))
              (insert "> live prompt\n"))
            (set-window-buffer window buffer)
            (with-selected-window window
              (goto-char (point-max))
              (recenter -1))
            (let ((anchor (with-current-buffer buffer
                            (save-excursion
                              (goto-char (point-max))
                              (forward-line -1)
                              (point)))))
              (should (< anchor
                         (with-current-buffer buffer (point-max))))
              (should (>= (window-end window t)
                          (with-current-buffer buffer (point-max))))
              (should (> (window-point window)
                         (window-start window)))
              (with-current-buffer buffer
                (should (codex-ide--transcript-window-follows-anchor-p
                         window
                         anchor)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-session-mode-suspends-tail-follow-after-navigation ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-tail-follow-nav*")))
      (unwind-protect
          (let ((window (selected-window)))
            (with-current-buffer buffer
              (erase-buffer)
              (insert "alpha\nbeta\n")
              (codex-ide-session-mode))
            (set-window-buffer window buffer)
            (with-selected-window window
              (goto-char (point-max))
              (setq-local codex-ide-session-mode--last-point (point))
              (setq-local codex-ide-session-mode--last-window-start (window-start))
              (forward-line -1)
              (codex-ide-session-mode--track-tail-follow-navigation))
            (should (window-parameter window
                                      'codex-ide-tail-follow-suspended)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-session-mode-clears-tail-follow-suspension-after-rejoining-tail ()
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (insert "alpha\nbeta\n")
      (codex-ide-session-mode)
      (switch-to-buffer (current-buffer))
      (set-window-parameter (selected-window) 'codex-ide-tail-follow-suspended t)
      (setq-local codex-ide-session-mode--last-point (point-min))
      (setq-local codex-ide-session-mode--last-window-start (window-start))
      (goto-char (point-max))
      (codex-ide-session-mode--track-tail-follow-navigation)
      (should-not (window-parameter (selected-window)
                                    'codex-ide-tail-follow-suspended)))))

(ert-deftest codex-ide-transcript-window-follow-respects-tail-follow-suspension ()
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (insert "alpha\nbeta\ngamma\n")
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (recenter -1)
      (let ((window (selected-window))
            (anchor (point-max)))
        (set-window-parameter window 'codex-ide-tail-follow-suspended t)
        (should-not (codex-ide--transcript-window-follows-anchor-p
                     window
                     anchor))
        (set-window-parameter window 'codex-ide-tail-follow-suspended nil)
        (should (codex-ide--transcript-window-follows-anchor-p
                 window
                 anchor))))))

(ert-deftest codex-ide-streaming-append-advances-window-that-was-following-tail ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-transcript-follow-tail*")))
      (unwind-protect
          (let ((window (selected-window)))
            (with-current-buffer buffer
              (erase-buffer)
              (dotimes (line 80)
                (insert (format "line %02d\n" line))))
            (set-window-buffer window buffer)
            (with-selected-window window
              (goto-char (point-max))
              (recenter -1))
            (dotimes (n 3)
              (with-current-buffer buffer
                (codex-ide--append-to-buffer buffer (format "delta %d\n" n)))
              (should (>= (window-end window t)
                          (with-current-buffer buffer
                            (point-max))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-streaming-append-does-not-advance-window-after-navigation-away-from-tail ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-transcript-suspended-tail*")))
      (unwind-protect
          (let ((window (selected-window)))
            (with-current-buffer buffer
              (erase-buffer)
              (codex-ide-session-mode)
              (dotimes (line 80)
                (insert (format "line %02d\n" line))))
            (set-window-buffer window buffer)
            (with-selected-window window
              (goto-char (point-max))
              (recenter -1)
              (setq-local codex-ide-session-mode--last-point (point))
              (setq-local codex-ide-session-mode--last-window-start (window-start))
              (goto-char (point-max))
              (forward-line -2)
              (codex-ide-session-mode--track-tail-follow-navigation))
            (let ((window-start-before (window-start window))
                  (window-point-before (window-point window)))
              (with-current-buffer buffer
                (codex-ide--append-to-buffer buffer "delta\n"))
              (should (= (window-start window) window-start-before))
              (should (= (window-point window) window-point-before))
              (should (window-parameter window 'codex-ide-tail-follow-suspended))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-streaming-append-resumes-after-rejoining-tail ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-transcript-rejoin-tail*")))
      (unwind-protect
          (let ((window (selected-window)))
            (with-current-buffer buffer
              (erase-buffer)
              (codex-ide-session-mode)
              (dotimes (line 80)
                (insert (format "line %02d\n" line))))
            (set-window-buffer window buffer)
            (with-selected-window window
              (goto-char (point-max))
              (recenter -1)
              (set-window-parameter window 'codex-ide-tail-follow-suspended t)
              (setq-local codex-ide-session-mode--last-point (point-min))
              (setq-local codex-ide-session-mode--last-window-start (window-start))
              (goto-char (point-max))
              (codex-ide-session-mode--track-tail-follow-navigation))
            (should-not (window-parameter window 'codex-ide-tail-follow-suspended))
            (with-current-buffer buffer
              (codex-ide--append-to-buffer buffer "delta\n"))
            (should (>= (window-end window t)
                        (with-current-buffer buffer
                          (point-max)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-streaming-markdown-render-preserves-scrolled-transcript-window ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-markdown-window*")))
      (unwind-protect
          (let* ((top-window (selected-window))
                 (bottom-window (split-window-below)))
            (ignore-errors
              (window-resize top-window
                             (- 8 (window-total-height top-window))))
            (with-current-buffer buffer
              (erase-buffer)
              (dotimes (line 400)
                (insert (format "context %02d\n" line)))
              (insert "\nUse `code` here.\n"))
            (set-window-buffer top-window buffer)
            (set-window-buffer bottom-window buffer)
            (set-window-start top-window (point-min) t)
            (set-window-point top-window (point-min))
            (with-selected-window bottom-window
              (goto-char (point-max))
              (recenter -1))
            (redisplay t)
            (cl-letf (((symbol-function 'codex-ide--transcript-window-follows-anchor-p)
                       (lambda (window _anchor)
                         (eq window bottom-window))))
              (let ((top-start (window-start top-window))
                    (top-point (window-point top-window)))
                (with-current-buffer buffer
                  (codex-ide--maybe-render-markdown-region (point-min) (point-max)))
                (should (= (window-start top-window) top-start))
                (should (= (window-point top-window) top-point))
                (with-selected-window bottom-window
                  (goto-char (point-max))
                  (search-backward "code")
                  (should (get-text-property (point) 'codex-ide-markdown))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-finish-turn-separates-active-prompt-from-output ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-current-input session "steer draft")
      (codex-ide--append-to-buffer (current-buffer) "Final answer.\n")
      (codex-ide--finish-turn session)
      (should (string-match-p
               (rx "Final answer." "\n\n\n> steer draft\n\n" string-end)
               (buffer-string)))
      (goto-char (marker-position
                  (codex-ide-session-input-prompt-start-marker session)))
      (should (equal (codex-ide-test--prompt-prefix-at-line) "> ")))))

(ert-deftest codex-ide-agent-delta-separates-active-prompt-from-output ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-current-input session "steer draft")
      (codex-ide--handle-notification
       session
       '((method . "item/agentMessage/delta")
         (params . ((itemId . "msg-1")
                    (delta . "Final answer.")))))
      (should (string-match-p
               (rx "Final answer." "\n\n\n> steer draft\n\n" string-end)
               (buffer-string)))
      (goto-char (marker-position
                  (codex-ide-session-input-prompt-start-marker session)))
      (should (equal (codex-ide-test--prompt-prefix-at-line) "> ")))))

(ert-deftest codex-ide-agent-deltas-do-not-accumulate-prompt-spacing ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-current-input session "steer draft")
      (dolist (delta '("First line.\n" "Second line.\n"))
        (codex-ide--handle-notification
         session
         `((method . "item/agentMessage/delta")
           (params . ((itemId . "msg-1")
                      (delta . ,delta))))))
      (should (string-match-p
               (rx "First line." "\n"
                   "Second line." "\n\n\n"
                   "> steer draft\n\n" string-end)
               (buffer-string)))
      (should-not (string-match-p
                   (rx "Second line." "\n\n\n\n\n" "> steer draft\n\n" string-end)
                   (buffer-string))))))

(ert-deftest codex-ide-agent-markdown-delta-separates-active-prompt-from-output ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-current-input session "steer draft")
      (codex-ide--handle-notification
       session
       '((method . "item/agentMessage/delta")
         (params . ((itemId . "msg-1")
                    (delta . "Use `code` now.\n")))))
      (should (string-match-p
               (rx "Use `code` now." "\n\n\n> steer draft\n\n" string-end)
               (buffer-string)))
      (goto-char (point-min))
      (search-forward "code")
      (should (get-text-property (1- (point)) 'codex-ide-markdown)))))

(ert-deftest codex-ide-command-output-delta-separates-active-prompt-from-output ()
  (let ((codex-ide-renderer-command-output-fold-on-start nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "submitted prompt")
        (codex-ide--begin-turn-display session)
        (codex-ide--replace-current-input session "steer draft")
        (codex-ide--render-item-start
         session
         '((id . "call-1")
           (type . "commandExecution")
           (command . ["echo" "hello"])))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "hello")))))
        (should (string-match-p
                 (rx "    hello" "\n\n\n> steer draft\n\n" string-end)
                 (buffer-string)))))))

(ert-deftest codex-ide-working-indicator-shows-as-prompt-help ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (should-not (string-match-p "Working\\.\\.\\." (buffer-string)))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Working..."))
      (codex-ide--replace-current-input session "steer me")
      (should-not (codex-ide-test--input-placeholder-text session)))))

(ert-deftest codex-ide-reasoning-indicator-shows-as-prompt-help ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "reason-1")
         (type . "reasoning")
         (summary . [])))
      (should-not (string-match-p "Working\\.\\.\\." (buffer-string)))
      (should-not (string-match-p "Reasoning\\.\\.\\." (buffer-string)))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Reasoning...")))))

(ert-deftest codex-ide-pending-indicator-replacement-updates-prompt-help ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--replace-pending-output-indicator session "Reasoning...\n")
      (should-not (string-match-p "Working\\.\\.\\." (buffer-string)))
      (should-not (string-match-p "Reasoning\\.\\.\\." (buffer-string)))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Reasoning...")))))

(ert-deftest codex-ide-first-rendered-item-clears-pending-output-indicator ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Working..."))
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . "echo hi")
         (cwd . "/tmp")))
      (should-not (string-match-p "Working\\.\\.\\." (buffer-string)))
      (should (equal (codex-ide-test--input-placeholder-text session)
                     "Running..."))
      (should (string-match-p "\\* Ran command" (buffer-string)))
      (should (string-match-p "  \\$ echo hi" (buffer-string))))))

(ert-deftest codex-ide-command-execution-omits-shell-wrapper-in-detail ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["/bin/zsh" "-lc" "if true; then echo hi; fi"])))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p "\\* Ran command" buffer-text))
        (should-not (string-match-p "/bin/zsh -lc" buffer-text))
        (should (string-match-p "  \\$ if true; then echo hi; fi" buffer-text)))
      (goto-char (point-min))
      (search-forward "if true")
      (should (memq 'font-lock-keyword-face
                    (ensure-list (get-text-property
                                  (match-beginning 0)
                                  'face)))))))

(ert-deftest codex-ide-command-execution-summarizes-sed-file-read ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["/bin/zsh" "-lc" "sed -n '10,20p' codex-ide.el"])))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Read codex-ide\\.el (lines 10 to 20)"
                 buffer-text))
        (should-not (string-match-p "\\$ sed -n" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-numbered-sed-file-read ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["/bin/zsh" "-lc" "nl -ba codex-ide.el | sed -n '30,40p'"])))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Read codex-ide\\.el (lines 30 to 40)"
                 buffer-text))
        (should-not (string-match-p "\\$ nl -ba" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-string-shell-pipeline-read ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . "/bin/zsh -lc \"nl -ba codex-ide.el | sed -n '30,40p'\"")))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Read codex-ide\\.el (lines 30 to 40)"
                 buffer-text))
        (should-not (string-match-p "\\$ nl -ba" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-rg-search ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["rg" "-n" "render-item-start" "codex-ide-renderer.el"])))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Searched codex-ide-renderer\\.el for \"render-item-start\""
                 buffer-text))
        (should-not (string-match-p "\\$ rg" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-quoted-rg-alternation ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . "rg -n \"summarizes-rg|summarizes-sed\" tests/codex-ide-tests.el codex-ide-renderer.el")))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Searched tests/codex-ide-tests\\.el and codex-ide-renderer\\.el for \"summarizes-rg|summarizes-sed\""
                 buffer-text))
        (should-not (string-match-p "\\$ rg" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-shell-wrapped-quoted-rg-alternation ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . "/bin/zsh -lc \"rg -n 'summarizes-rg|summarizes-sed' tests/codex-ide-tests.el codex-ide-renderer.el\"")))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Searched tests/codex-ide-tests\\.el and codex-ide-renderer\\.el for \"summarizes-rg|summarizes-sed\""
                 buffer-text))
        (should-not (string-match-p "\\$ rg" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-shell-wrapped-rg-search ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["/bin/zsh" "-lc" "rg --heading -n 'Ran command' tests"])))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Searched tests for \"Ran command\""
                 buffer-text))
        (should-not (string-match-p "/bin/zsh -lc" buffer-text))
        (should-not (string-match-p "\\$ rg" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-rg-explicit-regexp ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["rg" "-e" "foo bar" "codex-ide.el" "tests"])))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Searched codex-ide\\.el and tests for \"foo bar\""
                 buffer-text))
        (should-not (string-match-p "\\$ rg" buffer-text))))))

(ert-deftest codex-ide-command-execution-summarizes-rg-three-paths ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["rg" "-n" "foo bar" "codex-ide.el" "codex-ide-renderer.el" "tests"])))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Searched codex-ide\\.el, codex-ide-renderer\\.el and tests for \"foo bar\""
                 buffer-text))
        (should-not (string-match-p "\\$ rg" buffer-text))))))

(ert-deftest codex-ide-command-execution-rg-completion-reports-hit-count ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :directory default-directory
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["rg" "-n" "needle" "codex-ide.el"])))
      (codex-ide--handle-notification
       session
       '((method . "item/commandExecution/outputDelta")
         (params . ((itemId . "call-1")
                    (delta . "codex-ide.el:10:needle\ncodex-ide.el:20:needle\n")))))
      (codex-ide--render-item-completion
       session
       '((id . "call-1")
         (type . "commandExecution")
         (status . "completed")))
      (let ((buffer-text (buffer-string)))
        (should (string-match-p
                 "\\* Searched codex-ide\\.el for \"needle\""
                 buffer-text))
        (should (string-match-p "  └ found 2 hits" buffer-text))))))

(ert-deftest codex-ide-command-execution-streams-output-and-folds-on-completion ()
  (let ((codex-ide-renderer-command-output-fold-on-start nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "submitted prompt")
        (codex-ide--begin-turn-display session)
        (codex-ide--render-item-start
         session
         '((id . "call-1")
           (type . "commandExecution")
           (command . ["echo" "hello"])))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "hello\nworld\n")))))
        (should (string-match-p "    hello\n    world" (buffer-string)))
        (goto-char (point-min))
        (search-forward "output: 2 lines, streaming [fold]")
        (let ((overlay (get-char-property
                        (match-beginning 0)
                        codex-ide-command-output-overlay-property)))
          (should (overlayp overlay))
          (should-not (overlay-get overlay 'invisible))
          (codex-ide--render-item-completion
           session
           '((id . "call-1")
             (type . "commandExecution")
             (status . "completed")
             (exitCode . 0)))
          (should (overlay-get overlay 'invisible))
          (should (overlay-get overlay :folded))
          (should (string-match-p "output: 2 lines \\[expand\\]"
                                  (buffer-string)))
          (should-not (string-match-p "    hello\n    world"
                                      (buffer-string))))))))

(ert-deftest codex-ide-command-output-face-extends-line-background ()
  (should (eq (face-attribute 'codex-ide-command-output-face :extend nil t)
              t)))

(ert-deftest codex-ide-user-prompt-face-extends-line-background ()
  (should (eq (face-attribute 'codex-ide-user-prompt-face :extend nil t)
              t)))

(ert-deftest codex-ide-input-prompt-has-tail-newline-for-extended-background ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (should (equal (buffer-string) "\n> \n\n"))
      (should (eq (get-text-property (1- (point-max)) 'face)
                  'codex-ide-user-prompt-face))
      (should (equal (codex-ide--current-input session) "")))))

(ert-deftest codex-ide-input-prompt-move-end-of-line-stays-at-text-end ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (insert "draft")
      (goto-char (marker-position (codex-ide-session-input-start-marker session)))
      (move-end-of-line 1)
      (should (= (point) (codex-ide--input-end-position session)))
      (should (string= (codex-ide--current-input session) "draft")))))

(ert-deftest codex-ide-input-prompt-move-beginning-of-line-skips-prefix ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session nil)
      (goto-char (codex-ide--input-end-position session))
      (move-beginning-of-line 1)
      (should (= (point)
                 (marker-position
                  (codex-ide-session-input-start-marker session)))))))

(ert-deftest codex-ide-input-prompt-sync-clamps-point-to-text-end ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :buffer (current-buffer)
                    :status "idle")))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "draft")
      (goto-char (point-max))
      (codex-ide--sync-prompt-minor-mode session)
      (should (= (point) (codex-ide--input-end-position session)))
      (insert " more")
      (should (string= (codex-ide--current-input session) "draft more")))))

(ert-deftest codex-ide-command-output-tails-rendered-lines ()
  (let ((codex-ide-renderer-command-output-max-rendered-lines 3)
        (codex-ide-renderer-command-output-max-rendered-chars nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "submitted prompt")
        (codex-ide--begin-turn-display session)
        (codex-ide--render-item-start
         session
         '((id . "call-1")
           (type . "commandExecution")
           (command . ["sh" "-c" "printf lots"])))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "one\ntwo\nthree\nfour\nfive\n")))))
        (let ((buffer-text (buffer-string))
              (state (codex-ide--item-state session "call-1")))
          (should (string-match-p
                   "output: 5 lines, showing last 3, streaming \\[expand\\] \\[full output\\]"
                   buffer-text))
          (let ((overlay (plist-get state :command-output-overlay)))
            (should (overlayp overlay))
            (should (overlay-get overlay 'invisible))
            (should (string-match-p
                     "one\ntwo\nthree\nfour\nfive\n"
                     (or (overlay-get overlay :output-fallback-text) ""))))
          (should (string-match-p "four\nfive"
                                  (plist-get state :output-text))))
        (codex-ide--render-item-completion
         session
         '((id . "call-1")
           (type . "commandExecution")
           (status . "completed")
           (exitCode . 0)))
        (should (string-match-p
                 "output: 5 lines, showing last 3 \\[expand\\] \\[full output\\]"
                 (buffer-string)))))))

(ert-deftest codex-ide-command-output-full-button-opens-uncapped-output ()
  (let ((codex-ide-renderer-command-output-max-rendered-lines 2)
        (codex-ide-renderer-command-output-max-rendered-chars nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "submitted prompt")
        (codex-ide--begin-turn-display session)
        (codex-ide--render-item-start
         session
         '((id . "call-1")
           (type . "commandExecution")
           (command . ["sh" "-c" "printf lots"])))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "one\ntwo\nthree\nfour\n")))))
        (goto-char (point-min))
        (search-forward "[full output]")
        (let ((button (button-at (match-beginning 0))))
          (should button)
          (push-button (match-beginning 0)))
        (should (string-match-p
                 "\\*codex-output\\[.*:call-1\\]\\*"
                 (buffer-name)))
        (should (derived-mode-p 'special-mode))
        (should (string-match-p
                 "\\$ printf lots\n\none\ntwo\nthree\nfour\n"
                 (buffer-string)))))))

(ert-deftest codex-ide-command-output-full-button-uses-stream-cache ()
  (let ((codex-ide-renderer-command-output-max-rendered-lines 2)
        (codex-ide-renderer-command-output-max-rendered-chars nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "submitted prompt")
        (codex-ide--begin-turn-display session)
        (codex-ide--render-item-start
         session
         '((id . "call-1")
           (type . "commandExecution")
           (command . ["sh" "-c" "printf lots"])))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "one\ntwo\n")))))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "three\nfour\n")))))
        (let* ((state (codex-ide--item-state session "call-1"))
               (overlay (plist-get state :command-output-overlay)))
          (should (equal (overlay-get overlay :output-fallback-text)
                         "one\ntwo\nthree\nfour\n"))
          (codex-ide--put-item-state
           session
           "call-1"
           (plist-put state :output-text nil))
          (codex-ide--open-command-output-overlay overlay))
        (should (string-match-p
                 "\\*codex-output\\[.*:call-1\\]\\*"
                 (buffer-name)))
        (should (derived-mode-p 'special-mode))
        (should (string-match-p
                 "\\$ printf lots\n\none\ntwo\nthree\nfour\n"
                 (buffer-string)))))))

(ert-deftest codex-ide-command-output-can-start-folded-while-streaming ()
  (let ((codex-ide-renderer-command-output-fold-on-start t))
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "submitted prompt")
        (codex-ide--begin-turn-display session)
        (codex-ide--render-item-start
         session
         '((id . "call-1")
           (type . "commandExecution")
           (command . ["echo" "hello"])))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "hello\n")))))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "world\n")))))
        (goto-char (point-min))
        (search-forward "output: 2 lines, streaming [expand]")
        (let ((overlay (get-char-property
                        (match-beginning 0)
                        codex-ide-command-output-overlay-property)))
          (should (overlayp overlay))
          (should (overlay-get overlay :folded))
          (should (overlay-get overlay 'invisible))
          (should-not (string-match-p "    hello\n    world"
                                      (buffer-string)))
          (codex-ide-toggle-command-output-at-point (match-beginning 0))
          (should-not (overlay-get overlay 'invisible))
          (should (string-match-p "    hello\n    world"
                                  (buffer-string)))
          (should (string-match-p "output: 2 lines, streaming \\[fold\\]"
                                  (buffer-string))))))))

(ert-deftest codex-ide-command-execution-keeps-output-before-start ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :directory default-directory
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--handle-notification
       session
       '((method . "item/commandExecution/outputDelta")
         (params . ((itemId . "call-1")
                    (delta . "early 1\nearly 2\n")))))
      (should-not (string-match-p "early 1" (buffer-string)))
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["sh" "-c" "printf output"])))
      (codex-ide--handle-notification
       session
       '((method . "item/commandExecution/outputDelta")
         (params . ((itemId . "call-1")
                    (delta . "late 3\n")))))
      (codex-ide--render-item-completion
       session
       '((id . "call-1")
         (type . "commandExecution")
         (status . "completed")
         (exitCode . 0)))
      (goto-char (point-min))
      (search-forward "output: 3 lines [expand]")
      (let ((overlay (get-char-property
                      (match-beginning 0)
                      codex-ide-command-output-overlay-property)))
        (should (overlayp overlay))
        (should (overlay-get overlay 'invisible))
        (should-not (string-match-p "    early 1"
                                    (buffer-string)))
        (codex-ide-toggle-command-output-at-point (match-beginning 0))
        (should-not (overlay-get overlay 'invisible))
        (should (string-match-p
                 "    early 1\n    early 2\n    late 3\n"
                 (buffer-string)))))))

(ert-deftest codex-ide-command-execution-renders-aggregated-output-when-no-stream ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :directory default-directory
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["echo" "hello"])))
      (codex-ide--render-item-completion
       session
       '((id . "call-1")
         (type . "commandExecution")
         (status . "completed")
         (exitCode . 0)
         (aggregatedOutput . "hello\nworld\n")))
      (goto-char (point-min))
      (search-forward "output: 2 lines [expand]")
      (let ((overlay (get-char-property
                      (match-beginning 0)
                      codex-ide-command-output-overlay-property)))
        (should (overlayp overlay))
        (should (overlay-get overlay 'invisible))
        (should-not (string-match-p "    hello\n    world" (buffer-string)))
        (codex-ide-toggle-command-output-at-point (match-beginning 0))
        (should-not (overlay-get overlay 'invisible))
        (should (string-match-p "    hello\n    world" (buffer-string)))))))

(ert-deftest codex-ide-mcp-tool-call-renders-expandable-result-block ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :directory default-directory
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "mcp-1")
         (type . "mcpToolCall")
         (server . "emacs")
         (tool . "emacs_get_buffer_text")
         (arguments . ((buffer . "scratch")))))
      (codex-ide--render-item-completion
       session
       '((id . "mcp-1")
         (type . "mcpToolCall")
         (status . "completed")
         (result . ((text . "line 1\nline 2\n")))))
      (goto-char (point-min))
      (search-forward "* Called emacs/emacs_get_buffer_text")
      (should (search-forward "result: 2 lines [expand]" nil t))
      (let ((overlay (get-char-property
                      (match-beginning 0)
                      codex-ide-item-result-overlay-property)))
        (should (overlayp overlay))
        (should (overlay-get overlay 'invisible))
        (should-not (string-match-p "    line 1\n    line 2" (buffer-string)))
        (codex-ide-toggle-item-result-at-point (match-beginning 0))
        (should-not (overlay-get overlay 'invisible))
        (should (string-match-p "result: 2 lines \\[fold\\]"
                                (buffer-string)))
        (should (string-match-p "    line 1\n    line 2"
                                (buffer-string)))))))

(ert-deftest codex-ide-mcp-tool-call-result-stays-with-call-when-created-late ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :directory default-directory
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "mcp-1")
         (type . "mcpToolCall")
         (server . "emacs")
         (tool . "emacs_get_buffer_text")))
      (codex-ide--handle-notification
       session
       '((method . "item/agentMessage/delta")
         (params . ((itemId . "msg-1")
                    (delta . "Later assistant text\n")))))
      (codex-ide--render-item-completion
       session
       '((id . "mcp-1")
         (type . "mcpToolCall")
         (status . "completed")
         (result . ((text . "tool result\n")))))
      (codex-ide--render-item-completion
       session
       '((id . "msg-1")
         (type . "agentMessage")
         (status . "completed")))
      (goto-char (point-min))
      (search-forward "* Called emacs/emacs_get_buffer_text")
      (let ((call-pos (match-beginning 0)))
        (search-forward "result: 1 line [expand]")
        (let ((result-pos (match-beginning 0)))
          (search-forward "Later assistant text")
          (let ((assistant-pos (match-beginning 0)))
            (should (< call-pos result-pos))
            (should (< result-pos assistant-pos))))))))

(ert-deftest codex-ide-command-output-stays-with-command-when-created-after-later-transcript-text ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :directory default-directory
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["git" "branch" "--show-current"])))
      (codex-ide--render-item-start
       session
       '((id . "call-2")
         (type . "commandExecution")
         (command . ["git" "log" "--oneline" "main..HEAD"])))
      (codex-ide--handle-notification
       session
       '((method . "item/agentMessage/delta")
         (params . ((itemId . "msg-1")
                    (delta . "Later assistant text\n")))))
      (codex-ide--render-item-completion
       session
       '((id . "call-1")
         (type . "commandExecution")
         (status . "completed")
         (exitCode . 0)
         (aggregatedOutput . "branch-name\n")))
      (codex-ide--render-item-completion
       session
       '((id . "call-2")
         (type . "commandExecution")
         (status . "completed")
         (exitCode . 0)
         (aggregatedOutput . "deadbeef commit\n")))
      (goto-char (point-min))
      (search-forward "* Ran command")
      (let ((first-command-pos (match-beginning 0)))
        (search-forward "output: 1 line [expand]")
        (let ((first-output-pos (match-beginning 0)))
          (search-forward "* Ran command")
          (let ((second-command-pos (match-beginning 0)))
            (search-forward "output: 1 line [expand]")
            (let ((second-output-pos (match-beginning 0)))
              (search-forward "Later assistant text")
              (let ((assistant-pos (match-beginning 0)))
                (should (< first-command-pos first-output-pos))
                (should (< first-output-pos second-command-pos))
                (should (< second-command-pos second-output-pos))
                (should (< second-output-pos assistant-pos))))))))))

(ert-deftest codex-ide-completed-command-output-stays-before-later-agent-text ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (let ((session (make-codex-ide-session
                    :directory default-directory
                    :buffer (current-buffer)
                    :status "idle"
                    :item-states (make-hash-table :test 'equal))))
      (setq-local codex-ide--session session)
      (codex-ide--insert-input-prompt session "submitted prompt")
      (codex-ide--begin-turn-display session)
      (codex-ide--render-item-start
       session
       '((id . "call-1")
         (type . "commandExecution")
         (command . ["git" "branch" "--show-current"])))
      (codex-ide--render-item-completion
       session
       '((id . "call-1")
         (type . "commandExecution")
         (status . "completed")
         (exitCode . 0)
         (aggregatedOutput . "branch-name\n")))
      (codex-ide--handle-notification
       session
       '((method . "item/agentMessage/delta")
         (params . ((itemId . "msg-1")
                    (delta . "Later assistant text\n")))))
      (codex-ide--render-item-completion
       session
       '((id . "msg-1")
         (type . "agentMessage")
         (status . "completed")))
      (goto-char (point-min))
      (search-forward "output: 1 line [expand]")
      (let ((output-pos (match-beginning 0)))
        (search-forward "Later assistant text")
        (should (< output-pos (match-beginning 0)))))))

(ert-deftest codex-ide-command-output-does-not-fold-following-assistant-message ()
  (let ((codex-ide-renderer-command-output-fold-on-start nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "submitted prompt")
        (codex-ide--begin-turn-display session)
        (codex-ide--render-item-start
         session
         '((id . "call-1")
           (type . "commandExecution")
           (command . ["sh" "-c" "echo start; sleep 1; echo end"])))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "start\n")))))
        (codex-ide--handle-notification
         session
         '((method . "item/agentMessage/delta")
           (params . ((itemId . "msg-1")
                      (delta . "assistant while command runs\n")))))
        (codex-ide--handle-notification
         session
         '((method . "item/commandExecution/outputDelta")
           (params . ((itemId . "call-1")
                      (delta . "end\n")))))
        (goto-char (point-min))
        (search-forward "output: 2 lines, streaming [fold]")
        (let* ((overlay (get-char-property
                         (match-beginning 0)
                         codex-ide-command-output-overlay-property))
               (output-text (buffer-substring-no-properties
                             (overlay-start overlay)
                             (overlay-end overlay))))
          (should (overlayp overlay))
          (should (string-match-p "    start\n    end\n" output-text))
          (should-not (string-match-p "assistant while command runs" output-text))
          (codex-ide--render-item-completion
           session
           '((id . "call-1")
             (type . "commandExecution")
             (status . "completed")
             (exitCode . 0)))
          (should (overlay-get overlay 'invisible))))))

  (ert-deftest codex-ide-command-output-ret-toggles-folded-block ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(codex-ide--render-item-start
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (command . ["echo" "hello"])))
	(codex-ide--render-item-completion
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (status . "completed")
           (exitCode . 0)
           (aggregatedOutput . "hello\nworld\n")))
	(goto-char (point-min))
	(search-forward "output: 2 lines")
	(let* ((header-pos (match-beginning 0))
               (overlay (get-char-property
			 header-pos
			 codex-ide-command-output-overlay-property)))
          (should (overlay-get overlay 'invisible))
          (goto-char header-pos)
          (should (eq (key-binding (kbd "RET"))
                      #'codex-ide-toggle-command-output-at-point))
          (call-interactively (key-binding (kbd "RET")))
          (should-not (overlay-get overlay 'invisible))
          (should (string-match-p "output: 2 lines \\[fold\\]"
                                  (buffer-string)))
          (should (string-match-p "    hello\n    world" (buffer-string)))
          (goto-char header-pos)
          (call-interactively (key-binding (kbd "RET")))
          (should (overlay-get overlay 'invisible))
          (should (string-match-p "output: 2 lines \\[expand\\]"
                                  (buffer-string)))
          (should-not (string-match-p "    hello\n    world"
                                      (buffer-string)))))))

  (ert-deftest codex-ide-command-output-expand-button-toggles-folded-block ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(codex-ide--render-item-start
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (command . ["echo" "hello"])))
	(codex-ide--render-item-completion
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (status . "completed")
           (exitCode . 0)
           (aggregatedOutput . "hello\nworld\n")))
	(goto-char (point-min))
	(search-forward "output: 2 lines")
	(let ((overlay (get-char-property
			(match-beginning 0)
			codex-ide-command-output-overlay-property))
              (keymap (get-char-property (match-beginning 0) 'keymap)))
          (should (overlay-get overlay 'invisible))
          (should (eq (lookup-key keymap (kbd "RET"))
                      #'codex-ide-toggle-command-output-at-point))
          (codex-ide-toggle-command-output-at-point)
          (should-not (overlay-get overlay 'invisible))
          (should (string-match-p "output: 2 lines \\[fold\\]"
                                  (buffer-string)))
          (should (string-match-p "    hello\n    world" (buffer-string)))
          (codex-ide-toggle-command-output-at-point)
          (should (overlay-get overlay 'invisible))
          (should (string-match-p "output: 2 lines \\[expand\\]"
                                  (buffer-string)))
          (should-not (string-match-p "    hello\n    world"
                                      (buffer-string)))))))

  (ert-deftest codex-ide-command-output-toggle-preserves-window-when-in-place ()
    (save-window-excursion
      (delete-other-windows)
      (let ((buffer (get-buffer-create " *codex-ide-command-output-toggle-window*")))
	(unwind-protect
            (let ((window (selected-window)))
              (with-current-buffer buffer
		(erase-buffer)
		(codex-ide-session-mode)
		(let ((session (make-codex-ide-session
				:directory default-directory
				:buffer buffer
				:status "idle"
				:item-states (make-hash-table :test 'equal))))
                  (setq-local codex-ide--session session)
                  (dotimes (n 120)
                    (codex-ide--append-agent-text
                     buffer
                     (format "context %03d\n" n)))
                  (codex-ide--insert-input-prompt session "submitted prompt")
                  (codex-ide--begin-turn-display session)
                  (codex-ide--render-item-start
                   session
                   '((id . "call-1")
                     (type . "commandExecution")
                     (command . ["echo" "hello"])))
                  (codex-ide--render-item-completion
                   session
                   '((id . "call-1")
                     (type . "commandExecution")
                     (status . "completed")
                     (exitCode . 0)
                     (aggregatedOutput . "hello\nworld\n")))
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (dotimes (n 200)
                      (insert (format "tail %03d\n" n)))))
		(goto-char (point-min))
		(search-forward "[expand]")
		(let ((toggle-pos (match-beginning 0)))
                  (set-window-buffer window buffer)
                  (set-window-point window toggle-pos)
                  (set-window-start window
                                    (save-excursion
                                      (goto-char toggle-pos)
                                      (forward-line -3)
                                      (point))
                                    t)
                  (redisplay t)
                  (let ((window-start-before (window-start window))
			(window-point-before (window-point window)))
                    (codex-ide-toggle-command-output-at-point toggle-pos)
                    (should (= (window-start window) window-start-before))
                    (should (= (window-point window) window-point-before))
                    (should (< (window-point window)
                               (with-current-buffer buffer
				 (point-max))))
                    (should (overlay-get
                             (get-char-property
                              toggle-pos
                              codex-ide-command-output-overlay-property)
                             'invisible))))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))))))

  (ert-deftest codex-ide-command-output-header-controls-are-scoped ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(codex-ide--render-item-start
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (command . ["echo" "hello"])))
	(codex-ide--render-item-completion
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (status . "completed")
           (exitCode . 0)
           (aggregatedOutput . "hello\nworld\n")))
	(goto-char (point-min))
	(search-forward "output: 2 lines ")
	(let ((prefix-pos (match-beginning 0)))
          (should-not (button-at prefix-pos))
          (should (eq (get-text-property prefix-pos 'keymap)
                      codex-ide-command-output-map))
          (should (eq (get-text-property prefix-pos 'face)
                      'codex-ide-item-detail-face)))
	(search-forward "[expand]")
	(let ((pos (match-beginning 0)))
          (should (button-at pos))
          (should-not (eq (get-text-property pos 'keymap)
                          codex-ide-command-output-map))
          (should (eq (lookup-key (get-text-property pos 'keymap) [mouse-2])
                      #'push-button))))))

  (ert-deftest codex-ide-command-execution-rg-completion-counts-aggregated-output ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(codex-ide--render-item-start
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (command . ["rg" "-n" "needle" "codex-ide.el"])))
	(codex-ide--render-item-completion
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (status . "completed")
           (exitCode . 0)
           (aggregatedOutput . "codex-ide.el:10:needle\ncodex-ide.el:20:needle\n")))
	(let ((buffer-text (buffer-string)))
          (should (string-match-p
                   "\\* Searched codex-ide\\.el for \"needle\""
                   buffer-text))
          (should (string-match-p "  └ found 2 hits" buffer-text))))))

  (ert-deftest codex-ide-command-execution-rg-completion-without-output-omits-hit-count ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(codex-ide--render-item-start
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (command . ["rg" "-n" "needle" "codex-ide.el"])))
	(codex-ide--render-item-completion
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (status . "completed")
           (exitCode . 0)))
	(let ((buffer-text (buffer-string)))
          (should (string-match-p
                   "\\* Searched codex-ide\\.el for \"needle\""
                   buffer-text))
          (should-not (string-match-p "  └ found 0 hits" buffer-text))
          (should-not (string-match-p "  └ found [0-9]+ hits" buffer-text))))))

  (ert-deftest codex-ide-command-execution-rg-no-match-reports-zero-hits ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :directory default-directory
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(codex-ide--render-item-start
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (command . ["rg" "-n" "missing" "codex-ide.el"])))
	(codex-ide--render-item-completion
	 session
	 '((id . "call-1")
           (type . "commandExecution")
           (status . "failed")
           (exitCode . 1)))
	(let ((buffer-text (buffer-string)))
          (should (string-match-p
                   "\\* Searched codex-ide\\.el for \"missing\""
                   buffer-text))
          (should (string-match-p "  └ found 0 hits" buffer-text))
          (should-not (string-match-p "failed with exit code 1" buffer-text))))))

  (ert-deftest codex-ide-empty-reasoning-rewrites-pending-output-indicator ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(codex-ide--render-item-start
	 session
	 '((id . "reason-1")
           (type . "reasoning")
           (summary . [])))
	(should-not (string-match-p "Working\\.\\.\\." (buffer-string)))
	(should-not (string-match-p "Reasoning\\.\\.\\." (buffer-string)))
	(should (equal (codex-ide-test--input-placeholder-text session)
                       "Reasoning..."))
	(codex-ide--ensure-agent-message-prefix session "msg-1")
	(should (equal (codex-ide-test--input-placeholder-text session)
                       "Running...")))))

  (ert-deftest codex-ide-reasoning-summary-deltas-accumulate-into-one-block ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(dolist (delta '("since" " reading" " should" " be" " allowed"))
          (codex-ide--handle-notification
           session
           `((method . "item/reasoning/summaryTextDelta")
             (params . ((itemId . "reason-1")
			(summaryIndex . 1)
			(delta . ,delta))))))
	(let ((buffer-text (buffer-string)))
          (should (equal (how-many "^\\* Reasoning: " (point-min) (point-max)) 1))
          (should (string-match-p
                   (regexp-quote "* Reasoning: since reading should be allowed\n")
                   buffer-text))))))

  (ert-deftest codex-ide-pending-output-indicator-uses-prompt-help ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session nil)
	(codex-ide--insert-pending-output-indicator
	 session
	 "Reasoning...\n")
	(should-not (string-match-p "Reasoning\\.\\.\\." (buffer-string)))
	(should (equal (codex-ide-test--input-placeholder-text session)
                       "Reasoning..."))
	(codex-ide--clear-pending-output-indicator session)
	(should (equal (codex-ide-test--input-placeholder-text session)
                       "Tell Codex what to do...")))))

  (ert-deftest codex-ide-finish-turn-clears-pending-output-indicator ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((session (make-codex-ide-session
                      :buffer (current-buffer)
                      :status "idle"
                      :item-states (make-hash-table :test 'equal))))
	(setq-local codex-ide--session session)
	(codex-ide--insert-input-prompt session "submitted prompt")
	(codex-ide--begin-turn-display session)
	(should (equal (codex-ide-test--input-placeholder-text session)
                       "Working..."))
	(codex-ide--finish-turn session)
	(should-not (string-match-p "Working\\.\\.\\." (buffer-string)))
	(should (codex-ide--input-prompt-active-p session))
	(goto-char (marker-position
                    (codex-ide-session-input-start-marker session)))
	(should (eolp)))))

  (ert-deftest codex-ide-session-markdown-faces-survive-font-lock-attempts ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((inhibit-read-only t))
	(insert "`code`")
	(codex-ide-renderer-render-markdown-region (point-min) (point-max))
	(font-lock-mode 1)
	(font-lock-ensure (point-min) (point-max))
	(goto-char (point-min))
	(search-forward "code")
	(should (eq (get-text-property (1- (point)) 'face)
                    'font-lock-keyword-face)))))

  (ert-deftest codex-ide-session-mode-binds-tab-to-button-navigation ()
    (with-temp-buffer
      (codex-ide-session-mode)
      (should (eq (key-binding (kbd "TAB")) #'codex-ide-session-mode-nav-forward))
      (should (eq (key-binding (kbd "<backtab>")) #'codex-ide-session-mode-nav-backward))))

  (ert-deftest codex-ide-start-session-new-initializes-thread-without-real-cli ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests '()))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method params)
						 (push (cons method params) requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/start" '((thread . ((id . "thread-test-1")))))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((session (codex-ide--start-session 'new)))
					(should (string= (codex-ide-session-thread-id session) "thread-test-1"))
					(should (equal (seq-remove (lambda (method)
								     (equal method "config/read"))
								   (mapcar #'car (nreverse requests)))
						       '("initialize" "thread/start")))
					(with-current-buffer (codex-ide-session-buffer session)
					  (should (derived-mode-p 'codex-ide-session-mode))
					  (should (codex-ide--input-prompt-active-p session))
					  (goto-char (marker-position
						      (codex-ide-session-input-start-marker session)))
					  (should (eolp)))))))))

  (ert-deftest codex-ide-start-session-new-honors-new-session-split ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests '()))
      (codex-ide-test-with-fixture project-dir
				   (save-window-excursion
				     (delete-other-windows)
				     (let ((origin-window (selected-window))
					   (codex-ide-new-session-split 'vertical)
					   (codex-ide-select-window-on-open nil))
				       (codex-ide-test-with-fake-processes
					(cl-letf (((symbol-function 'codex-ide--ensure-cli)
						   (lambda () t))
						  ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
						   (lambda () nil))
						  ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
						   (lambda () nil))
						  ((symbol-function 'codex-ide--request-sync)
						   (lambda (_session method params)
						     (push (cons method params) requests)
						     (pcase method
						       ("initialize" '((ok . t)))
						       ("thread/start" '((thread . ((id . "thread-split-1")))))
						       (_ (ert-fail (format "Unexpected method %s" method)))))))
					  (let* ((origin-left (nth 0 (window-edges origin-window)))
						 (session (codex-ide--start-session 'new))
						 (session-window (get-buffer-window
								  (codex-ide-session-buffer session))))
					    (should (window-live-p session-window))
					    (should-not (eq session-window origin-window))
					    (should (> (nth 0 (window-edges session-window)) origin-left))
					    (should (string= (codex-ide-session-thread-id session)
							     "thread-split-1"))
					    (should (equal (seq-remove (lambda (method)
									 (equal method "config/read"))
								       (mapcar #'car (nreverse requests)))
							   '("initialize" "thread/start")))))))))))

  (ert-deftest codex-ide-session-baseline-prompt-ignores-empty-strings ()
    (let ((codex-ide-session-baseline-prompt "   "))
      (should-not (codex-ide--format-session-context))))

  (ert-deftest codex-ide-session-baseline-prompt-default-includes-table-guidance ()
    (let ((formatted (codex-ide--format-session-context)))
      (should (string-match-p "Responses are rendered as Markdown in an Emacs buffer" formatted))
      (should (string-match-p "Markdown pipe tables are rendered as visible tables" formatted))
      (should (string-match-p "wrap code-like identifiers, filenames, paths, symbols, and expressions in backticks" formatted))
      (should (string-match-p "Avoid bare underscores or asterisks for code-like text inside tables" formatted))))

  (ert-deftest codex-ide-thread-choice-candidates-disambiguate-duplicate-previews ()
    (let* ((first-thread '((id . "thread-12345678")
                           (preview . "Investigate failure")))
           (second-thread '((id . "thread-abcdefgh")
                            (preview . "Investigate failure")))
           (choices (codex-ide--thread-choice-candidates
                     (list first-thread second-thread))))
      (should
       (equal
	(mapcar #'car choices)
	'("Investigate failure [thread-1]"
          "Investigate failure [thread-a]")))))

  (ert-deftest codex-ide-pick-thread-returns-selected-thread-object ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (selected nil)
          (recorded-extra-properties nil)
          (thread '((id . "thread-12345678")
                    (createdAt . 1744038896)
                    (preview . "[Emacs context]\n[/Emacs context]\n\nInvestigate failure"))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--list-threads)
						 (lambda (_session) (list thread)))
						((symbol-function 'completing-read)
						 (lambda (_prompt collection &rest _args)
						   (setq recorded-extra-properties completion-extra-properties)
						   (setq selected (caar collection))
						   selected)))
					(should (equal (codex-ide--pick-thread session) thread))
					(should (equal selected "Investigate failure"))
					(should (eq (plist-get recorded-extra-properties :display-sort-function)
						    'identity))
					(should (eq (plist-get recorded-extra-properties :cycle-sort-function)
						    'identity))
					(let ((affixation
					       (car (funcall (plist-get recorded-extra-properties
									:affixation-function)
							     (list selected)))))
					  (should (equal (car affixation) selected))
					  (should (equal (nth 1 affixation)
							 (format "%s "
								 (format-time-string "%Y-%m-%dT%H:%M:%S%z"
										     (seconds-to-time 1744038896)))))
					  (should (equal (nth 2 affixation) " [thread-1]")))))))))

  (ert-deftest codex-ide-pick-thread-excludes-omitted-thread-id ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (selected nil)
          (current-thread '((id . "thread-current")
                            (preview . "Current thread")))
          (other-thread '((id . "thread-other")
                          (preview . "Other thread"))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--list-threads)
						 (lambda (_session) (list current-thread other-thread)))
						((symbol-function 'completing-read)
						 (lambda (_prompt collection &rest _args)
						   (setq selected (caar collection))
						   selected)))
					(should (equal (codex-ide--pick-thread session "thread-current")
						       other-thread))
					(should (equal selected "Other thread"))))))))

  (ert-deftest codex-ide-list-threads-uses-configured-default-limit ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (captured-params nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((codex-ide-thread-list-default-limit 100)
					  (session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--request-sync)
						 (lambda (_session method params)
						   (setq captured-params params)
						   (should (equal method "thread/list"))
						   '((data . [((id . "thread-default"))]))))
						((symbol-function 'codex-ide-session-directory)
						 (lambda (_session) project-dir)))
					(should (equal (codex-ide--list-threads session)
						       '(((id . "thread-default")))))
					(should (equal captured-params
						       `((cwd . ,project-dir)
							 (limit . 100)
							 (sortKey . "updated_at"))))))))))

  (ert-deftest codex-ide-list-threads-accepts-explicit-limit-and-sort-key ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (captured-params nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((codex-ide-thread-list-default-limit 100)
					  (session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--request-sync)
						 (lambda (_session method params)
						   (setq captured-params params)
						   (should (equal method "thread/list"))
						   '((data . [((id . "thread-custom"))]))))
						((symbol-function 'codex-ide-session-directory)
						 (lambda (_session) project-dir)))
					(should (equal (codex-ide--list-threads
							session
							:limit 25
							:sort-key "created_at")
						       '(((id . "thread-custom")))))
					(should (equal captured-params
						       `((cwd . ,project-dir)
							 (limit . 25)
							 (sortKey . "created_at"))))))))))

  (ert-deftest codex-ide-ensure-query-session-for-thread-selection-creates-and-initializes-session ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((session (codex-ide--ensure-query-session-for-thread-selection
						      project-dir)))
					(should (codex-ide-session-p session))
					(should (memq session codex-ide--sessions))
					(should (equal (seq-remove (lambda (method)
								     (equal method "config/read"))
								   (nreverse requests))
						       '("initialize")))
					(should (string= (codex-ide-session-status session) "idle"))
					(should (codex-ide-session-query-only session))
					(should-not (codex-ide-session-buffer session))
					(should (buffer-live-p (codex-ide-test--log-buffer session)))))))))

  (ert-deftest codex-ide-show-session-buffer-errors-for-query-only-session ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((session (codex-ide--ensure-query-session-for-thread-selection
						      project-dir)))
					(should-error (codex-ide--show-session-buffer session)
						      :type 'user-error)
					(should (equal (seq-remove (lambda (method)
								     (equal method "config/read"))
								   (nreverse requests))
						       '("initialize")))))))))

  (ert-deftest codex-ide-show-or-resume-thread-creates-real-session-from-query-only-session ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil)
          (thread-read
           '((thread . ((id . "thread-reused-1")))
             (turns . (((id . "turn-1")
			(items . (((type . "userMessage")
                                   (content . (((type . "text")
						(text . "Reuse this buffer")))))
                                  ((type . "agentMessage")
                                   (id . "item-1")
                                   (text . "Buffer reused."))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/read" thread-read)
						   ("thread/resume" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method)))))
					       )
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window))))
				      (let ((query-session (codex-ide--ensure-query-session-for-thread-selection
							    project-dir)))
					(setq requests nil)
					(let ((session (codex-ide--show-or-resume-thread "thread-reused-1"
											 project-dir)))
					  (should-not (eq session query-session))
					  (should (= (length codex-ide--sessions) 2))
					  (should (codex-ide-session-query-only query-session))
					  (should-not (codex-ide-session-query-only session))
					  (should (equal (seq-remove (lambda (method)
								       (equal method "config/read"))
								     (nreverse requests))
							 '("initialize" "thread/read" "thread/resume")))
					  (should (string= (codex-ide-session-thread-id session)
							   "thread-reused-1"))
					  (with-current-buffer (codex-ide-session-buffer session)
					    (let ((buffer-text (buffer-string)))
					      (should-not (string-match-p "Kill Codex session buffer" buffer-text))
					      (should (string-match-p "^> Reuse this buffer" buffer-text))
					      (should (string-match-p "Buffer reused\\." buffer-text)))))))))))

  (ert-deftest codex-ide-start-session-resume-aborts-cleanly-on-picker-quit ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
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
						   (_ (ert-fail (format "Unexpected method %s" method)))))
					       )
					      ((symbol-function 'codex-ide--pick-thread)
					       (lambda (&rest _) (signal 'quit nil))))
				      (should
				       (eq (condition-case nil
					       (progn
						 (codex-ide--start-session 'resume)
						 :no-quit)
					     (quit :quit))
					   :quit))
				      (should-not (codex-ide--get-session))
				      (should-not (codex-ide--has-live-sessions-p)))))))

  (ert-deftest codex-ide-start-session-resume-keeps-existing-session-on-picker-quit ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/start" '((thread . ((id . "thread-current")))))
						   (_ (ert-fail (format "Unexpected method %s" method))))))
					      ((symbol-function 'codex-ide--pick-thread)
					       (lambda (&rest _) (signal 'quit nil))))
				      (let ((session (codex-ide--start-session 'new)))
					(should
					 (eq (condition-case nil
						 (progn
						   (codex-ide--start-session 'resume)
						   :no-quit)
					       (quit :quit))
					     :quit))
					(should (eq (codex-ide--get-session) session))
					(should (process-live-p (codex-ide-session-process session)))
					(should (buffer-live-p (codex-ide-session-buffer session)))))))))

  (ert-deftest codex-ide-start-session-new-inserts-single-empty-prompt ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/start" '((thread . ((id . "thread-current")))))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((session (codex-ide--start-session 'new)))
					(with-current-buffer (codex-ide-session-buffer session)
					  (should (codex-ide--input-prompt-active-p session))
					  (goto-char (marker-position
						      (codex-ide-session-input-start-marker session)))
					  (should (eolp)))))))))

  (ert-deftest codex-ide-input-prompt-prefix-is-read-only ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "hello")
					(goto-char (marker-position
						    (codex-ide-session-input-start-marker session)))
					(should-error (delete-backward-char 1) :type 'text-read-only)
					(goto-char (marker-position
						    (codex-ide-session-input-prompt-start-marker session)))
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					(goto-char (marker-position
						    (codex-ide-session-input-start-marker session)))
					(should (looking-at-p "hello"))
					(should (string= (codex-ide--current-input session) "hello"))))))))

  (ert-deftest codex-ide-input-prompt-allows-insert-at-input-start ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session nil)
					(goto-char (marker-position
						    (codex-ide-session-input-start-marker session)))
					(insert "h")
					(goto-char (marker-position
						    (codex-ide-session-input-prompt-start-marker session)))
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					(goto-char (marker-position
						    (codex-ide-session-input-start-marker session)))
					(should (looking-at-p "h"))
					(should (string= (codex-ide--current-input session) "h"))))))))

  (ert-deftest codex-ide-track-active-buffer-refreshes-all-session-headers-in-project ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (file-path (codex-ide-test--make-project-file
                       project-dir "src/example.el" "(message \"hello\")\n")))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((first (codex-ide--create-process-session))
					  (second (codex-ide--create-process-session)))
				      (with-current-buffer (find-file-noselect file-path)
					(setq-local default-directory (file-name-as-directory project-dir))
					(goto-char (point-min))
					(forward-line 0)
					(codex-ide--track-active-buffer (current-buffer)))
				      (dolist (session (list first second))
					(with-current-buffer (codex-ide-session-buffer session)
					  (should (string-match-p "example\\.el:1"
								  (format "%s" header-line-format))))))))))

  (ert-deftest codex-ide-prompt-displays-session-buffer-in-other-window ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (captured-action nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-test-window-action")
				      (cl-letf (((symbol-function 'read-from-minibuffer)
						 (lambda (&rest _args) "Explain this"))
						((symbol-function 'codex-ide--ensure-session-for-current-project)
						 (lambda () session))
						((symbol-function 'codex-ide-display-buffer)
						 (lambda (_buffer &optional action)
						   (setq captured-action action)
						   (selected-window)))
						((symbol-function 'codex-ide--request-sync)
						 (lambda (&rest _args) nil)))
					(codex-ide-prompt)))))
      (should (equal captured-action
                     '((display-buffer-reuse-window
			display-buffer-use-some-window
			display-buffer-pop-up-window)
                       (inhibit-same-window . t))))))

  (ert-deftest codex-ide-submit-renders-sent-context-below-prompt ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (file-path (codex-ide-test--make-project-file
                       project-dir "src/example.el" "(message \"hello\")\n"))
           (submitted nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-test-context-line")
				      (with-current-buffer (find-file-noselect file-path)
					(setq-local default-directory (file-name-as-directory project-dir))
					(goto-char (point-min))
					(forward-char 3)
					(let ((context (codex-ide--make-buffer-context)))
					  (puthash (alist-get 'project-dir context)
						   context
						   codex-ide--active-buffer-contexts)))
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "Explain this")
					(cl-letf (((symbol-function 'codex-ide--request-sync)
						   (lambda (_session _method params)
						     (setq submitted params)
						     nil)))
					  (codex-ide--submit-prompt)
					  (let ((buffer-text (buffer-string))
						(input (alist-get 'input submitted)))
					    (should (string-match-p
						     "\n> Explain this\n\nContext: example\\.el 1:3"
						     buffer-text))
					    (goto-char (point-min))
					    (re-search-forward "^> Explain this$")
					    (beginning-of-line)
					    (should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					    (should (string-match-p "\\[Emacs prompt context\\]"
								    (alist-get 'text (aref input 0))))
					    (should (string-match-p "\\[/Emacs prompt context\\]"
								    (alist-get 'text (aref input 0))))))))))))

  (ert-deftest codex-ide-submit-steers-running-turn-by-default ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-running-submit-action 'steer)
          (submitted nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-steer-1"
					    (codex-ide-session-current-turn-id session) "turn-steer-1"
					    (codex-ide-session-output-prefix-inserted session) t
					    (codex-ide-session-status session) "running")
				      (with-current-buffer (codex-ide-session-buffer session)
					(setq-local codex-ide--session session))
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "Actually run tests first")
					(cl-letf (((symbol-function 'codex-ide--request-sync)
						   (lambda (_session method params)
						     (setq submitted (list method params))
						     '((turnId . "turn-steer-1")))))
					  (codex-ide-submit)))
				      (should (equal (car submitted) "turn/steer"))
				      (let* ((params (cadr submitted))
					     (input (alist-get 'input params)))
					(should (equal (alist-get 'threadId params) "thread-steer-1"))
					(should (equal (alist-get 'expectedTurnId params) "turn-steer-1"))
					(should (string-match-p "Actually run tests first"
								(alist-get 'text (aref input 0)))))
				      (with-current-buffer (codex-ide-session-buffer session)
					(should (codex-ide--input-prompt-active-p session))
					(goto-char (marker-position
						    (codex-ide-session-input-prompt-start-marker session)))
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					(codex-ide--append-to-buffer (current-buffer) "sleep 10 still running.")
					(should (string-match-p
						 (rx "Actually run tests first"
						     "\nsleep 10 still running.")
						 (buffer-string)))
					(should-not (string-match-p "Steered this turn:" (buffer-string)))
					(goto-char (point-min))
					(search-forward "sleep 10 still running.")
					(should-not (eq (get-text-property (match-beginning 0) 'face)
							'codex-ide-user-prompt-face))))))))

  (ert-deftest codex-ide-submit-queues-running-turn-when-configured ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-running-submit-action 'queue)
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-queue-1"
					    (codex-ide-session-current-turn-id session) "turn-current"
					    (codex-ide-session-output-prefix-inserted session) t
					    (codex-ide-session-status session) "running")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "Do this next")
					(cl-letf (((symbol-function 'codex-ide--request-sync)
						   (lambda (_session method params)
						     (push (list method params) requests)
						     '((turn . ((id . "turn-next")))))))
					  (codex-ide-submit)
					  (should (= (length (codex-ide--queued-prompts session)) 1))
					  (should (string-match-p
						   (rx "Queued turns:"
						       "\n  1. Do this next"
						       "\n\n"
						       string-end)
						   (buffer-string)))
					  (goto-char (marker-position
						      (codex-ide-session-input-prompt-start-marker session)))
					  (should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					  (codex-ide--replace-current-input session "And then this")
					  (codex-ide-submit)
					  (should (= (length (codex-ide--queued-prompts session)) 2))
					  (should (string-match-p
						   (rx "Queued turns:"
						       "\n  1. Do this next"
						       "\n  2. And then this"
						       "\n\n\n> \n\n"
						       string-end)
						   (buffer-string)))
					  (codex-ide--handle-notification
					   session
					   '((method . "turn/completed")
					     (params . ((turn . ((id . "turn-current")))))))))
				      (should (= (length requests) 1))
				      (should (equal (caar requests) "turn/start"))
				      (let* ((params (cadar requests))
					     (input (alist-get 'input params)))
					(should (equal (alist-get 'threadId params) "thread-queue-1"))
					(should (string-match-p "Do this next"
								(alist-get 'text (aref input 0)))))
				      (should (= (length (codex-ide--queued-prompts session)) 1))
				      (with-current-buffer (codex-ide-session-buffer session)
					(should-not (string-match-p
						     "Working\\.\\.\\."
						     (buffer-string)))
					(should (equal (codex-ide-test--input-placeholder-text session)
                                                       "Working..."))
					(should (string-match-p
						 (rx "Queued turns:" "\n  1. And then this")
						 (buffer-string)))))))))

  (ert-deftest codex-ide-submit-includes-reasoning-effort-when-configured ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (submitted nil)
          (codex-ide-reasoning-effort "high"))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-test-effort")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "Explain this")
					(cl-letf (((symbol-function 'codex-ide--request-sync)
						   (lambda (_session _method params)
						     (setq submitted params)
						     nil)))
					  (codex-ide--submit-prompt)))
				      (should (equal (alist-get 'effort submitted) "high")))))))

  (ert-deftest codex-ide-submit-includes-model-when-configured ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (submitted nil)
          (codex-ide-model "gpt-5.4-mini"))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-test-model")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "Explain this")
					(cl-letf (((symbol-function 'codex-ide--request-sync)
						   (lambda (_session _method params)
						     (setq submitted params)
						     nil)))
					  (codex-ide--submit-prompt)))
				      (should (equal (alist-get 'model submitted) "gpt-5.4-mini")))))))

  (ert-deftest codex-ide-submit-includes-live-thread-settings-when-configured ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (submitted nil)
          (codex-ide-approval-policy "never")
          (codex-ide-sandbox-mode "workspace-write")
          (codex-ide-personality "friendly"))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-test-live-settings")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "Explain this")
					(cl-letf (((symbol-function 'codex-ide--request-sync)
						   (lambda (_session _method params)
						     (setq submitted params)
						     nil)))
					  (codex-ide--submit-prompt)))
				      (should (equal (alist-get 'approvalPolicy submitted) "never"))
				      (should (equal (alist-get 'sandboxPolicy submitted)
						     `((type . "workspaceWrite")
						       (writableRoots . [,(codex-ide--get-working-directory)]))))
				      (should (equal (alist-get 'personality submitted) "friendly")))))))

  (ert-deftest codex-ide-submit-remembers-submitted-model-for-header ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-model "gpt-5.4-mini"))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (updated nil))
				      (setf (codex-ide-session-thread-id session) "thread-test-model-header")
				      (codex-ide--session-metadata-put session :model-name "gpt-5.3")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "Explain this")
					(cl-letf (((symbol-function 'codex-ide--request-sync)
						   (lambda (&rest _) nil))
						  ((symbol-function 'codex-ide--update-header-line)
						   (lambda (_session)
						     (setq updated t))))
					  (codex-ide--submit-prompt)))
				      (should updated)
				      (should (equal (codex-ide--server-model-name session)
						     "gpt-5.4-mini")))))))

  (ert-deftest codex-ide-header-line-uses-updated-compact-format ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (file-path
            (codex-ide-test--make-project-file
             project-dir "src/codex-ide-transcript.el" "(message \"hello\")\n")))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (codex-ide--session-metadata-put
				       session :rate-limits
				       '((primary . ((usedPercent . 15)
						     (windowDurationMins . 300)))
					 (secondary . ((usedPercent . 3)
						       (windowDurationMins . 10080)))
					 (planType . "prolite")))
				      (codex-ide--session-metadata-put
				       session :token-usage
				       '((total . ((totalTokens . 305500)))
					 (modelContextWindow . 258400)
					 (last . ((inputTokens . 42800)
						  (cachedInputTokens . 26100)
						  (outputTokens . 244)
						  (reasoningOutputTokens . 68)))))
				      (with-current-buffer (find-file-noselect file-path)
					(setq-local default-directory (file-name-as-directory project-dir))
					(rename-buffer "focused-source-buffer" t)
					(goto-char (point-min))
					(forward-line 0)
					(forward-char 1)
					(codex-ide--track-active-buffer (current-buffer)))
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--update-header-line session)
					(should
					 (equal
					  (format-mode-line header-line-format)
					  "Focus: focused-source-buffer | Model: gpt-5.4 | Quota: 15%/5h 3%/wk (prolite) | Context: 305.5k/258.4k | Last[in,cache,out,reason]: 42.8k,26.1k,244,68"))))))))

  (ert-deftest codex-ide-header-line-shows-model-name ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--update-header-line session)
					(should (string-match-p "Model: gpt-5\\.4"
								(substring-no-properties header-line-format)))))))))

  (ert-deftest codex-ide-header-line-shows-model-reasoning-effort-when-set ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (codex-ide--session-metadata-put session :reasoning-effort "high")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--update-header-line session)
					(should (string-match-p "Model: gpt-5\\.4 (high)"
								(format-mode-line header-line-format)))))))))

  (ert-deftest codex-ide-header-line-prefers-session-model-over-global-default ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-model "gpt-5.4"))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-1")
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4-mini")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--update-header-line session)
					(should (string-match-p "Model: gpt-5\\.4-mini"
								(substring-no-properties header-line-format)))
					(should-not (string-match-p "Model: gpt-5\\.4\\([^.-]\\|$\\)"
								    (substring-no-properties header-line-format)))))))))

  (ert-deftest codex-ide-header-line-requests-server-model-when-session-model-is-unset ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-model "gpt-5.4")
          (requested nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--ensure-server-model-name)
						 (lambda (_session)
						   (setq requested t))))
					(with-current-buffer (codex-ide-session-buffer session)
					  (codex-ide--update-header-line session)
					  (should requested)
					  (should-not (string-match-p "Model:"
								      (substring-no-properties header-line-format))))))))))

  (ert-deftest codex-ide-header-line-uses-server-model-when-local-model-is-unset ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-model nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--server-model-name)
						 (lambda (_session)
						   "gpt-5.4-mini")))
					(with-current-buffer (codex-ide-session-buffer session)
					  (codex-ide--update-header-line session)
					  (should (string-match-p "Model: gpt-5\\.4-mini"
								  (substring-no-properties header-line-format))))))))))

  (ert-deftest codex-ide-available-model-names-queries-model-list ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requested-method nil)
          (requested-params nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--ensure-cli)
						 (lambda () t))
						((symbol-function 'codex-ide--cleanup-dead-sessions)
						 (lambda () nil))
						((symbol-function 'codex-ide--ensure-active-buffer-tracking)
						 (lambda () nil))
						((symbol-function 'codex-ide--query-session-for-thread-selection)
						 (lambda (&optional _directory) session))
						((symbol-function 'codex-ide--request-sync)
						 (lambda (_session method params)
						   (setq requested-method method
							 requested-params params)
						   '((data . (((id . "gpt-5.4") (model . "gpt-5.4"))
							      ((id . "gpt-5.4-mini") (model . "gpt-5.4-mini"))))
						     (nextCursor . nil)))))
					(should (equal (codex-ide--available-model-names)
						       '("gpt-5.4" "gpt-5.4-mini")))
					(should (equal requested-method "model/list"))
					(should (equal requested-params '((limit . 100))))))))))

  (ert-deftest codex-ide-config-read-sends-object-params-with-cwd ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requested nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--request-sync)
						 (lambda (_session method params)
						   (setq requested (cons method params))
						   '((config . ((model . "gpt-5.4")))))))
					(should (equal (codex-ide--config-read session)
						       '((config . ((model . "gpt-5.4"))))))
					(should (equal (car requested) "config/read"))
					(should (equal (cdr requested)
						       `((includeLayers . :json-false)
							 (cwd . ,(codex-ide-session-directory session)))))))))))

  (ert-deftest codex-ide-server-model-name-prefers-config-read-model ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil)
          (requested-params nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--request-async)
						 (lambda (_session method params callback)
						   (push method requests)
						   (push params requested-params)
						   (funcall callback
							    '((config . ((model . "gpt-5.4"))))
							    nil)
						   1)))
					(should-not (codex-ide--server-model-name session))
					(codex-ide--ensure-server-model-name session)
					(should (equal (codex-ide--server-model-name session) "gpt-5.4"))
					(should (equal requests '("config/read")))
					(should (equal (car requested-params)
						       `((includeLayers . :json-false)
							 (cwd . ,(codex-ide-session-directory session)))))
					(should-not (codex-ide--session-metadata-get
						     session
						     :model-name-requested))
					(codex-ide--ensure-server-model-name session)
					(should (equal requests '("config/read")))))))))

  (ert-deftest codex-ide-server-model-name-ignores-stale-config-read-after-model-known ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (callback nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--request-async)
						 (lambda (_session method _params cb)
						   (should (equal method "config/read"))
						   (setq callback cb)
						   1)))
					(codex-ide--ensure-server-model-name session)
					(should callback)
					(codex-ide--set-session-model-name session "gpt-5.4")
					(funcall callback '((config . ((model . "gpt-5.3")))) nil)
					(should (equal (codex-ide--server-model-name session)
						       "gpt-5.4"))
					(should-not (codex-ide--session-metadata-get
						     session
						     :model-name-requested))))))))

  (ert-deftest codex-ide-item-completed-remembers-session-model-name ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (updated nil))
				      (cl-letf (((symbol-function 'codex-ide--update-header-line)
						 (lambda (_session)
						   (setq updated t))))
					(codex-ide--handle-notification
					 session
					 '((method . "item/completed")
					   (params . ((item . ((id . "item-1")
							       (type . "agentMessage")
							       (model . "gpt-5.4-mini")
							       (status . "completed"))))))))
				      (should (equal (codex-ide--server-model-name session)
						     "gpt-5.4-mini"))
				      (should updated))))))

  (ert-deftest codex-ide-item-started-updates-session-model-name ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (updated nil))
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (cl-letf (((symbol-function 'codex-ide--update-header-line)
						 (lambda (_session)
						   (setq updated t))))
					(codex-ide--handle-notification
					 session
					 '((method . "item/started")
					   (params . ((item . ((id . "item-1")
							       (type . "agentMessage")
							       (model . "gpt-5.4-mini"))))))))
				      (should (equal (codex-ide--server-model-name session)
						     "gpt-5.4-mini"))
				      (should updated))))))

  (ert-deftest codex-ide-item-started-refreshes-server-model-when-payload-lacks-model ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (updated nil))
				      (codex-ide--session-metadata-put session :model-name :unknown)
				      (codex-ide--session-metadata-put session :model-name-requested t)
				      (cl-letf (((symbol-function 'codex-ide--request-async)
						 (lambda (_session method params callback)
						   (push (cons method params) requests)
						   (funcall callback
							    '((config . ((model . "gpt-5.4"))))
							    nil)
						   1))
						((symbol-function 'codex-ide--update-header-line)
						 (lambda (_session)
						   (setq updated t))))
					(codex-ide--handle-notification
					 session
					 '((method . "item/started")
					   (params . ((item . ((id . "item-1")
							       (type . "reasoning"))))))))
				      (should (equal (codex-ide--server-model-name session) "gpt-5.4"))
				      (should-not (codex-ide--session-metadata-get
						   session
						   :model-name-requested))
				      (should updated)
				      (should (equal (nreverse requests)
						     `(("config/read"
							(includeLayers . :json-false)
							(cwd . ,(codex-ide-session-directory session)))))))))))

  (ert-deftest codex-ide-item-started-does-not-refresh-known-session-model ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (updated nil))
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (cl-letf (((symbol-function 'codex-ide--request-async)
						 (lambda (&rest args)
						   (push args requests)
						   (ert-fail "Did not expect config/read refresh")))
						((symbol-function 'codex-ide--update-header-line)
						 (lambda (_session)
						   (setq updated t))))
					(codex-ide--handle-notification
					 session
					 '((method . "item/started")
					   (params . ((item . ((id . "item-1")
							       (type . "reasoning"))))))))
				      (should-not updated)
				      (should (equal (codex-ide--server-model-name session) "gpt-5.4"))
				      (should-not requests))))))

  (ert-deftest codex-ide-web-search-completion-renders-query-details-from-completed-item ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (primary-query
					   "site:docs.astral.sh uv index lockfile environment variable index-url lockfile localhost")
					  (secondary-query
					   "site:docs.astral.sh uv lockfile embeds index URL sources index strategy"))
				      (codex-ide--handle-notification
				       session
				       '((method . "turn/started")
					 (params . ((turn . ((id . "turn-1")))))))
				      (codex-ide--handle-notification
				       session
				       '((method . "item/started")
					 (params . ((item . ((type . "webSearch")
							     (id . "web-search-1")
							     (query . "")
							     (action . ((type . "other")))))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/completed")
					 (params . ((item . ((type . "webSearch")
							     (id . "web-search-1")
							     (status . "completed")
							     (query . ,primary-query)
							     (action . ((type . "search")
									(query . ,primary-query)
									(queries . (,primary-query
										    ,secondary-query))))))))))
				      (with-current-buffer (codex-ide-session-buffer session)
					(let ((text (buffer-string)))
					  (should (string-match-p "\\* Searched the web" text))
					  (should (string-match-p
						   (regexp-quote (format "  └ %s" primary-query))
						   text))
					  (should (string-match-p
						   (regexp-quote (format "  └ %s" secondary-query))
						   text)))))))))

  (ert-deftest codex-ide-turn-started-does-not-refresh-known-session-model ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session))
					  (updated nil))
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (cl-letf (((symbol-function 'codex-ide--request-async)
						 (lambda (&rest args)
						   (push args requests)
						   (ert-fail "Did not expect config/read refresh")))
						((symbol-function 'codex-ide--update-header-line)
						 (lambda (_session)
						   (setq updated t))))
					(codex-ide--handle-notification
					 session
					 '((method . "turn/started")
					   (params . ((turn . ((id . "turn-1")))))))
					(should updated)
					(should (equal (codex-ide--server-model-name session) "gpt-5.4"))
					(should-not requests)))))))

  (ert-deftest codex-ide-resume-thread-into-session-prefers-thread-model-over-local-default ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-model "gpt-5.4")
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method params)
						 (push (cons method params) requests)
						 (pcase method
						   ("thread/read"
						    '((thread . ((id . "thread-explicit-1")
								 (model . "gpt-5.4-mini")
								 (name . "Explicit flow")
								 (preview . "Replay exact thread")))
						      (turns . (((id . "turn-1")
								 (items . (((type . "userMessage")
									    (content . (((type . "text")
											 (text . "Resume this exact thread.")))))
									   ((type . "agentMessage")
									    (id . "item-1")
									    (text . "Exact thread resumed.")))))))))
						   ("thread/resume" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method))))))
					      ((symbol-function 'codex-ide--request-async)
					       (lambda (&rest _) 1)))
				      (let ((session (codex-ide--create-process-session)))
					(should (eq (codex-ide--resume-thread-into-session
						     session "thread-explicit-1" "Resumed")
						    session))
					(should (equal (codex-ide--server-model-name session)
						       "gpt-5.4-mini"))
					(should (equal (mapcar #'car (nreverse requests))
						       '("thread/read" "thread/resume")))))))))

  (ert-deftest codex-ide-server-model-name-becomes-unknown-when-config-read-has-no-model ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--request-async)
						 (lambda (_session method _params callback)
						   (push method requests)
						   (should (equal method "config/read"))
						   (funcall callback '((config . ((approvalPolicy . "never")))) nil)
						   1)))
					(should-not (codex-ide--server-model-name session))
					(codex-ide--ensure-server-model-name session)
					(should (equal (codex-ide--server-model-name session)
						       "unknown"))
					(should (equal requests '("config/read")))))))))

  (ert-deftest codex-ide-server-model-name-becomes-unknown-when-config-read-errors ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (cl-letf (((symbol-function 'codex-ide--request-async)
						 (lambda (_session method _params callback)
						   (push method requests)
						   (should (equal method "config/read"))
						   (funcall callback nil '((message . "boom")))
						   1)))
					(should-not (codex-ide--server-model-name session))
					(codex-ide--ensure-server-model-name session)
					(should (equal (codex-ide--server-model-name session)
						       "unknown"))
					(should (equal requests '("config/read")))))))))

  (ert-deftest codex-ide-process-filter-handles-responses-notifications-and-partials ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (response-result nil)
          (response-error :unset))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let* ((session (codex-ide--create-process-session))
					   (process (codex-ide-session-process session)))
				      (puthash 7
					       (lambda (result error)
						 (setq response-result result
						       response-error error))
					       (codex-ide-session-pending-requests session))
				      (codex-ide--process-filter
				       process
				       "{\"id\":7,\"result\":{\"ok\":true}}\n{\"method\":\"thread/status/changed\",\"params\":{\"thread\":{\"status\":\"running\"}}}")
				      (should (equal (codex-ide-session-partial-line session)
						     "{\"method\":\"thread/status/changed\",\"params\":{\"thread\":{\"status\":\"running\"}}}"))
				      (codex-ide--process-filter process "\n")
				      (should (equal response-result '((ok . t))))
				      (should (null response-error))
				      (should (string= (codex-ide-session-status session) "running")))))))

  (ert-deftest codex-ide-command-approval-renders-inline-buttons-and-resolves-on-click ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (displayed-buffer nil)
          (message-text nil)
          (codex-ide-model "gpt-5.4"))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let* ((session (codex-ide--create-process-session))
					   (process (codex-ide-session-process session)))
				      (setf (codex-ide-session-current-turn-id session) "turn-approval-1"
					    (codex-ide-session-status session) "running")
				      (codex-ide--insert-input-prompt session nil)
				      (codex-ide--refresh-input-placeholder session)
				      (should (equal (codex-ide-test--input-placeholder-text session)
						     "Running..."))
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (cl-letf (((symbol-function 'run-at-time)
						 (lambda (_time _repeat function)
						   (funcall function)))
						((symbol-function 'codex-ide-display-buffer)
						 (lambda (buffer &optional _action)
						   (setq displayed-buffer buffer)
						   (selected-window)))
						((symbol-function 'message)
						 (lambda (format-string &rest args)
						   (setq message-text (apply #'format format-string args))))
						((symbol-function 'completing-read)
						 (lambda (&rest _)
						   (ert-fail "approval should not use completing-read"))))
					(codex-ide--handle-command-approval
					 session
					 42
					 '((command . "git status")
					   (reason . "inspect worktree")
					   (proposedExecpolicyAmendment . ["git" "status"]))))
				      (should (eq displayed-buffer (codex-ide-session-buffer session)))
				      (should (equal message-text
						     (format "Codex approval required in %s"
							     (buffer-name (codex-ide-session-buffer session)))))
				      (should (string= (codex-ide-session-status session) "approval"))
				      (should (equal (codex-ide-test--input-placeholder-text session)
						     "Seeking approval..."))
				      (should (string-match-p "Codex:Approval"
							      (codex-ide-renderer-mode-line-status session)))
				      (should (= (hash-table-count (codex-ide--pending-approvals session)) 1))
				      (with-current-buffer (codex-ide-session-buffer session)
					(let ((text (buffer-string))
					      (separator (string-trim-right
							  (codex-ide-renderer-output-separator-string))))
					  (should (string-match-p
						   (concat "\n\n"
							   (regexp-quote separator)
							   "\n\n\\[Approval required\\]\n\n"
							   "Run the following command\\?\n\n"
							   "    git status\n\n"
							   "Reason: inspect worktree\n"
							   "\\[accept\\]\n"
							   "\\[accept for session\\]\n"
							   "\\[accept and allow prefix (git status)\\]\n"
							   "\\[decline\\]\n"
							   "\\[cancel turn\\]\n\n")
						   text))
					  (should (string-match-p "Reason: inspect worktree" text))
					  (should-not (string-match-p "Codex approval required" text))
					  (should-not (string-match-p "Proposed prefix:" text))
					  (should-not (string-match-p "Status: Pending" text))
					  (should-not (string-match-p "Choose:" text))
					  (should (string-match-p "\\[accept for session\\]" text)))
					(goto-char (point-min))
					(search-forward (string-trim-right
							 (codex-ide-renderer-output-separator-string)))
					(should (eq (get-text-property (match-beginning 0) 'face)
						    'codex-ide-output-separator-face))
					(search-forward "[Approval required]")
					(should (eq (get-text-property (match-beginning 0) 'face)
						    'codex-ide-approval-header-face))
					(search-forward "Run the following command?")
					(should (eq (get-text-property (match-beginning 0) 'face)
						    'codex-ide-approval-label-face))
					(search-forward "git status")
					(should (eq (get-text-property (match-beginning 0) 'face)
						    'codex-ide-item-summary-face))
					(goto-char (point-min))
					(search-forward "[accept for session]")
					(backward-char 1)
					(push-button))
				      (let* ((sent (codex-ide-test-process-sent-strings process))
					     (payload (json-parse-string (car sent)
									 :object-type 'alist
									 :array-type 'list)))
					(should (= (length sent) 1))
					(should (equal (alist-get 'id payload) 42))
					(should (equal (alist-get 'decision (alist-get 'result payload))
						       "acceptForSession")))
				      (should (= (hash-table-count (codex-ide--pending-approvals session)) 0))
				      (should (string= (codex-ide-session-status session) "running"))
				      (with-current-buffer (codex-ide-session-buffer session)
					(should (string-match-p "\\[cancel turn\\]\n\nSelected: accept for session\n[^ \n]"
								(concat (buffer-string) "x")))
					(goto-char (point-max))
					(search-backward "Selected:")
					(should (eq (get-text-property (point) 'face)
						    'codex-ide-approval-label-face))
					(goto-char (point-min))
					(search-forward "[accept for session]")
					(backward-char 1)
					(should-not (button-at (point))))
				      (should (= (length (codex-ide-test-process-sent-strings process)) 1)))))))

  (ert-deftest codex-ide-command-approval-navigation-within-block-preserves-tail-follow ()
    (save-window-excursion
      (delete-other-windows)
      (let ((project-dir (codex-ide-test--make-temp-project))
            (codex-ide-model "gpt-5.4"))
	(codex-ide-test-with-fixture project-dir
				     (codex-ide-test-with-fake-processes
				      (let* ((session (codex-ide--create-process-session))
					     (buffer (codex-ide-session-buffer session))
					     (window (selected-window)))
					(setf (codex-ide-session-current-turn-id session) "turn-approval-preserve"
					      (codex-ide-session-status session) "running")
					(cl-letf (((symbol-function 'run-at-time)
						   (lambda (_time _repeat function)
						     (funcall function)))
						  ((symbol-function 'codex-ide-display-buffer)
						   (lambda (_buffer &optional _action) window))
						  ((symbol-function 'message)
						   (lambda (&rest _) nil)))
					  (codex-ide--handle-command-approval
					   session
					   42
					   '((command . "git status")
					     (reason . "inspect worktree"))))
					(set-window-buffer window buffer)
					(with-selected-window window
					  (goto-char (point-max))
					  (setq-local codex-ide-session-mode--last-point (point))
					  (setq-local codex-ide-session-mode--last-window-start (window-start))
					  (forward-line -1)
					  (codex-ide-session-mode--track-tail-follow-navigation))
					(should-not (window-parameter window 'codex-ide-tail-follow-suspended))))))))

  (ert-deftest codex-ide-command-approval-navigation-above-block-suspends-tail-follow ()
    (save-window-excursion
      (delete-other-windows)
      (let ((project-dir (codex-ide-test--make-temp-project))
            (codex-ide-model "gpt-5.4"))
	(codex-ide-test-with-fixture project-dir
				     (codex-ide-test-with-fake-processes
				      (let* ((session (codex-ide--create-process-session))
					     (buffer (codex-ide-session-buffer session))
					     (window (selected-window))
					     approval-start)
					(setf (codex-ide-session-current-turn-id session) "turn-approval-suspend"
					      (codex-ide-session-status session) "running")
					(cl-letf (((symbol-function 'run-at-time)
						   (lambda (_time _repeat function)
						     (funcall function)))
						  ((symbol-function 'codex-ide-display-buffer)
						   (lambda (_buffer &optional _action) window))
						  ((symbol-function 'message)
						   (lambda (&rest _) nil)))
					  (codex-ide--handle-command-approval
					   session
					   42
					   '((command . "git status")
					     (reason . "inspect worktree"))))
					(setq approval-start
					      (marker-position
					       (plist-get (gethash 42 (codex-ide--pending-approvals session))
							  :start-marker)))
					(set-window-buffer window buffer)
					(with-selected-window window
					  (goto-char (point-max))
					  (setq-local codex-ide-session-mode--last-point (point))
					  (setq-local codex-ide-session-mode--last-window-start (window-start))
					  (goto-char (max (point-min) (1- approval-start)))
					  (codex-ide-session-mode--track-tail-follow-navigation))
					(should (window-parameter window 'codex-ide-tail-follow-suspended))))))))

  (ert-deftest codex-ide-command-approval-strips-shell-wrapper-without-summarizing ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (codex-ide-model "gpt-5.4"))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-current-turn-id session) "turn-approval-1"
					    (codex-ide-session-status session) "running")
				      (codex-ide--session-metadata-put session :model-name "gpt-5.4")
				      (cl-letf (((symbol-function 'run-at-time)
						 (lambda (_time _repeat function)
						   (funcall function)))
						((symbol-function 'codex-ide-display-buffer)
						 (lambda (_buffer &optional _action) (selected-window)))
						((symbol-function 'message)
						 (lambda (&rest _) nil)))
					(codex-ide--handle-command-approval
					 session
					 43
					 '((command . "/bin/zsh -lc \"rg -n 'summarizes-rg|summarizes-sed' tests/codex-ide-tests.el codex-ide-renderer.el\"")
					   (reason . "inspect matches"))))
				      (with-current-buffer (codex-ide-session-buffer session)
					(let ((text (buffer-string)))
					  (should (string-match-p
						   "    rg -n 'summarizes-rg|summarizes-sed' tests/codex-ide-tests\\.el codex-ide-renderer\\.el\n\n"
						   text))
					  (should-not (string-match-p "/bin/zsh -lc" text))
					  (should-not (string-match-p "Searched 2 paths" text)))))))))

  (ert-deftest codex-ide-approval-resolution-stays-above-running-prompt-output ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-current-turn-id session) "turn-approval-2"
					    (codex-ide-session-output-prefix-inserted session) t
					    (codex-ide-session-status session) "running")
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "draft"))
				      (codex-ide--render-item-start
				       session
				       '((id . "call-approved")
					 (type . "commandExecution")
					 (command . "git status")
					 (cwd . "/tmp")))
				      (cl-letf (((symbol-function 'run-at-time)
						 (lambda (_time _repeat function)
						   (funcall function)))
						((symbol-function 'codex-ide-display-buffer)
						 (lambda (_buffer &optional _action) (selected-window)))
						((symbol-function 'message)
						 (lambda (&rest _) nil)))
					(codex-ide--handle-command-approval
					 session
					 43
					 '((command . "git status"))))
				      (with-current-buffer (codex-ide-session-buffer session)
					(goto-char (point-min))
					(search-forward "[accept]")
					(backward-char 1)
					(push-button)
					(codex-ide--replace-current-input session "steer while approved command runs")
					(codex-ide--freeze-active-input-prompt session)
					(codex-ide--insert-input-prompt session)
					(codex-ide--render-item-completion
					 session
					 '((id . "call-approved")
					   (type . "commandExecution")
					   (status . "completed")
					   (aggregatedOutput . "Switched to branch main\nYour branch is up to date\n")))
					(should (string-match-p
						 (rx "Selected: accept"
						     (* anything)
						     "\n  " (or "└" "\342\224\224") " output: 2 lines"
						     (* anything))
						 (buffer-string)))
					(should (string-match-p "steer while approved command runs\\'"
								(buffer-string)))
					(goto-char (marker-position
						    (codex-ide-session-input-prompt-start-marker session)))
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> ")))))))))

(ert-deftest codex-ide-command-approval-does-not-display-nonvisible-buffer-when-disabled ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (message-text nil)
        (codex-ide-buffer-display-when-approval-required nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-current-turn-id session) "turn-approval-hidden"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'run-at-time)
					       (lambda (_time _repeat function)
						 (funcall function)))
					      ((symbol-function 'get-buffer-window)
					       (lambda (&rest _) nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (&rest _)
						 (ert-fail "hidden approval buffer should not be displayed")))
					      ((symbol-function 'message)
					       (lambda (format-string &rest args)
						 (setq message-text (apply #'format format-string args)))))
				      (codex-ide--handle-command-approval
				       session
				       44
				       '((command . "sort"))))
				    (should (string= (codex-ide-session-status session) "approval"))
				    (should (= (hash-table-count (codex-ide--pending-approvals session)) 1))
				    (should (equal message-text
						   (format "Codex approval required in %s"
							   (buffer-name (codex-ide-session-buffer session)))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (string-match-p "\\[Approval required\\]"
							      (buffer-string)))))))))

(ert-deftest codex-ide-file-change-approval-renders-diff-before-buttons ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (process (codex-ide-session-process session))
					 (opened-diff nil)
					 (opened-diff-buffer-name nil)
					 (expected-diff
					  (string-join
					   '("diff --git a/foo.txt b/foo.txt"
					     "--- a/foo.txt"
					     "+++ b/foo.txt"
					     "@@ -1 +1 @@"
					     "-old"
					     "+new")
					   "\n"))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-approval"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'run-at-time)
					       (lambda (_time _repeat function)
						 (funcall function)))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide-diff-open-buffer)
					       (lambda (text &optional buffer-name)
						 (setq opened-diff text)
						 (setq opened-diff-buffer-name buffer-name)
						 nil))
					      ((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-file-change-approval
				       session
				       45
				       '((itemId . "file-change-1")
					 (reason . "edit foo.txt")))
				      (with-current-buffer (codex-ide-session-buffer session)
					(let ((text (buffer-string)))
					  (should (string-match-p "Approve file changes: edit foo\\.txt" text))
					  (should (string-match-p "Proposed changes:\n\n" text))
					  (should (< (string-match-p "Proposed changes:" text)
						     (string-match-p "diff: foo\\.txt (\\+1/-1, 6 lines) \\[fold\\] \\[open diff\\]" text)))
					  (should (< (string-match-p "\\[open diff\\]" text)
						     (string-match-p "\\[accept\\]" text)))
					  (should (string-match-p "diff --git a/foo\\.txt b/foo\\.txt" text))
					  (should (string-match-p "-old" text))
					  (should (string-match-p "+new" text)))
					(goto-char (point-min))
					(search-forward "Proposed changes:")
					(should (eq (get-text-property (match-beginning 0) 'face)
						    'codex-ide-approval-label-face))
					(search-forward "-old")
					(should (eq (get-text-property (match-beginning 0) 'face)
						    'codex-ide-file-diff-removed-face))
					(search-forward "+new")
					(should (eq (get-text-property (match-beginning 0) 'face)
						    'codex-ide-file-diff-added-face))
					(goto-char (point-min))
					(search-forward "[open diff]")
					(backward-char 1)
					(button-activate (button-at (point)))
					(should (equal opened-diff expected-diff))
					(should (equal opened-diff-buffer-name
						       (codex-ide-diff-buffer-name-for-session
							(codex-ide-session-buffer session))))
					(goto-char (point-min))
					(search-forward "[accept]")
					(backward-char 1)
					(button-activate (button-at (point)))))
				    (let* ((payloads
					    (mapcar (lambda (json)
						      (json-parse-string json
									 :object-type 'alist
									 :array-type 'list))
						    (codex-ide-test-process-sent-strings process)))
					   (payload (seq-find (lambda (item)
								(equal (alist-get 'id item) 45))
							      payloads)))
				      (should payload)
				      (should (equal (alist-get 'id payload) 45))
				      (should (equal (alist-get 'decision (alist-get 'result payload))
						     "accept")))
				    (codex-ide--handle-notification
				     session
				     `((method . "item/completed")
				       (params . ((item . ((type . "fileChange")
							   (id . "file-change-1")
							   (changes . (((path . "foo.txt")
									(diff . ,diff-text))))
							   (status . "completed")))))))
				    (codex-ide--handle-notification
				     session
				     `((method . "item/completed")
				       (params . ((item . ((type . "fileChange")
							   (id . "file-change-1")
							   (changes . (((path . "foo.txt")
									(diff . ,diff-text))))
							   (status . "completed")))))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let* ((text (buffer-string))
					     (first-diff (string-match "diff --git a/foo\\.txt b/foo\\.txt" text))
					     (first-diff-end (and first-diff (match-end 0))))
					(should first-diff)
					(should-not
					 (string-match-p "diff --git a/foo\\.txt b/foo\\.txt"
							 text
							 first-diff-end)))))))))

(ert-deftest codex-ide-diff-config-defaults ()
  (should (= codex-ide-diff-inline-fold-threshold 12))
  (should (eq codex-ide-diff-auto-display-policy 'never)))

(ert-deftest codex-ide-diff-data-combined-turn-diff-text-prefers-running-turn ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (diff-1 (string-join
						  '("diff --git a/foo.txt b/foo.txt"
						    "--- a/foo.txt"
						    "+++ b/foo.txt"
						    "@@ -1 +1 @@"
						    "-old"
						    "+new")
						  "\n"))
					 (diff-2 (string-join
						  '("diff --git a/bar.txt b/bar.txt"
						    "--- a/bar.txt"
						    "+++ b/bar.txt"
						    "@@ -2 +2 @@"
						    "-before"
						    "+after")
						  "\n")))
				    (codex-ide--register-submitted-turn-prompt session "submitted prompt")
				    (cl-letf (((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       '((method . "turn/started")
					 (params . ((turn . ((id . "turn-running")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-1))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/completed")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-1))))
							     (status . "completed")))))))
				      (codex-ide--handle-notification
				       session
				       '((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-2")
							     (changes . (((path . "bar.txt"))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/fileChange/outputDelta")
					 (params . ((itemId . "file-change-2")
						    (delta . ,diff-2)))))
				      (should
				       (equal (codex-ide-diff-data-combined-turn-diff-text
                                               session)
					      (concat
					       (codex-ide--file-change-diff-text
						`((type . "fileChange")
						  (changes . (((path . "foo.txt")
							       (diff . ,diff-1))))))
					       "\n\n"
					       diff-2)))))))))

(ert-deftest codex-ide-diff-data-combined-turn-diff-text-errors-when-no-diffs-exist ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (codex-ide--register-submitted-turn-prompt session "submitted prompt")
				    (setf (codex-ide-session-current-turn-id session) "turn-empty")
				    (should-error
				     (codex-ide-diff-data-combined-turn-diff-text
                                      session)
				     :type 'user-error))))))

(ert-deftest codex-ide-diff-data-combined-turn-diff-text-uses-tracked-completed-turn-when-idle ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n"))
					 (item `((type . "fileChange")
						 (id . "file-change-1")
						 (changes . (((path . "foo.txt")
							      (diff . ,diff-text)))))))
				    (codex-ide--register-submitted-turn-prompt session "submitted prompt")
				    (setf (codex-ide-session-current-turn-id session) "turn-completed")
				    (codex-ide--mark-current-turn-diff-started session "turn-completed")
				    (codex-ide--put-current-turn-file-change session "file-change-1" item)
				    (codex-ide--mark-current-turn-diff-completed session)
				    (setf (codex-ide-session-current-turn-id session) nil)
				    (cl-letf (((symbol-function 'codex-ide--read-turn-combined-diff-text)
					       (lambda (&rest _args)
						 (should nil))))
				      (should
				       (equal (codex-ide-diff-data-combined-turn-diff-text
                                               session)
					      (codex-ide--file-change-diff-text
					       item)))))))))

(ert-deftest codex-ide-file-change-completion-renders-open-diff-button ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-change"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/completed")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "completed"))))))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((text (buffer-string)))
					(should
					 (string-match-p
					  "diff: foo\\.txt (\\+1/-1, 6 lines) \\[fold\\] \\[open diff\\]"
					  text))
					(should (string-match-p "diff --git a/foo\\.txt b/foo\\.txt" text))
					(should (string-match-p "\\[open diff\\]" text))
					(should (< (string-match-p "\\[open diff\\]" text)
						   (string-match-p "diff --git a/foo\\.txt b/foo\\.txt"
								   text))))))))))

(ert-deftest codex-ide-file-change-open-diff-button-ret-opens-buffer ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-change-ret"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/completed")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "completed"))))))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (goto-char (point-min))
				      (search-forward "[open diff]")
				      (backward-char 1)
				      (let* ((button (button-at (point)))
					     (keymap (and button (button-get button 'keymap))))
					(should button)
					(should (eq (lookup-key keymap (kbd "RET"))
						    #'push-button))
					(should (eq (key-binding (kbd "RET"))
						    #'push-button)))))))))

(ert-deftest codex-ide-file-change-completion-does-not-auto-open-diff-by-default ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-diff-auto-display-policy 'approval-only))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (opened-diff nil)
					 (opened-diff-buffer-name nil)
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-change"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'codex-ide-diff-open-buffer)
					       (lambda (text &optional buffer-name)
						 (setq opened-diff text)
						 (setq opened-diff-buffer-name buffer-name)
						 nil))
					      ((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/completed")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "completed"))))))))
				    (should-not opened-diff)
				    (should-not opened-diff-buffer-name))))))

(ert-deftest codex-ide-file-change-completion-auto-opens-diff-when-policy-is-always ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-diff-auto-display-policy 'always))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (opened-diff nil)
					 (opened-diff-buffer-name nil)
					 (expected-diff
					  (string-join
					   '("diff --git a/foo.txt b/foo.txt"
					     "--- a/foo.txt"
					     "+++ b/foo.txt"
					     "@@ -1 +1 @@"
					     "-old"
					     "+new")
					   "\n"))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-change"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'codex-ide-diff-open-buffer)
					       (lambda (text &optional buffer-name)
						 (setq opened-diff text)
						 (setq opened-diff-buffer-name buffer-name)
						 nil))
					      ((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/completed")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "completed"))))))))
				    (should (equal opened-diff expected-diff))
				    (should (equal opened-diff-buffer-name
						   (codex-ide-diff-buffer-name-for-session
						    (codex-ide-session-buffer session)))))))))

(ert-deftest codex-ide-file-change-approval-auto-opens-diff-by-default ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-diff-auto-display-policy 'approval-only))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (opened-diff nil)
					 (opened-diff-buffer-name nil)
					 (expected-diff
					  (string-join
					   '("diff --git a/foo.txt b/foo.txt"
					     "--- a/foo.txt"
					     "+++ b/foo.txt"
					     "@@ -1 +1 @@"
					     "-old"
					     "+new")
					   "\n"))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-approval"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'run-at-time)
					       (lambda (_time _repeat function)
						 (funcall function)))
					      ((symbol-function 'get-buffer-window)
					       (lambda (&rest _)
						 (selected-window)))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide-diff-open-buffer)
					       (lambda (text &optional buffer-name)
						 (setq opened-diff text)
						 (setq opened-diff-buffer-name buffer-name)
						 nil))
					      ((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-file-change-approval
				       session
				       45
				       '((itemId . "file-change-1")
					 (reason . "edit foo.txt"))))
				    (should (equal opened-diff expected-diff))
				    (should (equal opened-diff-buffer-name
						   (codex-ide-diff-buffer-name-for-session
						    (codex-ide-session-buffer session)))))))))

(ert-deftest codex-ide-file-change-approval-auto-open-keeps-buttons-out-of-diff-buffer ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-diff-auto-display-policy 'approval-only))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (diff-buffer-name
					  (codex-ide-diff-buffer-name-for-session
					   (codex-ide-session-buffer session)))
					 diff-buffer
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-approval"
					  (codex-ide-session-status session) "running")
				    (unwind-protect
					(cl-letf (((symbol-function 'run-at-time)
						   (lambda (_time _repeat function)
						     (funcall function)))
						  ((symbol-function 'get-buffer-window)
						   (lambda (&rest _)
						     (selected-window)))
						  ((symbol-function 'codex-ide-display-buffer)
						   (lambda (buffer &optional _action)
						     ;; Simulate the auto-open path switching the current
						     ;; buffer while approval rendering is still active.
						     (when (equal (buffer-name buffer) diff-buffer-name)
						       (setq diff-buffer buffer)
						       (set-buffer buffer))
						     (selected-window)))
						  ((symbol-function 'message)
						   (lambda (&rest _) nil)))
					  (codex-ide--handle-notification
					   session
					   `((method . "item/started")
					     (params . ((item . ((type . "fileChange")
								 (id . "file-change-1")
								 (changes . (((path . "foo.txt")
									      (diff . ,diff-text))))
								 (status . "inProgress")))))))
					  (codex-ide--handle-file-change-approval
					   session
					   45
					   '((itemId . "file-change-1")
					     (reason . "edit foo.txt")))
					  (should (buffer-live-p diff-buffer))
					  (should (equal (buffer-name diff-buffer) diff-buffer-name))
					  (with-current-buffer (codex-ide-session-buffer session)
					    (let ((text (buffer-string)))
					      (should (string-match-p "\\[accept\\]" text))
					      (should (string-match-p "\\[accept for session\\]" text))
					      (should (string-match-p "\\[decline\\]" text))
					      (should (string-match-p "\\[cancel turn\\]" text))))
					  (with-current-buffer diff-buffer
					    (let ((text (buffer-string)))
					      (should-not (string-match-p "\\[accept\\]" text))
					      (should-not (string-match-p "\\[accept for session\\]" text))
					      (should-not (string-match-p "\\[decline\\]" text))
					      (should-not (string-match-p "\\[cancel turn\\]" text))
					      (should (string-match-p "diff --git a/foo\\.txt b/foo\\.txt" text)))))
				      (when (buffer-live-p diff-buffer)
					(kill-buffer diff-buffer))))))))

(ert-deftest codex-ide-file-change-approval-does-not-auto-open-diff-when-session-stays-hidden ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-diff-auto-display-policy 'approval-only)
        (codex-ide-buffer-display-when-approval-required nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (opened-diff nil)
					 (opened-diff-buffer-name nil)
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-approval-hidden"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'run-at-time)
					       (lambda (_time _repeat function)
						 (funcall function)))
					      ((symbol-function 'get-buffer-window)
					       (lambda (&rest _) nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (&rest _)
						 (ert-fail "hidden approval buffer should not be displayed")))
					      ((symbol-function 'codex-ide-diff-open-buffer)
					       (lambda (text &optional buffer-name)
						 (setq opened-diff text)
						 (setq opened-diff-buffer-name buffer-name)
						 nil))
					      ((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-file-change-approval
				       session
				       45
				       '((itemId . "file-change-1")
					 (reason . "edit foo.txt"))))
				    (should-not opened-diff)
				    (should-not opened-diff-buffer-name))))))

(ert-deftest codex-ide-file-change-approval-auto-opens-diff-when-session-is-already-visible ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-diff-auto-display-policy 'approval-only)
        (codex-ide-buffer-display-when-approval-required nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (opened-diff nil)
					 (opened-diff-buffer-name nil)
					 (expected-diff
					  (string-join
					   '("diff --git a/foo.txt b/foo.txt"
					     "--- a/foo.txt"
					     "+++ b/foo.txt"
					     "@@ -1 +1 @@"
					     "-old"
					     "+new")
					   "\n"))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-approval-visible"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'run-at-time)
					       (lambda (_time _repeat function)
						 (funcall function)))
					      ((symbol-function 'get-buffer-window)
					       (lambda (&rest _)
						 (selected-window)))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide-diff-open-buffer)
					       (lambda (text &optional buffer-name)
						 (setq opened-diff text)
						 (setq opened-diff-buffer-name buffer-name)
						 nil))
					      ((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-file-change-approval
				       session
				       45
				       '((itemId . "file-change-1")
					 (reason . "edit foo.txt"))))
				    (should (equal opened-diff expected-diff))
				    (should (equal opened-diff-buffer-name
						   (codex-ide-diff-buffer-name-for-session
						    (codex-ide-session-buffer session)))))))))

(ert-deftest codex-ide-file-change-completion-folds-inline-diff-when-threshold-exceeded ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-diff-inline-fold-threshold 4))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (diff-text (string-join
						     '("diff --git a/foo.txt b/foo.txt"
						       "--- a/foo.txt"
						       "+++ b/foo.txt"
						       "@@ -1 +1 @@"
						       "-old"
						       "+new")
						     "\n")))
				    (setf (codex-ide-session-current-turn-id session) "turn-file-change"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/started")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "inProgress")))))))
				      (codex-ide--handle-notification
				       session
				       `((method . "item/completed")
					 (params . ((item . ((type . "fileChange")
							     (id . "file-change-1")
							     (changes . (((path . "foo.txt")
									  (diff . ,diff-text))))
							     (status . "completed"))))))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (goto-char (point-min))
				      (search-forward "diff: foo.txt (+1/-1, 6 lines) [expand] [open diff]")
				      (let ((overlay (get-char-property
						      (match-beginning 0)
						      codex-ide-item-result-overlay-property)))
					(should (overlayp overlay))
					(should (overlay-get overlay 'invisible))
					(should-not (string-match-p "diff --git a/foo\\.txt b/foo\\.txt"
								    (buffer-string)))
					(codex-ide-toggle-item-result-at-point (match-beginning 0))
					(should-not (overlay-get overlay 'invisible))
					(should (string-match-p "diff --git a/foo\\.txt b/foo\\.txt"
								(buffer-string))))))))))

(ert-deftest codex-ide-permissions-approval-inline-decline-sends-empty-permissions ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (process (codex-ide-session-process session)))
				    (setf (codex-ide-session-current-turn-id session) "turn-approval-2"
					  (codex-ide-session-status session) "running")
				    (cl-letf (((symbol-function 'run-at-time)
					       (lambda (_time _repeat function)
						 (funcall function)))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'message)
					       (lambda (&rest _) nil)))
				      (codex-ide--handle-permissions-approval
				       session
				       43
				       '((reason . "run a tool")
					 (permissions . (((tool . "shell")))))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (string-match-p "\\[Approval required\\]\n\nReason: run a tool"
							      (buffer-string)))
				      (goto-char (point-min))
				      (search-forward "[decline]")
				      (backward-char 1)
				      (push-button))
				    (let* ((payloads
					    (mapcar (lambda (json)
						      (json-parse-string json
									 :object-type 'alist
									 :array-type 'list))
						    (codex-ide-test-process-sent-strings process)))
					   (payload (seq-find (lambda (item)
								(equal (alist-get 'id item) 43))
							      payloads))
					   (result (alist-get 'result payload)))
				      (should payload)
				      (should (equal (alist-get 'id payload) 43))
				      (should (equal (alist-get 'permissions result) nil))
				      (should-not (alist-get 'scope result))))))))

(ert-deftest codex-ide-process-sentinel-renders-startup-failure-from-stderr ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (process (codex-ide-session-process session))
					 (stderr-process (codex-ide-session-stderr-process session)))
				    (codex-ide--stderr-filter stderr-process "CODEX_HOME does not exist\n")
				    (setf (codex-ide-test-process-live process) nil)
				    (codex-ide--process-sentinel process "failed\n")
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (string-match-p "Codex process exited: Codex startup failed." (buffer-string)))
				      (should (string-match-p "CODEX_HOME does not exist" (buffer-string))))
				    (should-not (memq session codex-ide--sessions)))))))

(ert-deftest codex-ide-stderr-filter-strips-ansi-and-logs-structured-lines ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (stderr-process (codex-ide-session-stderr-process session)))
				    (codex-ide--stderr-filter
				     stderr-process
				     "\x1b[2m2026-04-09T16:58:08.078004Z\x1b[0m \x1b[31mERROR\x1b[0m failed to connect\n")
				    (with-current-buffer (codex-ide-test--log-buffer session)
				      (let ((text (buffer-string)))
					(should (string-match-p "stderr: 2026-04-09T16:58:08.078004Z ERROR failed to connect" text))
					(should-not (string-match-p "\x1b\\[" text))))
				    (should (equal (codex-ide--session-metadata-get session :stderr-partial) ""))
				    (should (string-match-p "2026-04-09T16:58:08.078004Z ERROR failed to connect"
							    (codex-ide--session-metadata-get session :stderr-tail))))))))

(ert-deftest codex-ide-thread-status-null-does-not-overwrite-running-state ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-status session) "running")
				    (codex-ide--handle-notification
				     session
				     '((method . "thread/status/changed")
				       (params . ((thread . ((status . nil)))))))
				    (should (string= (codex-ide-session-status session) "running"))
				    (should (string-match-p "Codex:Running"
							    (codex-ide-renderer-mode-line-status session))))))))

(ert-deftest codex-ide-normalize-session-status-maps-server-status-types ()
  (should (equal (codex-ide--normalize-session-status '((type . "active"))) "running"))
  (should (equal (codex-ide--normalize-session-status '((type . "systemError"))) "error"))
  (should (equal (codex-ide--normalize-session-status '((type . "completed"))) "idle")))

(ert-deftest codex-ide-thread-status-changed-reads-direct-status-payload ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-status session) "running")
				    (codex-ide--handle-notification
				     session
				     '((method . "thread/status/changed")
				       (params . ((threadId . "thread-1")
						  (status . ((type . "systemError")))))))
				    (should (string= (codex-ide-session-status session) "error"))
				    (should (string-match-p "Codex:Error"
							    (codex-ide-renderer-mode-line-status session))))))))

(ert-deftest codex-ide-error-notification-handles-authentication-failures-gracefully ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-auth-1")
				    (codex-ide--handle-notification
				     session
				     '((method . "turn/started")
				       (params . ((turn . ((id . "turn-auth-1")))))))
				    (codex-ide--handle-notification
				     session
				     '((method . "error")
				       (params . ((message . "Authentication failed. Please login again.")))))
				    (should-not (codex-ide-session-current-turn-id session))
				    (should (string= (codex-ide-session-status session) "idle"))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (string-match-p "Codex notification: Codex authentication failed." (buffer-string)))
				      (should (string-match-p "Run `codex login`" (buffer-string)))
				      (should (codex-ide--input-prompt-active-p session))
				      (goto-char (marker-position
						  (codex-ide-session-input-start-marker session)))
				      (should (eolp))))))))

(ert-deftest codex-ide-error-notification-retries-stay-concise-and-keep-turn-open ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-retry-1")
				    (codex-ide--handle-notification
				     session
				     '((method . "turn/started")
				       (params . ((turn . ((id . "turn-retry-1")))))))
				    (codex-ide--handle-notification
				     session
				     '((method . "error")
				       (params . ((error . ((message . "Reconnecting... 2/5")
							    (additionalDetails . "We're currently experiencing high demand.")))
						  (willRetry . t)
						  (turnId . "turn-retry-1")))))
				    (should (string= (codex-ide-session-status session) "running"))
				    (should (equal (codex-ide-session-current-turn-id session) "turn-retry-1"))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((text (buffer-string)))
					(should (string-match-p "\\[Codex retrying\\] Reconnecting... 2/5" text))
					(should (string-match-p
						 "  └ additionalDetails: We're currently experiencing high demand\\."
						 text))
					(should-not (string-match-p "Inspect the session log for details" text))
					(should-not (string-match-p "\\[Codex notification:" text))
					(should (codex-ide--input-prompt-active-p session))))
				    (with-current-buffer (codex-ide-test--log-buffer session)
				      (should (string-match-p "Retryable Codex error: Reconnecting... 2/5"
							      (buffer-string)))))))))

(ert-deftest codex-ide-error-notification-handles-rate-limits-gracefully ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-rate-1")
				    (codex-ide--handle-notification
				     session
				     '((method . "turn/started")
				       (params . ((turn . ((id . "turn-rate-1")))))))
				    (codex-ide--handle-notification
				     session
				     '((method . "error")
				       (params . ((message . "Rate limit exceeded (429 Too Many Requests)")))))
				    (should (string= (codex-ide-session-status session) "idle"))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (string-match-p "Codex notification: Codex is rate limited." (buffer-string)))
				      (should (string-match-p "Wait for quota to recover" (buffer-string)))))))))

(ert-deftest codex-ide-error-notification-final-auth-failure-omits-raw-payload ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-auth-final-1")
				    (codex-ide--handle-notification
				     session
				     '((method . "turn/started")
				       (params . ((turn . ((id . "turn-auth-final-1")))))))
				    (codex-ide--handle-notification
				     session
				     '((method . "error")
				       (params . ((error . ((message . "unexpected status 401 Unauthorized")
							    (additionalDetails . "Missing bearer or basic authentication in header")))
						  (willRetry . :json-false)
						  (turnId . "turn-auth-final-1")))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((text (buffer-string)))
					(should (string-match-p "Codex notification: Codex authentication failed." text))
					(should (string-match-p
						 "  └ unexpected status 401 Unauthorized"
						 text))
					(should (string-match-p
						 "  └ additionalDetails: Missing bearer or basic authentication in header"
						 text))
					(should-not (string-match-p "\\(willRetry\\|turnId\\|codexErrorInfo\\)" text))))
				    (should (string= (codex-ide-session-status session) "idle")))))))

(ert-deftest codex-ide-turn-completed-after-final-error-does-not-open-duplicate-prompt ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-auth-final-2")
				    (codex-ide--handle-notification
				     session
				     '((method . "turn/started")
				       (params . ((turn . ((id . "turn-auth-final-2")))))))
				    (codex-ide--handle-notification
				     session
				     '((method . "error")
				       (params . ((error . ((message . "unexpected status 401 Unauthorized")
							    (additionalDetails . "Missing bearer or basic authentication in header")))
						  (willRetry . :json-false)
						  (turnId . "turn-auth-final-2")))))
				    (codex-ide--handle-notification
				     session
				     '((method . "turn/completed")
				       (params . ((turnId . "turn-auth-final-2")))))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let* ((text (buffer-string))
					     (prompt-start
					      (marker-position
					       (codex-ide-session-input-prompt-start-marker session))))
					(should (codex-ide--input-prompt-active-p session))
					(should prompt-start)
					(goto-char prompt-start)
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					(goto-char (marker-position
						    (codex-ide-session-input-start-marker session)))
					(should (eolp))
					(should-not (string-match-p "> \n\n> " text)))))))))

(ert-deftest codex-ide-notification-error-info-tolerates-non-alist-codex-error-info ()
  (should
   (equal
    (codex-ide--notification-error-info
     '((error . ((message . "unexpected status 401 Unauthorized")
                 (codexErrorInfo . "other")
                 (additionalDetails . "Missing bearer or basic authentication in header")))
       (willRetry . :json-false)
       (turnId . "turn-1")))
    '((message . "unexpected status 401 Unauthorized")
      (details . "Missing bearer or basic authentication in header")
      (http-status . nil)
      (will-retry . nil)
      (turn-id . "turn-1")))))

(ert-deftest codex-ide-agent-text-carries-log-marker-property ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (let* ((session (codex-ide--create-process-session))
					 (line "{\"method\":\"item/reasoning/summaryTextDelta\",\"params\":{\"delta\":\"Reasoning summary\"}}"))
				    (codex-ide--process-message session line)
				    (with-current-buffer (codex-ide-session-buffer session)
				      (goto-char (point-min))
				      (search-forward "Reasoning summary")
				      (let ((marker (get-text-property (1- (point)) codex-ide-log-marker-property)))
					(should (markerp marker))
					(should (eq (marker-buffer marker)
						    (codex-ide-test--log-buffer session)))
					(with-current-buffer (marker-buffer marker)
					  (goto-char marker)
					  (should (looking-at-p
						   (regexp-quote
						    (format "[%s"
							    (format-time-string "%Y-")))))
					  (should (search-forward "Processing incoming notification line:" nil t))
					  (should (search-forward line nil t)))))))))

  (ert-deftest codex-ide-agent-text-carries-item-type-property ()
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
				       '((method . "item/started")
					 (params . ((item . ((id . "cmd-1")
							     (type . "commandExecution")
							     (cwd . "/tmp")))))))
				      (codex-ide--handle-notification
				       session
				       '((method . "item/reasoning/summaryTextDelta")
					 (params . ((delta . "Reasoning summary")))))
				      (codex-ide--handle-notification
				       session
				       '((method . "item/agentMessage/delta")
					 (params . ((itemId . "msg-1")
						    (delta . "Final answer")))))
				      (with-current-buffer (codex-ide-session-buffer session)
					(goto-char (point-min))
					(search-forward "Ran")
					(should (equal (get-text-property
							(1- (point))
							codex-ide-agent-item-type-property)
						       "commandExecution"))
					(goto-char (point-min))
					(search-forward "cwd: /tmp")
					(should (equal (get-text-property
							(1- (point))
							codex-ide-agent-item-type-property)
						       "commandExecution"))
					(goto-char (point-min))
					(search-forward "Reasoning summary")
					(should (equal (get-text-property
							(1- (point))
							codex-ide-agent-item-type-property)
						       "reasoning"))
					(goto-char (point-min))
					(search-forward "Final answer")
					(should (equal (get-text-property
							(1- (point))
							codex-ide-agent-item-type-property)
						       "agentMessage"))))))))

  (ert-deftest codex-ide-mcp-bridge-json-tool-call-returns-json-response ()
    (cl-letf (((symbol-function 'codex-ide-mcp-bridge--json-tool-call)
               (lambda (payload)
		 (should (equal payload "{\"name\":\"test_tool\",\"params\":{\"value\":7}}"))
		 "{\"ok\":true,\"value\":7}")))
      (should (equal
               (codex-ide-mcp-bridge--json-tool-call
		"{\"name\":\"test_tool\",\"params\":{\"value\":7}}")
               "{\"ok\":true,\"value\":7}"))))

  (ert-deftest codex-ide-format-thread-updated-at-formats-local-iso-timestamp ()
    (let ((updated-at 1744038896))
      (should
       (equal
	(codex-ide--format-thread-updated-at updated-at)
	(format-time-string "%Y-%m-%dT%H:%M:%S%z"
                            (seconds-to-time updated-at))))))

  (ert-deftest codex-ide-thread-choice-preview-strips-emacs-context-from-preview ()
    (should
     (equal
      (codex-ide--thread-choice-preview
       (concat "[Emacs session context]\n"
               "Use Emacs-aware behavior.\n"
               "[/Emacs session context]\n\n"
               "[Emacs prompt context]\n"
               "Buffer: example.el\n"
               "[/Emacs prompt context]\n\n  Explain the failure"))
      "Explain the failure")))

  (ert-deftest codex-ide-thread-choice-preview-hides-truncated-emacs-context-prefix ()
    (should
     (equal
      (codex-ide--thread-choice-preview
       (concat "[Emacs session context]\n"
               "Take the following into account.\n"
               "Prefer Emacs-aware behavior"))
      "")))

  (ert-deftest codex-ide-thread-read-display-user-text-strips-emacs-context-prefix ()
    (should
     (equal
      (codex-ide--thread-read-display-user-text
       (concat "[Emacs session context]\n"
               "Use Emacs-aware behavior.\n"
               "[/Emacs session context]\n\n"
               "[Emacs prompt context]\n"
               "Buffer: example.el\n"
               "Cursor: line 10, column 2\n"
               "[/Emacs prompt context]\n\n"
               "Explain the failure"))
      "Explain the failure")))

  (ert-deftest codex-ide-thread-read-display-user-text-preserves-multiline-prompts ()
    (should
     (equal
      (codex-ide--thread-read-display-user-text
       (concat "[Emacs prompt context]\n"
               "Buffer: example.el\n"
               "[/Emacs prompt context]\n\n"
               "First line\n"
               "Second line\n"
               "Third line"))
      "First line\nSecond line\nThird line")))

  (ert-deftest codex-ide-restore-thread-read-transcript-errors-when-turns-are-missing ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should-error
				     (codex-ide--restore-thread-read-transcript
				      session
				      '((thread . ((id . "thread-missing-turns")))))
				     :type 'error)))))

  (ert-deftest codex-ide-restore-thread-read-transcript-errors-on-unrenderable-turn-shape ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should-error
				     (codex-ide--restore-thread-read-transcript
				      session
				      '((thread . ((id . "thread-unsupported")
						   (turns . (((id . "turn-1")
							      (items . (((type . "unknownItem")
									 (id . "item-1")))))))))))
				     :type 'error)))))

  (ert-deftest codex-ide-restore-thread-read-transcript-replays-command-output ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (thread-read
            `((thread . ((id . "thread-restore-command-1")
                         (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "Run a command")))))
                                              ((type . "commandExecution")
                                               (id . "item-command-1")
                                               (command . "printf 'hello\\n'")
                                               (cwd . ,project-dir)
                                               (aggregatedOutput . "hello\n")
                                               (exitCode . 0)
                                               (status . "completed"))
                                              ((type . "agentMessage")
                                               (id . "item-agent-1")
                                               (text . "Command finished."))))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((buffer-text (buffer-string)))
					(should (string-match-p "\\* Ran command" buffer-text))
					(should (string-match-p "printf 'hello\\\\n'" buffer-text))
					(should (string-match-p "output: 1 line" buffer-text))
					(should (string-match-p "    hello" buffer-text))
					(should (string-match-p "Command finished\\." buffer-text))))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-replays-file-change-diff ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (diff-text (string-join
                       '("diff --git a/foo.txt b/foo.txt"
                         "@@ -1 +1 @@"
                         "-old"
                         "+new")
                       "\n"))
           (thread-read
            `((thread . ((id . "thread-restore-file-change-1")
                         (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "Change foo")))))
                                              ((type . "fileChange")
                                               (id . "item-file-change-1")
                                               (changes . (((path . "foo.txt")
                                                            (kind . "modified")
                                                            (diff . ,diff-text))))
                                               (status . "completed"))
                                              ((type . "agentMessage")
                                               (id . "item-agent-1")
                                               (text . "Updated `foo.txt`."))))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((buffer-text (buffer-string)))
					(should (string-match-p "\\* Prepared 1 file change" buffer-text))
					(should (string-match-p "modified foo\\.txt" buffer-text))
					(should (string-match-p
						 "diff: foo\\.txt (\\+1/-1, 4 lines) \\[fold\\] \\[open diff\\]"
						 buffer-text))
					(should (string-match-p "diff --git a/foo\\.txt b/foo\\.txt" buffer-text))
					(should (string-match-p "Updated `foo\\.txt`\\." buffer-text))))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-keeps-combined-diff-available ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (diff-text (string-join
                       '("diff --git a/foo.txt b/foo.txt"
                         "@@ -1 +1 @@"
                         "-old"
                         "+new")
                       "\n"))
           (expected-diff diff-text)
           (thread-read
            `((thread . ((id . "thread-restore-combined-diff-1")
                         (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "Change foo")))))
                                              ((type . "fileChange")
                                               (id . "item-file-change-1")
                                               (changes . (((path . "foo.txt")
                                                            (kind . "modified")
                                                            (diff . ,diff-text))))
                                               (status . "completed"))
                                              ((type . "agentMessage")
                                               (id . "item-agent-1")
                                               (text . "Updated `foo.txt`."))))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (setf (codex-ide-session-thread-id session)
					  "thread-restore-combined-diff-1")
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (should-not (codex-ide--current-turn-diff-entry session))
				    (cl-letf (((symbol-function 'codex-ide--read-thread)
					       (lambda (&rest _args)
						 (error "thread read unavailable"))))
				      (should
				       (equal (codex-ide-diff-data-combined-turn-diff-text
					       session)
					      expected-diff))
				      (should
				       (equal (codex-ide-diff-data-combined-turn-diff-text
					       session
					       "turn-1")
					      expected-diff)))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-augments-from-rollout-storage ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (rollout-path (expand-file-name "rollout-thread.jsonl" project-dir))
           (session nil)
           (patch-text (string-join
                        '("*** Begin Patch"
                          "*** Update File: foo.txt"
                          "@@"
                          "-old"
                          "+new"
                          "*** End Patch")
                        "\n"))
           (thread-read
            `((thread . ((id . "thread-restore-rollout-1")
                         (path . ,rollout-path)
                         (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "Run and patch")))))
                                              ((type . "agentMessage")
                                               (id . "item-agent-1")
                                               (text . "Fallback should not duplicate."))))))))))))
      (with-temp-file rollout-path
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
                                               `((cmd . "printf 'hello\\n'")
                                                 (workdir . ,project-dir)))))))
                   ((type . "response_item")
                    (payload . ((type . "function_call")
                                (name . "emacs_get_all_buffers")
                                (namespace . "mcp__codex_ide_emacs_mcp__")
                                (call_id . "call-mcp-1")
                                (arguments . "{}"))))
                   ((type . "response_item")
                    (payload . ((type . "function_call_output")
                                (call_id . "call-mcp-1")
                                (output . "{\"files\":[]}"))))
                   ((type . "response_item")
                    (payload . ((type . "function_call")
                                (name . "future_unrecognized_call")
                                (call_id . "call-future-1")
                                (arguments . "{\"changed\":true}"))))
                   ((type . "response_item")
                    (payload . ((type . "function_call_output")
                                (call_id . "call-future-1")
                                (output . "should not render"))))
                   ((type . "response_item")
                    (payload . ((type . "function_call_output")
                                (call_id . "call-command-1")
                                (output . ,(concat
                                            "Chunk ID: abc123\n"
                                            "Wall time: 0.0000 seconds\n"
                                            "Process exited with code 0\n"
                                            "Original token count: 1\n"
                                            "Output:\n"
                                            "hello\n")))))
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
                   ((type . "response_item")
                    (payload . ((type . "custom_tool_call")
                                (name . "future_custom_tool")
                                (call_id . "call-custom-future-1")
                                (input . "opaque"))))
                   ((type . "response_item")
                    (payload . ((type . "custom_tool_call_output")
                                (call_id . "call-custom-future-1")
                                (output . "should not render"))))
                   ((type . "response_item")
                    (payload . ((type . "message")
                                (role . "assistant")
                                (content . (((type . "output_text")
                                             (text . "Done.")))))))
                   ((type . "event_msg")
                    (payload . ((type . "task_complete"))))))
          (insert (json-encode entry) "\n")))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((buffer-text (buffer-string)))
					(should (string-match-p "\\* Ran command" buffer-text))
					(should (string-match-p "    hello" buffer-text))
					(should-not (string-match-p "Chunk ID:" buffer-text))
					(should-not (string-match-p "Original token count:" buffer-text))
					(should (string-match-p
						 "\\* Called mcp__codex_ide_emacs_mcp__/emacs_get_all_buffers"
						 buffer-text))
					(should (string-match-p "\\* Prepared 1 file change" buffer-text))
					(should (string-match-p "--- a/foo\\.txt" buffer-text))
					(should (string-match-p "\\+\\+\\+ b/foo\\.txt" buffer-text))
					(should-not (string-match-p "\\*\\*\\* Begin Patch" buffer-text))
					(should-not (string-match-p "\\*\\*\\* Update File:" buffer-text))
					(should-not (string-match-p "\\*\\*\\* End Patch" buffer-text))
					(should-not (string-match-p "future_unrecognized_call" buffer-text))
					(should-not (string-match-p "future_custom_tool" buffer-text))
					(should-not (string-match-p "should not render" buffer-text))
					(should (string-match-p "Done\\." buffer-text))
					(should-not (string-match-p
						     "Fallback should not duplicate"
						     buffer-text))
					(should
					 (< (string-match-p "First\\." buffer-text)
					    (string-match-p "\\* Ran command" buffer-text)
					    (string-match-p "    hello" buffer-text)
					    (string-match-p "Second\\." buffer-text)
					    (string-match-p "\\* Prepared 1 file change" buffer-text)
					    (string-match-p "--- a/foo\\.txt" buffer-text)
					    (string-match-p "Done\\." buffer-text)))))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-replays-item-based-turns ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (thread-read
            '((thread . ((id . "thread-restore-1")
			 (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "[Emacs context]\nBuffer: my-table.py\n[/Emacs context]\n\nWhat DB columns are on MyTable?")))))
                                              ((type . "agentMessage")
                                               (id . "item-1")
                                               (text . "Columns include `my_table_id` and `price`."))))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((buffer-text (buffer-string)))
					(should (string-match-p "^> What DB columns are on MyTable\\?" buffer-text))
					(should-not (string-match-p "\\[Emacs context\\]" buffer-text))
					(should (string-match-p "Columns include `my_table_id` and `price`\\." buffer-text))
					(should (string-match-p
						 (concat (regexp-quote "Columns include `my_table_id` and `price`.")
							 "\n"
							 (regexp-quote
							  (codex-ide-renderer-restored-transcript-separator-string)))
						 buffer-text))
					(goto-char (point-min))
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					(goto-char (point-min))
					(search-forward "my_table_id")
					(should (eq (get-text-property (1- (point)) 'face)
						    'font-lock-keyword-face))))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-keeps-blank-line-between-agent-and-next-prompt ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (turn-1
            `((id . "turn-1")
              (items . (((type . "userMessage")
			 (content . (((type . "text")
                                      (text . "What DB columns are on MyTable?")))))
			((type . "agentMessage")
			 (id . "item-1")
			 (text . "If you want, I can also give this as the exact SQL-ish schema shape with field types/nullability."))))))
           (turn-2
            `((id . "turn-2")
              (items . (((type . "userMessage")
			 (content . (((type . "text")
                                      (text . "What is MyTable's primary key?")))))
			((type . "agentMessage")
			 (id . "item-2")
			 (text . "`MyTable`'s primary key is `my_table_id`."))))))
           (thread-read
            `((thread . ((id . "thread-restore-2")
			 (turns . (,turn-1 ,turn-2)))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should
				       (string-match-p
					(concat
					 (regexp-quote
					  "If you want, I can also give this as the exact SQL-ish schema shape with field types/nullability.")
					 "\n\n\n> What is MyTable's primary key\\?")
					(buffer-string)))
				      (goto-char (point-min))
				      (search-forward "> What is MyTable's primary key?")
				      (should-not (get-text-property (- (match-beginning 0) 2) 'face))
				      (should (eq (get-text-property (1- (match-beginning 0)) 'face)
						  'codex-ide-user-prompt-face)))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-keeps-prompt-face-after-separator ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (thread-read
            '((thread . ((id . "thread-restore-face-1")
			 (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "Show the last prompt")))))
                                              ((type . "agentMessage")
                                               (id . "item-1")
                                               (text . "Here is the restored reply."))))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (goto-char (marker-position
						  (codex-ide-session-input-start-marker session)))
				      (insert "draft")
				      (should (eq (get-text-property (1- (point)) 'face)
						  'codex-ide-user-prompt-face))
				      (should (eq (get-text-property (point) 'face) nil))
				      (let ((prefix (codex-ide-test--prompt-prefix-at-line)))
					(should (equal prefix "> "))
					(goto-char (marker-position
						    (codex-ide-session-input-prompt-start-marker session)))
					(should (eq (get-text-property (point) 'face)
						    'codex-ide-user-prompt-face)))
				      (goto-char (point-min))
				      (search-forward "> Show the last prompt")
				      (should (eq (get-text-property (1- (match-beginning 0)) 'face)
						  'codex-ide-user-prompt-face))
				      (should (eq (get-text-property (point) 'face)
						  'codex-ide-user-prompt-face))
				      (goto-char (point-min))
				      (search-forward "[End of restored session]")
				      (should (eq (get-text-property (match-beginning 0) 'face)
						  'codex-ide-output-separator-face)))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-renders-trailing-pipe-tables ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (thread-read
            '((thread . ((id . "thread-restore-table-1")
			 (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "Show a table")))))
                                              ((type . "agentMessage")
                                               (id . "item-1")
                                               (text . "| Number | Square |\n| --- | ---: |\n| 1 | 1 |\n| 2 | 4 |\n"))))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((buffer-text (buffer-string)))
					(should (string-match-p "^| Number | Square |" buffer-text))
					(should (string-match-p "^| 1      |      1 |$" buffer-text))
					(should-not (string-match-p "^| 1 | 1 |$" buffer-text))))))))

  (ert-deftest codex-ide-restore-thread-read-transcript-preserves-multiline-user-prompts ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (session nil)
           (thread-read
            '((thread . ((id . "thread-restore-3")
			 (turns . (((id . "turn-1")
                                    (items . (((type . "userMessage")
                                               (content . (((type . "text")
                                                            (text . "[Emacs context]\nBuffer: my-table.py\n[/Emacs context]\n\nLine one\nLine two\nLine three")))))
                                              ((type . "agentMessage")
                                               (id . "item-1")
                                               (text . "Acknowledged."))))))))))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (setq session (codex-ide--create-process-session))
				    (should (codex-ide--restore-thread-read-transcript session thread-read))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (should (string-match-p
					       (regexp-quote "Line one\nLine two\nLine three")
					       (buffer-string)))
				      (goto-char (point-min))
				      (should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
				      (should-not (string-match-p "\\[Emacs context\\]" (buffer-string))))))))

  (ert-deftest codex-ide-start-session-resume-replays-thread-read-transcript ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests '())
          (thread '((id . "thread-resume-1")
                    (preview . "Resume flow"))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide--pick-thread)
					       (lambda (&rest _) thread))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method params)
						 (push (cons method params) requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/read"
						    '((thread . ((id . "thread-resume-1")
								 (name . "Resume flow")
								 (preview . "Investigate stale prompt")))
						      (turns . (((id . "turn-1")
								 (items . (((type . "userMessage")
									    (content . (((type . "text")
											 (text . "Why is resume stale?")))))
									   ((type . "agentMessage")
									    (id . "item-1")
									    (text . "The prompt was restored too early.")))))))))
						   ("thread/resume" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((session (codex-ide--start-session 'resume)))
					(should (string= (codex-ide-session-thread-id session) "thread-resume-1"))
					(should (equal (seq-remove (lambda (method)
								     (equal method "config/read"))
								   (mapcar #'car (nreverse requests)))
						       '("initialize" "thread/read" "thread/resume")))
					(with-current-buffer (codex-ide-session-buffer session)
					  (let ((buffer-text (buffer-string)))
					    (should (string-match-p "^Why is resume stale\\?" buffer-text))
					    (should (string-match-p "The prompt was restored too early\\." buffer-text))
					    (should (string-match-p
						     (concat (regexp-quote "The prompt was restored too early.")
							     "\n"
							     (regexp-quote
							      (codex-ide-renderer-restored-transcript-separator-string)))
						     buffer-text))
					    (goto-char (point-max))
					    (forward-line 0)
					    (should (equal (codex-ide-test--prompt-prefix-at-line) "> "))))))))))

  (ert-deftest codex-ide-resume-thread-into-session-replays-thread-read-transcript ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests '()))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method params)
						 (push (cons method params) requests)
						 (pcase method
						   ("thread/read"
						    '((thread . ((id . "thread-explicit-1")
								 (name . "Explicit flow")
								 (preview . "Replay exact thread")))
						      (turns . (((id . "turn-1")
								 (items . (((type . "userMessage")
									    (content . (((type . "text")
											 (text . "Resume this exact thread.")))))
									   ((type . "agentMessage")
									    (id . "item-1")
									    (text . "Exact thread resumed.")))))))))
						   ("thread/resume" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((session (codex-ide--create-process-session)))
					(should (eq (codex-ide--resume-thread-into-session
						     session "thread-explicit-1" "Resumed")
						    session))
					(should (string= (codex-ide-session-thread-id session)
							 "thread-explicit-1"))
					(should (numberp
						 (codex-ide-session-last-thread-attached-at session)))
					(should (equal (mapcar #'car (nreverse requests))
						       '("thread/read" "thread/resume")))
					(with-current-buffer (codex-ide-session-buffer session)
					  (let ((buffer-text (buffer-string)))
					    (should (string-match-p "^Resume this exact thread\\." buffer-text))
					    (should (string-match-p "Exact thread resumed\\." buffer-text))))))))))

  (ert-deftest codex-ide-start-session-resume-replaces-existing-session-with-selected-thread ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests '())
          (selected-thread '((id . "thread-resume-2")
                             (preview . "Other thread"))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/start" '((thread . ((id . "thread-current")))))
						   ("thread/read"
						    '((thread . ((id . "thread-resume-2")
								 (turns . (((id . "turn-1")
									    (items . (((type . "userMessage")
										       (content . (((type . "text")
												    (text . "Switch threads")))))
										      ((type . "agentMessage")
										       (id . "item-1")
										       (text . "Switched.")))))))))))
						   ("thread/resume" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method))))))
					      ((symbol-function 'codex-ide--pick-thread)
					       (lambda (&optional _session omit-thread-id)
						 (should (equal omit-thread-id "thread-current"))
						 selected-thread)))
				      (let ((original-session (codex-ide--start-session 'new)))
					(setq requests nil)
					(with-current-buffer (codex-ide-session-buffer original-session)
					  (let ((new-session (codex-ide--start-session 'resume)))
					    (should-not (eq new-session original-session))
					    (should (string= (codex-ide-session-thread-id new-session)
							     "thread-resume-2"))
					    (should (equal (seq-remove (lambda (method)
									 (equal method "config/read"))
								       (nreverse requests))
							   '("initialize" "thread/read" "thread/resume")))
					    (with-current-buffer (codex-ide-session-buffer new-session)
					      (should (string-match-p "^Switch threads"
								      (buffer-string))))))))))))

  (ert-deftest codex-ide-start-session-resume-reuses-existing-session-for-selected-thread ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (selected-thread '((id . "thread-reused")
                             (preview . "Existing thread")))
          (displayed nil)
          (requests '()))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (buffer &optional _action)
						 (setq displayed buffer)
						 (selected-window)))
					      ((symbol-function 'codex-ide--pick-thread)
					       (lambda (&optional _session omit-thread-id)
						 (should (equal omit-thread-id "thread-current"))
						 selected-thread))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/start" '((thread . ((id . "thread-current")))))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((current-session (codex-ide--start-session 'new))
					    reused-session)
					(setq requests nil)
					(setq reused-session (codex-ide--start-session 'new))
					(setf (codex-ide-session-thread-id reused-session) "thread-reused")
					(setq requests nil)
					(with-current-buffer (codex-ide-session-buffer current-session)
					  (let ((result (codex-ide--start-session 'resume)))
					    (should (eq result reused-session))
					    (should (eq displayed (codex-ide-session-buffer reused-session)))
					    (should (equal requests '()))
					    (should (process-live-p (codex-ide-session-process current-session)))
					    (should (process-live-p (codex-ide-session-process reused-session)))))))))))

  (ert-deftest codex-ide-start-session-continue-reuses-existing-session-for-latest-thread ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (displayed nil)
          (requests '()))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (buffer &optional _action)
						 (setq displayed buffer)
						 (selected-window)))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/start" '((thread . ((id . "thread-current")))))
						   ("thread/list" '((data . [((id . "thread-latest")
									      (createdAt . 1)
									      (preview . "Latest"))])))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (let ((current-session (codex-ide--start-session 'new))
					    latest-session)
					(setq requests nil)
					(setq latest-session (codex-ide--start-session 'new))
					(setf (codex-ide-session-thread-id latest-session) "thread-latest")
					(setq requests nil)
					(let ((result (codex-ide--start-session 'continue)))
					  (should (eq result latest-session))
					  (should (eq displayed (codex-ide-session-buffer latest-session)))
					  (should (equal (nreverse requests) '("thread/list")))
					  (should (process-live-p (codex-ide-session-process current-session)))
					  (should (process-live-p (codex-ide-session-process latest-session))))))))))

  (ert-deftest codex-ide-start-session-resume-errors-when-thread-read-is-not-replayable ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (thread '((id . "thread-resume-bad")
                    (preview . "Bad resume"))))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--ensure-cli)
					       (lambda () t))
					      ((symbol-function 'codex-ide-mcp-bridge-prompt-to-enable)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-mcp-bridge-ensure-server)
					       (lambda () nil))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action) (selected-window)))
					      ((symbol-function 'codex-ide--pick-thread)
					       (lambda (&rest _) thread))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/read"
						    '((thread . ((id . "thread-resume-bad")
								 (turns . (((id . "turn-1")
									    (items . (((type . "commandExecution")
										       (id . "item-1")))))))))))
						   ("thread/resume" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method)))))))
				      (should-error
				       (codex-ide--start-session 'resume)
				       :type 'error)
				      (should-not (codex-ide--get-session))
				      (should-not (codex-ide--has-live-sessions-p)))))))

  (ert-deftest codex-ide-mcp-bridge-get-buffer-info-returns-shared-buffer-shape ()
    (let ((buffer (generate-new-buffer " *codex-ide-mcp-bridge-info*")))
      (unwind-protect
          (with-current-buffer buffer
            (emacs-lisp-mode)
            (set-buffer-modified-p t)
            (setq buffer-read-only t)
            (should
             (equal
              (codex-ide-mcp-bridge--tool-call--get_buffer_info
               `((buffer . ,(buffer-name buffer))))
              `((buffer . ,(buffer-name buffer))
		(file . nil)
		(major-mode . "emacs-lisp-mode")
		(modified . t)
		(read-only . t)))))
	(kill-buffer buffer))))

  (ert-deftest codex-ide-mcp-bridge-get-all-buffers-uses-shared-buffer-info ()
    (let* ((project-dir (codex-ide-test--make-temp-project))
           (file-path (codex-ide-test--make-project-file
                       project-dir "lib/buffer-info.el" "(message \"hi\")\n"))
           (buffer (find-file-noselect file-path)))
      (unwind-protect
          (with-current-buffer buffer
            (emacs-lisp-mode)
            (set-buffer-modified-p t)
            (let* ((result (codex-ide-mcp-bridge--tool-call--get_all_buffers nil))
                   (files (alist-get 'files result nil nil #'equal))
                   (entry (seq-find
                           (lambda (item)
                             (equal (alist-get 'buffer item nil nil #'equal)
                                    (buffer-name buffer)))
                           files)))
              (should entry)
              (should
               (equal entry
                      (codex-ide-mcp-bridge--tool-call--get_buffer_info
                       `((buffer . ,(buffer-name buffer))))))))
	(kill-buffer buffer))))

  (ert-deftest codex-ide-mcp-bridge-get-buffer-text-returns-full-buffer-contents ()
    (let ((buffer (generate-new-buffer " *codex-ide-mcp-bridge-text*")))
      (unwind-protect
          (with-current-buffer buffer
            (insert "alpha\nbeta\n")
            (should
             (equal
              (codex-ide-mcp-bridge--tool-call--get_buffer_text
               `((buffer . ,(buffer-name buffer))))
              `((buffer . ,(buffer-name buffer))
		(text . "alpha\nbeta\n")))))
	(kill-buffer buffer))))

  (ert-deftest codex-ide-mcp-server-schema-includes-buffer-info-and-text-tools ()
    (with-temp-buffer
      (insert-file-contents (expand-file-name "bin/codex-ide-mcp-server.py"
                                              default-directory))
      (should (re-search-forward "name=\"emacs_get_buffer_info\"" nil t))
      (should (re-search-forward "name=\"emacs_get_buffer_text\"" nil t))))

  (ert-deftest codex-ide-stop-errors-outside-session-buffer ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (with-temp-buffer
				     (setq-local default-directory (file-name-as-directory project-dir))
				     (should-error (codex-ide-stop) :type 'user-error)))))

  (ert-deftest codex-ide-stop-stops-current-session-buffer-only ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests '()))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 '((ok . t)))))
				      (let ((session (codex-ide--create-process-session)))
					(setf (codex-ide-session-thread-id session) "thread-stop-1")
					(with-current-buffer (codex-ide-session-buffer session)
					  (codex-ide-stop))
					(should (equal requests '("thread/unsubscribe")))
					(should-not (memq session codex-ide--sessions))
					(should-not (buffer-live-p (codex-ide-session-buffer session)))))))))

  (ert-deftest codex-ide-reset-current-session-errors-outside-session-buffer ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (with-temp-buffer
				     (setq-local default-directory (file-name-as-directory project-dir))
				     (should-error (codex-ide-reset-current-session) :type 'user-error)))))

  (ert-deftest codex-ide-reset-current-session-restarts-in-current-buffer ()
    (let ((project-dir (codex-ide-test--make-temp-project))
          (requests '()))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (setq requests (append requests (list method)))
						 (pcase method
						   ("thread/start" '((thread . ((id . "thread-reset-new")))))
						   (_ '((ok . t)))))))
				      (let* ((session (codex-ide--create-process-session))
					     (buffer (codex-ide-session-buffer session))
					     (buffer-name (buffer-name buffer))
					     (old-process (codex-ide-session-process session))
					     (new-session nil))
					(setf (codex-ide-session-thread-id session) "thread-reset-old")
					(with-current-buffer buffer
					  (let ((inhibit-read-only t))
					    (goto-char (point-max))
					    (insert "old transcript\n")
					    (goto-char (point-min))
					    (codex-ide--style-user-prompt-region
					     (line-beginning-position)
					     (line-end-position)))
					  (codex-ide--insert-input-prompt session "draft reset prompt")
					  (should (= (codex-ide-test--prompt-line-count) 2))
					  (setq new-session (codex-ide-reset-current-session)))
					(should-not (eq new-session session))
					(should (eq (codex-ide-session-buffer new-session) buffer))
					(should (equal (buffer-name buffer) buffer-name))
					(should-not (codex-ide-test-process-live old-process))
					(should-not (memq session codex-ide--sessions))
					(should (memq new-session codex-ide--sessions))
					(should (codex-ide-test-process-live
						 (codex-ide-session-process new-session)))
					(should (string= (codex-ide-session-thread-id new-session)
							 "thread-reset-new"))
					(with-current-buffer buffer
					  (should (eq (codex-ide--session-for-current-buffer) new-session))
					  (should-not (string-match-p "old transcript" (buffer-string)))
					  (should (= (codex-ide-test--prompt-line-count) 1))
					  (goto-char (marker-position
						      (codex-ide-session-input-prompt-start-marker
						       new-session)))
					  (should (equal (codex-ide-test--prompt-prefix-at-line) "> ")))
					(should (equal (seq-remove (lambda (method)
								     (equal method "config/read"))
								   requests)
						       '("thread/unsubscribe"
							 "initialize"
							 "thread/start")))))))))

  (ert-deftest codex-ide-push-prompt-history-deduplicates-and-trims ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (codex-ide--push-prompt-history session "first")
				      (codex-ide--push-prompt-history session "second   ")
				      (codex-ide--push-prompt-history session "first")
				      (codex-ide--push-prompt-history session "   ")
				      (should (equal (codex-ide--project-persisted-get :prompt-history session)
						     '("first" "second"))))))))

  (ert-deftest codex-ide-browse-prompt-history-replaces-current-input ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-history-1")
				      (codex-ide--project-persisted-put
				       :prompt-history
				       '("latest prompt" "older prompt")
				       session)
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "draft")
					(codex-ide--browse-prompt-history -1)
					(should (string= (codex-ide--current-input session) "latest prompt"))
					(should (= (codex-ide-session-prompt-history-index session) 0))
					(codex-ide--browse-prompt-history -1)
					(should (string= (codex-ide--current-input session) "older prompt"))
					(should (= (codex-ide-session-prompt-history-index session) 1))
					(should-error (codex-ide--browse-prompt-history -1) :type 'user-error)
					(codex-ide--browse-prompt-history 1)
					(should (string= (codex-ide--current-input session) "latest prompt"))
					(codex-ide--browse-prompt-history 1)
					(should (string= (codex-ide--current-input session) ""))
					(should-not (codex-ide-session-prompt-history-index session))))))))

  (ert-deftest codex-ide-goto-prompt-line-navigates-between-prompts ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (with-current-buffer (codex-ide-session-buffer session)
					(let ((inhibit-read-only t)
					      first-input
					      second-input)
					  (erase-buffer)
					  (insert "> first prompt\nassistant reply\n> second prompt\n")
					  (goto-char (point-min))
					  (codex-ide--style-user-prompt-region
					   (line-beginning-position)
					   (line-end-position))
					  (setq first-input (+ (line-beginning-position) 2))
					  (forward-line 2)
					  (codex-ide--style-user-prompt-region
					   (line-beginning-position)
					   (line-end-position))
					  (setq second-input (+ (line-beginning-position) 2))
					  (goto-char (point-max))
					  (forward-line -1)
					  (should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					  (forward-char 2)
					  (should (looking-at-p "second prompt"))
					  (codex-ide--goto-prompt-line -1)
					  (should (= (point) first-input))
					  (should (looking-at-p "first prompt"))
					  (should-error (codex-ide--goto-prompt-line -1) :type 'user-error)
					  (codex-ide--goto-prompt-line 1)
					  (should (= (point) second-input))
					  (should (looking-at-p "second prompt")))
					(should-error (codex-ide--goto-prompt-line 1) :type 'user-error))))))

    (ert-deftest codex-ide-goto-prompt-line-lands-at-editable-input-start ()
      (let ((project-dir (codex-ide-test--make-temp-project)))
	(codex-ide-test-with-fixture project-dir
				     (codex-ide-test-with-fake-processes
				      (let ((session (codex-ide--create-process-session)))
					(with-current-buffer (codex-ide-session-buffer session)
					  (let ((inhibit-read-only t))
					    (erase-buffer)
					    (insert "> first prompt\nassistant reply\n")
					    (goto-char (point-min))
					    (codex-ide--style-user-prompt-region
					     (line-beginning-position)
					     (line-end-position)))
					  (codex-ide--insert-input-prompt session nil)
					  (goto-char (point-min))
					  (codex-ide--goto-prompt-line -1)
					  (codex-ide--goto-prompt-line 1)
					  (should (= (point)
						     (marker-position
						      (codex-ide-session-input-start-marker session))))
					  (insert "draft")
					  (goto-char (marker-position
						      (codex-ide-session-input-prompt-start-marker session)))
					  (should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					  (goto-char (marker-position
						      (codex-ide-session-input-start-marker session)))
					  (should (looking-at-p "draft")))))))))

  (ert-deftest codex-ide-input-prompt-uses-fields-to-skip-prefix-at-bol ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (with-current-buffer (codex-ide-session-buffer session)
					(codex-ide--insert-input-prompt session "first line\nsecond line")
					(goto-char (marker-position
						    (codex-ide-session-input-prompt-start-marker session)))
					(should (eq (get-text-property (point) 'field)
						    'codex-ide-prompt-prefix))
					(goto-char (marker-position
						    (codex-ide-session-input-start-marker session)))
					(should (eq (get-char-property (point) 'field)
						    'codex-ide-active-input))
					(goto-char (point-max))
					(forward-line -1)
					(end-of-line)
					(move-beginning-of-line 1)
					(should (= (point) (line-beginning-position)))
					(goto-char (point-max))
					(forward-line -1)
					(forward-char 3)
					(move-beginning-of-line 0)
					(should (= (point)
						   (marker-position
						    (codex-ide-session-input-start-marker session))))
					(should (looking-at-p "first line"))))))))

  (ert-deftest codex-ide-reopen-input-after-submit-error-resets-turn-state ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-submit-1"
					    (codex-ide-session-current-turn-id session) "turn-1"
					    (codex-ide-session-current-message-item-id session) "item-1"
					    (codex-ide-session-current-message-prefix-inserted session) t
					    (codex-ide-session-current-message-start-marker session) (point-marker)
					    (codex-ide-session-output-prefix-inserted session) t
					    (codex-ide-session-status session) "running")
				      (puthash "item-1" '(:type "agentMessage")
					       (codex-ide-session-item-states session))
				      (codex-ide--reopen-input-after-submit-error
				       session
				       "retry prompt"
				       '(error "boom"))
				      (should-not (codex-ide-session-current-turn-id session))
				      (should-not (codex-ide-session-current-message-item-id session))
				      (should-not (codex-ide-session-current-message-prefix-inserted session))
				      (should-not (codex-ide-session-output-prefix-inserted session))
				      (should (string= (codex-ide-session-status session) "idle"))
				      (should (= (hash-table-count (codex-ide-session-item-states session)) 0))
				      (with-current-buffer (codex-ide-session-buffer session)
					(should (string-match-p "\\[Submit failed\\] boom" (buffer-string)))
					(goto-char (point-max))
					(forward-line 0)
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> "))
					(goto-char (marker-position
						    (codex-ide-session-input-start-marker session)))
					(should (looking-at-p "retry prompt"))))))))

  (ert-deftest codex-ide-finish-turn-resets-state-and-opens-fresh-prompt ()
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
				   (codex-ide-test-with-fake-processes
				    (let ((session (codex-ide--create-process-session)))
				      (setf (codex-ide-session-thread-id session) "thread-finish-1"
					    (codex-ide-session-current-turn-id session) "turn-1"
					    (codex-ide-session-current-message-item-id session) "item-1"
					    (codex-ide-session-current-message-prefix-inserted session) t
					    (codex-ide-session-current-message-start-marker session) (point-marker)
					    (codex-ide-session-output-prefix-inserted session) t
					    (codex-ide-session-interrupt-requested session) t
					    (codex-ide-session-status session) "running")
				      (puthash "item-1" '(:type "agentMessage")
					       (codex-ide-session-item-states session))
				      (codex-ide--finish-turn session "[Agent interrupted]")
				      (should-not (codex-ide-session-current-turn-id session))
				      (should-not (codex-ide-session-current-message-item-id session))
				      (should-not (codex-ide-session-current-message-prefix-inserted session))
				      (should-not (codex-ide-session-output-prefix-inserted session))
				      (should-not (codex-ide-session-interrupt-requested session))
				      (should (string= (codex-ide-session-status session) "idle"))
				      (should (= (hash-table-count (codex-ide-session-item-states session)) 0))
				      (with-current-buffer (codex-ide-session-buffer session)
					(should (string-match-p "\\[Agent interrupted\\]" (buffer-string)))
					(goto-char (point-max))
					(forward-line 0)
					(should (equal (codex-ide-test--prompt-prefix-at-line) "> "))))))))

  (ert-deftest codex-ide-mcp-bridge-request-exempt-from-approval-matches-server-name ()
    (let ((codex-ide-emacs-bridge-require-approval nil)
          (codex-ide-emacs-tool-bridge-name "emacs"))
      (should
       (codex-ide-mcp-bridge-request-exempt-from-approval-p
	'((serverName . "emacs")
          (message . "Allow the emacs MCP server to run tool \"emacs_show_file_buffer\"?"))))
      (let ((codex-ide-emacs-bridge-require-approval t))
	(should-not
	 (codex-ide-mcp-bridge-request-exempt-from-approval-p
          '((serverName . "emacs")))))
      (let ((codex-ide-emacs-bridge-require-approval nil))
	(should-not
	 (codex-ide-mcp-bridge-request-exempt-from-approval-p
          '((serverName . "other")))))))

  (ert-deftest codex-ide-clear-markdown-properties-preserves-non-markdown-faces ()
    (with-temp-buffer
      (insert "prefix `code` suffix\n* Ran command\n")
      (let* ((markdown-start 1)
             (markdown-end (1+ (line-end-position)))
             (summary-start (save-excursion
                              (goto-char (point-min))
                              (forward-line 1)
                              (point)))
             (summary-end (line-end-position 2)))
	(add-text-properties summary-start summary-end
                             '(face codex-ide-item-summary-face))
	(codex-ide-renderer-render-markdown-region markdown-start markdown-end)
	(should (eq (get-text-property summary-start 'face)
                    'codex-ide-item-summary-face))
	(codex-ide-renderer-render-markdown-region markdown-start (point-max))
	(should (eq (get-text-property summary-start 'face)
                    'codex-ide-item-summary-face)))))

  (ert-deftest codex-ide-render-markdown-region-renders-file-links ()
    (with-temp-buffer
      (insert "See [`foo.el`](/tmp/foo.el#L3C2)\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "foo.el")
      (let ((pos (1- (point))))
	(should (button-at pos))
	(should (eq (get-text-property pos 'face) 'link))
	(should (eq (get-text-property pos 'action) #'codex-ide-renderer-open-file-link))
	(should (equal (get-text-property pos 'codex-ide-path) "/tmp/foo.el"))
	(should (= (get-text-property pos 'codex-ide-line) 3))
	(should (= (get-text-property pos 'codex-ide-column) 2))
	(should (equal (get-text-property pos 'display) "foo.el")))))

  (ert-deftest codex-ide-parse-file-link-target-normalizes-escaped-slashes ()
    (should
     (equal (codex-ide-renderer-parse-file-link-target "\\/tmp\\/foo.el#L3C2")
            '("/tmp/foo.el" 3 2))))

  (ert-deftest codex-ide-parse-file-link-target-ignores-permissive-wrappers ()
    (should
     (equal (codex-ide-renderer-parse-file-link-target "</tmp/foo.txt>")
            '("/tmp/foo.txt" nil nil)))
    (should
     (equal (codex-ide-renderer-parse-file-link-target "</tmp/foo.txt:123>")
            '("/tmp/foo.txt" 123 nil)))
    (should
     (equal (codex-ide-renderer-parse-file-link-target "    /tmp/foo.txt   ")
            '("/tmp/foo.txt" nil nil)))
    (should
     (equal (codex-ide-renderer-parse-file-link-target "  </tmp/foo.txt:888  ")
            '("/tmp/foo.txt" 888 nil))))

  (ert-deftest codex-ide-render-markdown-region-renders-file-links-with-escaped-slashes ()
    (with-temp-buffer
      (insert "See [`foo.el`](\\/tmp\\/foo.el#L3C2)\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "foo.el")
      (let ((pos (1- (point))))
	(should (button-at pos))
	(should (eq (get-text-property pos 'face) 'link))
	(should (eq (get-text-property pos 'action) #'codex-ide-renderer-open-file-link))
	(should (equal (get-text-property pos 'codex-ide-path) "/tmp/foo.el"))
	(should (= (get-text-property pos 'codex-ide-line) 3))
	(should (= (get-text-property pos 'codex-ide-column) 2))
	(should (equal (get-text-property pos 'display) "foo.el")))))

  (ert-deftest codex-ide-render-markdown-region-renders-file-links-with-permissive-wrappers ()
    (with-temp-buffer
      (insert
       (mapconcat
	#'identity
	'("One [`foo.txt`](</tmp/foo.txt>)"
          "Two [`foo.txt`](</tmp/foo.txt:123>)"
          "Three [`foo.txt`](    /tmp/foo.txt   )"
          "Four [`foo.txt`](  </tmp/foo.txt:888  )")
	"\n"))
      (insert "\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max))
      (goto-char (point-min))
      (dolist (expected-line '(nil 123 nil 888))
	(search-forward "foo.txt")
	(let ((pos (1- (point))))
          (should (button-at pos))
          (should (equal (get-text-property pos 'codex-ide-path) "/tmp/foo.txt"))
          (if expected-line
              (should (= (get-text-property pos 'codex-ide-line) expected-line))
            (should-not (get-text-property pos 'codex-ide-line)))
          (should-not (get-text-property pos 'codex-ide-column))))))

  (ert-deftest codex-ide-render-markdown-region-renders-inline-code ()
    (with-temp-buffer
      (insert "prefix `code` suffix")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "code")
      (let ((code-pos (- (point) 2))
            (open-tick-pos 8)
            (close-tick-pos 13))
	(should (eq (get-text-property code-pos 'face) 'font-lock-keyword-face))
	(should (get-text-property code-pos 'codex-ide-markdown))
	(should (equal (get-text-property open-tick-pos 'display) ""))
	(should (equal (get-text-property close-tick-pos 'display) "")))))

  (ert-deftest codex-ide-render-markdown-region-renders-fenced-code-blocks ()
    (with-temp-buffer
      (insert "```elisp\n(setq x 1)\n```\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max))
      (goto-char (point-min))
      (should (equal (get-text-property (point-min) 'display) ""))
      (search-forward "setq")
      (let ((code-pos (- (point) 2)))
	(should (get-text-property code-pos 'codex-ide-markdown))
	(should (or (get-text-property code-pos 'face)
                    (get-text-property code-pos 'font-lock-face))))
      (goto-char (point-max))
      (forward-line -1)
      (should (equal (get-text-property (point) 'display) ""))))

  (ert-deftest codex-ide-render-markdown-region-renders-pipe-tables ()
    (with-temp-buffer
      (insert "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max))
      (let ((rendered (buffer-string)))
	(should (string-match-p "^| Name | Age |" rendered))
	(should (string-match-p "^|------|----:|$" rendered))
	(should (string-match-p "^| Bob  |   3 |$" rendered)))
      (should-not (get-text-property (point-min) 'display))
      (should (get-text-property (point-min) 'codex-ide-markdown-table-original))))

  (ert-deftest codex-ide-render-markdown-region-renders-file-links-inside-pipe-tables ()
    (with-temp-buffer
      (insert "| File |\n| --- |\n| [`foo.el`](/tmp/foo.el#L3C2) |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max))
      (let ((rendered (buffer-string)))
	(should (string-match-p "^| File   |" rendered))
	(should (string-match-p "^| foo\\.el |" rendered))
	(should-not (string-match-p (regexp-quote "[`foo.el`](/tmp/foo.el#L3C2)")
                                    rendered)))
      (goto-char (point-min))
      (search-forward "foo.el")
      (let ((pos (match-beginning 0)))
	(should (button-at pos))
	(should (equal (get-text-property pos 'codex-ide-path) "/tmp/foo.el"))
	(should (equal (get-text-property pos 'codex-ide-line) 3))
	(should (equal (get-text-property pos 'codex-ide-column) 2)))))

  (ert-deftest codex-ide-render-markdown-region-defers-trailing-pipe-tables ()
    (with-temp-buffer
      (insert "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n")
      (codex-ide-renderer-render-markdown-region (point-min) (point-max) nil)
      (should (equal (buffer-string)
                     "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n"))
      (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
      (should (string-match-p "^| Bob  |   3 |$" (buffer-string)))
      (should-not (get-text-property (point-min) 'display))))

  (ert-deftest codex-ide-agent-message-completion-renders-trailing-pipe-tables ()
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
					(should (string-match-p
						 (regexp-quote "| Bob | 3 |")
						 (buffer-string))))
				      (codex-ide--handle-notification
				       session
				       '((method . "item/completed")
					 (params . ((item . ((id . "msg-1")
							     (type . "agentMessage")
							     (status . "completed")))))))
				      (with-current-buffer (codex-ide-session-buffer session)
					(let ((rendered (buffer-string)))
					  (should (string-match-p "^| Name | Age |" rendered))
					  (should (string-match-p "^| Bob  |   3 |$" rendered))
					  (should-not (text-property-any
						       (point-min)
						       (point-max)
						       'display
						       t))
					  (goto-char (point-min))
					  (search-forward "| Bob  |   3 |")
					  (should (get-text-property
						   (match-beginning 0)
						   'codex-ide-markdown-table-original)))))))))

  (ert-deftest codex-ide-render-file-change-diff-text-omits-detail-prefix ()
    (with-temp-buffer
      (codex-ide--render-file-change-diff-text
       (current-buffer)
       (mapconcat #'identity
                  '("diff --git a/foo b/foo"
                    "@@ -1 +1 @@"
                    "-old"
                    "+new")
                  "\n"))
      (should (equal (buffer-string)
                     (concat
                      "diff:\n"
                      "diff --git a/foo b/foo\n"
                      "@@ -1 +1 @@\n"
                      "-old\n"
                      "+new\n")))))

  (ert-deftest codex-ide-package-generate-autoloads-captures-public-entry-points ()
    (let* ((temp-dir (make-temp-file "codex-ide-autoloads-" t))
           (autoload-file nil))
      (unwind-protect
          (progn
            (dolist (file '("codex-ide.el"
                            "codex-ide-delete-session-thread.el"
                            "codex-ide-mcp-bridge.el"
                            "codex-ide-transient.el"))
              (copy-file (expand-file-name file codex-ide-test--root-directory)
			 (expand-file-name file temp-dir)
			 t))
            (setq autoload-file
                  (expand-file-name
                   (package-generate-autoloads "codex-ide" temp-dir)
                   temp-dir))
            (should (file-exists-p autoload-file))
            (with-temp-buffer
              (insert-file-contents autoload-file)
              (let ((contents (buffer-string)))
		(should (string-match-p "(get 'codex-ide 'custom-loads)" contents))
		(should (string-match-p "(custom-autoload 'codex-ide-cli-path " contents))
		(should (string-match-p "(autoload 'codex-ide " contents))
		(should (string-match-p "(autoload 'codex-ide-delete-session-thread "
					contents))
		(should (string-match-p "(autoload 'codex-ide-menu " contents))
		(should (string-match-p "(autoload 'codex-ide-mcp-bridge-enable "
					contents)))))
	(delete-directory temp-dir t)))))

(ert-deftest codex-ide-render-markdown-region-renders-emphasis ()
  (with-temp-buffer
    (insert "This is **bold** and *italic* plus __strong_with_underscores__ and _emphasis_.\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "This is bold and italic plus strong_with_underscores and emphasis.\n"))
    (goto-char (point-min))
    (search-forward "bold")
    (should (eq (get-text-property (1- (point)) 'face) 'bold))
    (search-forward "italic")
    (should (eq (get-text-property (1- (point)) 'face) 'italic))
    (search-forward "strong_with_underscores")
    (should (eq (get-text-property (1- (point)) 'face) 'bold))
    (search-forward "emphasis")
    (should (eq (get-text-property (1- (point)) 'face) 'italic))))

(ert-deftest codex-ide-render-markdown-region-keeps-emphasis-after-rerender ()
  (with-temp-buffer
    (insert "This is **bold** and _italic_.\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (should (equal (buffer-string) "This is bold and italic.\n"))
    (goto-char (point-min))
    (search-forward "bold")
    (should (eq (get-text-property (1- (point)) 'face) 'bold))
    (search-forward "italic")
    (should (eq (get-text-property (1- (point)) 'face) 'italic))))

(ert-deftest codex-ide-render-markdown-region-keeps-intraword-underscores-literal ()
  (with-temp-buffer
    (insert "Keep my_table_id literal.\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "Keep my_table_id literal.\n"))
    (goto-char (point-min))
    (search-forward "my_table_id")
    (should-not (text-property-not-all (match-beginning 0) (match-end 0)
                                       'face nil))))

(ert-deftest codex-ide-render-markdown-region-renders-bold-with-internal-underscores ()
  (with-temp-buffer
    (insert "Render **bold_with_underscores** and __strong_with_underscores__.\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "Render bold_with_underscores and strong_with_underscores.\n"))
    (goto-char (point-min))
    (search-forward "bold_with_underscores")
    (should (eq (get-text-property (1- (point)) 'face) 'bold))
    (search-forward "strong_with_underscores")
    (should (eq (get-text-property (1- (point)) 'face) 'bold))))

(ert-deftest codex-ide-render-markdown-region-renders-table-emphasis ()
  (with-temp-buffer
    (insert "| Kind | Value |\n| --- | --- |\n| **Bold** | _italic_ |\n| Star bold | **bold_with_underscores** |\n| Underscore bold | __strong_with_underscores__ |\n")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
    (goto-char (point-min))
    (search-forward "Bold")
    (should (memq 'bold
                  (ensure-list (get-text-property (1- (point)) 'face))))
    (search-forward "italic")
    (should (memq 'italic
                  (ensure-list (get-text-property (1- (point)) 'face))))
    (search-forward "bold_with_underscores")
    (should (memq 'bold
                  (ensure-list (get-text-property (1- (point)) 'face))))
    (search-forward "strong_with_underscores")
    (should (memq 'bold
                  (ensure-list (get-text-property (1- (point)) 'face))))))

(ert-deftest codex-ide-render-markdown-region-caches-code-block-font-lock-setup ()
  (let ((codex-ide--font-lock-spec-cache (make-hash-table :test 'eq))
        (mode-call-count 0))
    (cl-letf (((symbol-function 'codex-ide-test-cached-mode)
               (lambda ()
                 (setq mode-call-count (1+ mode-call-count))
                 (kill-all-local-variables)
                 (setq major-mode 'codex-ide-test-cached-mode)
                 (setq mode-name "Codex Test Cached")
                 (setq-local font-lock-defaults
                             '((("\\_<foo\\_>" . font-lock-keyword-face)))))))
      (with-temp-buffer
        (insert "```codex-ide-test-cached\nfoo\n```\n")
        (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
        (codex-ide-renderer-render-markdown-region (point-min) (point-max) t)
        (should (= mode-call-count 1))
        (goto-char (point-min))
        (search-forward "foo")
        (should (memq 'font-lock-keyword-face
                      (ensure-list
                       (get-text-property (1- (point)) 'face))))))))

(provide 'codex-ide-tests)

;;; codex-ide-tests.el ends here
