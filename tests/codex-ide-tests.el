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
(require 'codex-ide-delete-session-thread-tests)
(require 'codex-ide-session-buffer-list-tests)
(require 'codex-ide-session-thread-list-tests)

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
                      (codex-ide--canonical-session-for-directory project-dir)))
          (with-current-buffer (codex-ide-session-buffer session)
            (should (derived-mode-p 'codex-ide-session-mode))
            (should visual-line-mode)
            (should (string-match-p "Codex session for" (buffer-string))))
          (with-current-buffer (codex-ide-session-log-buffer session)
            (should (derived-mode-p 'codex-ide-log-mode))
            (should (equal (buffer-name)
                           (format "*%s-log[%s]*"
                                   codex-ide-buffer-name-prefix
                                   (file-name-nondirectory
                                    (directory-file-name project-dir)))))
            (should (string-match-p "Codex log for" (buffer-string))))
          (should
           (equal (plist-get (codex-ide-test-process-plist
                              (codex-ide-session-process session))
                             'codex-session)
                  session)))))))

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
              (should (eq shown (codex-ide-session-buffer session)))
              (should (eq (current-buffer) (codex-ide-session-buffer session))))))))))

(ert-deftest codex-ide-display-buffer-prefers-window-already-showing-buffer ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (delete-other-windows)
        (let* ((origin-buffer (current-buffer))
               (target-buffer (get-buffer-create " *codex-display-target*"))
               (session-window nil)
               (target-window nil))
          (codex-ide-test-with-fake-processes
            (let ((session (codex-ide--create-process-session)))
              (setq session-window (split-window-right))
              (set-window-buffer session-window (codex-ide-session-buffer session))
              (select-window (selected-window))
              (setq target-window (split-window-below))
              (set-window-buffer (selected-window) origin-buffer)
              (set-window-buffer target-window target-buffer)
              (select-window (get-buffer-window origin-buffer))
              (let ((codex-ide-focus-on-open t))
                (let ((codex-ide-display-buffer-options
                       '(:reuse-buffer-window :reuse-mode-window)))
                  (should (eq (codex-ide-display-buffer target-buffer)
                              target-window)))
                (should (eq (selected-window) target-window))
                (should (eq (window-buffer target-window) target-buffer))
                (let ((codex-ide-display-buffer-options
                       '(:reuse-buffer-window :reuse-mode-window)))
                  (should (eq (codex-ide-display-buffer target-buffer)
                              target-window)))))))))))

(ert-deftest codex-ide-display-buffer-reuses-visible-codex-window-when-enabled ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (delete-other-windows)
        (let ((target-buffer (get-buffer-create " *codex-display-mode-target*")))
          (codex-ide-test-with-fake-processes
            (let* ((origin-window (selected-window))
                   (session (codex-ide--create-process-session))
                   (session-window (split-window-right)))
              (set-window-buffer session-window (codex-ide-session-buffer session))
              (let ((codex-ide-focus-on-open nil))
                (let ((codex-ide-display-buffer-options '(:reuse-mode-window)))
                  (should (eq (codex-ide-display-buffer target-buffer)
                              session-window)))
                (should (eq (window-buffer session-window) target-buffer))
                (should (eq (selected-window) origin-window))))))))))

(ert-deftest codex-ide-display-buffer-falls-back-to-selected-window ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (remembered nil))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (delete-other-windows)
        (let ((target-buffer (get-buffer-create " *codex-display-fallback*")))
          (cl-letf (((symbol-function 'codex-ide--remember-buffer-context-before-switch)
                     (lambda (&optional buffer)
                       (setq remembered (or buffer (current-buffer))))))
            (let ((origin-window (selected-window))
                  (origin-buffer (current-buffer))
                  (codex-ide-focus-on-open nil))
              (should (eq (codex-ide-display-buffer target-buffer)
                          origin-window))
              (should (eq remembered origin-buffer))
              (should (eq (window-buffer origin-window) target-buffer))
              (should (eq (selected-window) origin-window)))))))))

(ert-deftest codex-ide-display-buffer-splits-when-new-window-is-enabled ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (delete-other-windows)
        (let ((target-buffer (get-buffer-create " *codex-display-split*")))
          (let ((origin-window (selected-window))
                (codex-ide-display-buffer-options '(:new-window))
                (codex-ide-focus-on-open nil))
            (let ((window (codex-ide-display-buffer target-buffer)))
              (should (window-live-p window))
              (should-not (eq window origin-window))
              (should (eq (window-buffer window) target-buffer))
              (should (= (length (window-list nil 'no-minibuf)) 2)))))))))

(ert-deftest codex-ide-display-new-session-buffer-uses-vertical-split ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (delete-other-windows)
        (let ((target-buffer (get-buffer-create " *codex-new-session-vertical*"))
              (origin-window (selected-window))
              (codex-ide-new-session-split 'vertical)
              (codex-ide-focus-on-open nil))
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
              (codex-ide-focus-on-open nil))
          (let* ((origin-top (nth 1 (window-edges origin-window)))
                 (window (codex-ide--display-new-session-buffer target-buffer))
                 (window-top (nth 1 (window-edges window))))
            (should (window-live-p window))
            (should-not (eq window origin-window))
            (should (> window-top origin-top))
            (should (eq (window-buffer window) target-buffer))
            (should (eq (selected-window) origin-window))))))))

(ert-deftest codex-ide-sessions-for-directory-returns-live-sessions-in-registry-order ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((first (codex-ide--create-process-session))
              second)
          (setq second (codex-ide--create-process-session))
          (should (equal (codex-ide--sessions-for-directory project-dir t)
                         (list second first)))
          (should (eq (codex-ide--canonical-session-for-directory project-dir)
                      first))
          (delete-process (codex-ide-session-process second))
          (codex-ide--cleanup-dead-sessions)
          (should (equal (codex-ide--sessions-for-directory project-dir t)
                         (list first)))
          (should (eq (codex-ide--canonical-session-for-directory project-dir)
                      first)))))))

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
          (should (equal (buffer-name (codex-ide-session-log-buffer second))
                         (format "*%s-log[%s]<1>*"
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

(ert-deftest codex-ide-session-mode-binds-tab-to-button-navigation ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (should (eq (key-binding (kbd "TAB")) #'forward-button))
    (should (eq (key-binding (kbd "<backtab>")) #'backward-button))))

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
                   (lambda (_buffer) (selected-window)))
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
              (goto-char (point-max))
              (forward-line 0)
              (should (looking-at-p "> ")))))))))

(ert-deftest codex-ide-start-session-new-honors-new-session-split ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (requests '()))
    (codex-ide-test-with-fixture project-dir
      (save-window-excursion
        (delete-other-windows)
        (let ((origin-window (selected-window))
              (codex-ide-new-session-split 'vertical)
              (codex-ide-focus-on-open nil))
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

(ert-deftest codex-ide-first-submit-injects-session-context-once ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "src/example.el" "(message \"hello\")\n"))
         (requests '()))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((codex-ide-session-baseline-prompt "Project background instructions")
              (session (codex-ide--create-process-session)))
          (setf (codex-ide-session-thread-id session) "thread-test-2")
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
                       (lambda (_session method params)
                         (push (cons method params) requests)
                         nil)))
              (codex-ide--submit-prompt)
              (codex-ide--finish-turn session)
              (codex-ide--replace-current-input session "Explain again")
              (codex-ide--submit-prompt)))
          (let* ((calls (seq-filter (lambda (entry) (equal (car entry) "turn/start"))
                                    (nreverse requests)))
                 (first-text (alist-get 'text (aref (alist-get 'input (cdr (nth 0 calls))) 0)))
                 (second-text (alist-get 'text (aref (alist-get 'input (cdr (nth 1 calls))) 0))))
            (should (string-match-p "\\[Emacs session context\\]" first-text))
            (should (string-match-p "Project background instructions" first-text))
            (should (string-match-p "\\[Emacs prompt context\\]" first-text))
            (should (string-match-p "Explain this" first-text))
            (should-not (string-match-p "\\[Emacs session context\\]" second-text))
            (should (string-match-p "\\[Emacs prompt context\\]" second-text))
            (should (string-match-p "Explain again" second-text))
            (should (codex-ide--session-metadata-get session :session-context-sent))))))))

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
            (should (string= (codex-ide-session-status session) "idle"))))))))

(ert-deftest codex-ide-show-session-buffer-adds-prompt-to-idle-query-session ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (requests nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (cl-letf (((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (push method requests)
                     (pcase method
                       ("initialize" '((ok . t)))
                       (_ (ert-fail (format "Unexpected method %s" method))))))
                  ((symbol-function 'codex-ide-display-buffer)
                   (lambda (_buffer) (selected-window))))
          (let ((session (codex-ide--ensure-query-session-for-thread-selection
                          project-dir)))
            (should-not (codex-ide--input-prompt-active-p session))
            (codex-ide--show-session-buffer session)
            (should (equal (seq-remove (lambda (method)
                                         (equal method "config/read"))
                                       (nreverse requests))
                           '("initialize")))
            (should (codex-ide--input-prompt-active-p session))
            (with-current-buffer (codex-ide-session-buffer session)
              (goto-char (marker-position
                          (codex-ide-session-input-prompt-start-marker session)))
              (should (looking-at-p "> ")))))))))

(ert-deftest codex-ide-show-or-resume-thread-reuses-idle-threadless-session ()
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
                   (lambda (_buffer) (selected-window))))
          (let ((query-session (codex-ide--ensure-query-session-for-thread-selection
                                project-dir)))
            (codex-ide--show-session-buffer query-session)
            (setq requests nil)
            (let ((session (codex-ide--show-or-resume-thread "thread-reused-1"
                                                             project-dir)))
              (should (eq session query-session))
              (should (= (length codex-ide--sessions) 1))
              (should (equal (seq-remove (lambda (method)
                                           (equal method "config/read"))
                                         (nreverse requests))
                             '("thread/read" "thread/resume")))
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
                   (lambda (_buffer) (selected-window)))
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
                   (lambda (_buffer) (selected-window)))
                  ((symbol-function 'codex-ide--request-sync)
                   (lambda (_session method _params)
                     (pcase method
                       ("initialize" '((ok . t)))
                       ("thread/start" '((thread . ((id . "thread-current")))))
                       (_ (ert-fail (format "Unexpected method %s" method)))))))
          (let ((session (codex-ide--start-session 'new)))
            (with-current-buffer (codex-ide-session-buffer session)
              (should (= (how-many "^> " (point-min) (point-max)) 1))
              (goto-char (point-max))
              (forward-line 0)
              (should (looking-at-p "> $")))))))))

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
            (should (looking-at-p "> hello"))
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
            (should (looking-at-p "> h"))
            (should (string= (codex-ide--current-input session) "h"))))))))

(ert-deftest codex-ide-compose-turn-input-includes-context-on-every-send ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "src/example.el" "(message \"hello\")\n")))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((codex-ide-session-baseline-prompt "Session instructions")
              (session (codex-ide--create-process-session)))
          (with-current-buffer (find-file-noselect file-path)
            (setq-local default-directory (file-name-as-directory project-dir))
            (goto-char (point-min))
            (forward-line 0)
            (move-to-column 3)
            (let ((context (codex-ide--make-buffer-context)))
              (puthash (alist-get 'project-dir context)
                       context
                       codex-ide--active-buffer-contexts)
              (let ((codex-ide--session session))
                (let* ((first-item (aref (codex-ide--compose-turn-input "Explain this") 0))
                       (_ (codex-ide--session-metadata-put session :session-context-sent t))
                       (second-item (aref (codex-ide--compose-turn-input "Explain again") 0))
                       (first-text (alist-get 'text first-item))
                       (second-text (alist-get 'text second-item)))
                  (should (string-match-p "Last file/buffer focused in Emacs: .*src/example\\.el"
                                          first-text))
                  (should-not (string-match-p "\\[Emacs session context\\]" second-text))
                  (should (string-match-p "\\[Emacs session context\\]" first-text))
                  (should (string-match-p "\\[/Emacs session context\\]" first-text))
                  (should (string-match-p "\\[Emacs prompt context\\]" first-text))
                  (should (string-match-p "\\[/Emacs prompt context\\]" first-text))
                  (should-not (string-match-p "\\[Emacs session context\\]" second-text))
                  (should (string-match-p "\\[Emacs prompt context\\]" second-text))
                  (should (string-match-p "Last file/buffer focused in Emacs: .*src/example\\.el"
                                          second-text))
                  (should (string-match-p "Explain again" second-text)))))))))))

(ert-deftest codex-ide-compose-turn-input-includes-selected-region-when-active ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "src/example.el" "(message \"hello\")\n"))
         (prompt-text nil))
    (codex-ide-test-with-fixture project-dir
      (let ((transient-mark-mode t))
        (with-current-buffer (find-file-noselect file-path)
          (setq-local default-directory (file-name-as-directory project-dir))
          (goto-char (point-min))
          (forward-char 1)
          (push-mark (point) t t)
          (forward-char 7)
          (activate-mark)
          (codex-ide--track-active-buffer (current-buffer)))
        (with-temp-buffer
          (setq default-directory (file-name-as-directory project-dir))
          (setq prompt-text (alist-get 'text (aref (codex-ide--compose-turn-input "Explain this") 0))))
        (should (string-match-p "Selected region: line 1, column 1 to line 1, column 8"
                                prompt-text))))))

(ert-deftest codex-ide-compose-turn-input-does-not-duplicate-prompt-context-block ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "src/example.el" "(message \"hello\")\n")))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-session-baseline-prompt "Session instructions")
            (session (codex-ide--create-process-session)))
        (with-current-buffer (find-file-noselect file-path)
          (setq-local default-directory (file-name-as-directory project-dir))
          (goto-char (point-min))
          (let ((context (codex-ide--make-buffer-context)))
            (puthash (alist-get 'project-dir context)
                     context
                     codex-ide--active-buffer-contexts)
            (let* ((prompt (codex-ide--format-buffer-context context))
                   (text (alist-get 'text
                                    (aref (let ((codex-ide--session session))
                                            (codex-ide--compose-turn-input prompt))
                                          0))))
              (with-temp-buffer
                (insert text)
                (goto-char (point-min))
                (should (= 1 (how-many "\\[Emacs prompt context\\]" (point-min) (point-max)))))
              (should (string-match-p "\\[Emacs session context\\]" text)))))))))

(ert-deftest codex-ide-prompt-uses-origin-buffer-context-for-non-file-buffers ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (other-dir (codex-ide-test--make-temp-project))
         (submitted nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((transient-mark-mode t)
              (session (codex-ide--create-process-session)))
          (setf (codex-ide-session-thread-id session) "thread-test-origin")
          (with-current-buffer (get-buffer-create "*codex origin*")
            (setq-local default-directory (file-name-as-directory other-dir))
            (erase-buffer)
            (insert "scratch buffer contents")
            (goto-char (point-min))
            (forward-char 1)
            (push-mark (point) t t)
            (forward-char 6)
            (activate-mark)
            (cl-letf (((symbol-function 'read-from-minibuffer)
                      (lambda (&rest _) "Explain this"))
                      ((symbol-function 'codex-ide--ensure-session-for-current-project)
                       (lambda () session))
                      ((symbol-function 'codex-ide-display-buffer)
                       (lambda (_buffer) (selected-window)))
                      ((symbol-function 'codex-ide--request-sync)
                       (lambda (_session _method params)
                         (setq submitted params)
                         nil)))
              (codex-ide-prompt)))
          (let* ((input (alist-get 'input submitted))
                 (text (alist-get 'text (aref input 0))))
            (should (string-match-p "\\[Emacs prompt context\\]" text))
            (should (string-match-p "\\[/Emacs prompt context\\]" text))
            (should (string-match-p
                     "Last file/buffer focused in Emacs: \\[buffer\\] \\*codex origin\\*"
                     text))
            (should (string-match-p "Buffer: \\*codex origin\\*" text))
            (should (string-match-p "Selected region: line 1, column 1 to line 1, column 7"
                                    text))
            (should (string-match-p "Explain this" text))))))))

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
                         "\n> Explain this\nContext: file=.*src/example\\.el\" buffer=\"example\\.el\" line=1 column=3"
                         buffer-text))
                (should (string-match-p "\\[Emacs prompt context\\]"
                                        (alist-get 'text (aref input 0))))
                (should (string-match-p "\\[/Emacs prompt context\\]"
                                        (alist-get 'text (aref input 0))))))))))))

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

(ert-deftest codex-ide-header-line-shows-reasoning-effort ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (codex-ide-reasoning-effort "high"))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (with-current-buffer (codex-ide-session-buffer session)
            (codex-ide--update-header-line session)
            (should (string-match-p "effort:high"
                                    (substring-no-properties header-line-format)))))))))

(ert-deftest codex-ide-header-line-shows-model-name ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (codex-ide--session-metadata-put session :model-name "gpt-5.4")
          (with-current-buffer (codex-ide-session-buffer session)
            (codex-ide--update-header-line session)
            (should (string-match-p "model:gpt-5\\.4"
                                    (substring-no-properties header-line-format)))))))))

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
            (should (string-match-p "model:gpt-5\\.4-mini"
                                    (substring-no-properties header-line-format)))
            (should-not (string-match-p "model:gpt-5\\.4\\([^.-]\\|$\\)"
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
              (should-not (string-match-p "model:"
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
              (should (string-match-p "model:gpt-5\\.4-mini"
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
          (codex-ide--session-metadata-put session :model-name "gpt-5.4")
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_time _repeat function)
                       (funcall function)))
                    ((symbol-function 'codex-ide-display-buffer)
                     (lambda (buffer)
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
          (should (string-match-p "Codex:Approval"
                                  (codex-ide--mode-line-status session)))
          (should (= (hash-table-count (codex-ide--pending-approvals session)) 1))
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((text (buffer-string))
                  (separator (string-trim-right
                              (codex-ide--output-separator-string))))
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
                             (codex-ide--output-separator-string)))
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
                     (lambda (_buffer) (selected-window)))
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
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((text (buffer-string)))
              (should (string-match-p "Approve file changes: edit foo\\.txt" text))
              (should (string-match-p "Proposed changes:\n\n" text))
              (should (< (string-match-p "Proposed changes:" text)
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
            (search-forward "[accept]")
            (backward-char 1)
            (push-button))
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
                     (lambda (_buffer) (selected-window)))
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
          (with-current-buffer (codex-ide-session-log-buffer session)
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
                                  (codex-ide--mode-line-status session))))))))

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
                                  (codex-ide--mode-line-status session))))))))

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
            (goto-char (point-max))
            (forward-line 0)
            (should (looking-at-p "> "))))))))

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
              (should-not (string-match-p "^> $" text))))
          (with-current-buffer (codex-ide-session-log-buffer session)
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
                   (prompts (how-many "^> " (point-min) (point-max))))
              (should (= prompts 1))
              (goto-char (point-max))
              (forward-line 0)
              (should (looking-at-p "> "))
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
                          (codex-ide-session-log-buffer session)))
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

(ert-deftest codex-ide-restore-thread-read-transcript-errors-on-unsupported-turn-shape ()
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
                                  (items . (((type . "commandExecution")
                                             (id . "item-1")))))))))))
         :type 'error)))))

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
                              (codex-ide--restored-transcript-separator-string)))
                     buffer-text))
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
             "\n\n> What is MyTable's primary key\\?")
            (buffer-string))))))))

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
                   (regexp-quote "> Line one\nLine two\nLine three")
                   (buffer-string)))
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
                   (lambda (_buffer) (selected-window)))
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
                (should (string-match-p "^> Why is resume stale\\?" buffer-text))
                (should (string-match-p "The prompt was restored too early\\." buffer-text))
                (should (string-match-p
                         (concat (regexp-quote "The prompt was restored too early.")
                                 "\n"
                                 (regexp-quote
                                  (codex-ide--restored-transcript-separator-string))
                                 "> ")
                         buffer-text))
                (goto-char (point-max))
                (forward-line 0)
                (should (looking-at-p "> "))))))))))

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
            (should (equal (mapcar #'car (nreverse requests))
                           '("thread/read" "thread/resume")))
            (with-current-buffer (codex-ide-session-buffer session)
              (let ((buffer-text (buffer-string)))
                (should (string-match-p "^> Resume this exact thread\\." buffer-text))
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
                   (lambda (_buffer) (selected-window)))
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
                  (should (string-match-p "^> Switch threads"
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
                   (lambda (buffer)
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
                   (lambda (buffer)
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
                   (lambda (_buffer) (selected-window)))
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

(ert-deftest codex-ide-mcp-bridge-get-all-open-file-buffers-uses-shared-buffer-info ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "lib/buffer-info.el" "(message \"hi\")\n"))
         (buffer (find-file-noselect file-path)))
    (unwind-protect
        (with-current-buffer buffer
          (emacs-lisp-mode)
          (set-buffer-modified-p t)
          (let* ((result (codex-ide-mcp-bridge--tool-call--get_all_open_file_buffers nil))
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
    (should (re-search-forward "name=\"get_buffer_info\"" nil t))
    (should (re-search-forward "name=\"get_buffer_text\"" nil t))))

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
                (insert "old transcript\n"))
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
              (should-not (string-match-p "old transcript" (buffer-string))))
            (should (equal (seq-remove (lambda (method)
                                         (equal method "config/read"))
                                       requests)
                           '("thread/unsubscribe"
                             "initialize"
                             "thread/start")))))))))

(ert-deftest codex-ide-context-payload-uses-explicit-non-file-origin-buffer ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((origin-buffer (generate-new-buffer " *codex-ide-origin*")))
        (unwind-protect
            (with-current-buffer origin-buffer
              (setq-local default-directory (file-name-as-directory project-dir))
              (insert "ephemeral")
              (goto-char (point-min))
              (forward-char 3)
              (let ((codex-ide--prompt-origin-buffer origin-buffer))
                (let* ((payload (codex-ide--context-payload-for-prompt))
                       (formatted (alist-get 'formatted payload))
                       (summary (alist-get 'summary payload)))
                  (should (string-match-p
                           "Last file/buffer focused in Emacs: \\[buffer\\]  \\*codex-ide-origin\\*"
                           formatted))
                  (should (string-match-p "Buffer:  \\*codex-ide-origin\\*" formatted))
                  (should (string-match-p "Cursor: line 1, column 3" formatted))
                  (should (string-match-p "Context: file=\"\\[buffer\\]  \\*codex-ide-origin\\*\"" summary))
                  (should (string-match-p "buffer=\" \\*codex-ide-origin\\*\"" summary)))))
          (kill-buffer origin-buffer))))))

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
              (setq first-input (+ (line-beginning-position) 2))
              (forward-line 2)
              (setq second-input (+ (line-beginning-position) 2))
              (goto-char (point-max))
              (forward-line -1)
              (should (looking-at-p "> second prompt"))
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
              (insert "> first prompt\nassistant reply\n"))
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
            (should (looking-at-p "> draft")))))))))

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
            (should (looking-at-p "> retry prompt"))))))))

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
            (should (looking-at-p "> "))))))))

(ert-deftest codex-ide-mcp-bridge-request-exempt-from-approval-detects-nested-bridge-mentions ()
  (let ((codex-ide-emacs-bridge-require-approval nil)
        (codex-ide-emacs-tool-bridge-name "emacs"))
    (should
     (codex-ide-mcp-bridge-request-exempt-from-approval-p
      `((tool . "shell")
        (metadata . ((server . "mcp_servers.emacs")
                     (arguments . [((name . "view_file_buffer"))
                                   ((path . "/tmp/example.el"))]))))))
    (let ((codex-ide-emacs-bridge-require-approval t))
      (should-not
       (codex-ide-mcp-bridge-request-exempt-from-approval-p
        '((metadata . ((server . "mcp_servers.emacs")))))))
    (let ((codex-ide-emacs-bridge-require-approval nil))
      (should-not
       (codex-ide-mcp-bridge-request-exempt-from-approval-p
        '((tool . "shell")
          (metadata . ((server . "mcp_servers.other")
                       (arguments . [((name . "different_tool"))])))))))))

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
      (codex-ide--render-markdown-region markdown-start markdown-end)
      (should (eq (get-text-property summary-start 'face)
                  'codex-ide-item-summary-face))
      (codex-ide--render-markdown-region markdown-start (point-max))
      (should (eq (get-text-property summary-start 'face)
                  'codex-ide-item-summary-face)))))

(ert-deftest codex-ide-render-markdown-region-renders-file-links ()
  (with-temp-buffer
    (insert "See [`foo.el`](/tmp/foo.el#L3C2)\n")
    (codex-ide--render-markdown-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "foo.el")
    (let ((pos (1- (point))))
      (should (button-at pos))
      (should (eq (get-text-property pos 'face) 'link))
      (should (eq (get-text-property pos 'action) #'codex-ide--open-file-link))
      (should (equal (get-text-property pos 'codex-ide-path) "/tmp/foo.el"))
      (should (= (get-text-property pos 'codex-ide-line) 3))
      (should (= (get-text-property pos 'codex-ide-column) 2))
      (should (equal (get-text-property pos 'display) "foo.el")))))

(ert-deftest codex-ide-parse-file-link-target-normalizes-escaped-slashes ()
  (should
   (equal (codex-ide--parse-file-link-target "\\/tmp\\/foo.el#L3C2")
          '("/tmp/foo.el" 3 2))))

(ert-deftest codex-ide-render-markdown-region-renders-file-links-with-escaped-slashes ()
  (with-temp-buffer
    (insert "See [`foo.el`](\\/tmp\\/foo.el#L3C2)\n")
    (codex-ide--render-markdown-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "foo.el")
    (let ((pos (1- (point))))
      (should (button-at pos))
      (should (eq (get-text-property pos 'face) 'link))
      (should (eq (get-text-property pos 'action) #'codex-ide--open-file-link))
      (should (equal (get-text-property pos 'codex-ide-path) "/tmp/foo.el"))
      (should (= (get-text-property pos 'codex-ide-line) 3))
      (should (= (get-text-property pos 'codex-ide-column) 2))
      (should (equal (get-text-property pos 'display) "foo.el")))))

(ert-deftest codex-ide-render-markdown-region-renders-inline-code ()
  (with-temp-buffer
    (insert "prefix `code` suffix")
    (codex-ide--render-markdown-region (point-min) (point-max))
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
    (codex-ide--render-markdown-region (point-min) (point-max))
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
    (codex-ide--render-markdown-region (point-min) (point-max))
    (let ((rendered (buffer-string)))
      (should (string-match-p "^| Name | Age |" rendered))
      (should (string-match-p "^|------|----:|$" rendered))
      (should (string-match-p "^| Bob  |   3 |$" rendered)))
    (should-not (get-text-property (point-min) 'display))
    (should (get-text-property (point-min) 'codex-ide-markdown-table-original))))

(ert-deftest codex-ide-render-markdown-region-renders-file-links-inside-pipe-tables ()
  (with-temp-buffer
    (insert "| File |\n| --- |\n| [`foo.el`](/tmp/foo.el#L3C2) |\n")
    (codex-ide--render-markdown-region (point-min) (point-max))
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
    (codex-ide--render-markdown-region (point-min) (point-max) nil)
    (should (equal (buffer-string)
                   "| Name | Age |\n| --- | ---: |\n| Bob | 3 |\n"))
    (codex-ide--render-markdown-region (point-min) (point-max) t)
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
    (codex-ide--render-markdown-region (point-min) (point-max))
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
    (codex-ide--render-markdown-region (point-min) (point-max))
    (codex-ide--render-markdown-region (point-min) (point-max))
    (should (equal (buffer-string) "This is bold and italic.\n"))
    (goto-char (point-min))
    (search-forward "bold")
    (should (eq (get-text-property (1- (point)) 'face) 'bold))
    (search-forward "italic")
    (should (eq (get-text-property (1- (point)) 'face) 'italic))))

(ert-deftest codex-ide-render-markdown-region-keeps-intraword-underscores-literal ()
  (with-temp-buffer
    (insert "Keep my_table_id literal.\n")
    (codex-ide--render-markdown-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "Keep my_table_id literal.\n"))
    (goto-char (point-min))
    (search-forward "my_table_id")
    (should-not (text-property-not-all (match-beginning 0) (match-end 0)
                                       'face nil))))

(ert-deftest codex-ide-render-markdown-region-renders-bold-with-internal-underscores ()
  (with-temp-buffer
    (insert "Render **bold_with_underscores** and __strong_with_underscores__.\n")
    (codex-ide--render-markdown-region (point-min) (point-max))
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
    (codex-ide--render-markdown-region (point-min) (point-max) t)
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
        (codex-ide--render-markdown-region (point-min) (point-max))
        (codex-ide--render-markdown-region (point-min) (point-max))
        (should (= mode-call-count 1))
        (goto-char (point-min))
        (search-forward "foo")
        (should (eq (get-text-property (1- (point)) 'face)
                    'font-lock-keyword-face))))))

(provide 'codex-ide-tests)

;;; codex-ide-tests.el ends here
