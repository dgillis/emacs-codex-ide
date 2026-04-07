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
          (should (eq session
                      (gethash (directory-file-name (file-truename project-dir))
                               codex-ide--sessions)))
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

(ert-deftest codex-ide-session-mode-enables-visual-line-mode-by-default ()
  (with-temp-buffer
    (codex-ide-session-mode)
    (should visual-line-mode)))

(ert-deftest codex-ide-session-mode-allows-opting-out-of-visual-line-mode ()
  (let ((codex-ide-session-enable-visual-line-mode nil))
    (with-temp-buffer
      (codex-ide-session-mode)
      (should-not visual-line-mode))))

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
                  ((symbol-function 'codex-ide--display-buffer-in-side-window)
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
            (should (equal (mapcar #'car (nreverse requests))
                           '("initialize" "thread/start")))
            (with-current-buffer (codex-ide-session-buffer session)
              (should (derived-mode-p 'codex-ide-session-mode))
              (goto-char (point-max))
              (forward-line 0)
              (should (looking-at-p "> ")))))))))

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
                  (updatedAt . 1744038896)
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
            (should
             (equal
              (funcall (plist-get recorded-extra-properties :affixation-function)
                       (list selected))
              `((,selected
                 ,(format "%s "
                          (format-time-string "%Y-%m-%dT%H:%M:%S%z"
                                              (seconds-to-time 1744038896)))
                 " [thread-1]"))))))))))

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

(ert-deftest codex-ide-compose-turn-input-includes-context-only-on-first-send ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "src/example.el" "(message \"hello\")\n")))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-include-active-buffer-context 'when-changed))
        (with-current-buffer (find-file-noselect file-path)
          (goto-char (point-min))
          (forward-line 0)
          (move-to-column 3)
          (let ((context (codex-ide--make-buffer-context)))
            (puthash (alist-get 'project-dir context)
                     context
                     codex-ide--active-buffer-contexts)
            (let* ((first-item (aref (codex-ide--compose-turn-input "Explain this") 0))
                   (second-item (aref (codex-ide--compose-turn-input "Explain again") 0))
                   (first-text (alist-get 'text first-item))
                   (second-text (alist-get 'text second-item)))
              (should (string-match-p "\\[Emacs context\\]" first-text))
              (should (string-match-p "\\[/Emacs context\\]" first-text))
              (should (string-match-p "Last file/buffer focused in Emacs: .*src/example\\.el"
                                      first-text))
              (should-not (string-match-p "\\[Emacs context\\]" second-text))
              (should (string= second-text "Explain again")))))))))

(ert-deftest codex-ide-compose-turn-input-includes-selected-region-when-active ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "src/example.el" "(message \"hello\")\n"))
         (prompt-text nil))
    (codex-ide-test-with-fixture project-dir
      (let ((codex-ide-include-active-buffer-context 'always)
            (transient-mark-mode t))
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

(ert-deftest codex-ide-prompt-uses-origin-buffer-context-for-non-file-buffers ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (other-dir (codex-ide-test--make-temp-project))
         (submitted nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((codex-ide-include-active-buffer-context 'always)
              (transient-mark-mode t)
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
                      ((symbol-function 'codex-ide--display-buffer-in-codex-window)
                       (lambda (_) (selected-window)))
                      ((symbol-function 'codex-ide--request-sync)
                       (lambda (_session _method params)
                         (setq submitted params)
                         nil)))
              (codex-ide-prompt)))
          (let* ((input (alist-get 'input submitted))
                 (text (alist-get 'text (aref input 0))))
            (should (string-match-p "\\[Emacs context\\]" text))
            (should (string-match-p "\\[/Emacs context\\]" text))
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
        (let ((codex-ide-include-active-buffer-context 'always)
              (session (codex-ide--create-process-session)))
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
                (should (string-match-p "\\[Emacs context\\]"
                                        (alist-get 'text (aref input 0))))
                (should (string-match-p "\\[/Emacs context\\]"
                                        (alist-get 'text (aref input 0))))))))))))

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

(ert-deftest codex-ide-trace-back-to-log-jumps-to-originating-notification-line ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (shown-buffer nil))
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
                (should (search-forward line nil t))))
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _)
                         (setq shown-buffer buffer)
                         (set-buffer buffer)
                         (selected-window))))
              (codex-ide--trace-back-to-log)
              (should (eq shown-buffer (codex-ide-session-log-buffer session)))
              (should (looking-at-p ".*Processing incoming notification line:")))))))))

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

(ert-deftest codex-ide-item-type-at-point-returns-and-reports-item-type ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (reported nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (codex-ide--handle-notification
           session
           '((method . "turn/started")
             (params . ((turn . ((id . "turn-1")))))))
          (codex-ide--handle-notification
           session
           '((method . "item/reasoning/summaryTextDelta")
             (params . ((delta . "Reasoning summary")))))
          (with-current-buffer (codex-ide-session-buffer session)
            (goto-char (point-min))
            (search-forward "Reasoning summary")
            (backward-char)
            (should (equal (codex-ide--item-type-at-point) "reasoning"))
            (cl-letf (((symbol-function 'called-interactively-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq reported (apply #'format format-string args)))))
              (codex-ide--item-type-at-point))
            (should (equal reported "reasoning"))))))))

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
     (concat "[Emacs context]\n"
             "Buffer: example.el\n"
             "[/Emacs context]\n\n  Explain the failure"))
    "Explain the failure")))

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

(ert-deftest codex-ide-send-active-buffer-context-submits-formatted-context ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "lib/example.el" "(+ 1 2)\n"))
         (submitted nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (setf (codex-ide-session-thread-id session) "thread-test-2")
          (with-current-buffer (find-file-noselect file-path)
            (setq-local default-directory (file-name-as-directory project-dir))
            (goto-char (point-min))
            (forward-char 2)
            (let ((context (codex-ide--make-buffer-context)))
              (puthash (alist-get 'project-dir context)
                       context
                       codex-ide--active-buffer-contexts)
              (cl-letf (((symbol-function 'codex-ide--submit-prompt)
                         (lambda (prompt)
                           (setq submitted prompt))))
                (codex-ide-send-active-buffer-context)
                (should (string-match-p "\\[Emacs context\\]" submitted))
                (should (string-match-p "\\[/Emacs context\\]" submitted))
                (should (string-match-p "Last file/buffer focused in Emacs: .*lib/example\\.el"
                                        submitted))
                (should (string-match-p "Buffer: example.el" submitted))))))))))

(ert-deftest codex-ide-send-active-buffer-context-includes-selected-region ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (file-path (codex-ide-test--make-project-file
                     project-dir "lib/example.el" "(+ 1 2)\n"))
         (submitted nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session))
              (transient-mark-mode t))
          (setf (codex-ide-session-thread-id session) "thread-test-3")
          (with-current-buffer (find-file-noselect file-path)
            (setq-local default-directory (file-name-as-directory project-dir))
            (goto-char (point-min))
            (forward-char 1)
            (push-mark (point) t t)
            (forward-char 4)
            (activate-mark)
            (codex-ide--track-active-buffer (current-buffer)))
          (with-current-buffer (codex-ide-session-buffer session)
            (cl-letf (((symbol-function 'codex-ide--submit-prompt)
                       (lambda (prompt)
                         (setq submitted prompt))))
              (codex-ide-send-active-buffer-context)))
          (should (string-match-p "Selected region: line 1, column 1 to line 1, column 5"
                                  submitted)))))))

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
              (should (string-match-p "(autoload 'codex-ide-menu " contents))
              (should (string-match-p "(autoload 'codex-ide-mcp-bridge-enable "
                                      contents)))))
      (delete-directory temp-dir t))))

(provide 'codex-ide-tests)

;;; codex-ide-tests.el ends here
