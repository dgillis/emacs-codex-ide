;;; codex-ide-context-tests.el --- Tests for codex-ide-context -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused tests for prompt context composition and buffer context helpers.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)

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
            (should (numberp
                     (codex-ide-session-last-prompt-submitted-at session)))
            (should (codex-ide--session-metadata-get session :session-context-sent))))))))

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
        (should (string-match-p "Selected region start: 2"
                                prompt-text))
        (should (string-match-p "Selected region end: 9"
                                prompt-text))
        (should (string-match-p "Selected region text: message"
                                prompt-text))))))

(ert-deftest codex-ide-buffer-selection-context-includes-bounds-and-text ()
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "hello world")
      (goto-char (point-min))
      (forward-char 1)
      (push-mark (point) t t)
      (forward-char 5)
      (activate-mark)
      (let ((selection (codex-ide--buffer-selection-context (current-buffer))))
        (should (= (alist-get 'start selection) 2))
        (should (= (alist-get 'end selection) 7))
        (should (= (alist-get 'start-line selection) 1))
        (should (= (alist-get 'start-column selection) 1))
        (should (= (alist-get 'end-line selection) 1))
        (should (= (alist-get 'end-column selection) 6))
        (should (equal (alist-get 'text selection) "ello ")))))) 

(ert-deftest codex-ide-buffer-selection-context-omits-text-past-limit ()
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert (make-string (1+ codex-ide--selection-text-limit) ?x))
      (goto-char (point-min))
      (push-mark (point) t t)
      (goto-char (point-max))
      (activate-mark)
      (let ((selection (codex-ide--buffer-selection-context (current-buffer))))
        (should (= (alist-get 'start selection) 1))
        (should (= (alist-get 'end selection)
                   (+ 2 codex-ide--selection-text-limit)))
        (should-not (alist-get 'text selection)))))) 

(ert-deftest codex-ide-context-with-selected-region-adds-selection ()
  (with-temp-buffer
    (let ((transient-mark-mode t)
          (context '((buffer-name . "example")
                     (line . 1)
                     (column . 0))))
      (insert "hello world")
      (goto-char (point-min))
      (forward-char 1)
      (push-mark (point) t t)
      (forward-char 5)
      (activate-mark)
      (let* ((context-with-selection
              (codex-ide--context-with-selected-region context (current-buffer)))
             (selection (alist-get 'selection context-with-selection)))
        (should (equal (alist-get 'buffer-name context-with-selection) "example"))
        (should (= (alist-get 'start selection) 2))
        (should (= (alist-get 'end selection) 7))
        (should (equal (alist-get 'text selection) "ello "))))))

(ert-deftest codex-ide-track-active-buffer-ignores-non-file-buffers ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((buffer (generate-new-buffer " *codex-ide-untracked*")))
        (unwind-protect
            (with-current-buffer buffer
              (setq-local default-directory (file-name-as-directory project-dir))
              (insert "ephemeral")
              (codex-ide--track-active-buffer buffer)
              (should-not (gethash (codex-ide--normalize-directory project-dir)
                                   codex-ide--active-buffer-contexts))
              (should-not (gethash (codex-ide--normalize-directory project-dir)
                                   codex-ide--active-buffer-objects)))
          (kill-buffer buffer))))))

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
         (submitted nil)
         (minibuffer-prompt nil))
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
                       (lambda (prompt &rest _)
                         (setq minibuffer-prompt prompt)
                         "Explain this"))
                      ((symbol-function 'codex-ide--ensure-session-for-current-project)
                       (lambda () session))
                      ((symbol-function 'codex-ide-display-buffer)
                       (lambda (_buffer &optional _action) (selected-window)))
                      ((symbol-function 'codex-ide--request-sync)
                       (lambda (_session _method params)
                         (setq submitted params)
                         nil)))
              (codex-ide-prompt)))
          (should (equal minibuffer-prompt
                         (format "Send prompt (%s): "
                                 (buffer-name (codex-ide-session-buffer session)))))
          (let* ((input (alist-get 'input submitted))
                 (text (alist-get 'text (aref input 0))))
            (should (string-match-p "\\[Emacs prompt context\\]" text))
            (should (string-match-p "\\[/Emacs prompt context\\]" text))
            (should (string-match-p
                     "Last file/buffer focused in Emacs: \\[buffer\\] \\*codex origin\\*"
                     text))
            (should (string-match-p "Buffer: \\*codex origin\\*" text))
            (should (string-match-p "Selected region start: 2"
                                    text))
            (should (string-match-p "Selected region end: 8"
                                    text))
            (should (string-match-p "Selected region text: cratch"
                                    text))
            (should (string-match-p "Explain this" text))))))))

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
                  (should (string-match-p "Cursor: point 4, line 1, column 3" formatted))
                  (should (string-match-p
                           (regexp-quote "Context:  *codex-ide-origin* 1:3")
                           summary)))))
          (kill-buffer origin-buffer))))))

(ert-deftest codex-ide-make-buffer-context-uses-explicit-working-dir-for-non-file-buffers ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (let ((buffer (generate-new-buffer " *codex-ide-origin*")))
        (unwind-protect
            (with-current-buffer buffer
              (setq-local default-directory (file-name-as-directory project-dir))
              (insert "ephemeral")
              (goto-char (point-min))
              (forward-char 3)
              (let ((context (codex-ide--make-buffer-context
                              buffer
                              :working-dir project-dir)))
                (should (equal (alist-get 'file context) nil))
                (should (equal (alist-get 'buffer-name context)
                               " *codex-ide-origin*"))
                (should (= (alist-get 'point context) 4))
                (should (= (alist-get 'line context) 1))
                (should (= (alist-get 'column context) 3))
                (should (equal (alist-get 'project-dir context)
                               (codex-ide--normalize-directory project-dir)))))
          (kill-buffer buffer))))))

(ert-deftest codex-ide-format-buffer-context-renders-selection-start-end-and-text ()
  (let ((formatted
         (codex-ide--format-buffer-context
          '((file . "/tmp/example.el")
            (buffer-name . "example.el")
            (point . 27)
            (line . 12)
            (column . 4)
            (project-dir . "/tmp/")
            (selection . ((start . 10)
                          (end . 18)
                          (text . "selected")))))))
    (should (string-match-p "Cursor: point 27, line 12, column 4" formatted))
    (should (string-match-p "Selected region start: 10" formatted))
    (should (string-match-p "Selected region end: 18" formatted))
    (should (string-match-p "Selected region text: selected" formatted))
    (should-not (string-match-p "Selected region: line" formatted))))

(ert-deftest codex-ide-format-buffer-context-summary-uses-short-selection-text ()
  (let* ((selection-text "selected")
         (summary
          (codex-ide--format-buffer-context-summary
           `((file . "/tmp/example.el")
             (buffer-name . "example.el")
             (line . 12)
             (column . 4)
             (project-dir . "/tmp/")
             (selection . ((start-line . 2)
                           (start-column . 1)
                           (end-line . 2)
                           (end-column . 9)
                           (text . ,selection-text)))))))
    (should (string-match-p
             (regexp-quote "Context: example.el 12:4 selection=\"selected\"")
             summary))))

(ert-deftest codex-ide-format-buffer-context-summary-uses-range-for-long-selection-text ()
  (let* ((selection-text "selected text")
         (summary
          (codex-ide--format-buffer-context-summary
           `((file . "/tmp/example.el")
             (buffer-name . "example.el")
             (line . 12)
             (column . 4)
             (project-dir . "/tmp/")
             (selection . ((start-line . 2)
                           (start-column . 1)
                           (end-line . 2)
                           (end-column . 14)
                           (text . ,selection-text)))))))
    (should (string-match-p
             (regexp-quote "Context: example.el 12:4 selection=2:1-2:14")
             summary))))

(ert-deftest codex-ide-format-buffer-context-summary-escapes-selection-newlines ()
  (let* ((selection-text "a\nb")
         (summary
          (codex-ide--format-buffer-context-summary
           `((file . "/tmp/example.el")
             (buffer-name . "example.el")
             (line . 12)
             (column . 4)
             (project-dir . "/tmp/")
             (selection . ((start-line . 2)
                           (start-column . 1)
                           (end-line . 2)
                           (end-column . 4)
                           (text . ,selection-text)))))))
    (should (string-match-p
             (regexp-quote "Context: example.el 12:4 selection=\"a\\\\nb\"")
             summary))))

(provide 'codex-ide-context-tests)

;;; codex-ide-context-tests.el ends here
