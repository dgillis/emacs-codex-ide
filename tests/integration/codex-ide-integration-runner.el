;;; codex-ide-integration-runner.el --- Interactive real Codex integration runner -*- lexical-binding: t; -*-

;;; Commentary:

;; Timer-driven integration runner for watching real codex-ide behavior in a
;; normal Emacs session.  This intentionally avoids ERT's synchronous test body
;; for the interactive path so Emacs remains responsive while Codex streams.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'codex-ide-integration-fixtures)
(require 'codex-ide-session-buffer-list)

(defvar codex-ide-integration-run-buffer-name "*codex-ide Integration*"
  "Buffer used by the interactive codex-ide integration runner.")

(defvar codex-ide-integration-run-poll-interval 0.5
  "Seconds between interactive integration runner polls.")

(defvar codex-ide-integration-run-keep-buffers t
  "Whether the interactive integration runner should keep session buffers.

Keeping buffers is useful while debugging real Codex behavior because the
transcript and log buffers remain available after the run finishes.")

(defvar codex-ide-integration-run--active-state nil
  "Current interactive integration run state.")

(define-error 'codex-ide-integration-run-failed
  "Interactive codex-ide integration run failed")

(cl-defstruct (codex-ide-integration-run-state
               (:constructor codex-ide-integration-run--state-create))
  project-dir
  buffer
  buffers-before
  session
  started-at
  timer
  completed
  failed
  completed-checks)

(defun codex-ide-integration-run--buffer ()
  "Return the interactive integration output buffer."
  (get-buffer-create codex-ide-integration-run-buffer-name))

(defun codex-ide-integration-run--log (state format-string &rest args)
  "Log FORMAT-STRING and ARGS to STATE's buffer and `*Messages*'."
  (let* ((text (apply #'format format-string args))
         (line (format "[%s] %s\n"
                       (format-time-string "%H:%M:%S")
                       text))
         (buffer (or (and state
                          (codex-ide-integration-run-state-buffer state))
                     (codex-ide-integration-run--buffer))))
    (codex-ide-integration--log "%s" text)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert line))))))

(defun codex-ide-integration-run--display-buffer (state)
  "Display STATE's integration output buffer."
  (when-let ((buffer (and state (codex-ide-integration-run-state-buffer state))))
    (when (buffer-live-p buffer)
      (display-buffer buffer))))

(defun codex-ide-integration-run--mark-check (state label)
  "Record LABEL as a completed checkpoint for STATE."
  (setf (codex-ide-integration-run-state-completed-checks state)
        (append (codex-ide-integration-run-state-completed-checks state)
                (list label)))
  (codex-ide-integration-run--log state "CHECK PASS: %s" label))

(defun codex-ide-integration-run--completed-checks-summary (state)
  "Return a display summary of STATE's completed checkpoints."
  (if-let ((checks (codex-ide-integration-run-state-completed-checks state)))
      (mapconcat (lambda (check)
                   (format "- %s" check))
                 checks
                 "\n")
    "- none"))

(defun codex-ide-integration-run--cancel-timer (state)
  "Cancel STATE's pending timer, if any."
  (when-let ((timer (and state (codex-ide-integration-run-state-timer state))))
    (cancel-timer timer)
    (setf (codex-ide-integration-run-state-timer state) nil)))

(defun codex-ide-integration-run-cancel ()
  "Cancel the active interactive codex-ide integration run."
  (interactive)
  (if-let ((state codex-ide-integration-run--active-state))
      (progn
        (codex-ide-integration-run--cancel-timer state)
        (setf (codex-ide-integration-run-state-completed state) t)
        (codex-ide-integration-run--log state "cancelled by user")
        (codex-ide-integration-run--display-buffer state))
    (message "No active codex-ide integration run")))

(defun codex-ide-integration-run--fail (state format-string &rest args)
  "Mark STATE failed with FORMAT-STRING and ARGS."
  (codex-ide-integration-run--cancel-timer state)
  (setf (codex-ide-integration-run-state-failed state) t
        (codex-ide-integration-run-state-completed state) t)
  (apply #'codex-ide-integration-run--log
         state
         (concat "FAIL: " format-string)
         args)
  (when-let ((session (codex-ide-integration-run-state-session state)))
    (codex-ide-integration-run--log
     state
     "failure context:\n%s"
     (codex-ide-integration--failure-context session)))
  (codex-ide-integration-run--log
   state
   "leaving Codex buffers alive for inspection")
  (codex-ide-integration-run--log
   state
   "completed checkpoints before failure:\n%s"
   (codex-ide-integration-run--completed-checks-summary state))
  (codex-ide-integration-run--log state "FINAL RESULT: FAIL")
  (codex-ide-integration-run--display-buffer state))

(defun codex-ide-integration-run--pass (state)
  "Mark STATE passed."
  (codex-ide-integration-run--cancel-timer state)
  (setf (codex-ide-integration-run-state-completed state) t)
  (if codex-ide-integration-run-keep-buffers
      (codex-ide-integration-run--log
       state
       "keeping Codex session buffers for inspection")
    (codex-ide-integration-run--cleanup state))
  (codex-ide-integration-run--log
   state
   "completed checkpoints:\n%s"
   (codex-ide-integration-run--completed-checks-summary state))
  (codex-ide-integration-run--log
   state
   "FINAL RESULT: PASS in %.1fs"
   (- (float-time) (codex-ide-integration-run-state-started-at state)))
  (codex-ide-integration-run--display-buffer state))

(defun codex-ide-integration-run--assert (state condition format-string &rest args)
  "Fail STATE unless CONDITION is non-nil."
  (unless condition
    (apply #'codex-ide-integration-run--fail state format-string args)
    (signal 'codex-ide-integration-run-failed
            (list (apply #'format format-string args)))))

(defun codex-ide-integration-run--schedule (state seconds function)
  "Schedule FUNCTION for STATE after SECONDS."
  (codex-ide-integration-run--cancel-timer state)
  (setf (codex-ide-integration-run-state-timer state)
        (run-at-time seconds nil function)))

(defun codex-ide-integration-run--wait (state description predicate on-success
                                              &optional timeout started-at last-log-at)
  "Wait asynchronously for PREDICATE, then call ON-SUCCESS.
DESCRIPTION is used for log output.  TIMEOUT is in seconds."
  (let* ((timeout (or timeout (codex-ide-integration--timeout)))
         (started-at (or started-at (float-time)))
         (last-log-at (or last-log-at 0)))
    (condition-case err
        (cond
         ((funcall predicate)
          (codex-ide-integration-run--log
           state
           "finished waiting for %s"
           description)
          (funcall on-success))
         ((> (- (float-time) started-at) timeout)
          (codex-ide-integration-run--fail
           state
           "timed out after %ss waiting for %s"
           timeout
           description))
         (t
          (when (>= (- (float-time) last-log-at) 5)
            (setq last-log-at (float-time))
            (codex-ide-integration-run--log
             state
             "still waiting for %s"
             description))
          (codex-ide-integration-run--schedule
           state
           codex-ide-integration-run-poll-interval
           (lambda ()
             (codex-ide-integration-run--wait
              state
              description
              predicate
              on-success
              timeout
              started-at
              last-log-at)))))
      (error
       (unless (codex-ide-integration-run-state-failed state)
         (codex-ide-integration-run--fail
          state
          "error while waiting for %s: %s"
          description
          (error-message-string err)))))))

(defun codex-ide-integration-run--session-idle-p (state)
  "Return non-nil when STATE's session is idle and ready."
  (when-let ((session (codex-ide-integration-run-state-session state)))
    (and (buffer-live-p (codex-ide-session-buffer session))
         (process-live-p (codex-ide-session-process session))
         (not (codex-ide-session-current-turn-id session))
         (string= (codex-ide-session-status session) "idle")
         (codex-ide--input-prompt-active-p session))))

(defun codex-ide-integration-run--assert-session-basics (state)
  "Assert the initial real Codex session basics for STATE."
  (let ((session (codex-ide-integration-run-state-session state)))
    (codex-ide-integration-run--log state "asserting session basics")
    (codex-ide-integration-run--assert
     state
     (codex-ide-session-p session)
     "session object was not created")
    (codex-ide-integration-run--assert
     state
     (process-live-p (codex-ide-session-process session))
     "app-server process is not live")
    (codex-ide-integration-run--assert
     state
     (buffer-live-p (codex-ide-session-buffer session))
     "session buffer is not live")
    (codex-ide-integration-run--assert
     state
     (and (stringp (codex-ide-session-thread-id session))
          (not (string-empty-p (codex-ide-session-thread-id session))))
     "session has no thread id")
    (codex-ide-integration-run--assert
     state
     (string= (codex-ide-session-status session) "idle")
     "session status is %S, not idle"
     (codex-ide-session-status session))
    (with-current-buffer (codex-ide-session-buffer session)
      (codex-ide-integration-run--assert
       state
       (derived-mode-p 'codex-ide-session-mode)
       "session buffer is not in codex-ide-session-mode")
      (codex-ide-integration-run--assert
       state
       (codex-ide--input-prompt-active-p session)
       "session input prompt is not active")
      (codex-ide-integration-run--assert
       state
       (string-match-p "Codex session for" (buffer-string))
       "session header was not rendered")))
  (codex-ide-integration-run--mark-check state "session startup and initial UI"))

(defun codex-ide-integration-run--submit-prompt
    (state token prefix next-step &optional prompt after-idle)
  "Submit a sentinel prompt for STATE and continue with NEXT-STEP.
TOKEN is the sentinel token and PREFIX names the current step.
When PROMPT is nil, use a simple sentinel prompt.  When AFTER-IDLE is non-nil,
call it after the turn returns to idle and before NEXT-STEP."
  (let* ((session (codex-ide-integration-run-state-session state))
         (buffer (codex-ide-session-buffer session))
         (prompt (or prompt
                     (format "Reply with one short sentence containing the exact token %s. Do not edit files."
                             token))))
    (codex-ide-integration-run--log
     state
     "%s: submitting prompt token=%s buffer=%S"
     prefix
     token
     (buffer-name buffer))
    (with-current-buffer buffer
      (codex-ide--replace-current-input session prompt)
      (codex-ide--submit-prompt))
    (codex-ide-integration-run--log
     state
     "%s: turn/start request returned; waiting for streamed token"
     prefix)
    (codex-ide-integration-run--wait
     state
     (format "%s token %s in agent response" prefix token)
     (lambda ()
       (>= (codex-ide-integration--buffer-match-count
            buffer
            (regexp-quote token))
           2))
     (lambda ()
       (codex-ide-integration-run--log
        state
        "%s: token observed; waiting for session idle"
        prefix)
       (codex-ide-integration-run--wait
        state
        (format "%s session idle after token" prefix)
        (lambda ()
          (codex-ide-integration-run--session-idle-p state))
        (lambda ()
          (codex-ide-integration-run--assert
           state
           (equal (with-current-buffer buffer
                    (codex-ide--current-input session))
                  "")
           "%s input prompt is not empty"
           prefix)
          (when after-idle
            (funcall after-idle buffer prompt))
          (codex-ide-integration-run--mark-check
           state
           (format "%s turn completed and rendered" prefix))
          (funcall next-step)))))))

(defun codex-ide-integration-run--check-session-list (state next-step)
  "Open and validate the live session buffer list for STATE."
  (let* ((session (codex-ide-integration-run-state-session state))
         (session-buffer-name (buffer-name (codex-ide-session-buffer session))))
    (codex-ide-integration-run--log state "opening live session buffer list")
    (codex-ide-session-buffer-list)
    (with-current-buffer "*Codex Session Buffers*"
      (codex-ide-integration-run--assert
       state
       (derived-mode-p 'codex-ide-session-buffer-list-mode)
       "session buffer list mode was not enabled")
      (codex-ide-integration-run--assert
       state
       (string-match-p (regexp-quote session-buffer-name) (buffer-string))
       "session buffer list did not include %s"
       session-buffer-name)
      (codex-ide-integration-run--assert
       state
       (seq-some
        (lambda (entry)
          (eq (car entry) session))
        (funcall tabulated-list-entries))
       "session buffer list entries did not include session for %s"
       session-buffer-name)
      (codex-ide-integration-run--assert
       state
       (string-match-p "Reply with one short sentence" (buffer-string))
       "session buffer list did not include last prompt preview")
      (codex-ide-integration-run--assert
       state
       (string-match-p "Idle" (buffer-string))
       "session buffer list did not show Idle status")))
  (codex-ide-integration-run--mark-check state "live session buffer list")
  (funcall next-step))

(defun codex-ide-integration-run--check-prompt-history
    (state first-prompt second-prompt latest-prompt next-step)
  "Validate prompt history navigation for STATE.
FIRST-PROMPT, SECOND-PROMPT, and LATEST-PROMPT are expected history entries."
  (let* ((session (codex-ide-integration-run-state-session state))
         (buffer (codex-ide-session-buffer session)))
    (codex-ide-integration-run--log state "checking prompt history navigation")
    (with-current-buffer buffer
      (codex-ide--browse-prompt-history -1)
      (codex-ide-integration-run--assert
       state
       (string= (codex-ide--current-input session) latest-prompt)
       "latest prompt history entry did not match")
      (codex-ide--browse-prompt-history -1)
      (codex-ide-integration-run--assert
       state
       (string= (codex-ide--current-input session) second-prompt)
       "second prompt history entry did not match")
      (codex-ide--browse-prompt-history -1)
      (codex-ide-integration-run--assert
       state
       (string= (codex-ide--current-input session) first-prompt)
       "first prompt history entry did not match")
      (codex-ide--browse-prompt-history 1)
      (codex-ide-integration-run--assert
       state
       (string= (codex-ide--current-input session) second-prompt)
       "prompt history forward navigation did not return second prompt")
      (codex-ide--browse-prompt-history 1)
      (codex-ide-integration-run--assert
       state
       (string= (codex-ide--current-input session) latest-prompt)
       "prompt history forward navigation did not return latest prompt")
      (codex-ide--browse-prompt-history 1)
      (codex-ide-integration-run--assert
       state
       (string= (codex-ide--current-input session) "")
       "prompt history did not return to empty draft")))
  (codex-ide-integration-run--mark-check state "prompt history navigation")
  (funcall next-step))

(defun codex-ide-integration-run--cleanup (state)
  "Clean up STATE after an interactive run."
  (when-let ((session (codex-ide-integration-run-state-session state)))
    (codex-ide-integration--cleanup-session session))
  (dolist (buffer (buffer-list))
    (unless (memq buffer (codex-ide-integration-run-state-buffers-before state))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

;;;###autoload
(defun codex-ide-integration-run ()
  "Run the interactive real Codex integration smoke flow."
  (interactive)
  (unless (codex-ide-integration--enabled-p)
    (user-error "Set CODEX_IDE_INTEGRATION_TESTS=1 to run real Codex integration tests"))
  (when (and codex-ide-integration-run--active-state
             (not (codex-ide-integration-run-state-completed
                   codex-ide-integration-run--active-state)))
    (codex-ide-integration-run-cancel))
  (let* ((project-dir (codex-ide-integration--project-directory))
         (buffer (codex-ide-integration-run--buffer))
         (state (codex-ide-integration-run--state-create
                 :project-dir project-dir
                 :buffer buffer
                 :buffers-before (buffer-list)
                 :started-at (float-time))))
    (setq codex-ide-integration-run--active-state state)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)))
    (pop-to-buffer buffer)
    (condition-case err
        (progn
          (codex-ide-integration--configure)
          (unless (executable-find codex-ide-cli-path)
            (user-error "Codex CLI not found: %s" codex-ide-cli-path))
          (setq codex-ide--cli-available nil)
          (setq codex-ide--sessions nil)
          (setq codex-ide--active-buffer-contexts (make-hash-table :test 'equal))
          (setq codex-ide--active-buffer-objects (make-hash-table :test 'equal))
          (setq codex-ide-persisted-project-state (make-hash-table :test 'equal))
          (setq codex-ide--session-metadata (make-hash-table :test 'eq))
          (codex-ide-integration-run--log state "requested project=%s" project-dir)
          (codex-ide-integration-run--log state "starting real Codex session")
          (let ((default-directory project-dir))
            (setf (codex-ide-integration-run-state-session state)
                  (codex-ide--start-session 'new)))
          (let ((session (codex-ide-integration-run-state-session state)))
            (setf (codex-ide-integration-run-state-project-dir state)
                  (codex-ide-integration--session-workspace session))
            (codex-ide-integration-run--log
             state
             "effective session workspace=%s"
             (codex-ide-integration-run-state-project-dir state))
            (codex-ide-integration-run--log
             state
             "started session buffer=%S thread=%S status=%S process-live=%S"
             (buffer-name (codex-ide-session-buffer session))
             (codex-ide-session-thread-id session)
             (codex-ide-session-status session)
             (process-live-p (codex-ide-session-process session))))
          (codex-ide-integration-run--wait
           state
           "initial session idle"
           (lambda ()
             (codex-ide-integration-run--session-idle-p state))
           (lambda ()
             (condition-case err
                 (progn
                   (codex-ide-integration-run--assert-session-basics state)
                   (let* ((ping-token
                           (codex-ide-integration--token "CODEX_IDE_IT_PING"))
                          (code-token
                           (codex-ide-integration--token "CODEX_IDE_IT_CODE"))
                          (list-token
                           (codex-ide-integration--token "CODEX_IDE_IT_LIST"))
                          (ping-prompt
                           (format "Reply with one short sentence containing the exact token %s. Do not edit files."
                                   ping-token))
                          (code-prompt
                           (format "Reply with only a fenced elisp code block containing this exact token in a comment: %s. Do not edit files."
                                   code-token))
                          (list-prompt
                           (format "Reply with one short sentence containing the exact token %s. Do not edit files."
                                   list-token)))
                     (codex-ide-integration-run--submit-prompt
                      state
                      ping-token
                      "prompt-response"
                      (lambda ()
                        (codex-ide-integration-run--submit-prompt
                         state
                         code-token
                         "markdown-code"
                         (lambda ()
                           (codex-ide-integration-run--submit-prompt
                            state
                            list-token
                            "session-list"
                            (lambda ()
                              (codex-ide-integration-run--check-session-list
                               state
                               (lambda ()
                                 (codex-ide-integration-run--check-prompt-history
                                  state
                                  ping-prompt
                                  code-prompt
                                  list-prompt
                                  (lambda ()
                                    (codex-ide-integration-run--pass state))))))
                            list-prompt))
                         code-prompt
                         (lambda (buffer _prompt)
                           (codex-ide-integration-run--log
                            state
                            "markdown-code: asserting rendered markdown properties")
                           (codex-ide-integration-run--assert
                            state
                            (codex-ide-integration--buffer-has-markdown-match-p
                             buffer
                             (regexp-quote code-token))
                            "markdown-code token was not rendered with markdown properties"))))
                      ping-prompt)))
               (error
                (codex-ide-integration-run--fail
                 state
                 "%s"
                 (error-message-string err)))))
           30)
          (codex-ide-integration-run--log
           state
           "async run scheduled; Emacs should remain responsive"))
      (error
       (codex-ide-integration-run--fail
        state
        "%s"
        (error-message-string err))))
    state))

(provide 'codex-ide-integration-runner)

;;; codex-ide-integration-runner.el ends here
