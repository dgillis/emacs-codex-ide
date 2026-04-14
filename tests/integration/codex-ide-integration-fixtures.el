;;; codex-ide-integration-fixtures.el --- Real Codex integration fixtures -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared helpers for opt-in integration tests that exercise codex-ide against
;; a real `codex app-server' process inside an isolated Emacs process.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)
(require 'codex-ide)

(defvar codex-ide-integration-default-timeout 180
  "Default timeout in seconds for real Codex integration waits.")

(defun codex-ide-integration--log (format-string &rest args)
  "Log a codex-ide integration message using FORMAT-STRING and ARGS."
  (apply #'message (concat "codex-ide-it: " format-string) args))

(defun codex-ide-integration--enabled-p ()
  "Return non-nil when real Codex integration tests are explicitly enabled."
  (member (getenv "CODEX_IDE_INTEGRATION_TESTS") '("1" "true" "yes")))

(defun codex-ide-integration-require-enabled ()
  "Skip the current test unless real Codex integration tests are enabled."
  (unless (codex-ide-integration--enabled-p)
    (ert-skip "Set CODEX_IDE_INTEGRATION_TESTS=1 to run real Codex integration tests")))

(defun codex-ide-integration--timeout ()
  "Return the configured integration wait timeout in seconds."
  (or (when-let ((value (getenv "CODEX_IDE_INTEGRATION_TIMEOUT")))
        (let ((seconds (string-to-number value)))
          (when (> seconds 0)
            seconds)))
      codex-ide-integration-default-timeout))

(defun codex-ide-integration--project-directory ()
  "Return the project directory for real Codex integration tests."
  (file-name-as-directory
   (expand-file-name
    (or (getenv "CODEX_IDE_INTEGRATION_PROJECT_DIR")
        default-directory))))

(defun codex-ide-integration--session-workspace (session)
  "Return SESSION's effective workspace directory."
  (file-name-as-directory
   (expand-file-name (codex-ide-session-directory session))))

(defun codex-ide-integration--token (&optional prefix)
  "Return a unique sentinel token using PREFIX."
  (format "%s_%s"
          (or prefix "CODEX_IDE_IT")
          (substring (md5 (format "%s:%s:%s"
                                  (float-time)
                                  (random)
                                  (emacs-pid)))
                     0 10)))

(defun codex-ide-integration--buffer-contains-p (buffer regexp)
  "Return non-nil when BUFFER contains REGEXP."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward regexp nil t)))))

(defun codex-ide-integration--buffer-match-count (buffer regexp)
  "Return how many times REGEXP appears in BUFFER."
  (if (not (buffer-live-p buffer))
      0
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (re-search-forward regexp nil t)
            (setq count (1+ count)))
          count)))))

(defun codex-ide-integration--buffer-has-markdown-match-p (buffer regexp)
  "Return non-nil when BUFFER has a REGEXP match marked as markdown."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let (found)
          (while (and (not found)
                      (re-search-forward regexp nil t))
            (setq found
                  (get-text-property (match-beginning 0)
                                     'codex-ide-markdown)))
          found)))))

(defun codex-ide-integration-submit-prompt-and-wait-for-token
    (session prompt token &optional description)
  "Submit PROMPT to SESSION and wait until TOKEN appears in the agent response.
DESCRIPTION is used in progress logs and timeout messages."
  (let ((buffer (codex-ide-session-buffer session))
        (description (or description token)))
    (codex-ide-integration--log
     "%s: submitting prompt token=%s buffer=%S"
     description
     token
     (buffer-name buffer))
    (with-current-buffer buffer
      (codex-ide--replace-current-input session prompt)
      (codex-ide--submit-prompt))
    (codex-ide-integration--log
     "%s: turn/start request returned; waiting for token"
     description)
    (codex-ide-integration--wait-until
     (lambda ()
       (>= (codex-ide-integration--buffer-match-count
            buffer
            (regexp-quote token))
           2))
     nil
     (format "%s agent response token %s\n%s"
             description
             token
             (codex-ide-integration--failure-context session)))
    (codex-ide-integration--log
     "%s: token observed; waiting for idle"
     description)
    (codex-ide-integration--wait-for-session-idle session)
    (codex-ide-integration--log
     "%s: completed"
     description)
    buffer))

(defun codex-ide-integration--wait-until (predicate &optional timeout description)
  "Wait until PREDICATE returns non-nil, or signal a timeout.
TIMEOUT is in seconds.  DESCRIPTION is included in timeout errors."
  (let* ((timeout (or timeout (codex-ide-integration--timeout)))
         (deadline (+ (float-time) timeout))
         (description (or description "integration condition"))
         (last-log-time 0)
        value)
    (codex-ide-integration--log "waiting up to %ss for %s" timeout description)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (when (>= (- (float-time) last-log-time) 5)
        (setq last-log-time (float-time))
        (codex-ide-integration--log "still waiting for %s" description))
      (accept-process-output nil 0.2))
    (unless value
      (codex-ide-integration--log "timed out waiting for %s" description)
      (error "Timed out waiting for %s" description))
    (codex-ide-integration--log "finished waiting for %s" description)
    value))

(defun codex-ide-integration--wait-for-session-idle (session &optional timeout)
  "Wait for SESSION to return to idle after a real Codex turn."
  (codex-ide-integration--log
   "waiting for session idle; current status=%S turn=%S thread=%S"
   (codex-ide-session-status session)
   (codex-ide-session-current-turn-id session)
   (codex-ide-session-thread-id session))
  (codex-ide-integration--wait-until
   (lambda ()
     (and (buffer-live-p (codex-ide-session-buffer session))
          (process-live-p (codex-ide-session-process session))
          (not (codex-ide-session-current-turn-id session))
          (string= (codex-ide-session-status session) "idle")
          (codex-ide--input-prompt-active-p session)
          session))
   timeout
   "Codex session to become idle"))

(defun codex-ide-integration--tail (buffer &optional chars)
  "Return the last CHARS characters of BUFFER without text properties."
  (if (not (buffer-live-p buffer))
      "<dead buffer>"
    (with-current-buffer buffer
      (buffer-substring-no-properties
       (max (point-min) (- (point-max) (or chars 4000)))
       (point-max)))))

(defun codex-ide-integration--failure-context (session)
  "Return diagnostic context for SESSION."
  (format "status=%S turn=%S thread=%S\n\nTranscript tail:\n%s\n\nLog tail:\n%s"
          (and session (codex-ide-session-status session))
          (and session (codex-ide-session-current-turn-id session))
          (and session (codex-ide-session-thread-id session))
          (if session
              (codex-ide-integration--tail
               (codex-ide-session-buffer session))
            "<no session>")
          (if (and session (codex-ide-session-log-buffer session))
              (codex-ide-integration--tail
               (codex-ide-session-log-buffer session))
            "<no log buffer>")))

(defun codex-ide-integration--configure ()
  "Configure codex-ide for real integration tests."
  (setq codex-ide-cli-path (or (getenv "CODEX_IDE_CLI_PATH") codex-ide-cli-path))
  (setq codex-ide-request-timeout
        (max codex-ide-request-timeout
             (min 60 (codex-ide-integration--timeout))))
  (setq codex-ide-session-baseline-prompt nil)
  ;; Keep the first integration tier focused on core app-server/session UI
  ;; behavior.  The Emacs MCP bridge adds another process, server readiness, and
  ;; tool-dispatch surface, so it should get its own opt-in integration coverage
  ;; once the baseline real-Codex path is stable.
  (setq codex-ide-want-mcp-bridge nil)
  (setq codex-ide-enable-emacs-tool-bridge nil)
  (when-let ((model (getenv "CODEX_IDE_INTEGRATION_MODEL")))
    (unless (string-empty-p model)
      (setq codex-ide-model model)))
  (setq codex-ide-approval-policy
        (or (getenv "CODEX_IDE_INTEGRATION_APPROVAL_POLICY")
            codex-ide-approval-policy))
  (setq codex-ide-sandbox-mode
        (or (getenv "CODEX_IDE_INTEGRATION_SANDBOX_MODE")
            codex-ide-sandbox-mode))
  (codex-ide-integration--log
   "configured cli=%S model=%S approval=%S sandbox=%S mcp=%S timeout=%s"
   codex-ide-cli-path
   codex-ide-model
   codex-ide-approval-policy
   codex-ide-sandbox-mode
   codex-ide-want-mcp-bridge
   (codex-ide-integration--timeout)))

(defun codex-ide-integration--cleanup-session (session)
  "Stop SESSION and kill its buffers."
  (when session
    (codex-ide-integration--log
     "cleaning up session status=%S thread=%S buffer=%S"
     (codex-ide-session-status session)
     (codex-ide-session-thread-id session)
     (and (codex-ide-session-buffer session)
          (buffer-name (codex-ide-session-buffer session))))
    (ignore-errors
      (when (and (codex-ide-session-process session)
                 (process-live-p (codex-ide-session-process session))
                 (codex-ide-session-thread-id session))
        (codex-ide-integration--log
         "unsubscribing thread %s"
         (codex-ide-session-thread-id session))
        (codex-ide--request-sync
         session
         "thread/unsubscribe"
         `((threadId . ,(codex-ide-session-thread-id session))))))
    (ignore-errors
      (when (and (codex-ide-session-process session)
                 (process-live-p (codex-ide-session-process session)))
        (codex-ide-integration--log "deleting app-server process")
        (delete-process (codex-ide-session-process session))))
    (ignore-errors
      (when (and (codex-ide-session-stderr-process session)
                 (process-live-p (codex-ide-session-stderr-process session)))
        (codex-ide-integration--log "deleting stderr process")
        (delete-process (codex-ide-session-stderr-process session))))
    (dolist (buffer (list (codex-ide-session-buffer session)
                          (codex-ide-session-log-buffer session)))
      (when (buffer-live-p buffer)
        (codex-ide-integration--log "killing buffer %s" (buffer-name buffer))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))
    (setq codex-ide--sessions (delq session codex-ide--sessions))
    (codex-ide-integration--log "session cleanup complete")))

(defmacro codex-ide-integration-with-session (&rest body)
  "Run BODY with a real Codex session in the integration project.
The variable `session' is bound to the live `codex-ide-session' object and
`project-dir' is bound to the integration project directory."
  (declare (indent 0) (debug t))
  `(progn
     (codex-ide-integration-require-enabled)
     (codex-ide-integration--configure)
     (unless (executable-find codex-ide-cli-path)
       (ert-skip (format "Codex CLI not found: %s" codex-ide-cli-path)))
     (let* ((project-dir (codex-ide-integration--project-directory))
            (default-directory project-dir)
            (buffers-before (buffer-list))
            (session nil)
            (codex-ide--cli-available nil)
            (codex-ide--sessions nil)
            (codex-ide--active-buffer-contexts (make-hash-table :test 'equal))
            (codex-ide--active-buffer-objects (make-hash-table :test 'equal))
            (codex-ide-persisted-project-state (make-hash-table :test 'equal))
            (codex-ide--session-metadata (make-hash-table :test 'eq)))
       (codex-ide-integration--log "requested project=%s" project-dir)
       (codex-ide-integration--log "starting real Codex session")
       (unwind-protect
           (progn
             (setq session (codex-ide--start-session 'new))
             (codex-ide-integration--log
              "effective session workspace=%s"
              (codex-ide-integration--session-workspace session))
             (codex-ide-integration--log
              "started session buffer=%S thread=%S status=%S process-live=%S"
              (buffer-name (codex-ide-session-buffer session))
              (codex-ide-session-thread-id session)
              (codex-ide-session-status session)
              (process-live-p (codex-ide-session-process session)))
             (codex-ide-integration--wait-for-session-idle session 30)
             ,@body)
         (codex-ide-integration--cleanup-session session)
         (dolist (buffer (buffer-list))
           (unless (memq buffer buffers-before)
             (when (buffer-live-p buffer)
               (codex-ide-integration--log
                "killing test-created buffer %s"
                (buffer-name buffer))
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer buffer)))))
         (codex-ide-integration--log "fixture cleanup complete")))))

(provide 'codex-ide-integration-fixtures)

;;; codex-ide-integration-fixtures.el ends here
