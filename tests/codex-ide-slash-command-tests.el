;;; codex-ide-slash-command-tests.el --- Tests for codex-ide slash commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-slash-command'.

;;; Code:

(require 'ert)
(require 'codex-ide)
(require 'codex-ide-slash-command)
(require 'codex-ide-test-fixtures)

(defvar codex-ide-slash-command-test--called nil)

(defun codex-ide-slash-command-test--command ()
  "Record that a test slash command was invoked."
  (interactive)
  (setq codex-ide-slash-command-test--called t))

(defun codex-ide-slash-command-test--capf-candidates (capf)
  "Return all completion candidates exposed by CAPF."
  (all-completions "" (nth 2 capf)))

(defmacro codex-ide-slash-command-test--with-this-command (command &rest body)
  "Run BODY with `this-command' set to COMMAND."
  (declare (indent 1))
  `(let ((previous-command this-command))
     (unwind-protect
         (progn
           (setq this-command ,command)
           ,@body)
       (setq this-command previous-command))))

(ert-deftest codex-ide-slash-command-default-registry-contains-session-commands ()
  (should (equal (codex-ide-slash-command-names)
                 '("buffers" "diff" "fast" "loop" "model" "reasoning" "sessions")))
  (should (eq (codex-ide-slash-command-entry-command
               (codex-ide-slash-command-lookup "loop"))
              'codex-ide-loop-jump-or-create))
  (should (eq (codex-ide-slash-command-entry-command
               (codex-ide-slash-command-lookup "model"))
              'codex-ide-slash-command-set-model))
  (should (eq (codex-ide-slash-command-entry-command
               (codex-ide-slash-command-lookup "reasoning"))
              'codex-ide-slash-command-set-reasoning-effort))
  (should (eq (codex-ide-slash-command-entry-command
               (codex-ide-slash-command-lookup "fast"))
              'codex-ide-slash-command-toggle-fast))
  (should (eq (codex-ide-slash-command-entry-command
               (codex-ide-slash-command-lookup "sessions"))
              'codex-ide-status))
  (should-not (codex-ide-slash-command-lookup "menu"))
  (should-not (codex-ide-slash-command-lookup "config"))
  (should-not (codex-ide-slash-command-lookup "status"))
  (should-not (codex-ide-slash-command-lookup "reset"))
  (should-not (codex-ide-slash-command-lookup "stop")))

(ert-deftest codex-ide-slash-command-dispatches-known-command ()
  (let ((codex-ide-slash-commands
         '(("test" codex-ide-slash-command-test--command "Test command.")))
        (codex-ide-slash-command-test--called nil))
    (should (codex-ide-slash-command-dispatch-prompt "/test"))
    (should codex-ide-slash-command-test--called)))

(ert-deftest codex-ide-slash-command-loop-dispatches-loop-command ()
  (let (called)
    (cl-letf (((symbol-function 'codex-ide-loop-jump-or-create)
               (lambda ()
                 (interactive)
                 (setq called t))))
      (should (codex-ide-slash-command-dispatch-prompt "/loop"))
      (should called))))

(ert-deftest codex-ide-slash-command-rejects-unknown-command ()
  (let ((codex-ide-slash-commands
         '(("test" codex-ide-slash-command-test--command "Test command."))))
    (should-error (codex-ide-slash-command-dispatch-prompt "/missing")
                  :type 'user-error)))

(ert-deftest codex-ide-slash-command-rejects-extra-arguments ()
  (let ((codex-ide-slash-commands
         '(("test" codex-ide-slash-command-test--command "Test command."))))
    (should-error (codex-ide-slash-command-dispatch-prompt "/test later")
                  :type 'user-error)))

(ert-deftest codex-ide-slash-command-ignores-normal-prompt-text ()
  (let ((codex-ide-slash-command-test--called nil))
    (should-not (codex-ide-slash-command-dispatch-prompt "explain /test"))
    (should-not codex-ide-slash-command-test--called)))

(ert-deftest codex-ide-slash-command-exact-p-requires-known-command-without-args ()
  (let ((codex-ide-slash-commands
         '(("test" codex-ide-slash-command-test--command "Test command."))))
    (should (codex-ide-slash-command-exact-p "/test"))
    (should (codex-ide-slash-command-exact-p " /test "))
    (should-not (codex-ide-slash-command-exact-p "/missing"))
    (should-not (codex-ide-slash-command-exact-p "/test later"))
    (should-not (codex-ide-slash-command-exact-p "explain /test"))))

(ert-deftest codex-ide-slash-command-completes-command-names-after-slash ()
  (with-temp-buffer
    (let* ((codex-ide-slash-commands
            '(("menu" ignore "Open menu.")
              ("model" ignore "Set model.")
              ("status" ignore "Open status.")))
           (input-start (copy-marker (point)))
           (session (make-codex-ide-session
                     :buffer (current-buffer)
                     :input-start-marker input-start)))
      (insert "/m")
      (let ((capf (codex-ide-slash-command-completion-at-point session)))
        (should capf)
        (should (= (nth 0 capf) (1+ (marker-position input-start))))
        (should (= (nth 1 capf) (point)))
        (should (equal (sort (all-completions "m" (nth 2 capf)) #'string<)
                       '("menu" "model")))))))

(ert-deftest codex-ide-slash-command-completion-starts-at-empty-name ()
  (with-temp-buffer
    (let* ((codex-ide-slash-commands
            '(("menu" ignore "Open menu.")
              ("status" ignore "Open status.")))
           (input-start (copy-marker (point)))
           (session (make-codex-ide-session
                     :buffer (current-buffer)
                     :input-start-marker input-start)))
      (insert "/")
      (let ((capf (codex-ide-slash-command-completion-at-point session)))
        (should capf)
        (should (equal (sort (codex-ide-slash-command-test--capf-candidates capf)
                             #'string<)
                       '("menu" "status")))))))

(ert-deftest codex-ide-slash-command-completion-exit-submits-exact-command ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            submitted)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (goto-char (codex-ide-session-input-start-marker session))
        (forward-char 5)
        (let* ((capf (codex-ide-slash-command-completion-at-point session))
               (exit-function (plist-get (nthcdr 3 capf) :exit-function)))
          (should exit-function)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'codex-ide-submit)
                     (lambda ()
                       (interactive)
                       (setq submitted t))))
            (codex-ide-slash-command-test--with-this-command
                'minibuffer-choose-completion
              (funcall exit-function "test" 'finished))))
        (should submitted)))))

(ert-deftest codex-ide-slash-command-completion-exit-submits-corfu-insert ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            submitted)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (goto-char (codex-ide-session-input-start-marker session))
        (forward-char 5)
        (let* ((capf (codex-ide-slash-command-completion-at-point session))
               (exit-function (plist-get (nthcdr 3 capf) :exit-function)))
          (should exit-function)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'codex-ide-submit)
                     (lambda ()
                       (interactive)
                       (setq submitted t))))
            (codex-ide-slash-command-test--with-this-command 'corfu-insert
              (funcall exit-function "test" 'finished))))
        (should submitted)))))

(ert-deftest codex-ide-slash-command-completion-exit-does-not-submit-on-tab ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            submitted)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (goto-char (codex-ide-session-input-start-marker session))
        (forward-char 5)
        (let* ((capf (codex-ide-slash-command-completion-at-point session))
               (exit-function (plist-get (nthcdr 3 capf) :exit-function)))
          (should exit-function)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'codex-ide-submit)
                     (lambda ()
                       (interactive)
                       (setq submitted t))))
            (codex-ide-slash-command-test--with-this-command 'completion-at-point
              (funcall exit-function "test" 'finished))))
        (should-not submitted)))))

(ert-deftest codex-ide-slash-command-completion-exit-does-not-submit-on-corfu-complete ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            submitted)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (goto-char (codex-ide-session-input-start-marker session))
        (forward-char 5)
        (let* ((capf (codex-ide-slash-command-completion-at-point session))
               (exit-function (plist-get (nthcdr 3 capf) :exit-function)))
          (should exit-function)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'codex-ide-submit)
                     (lambda ()
                       (interactive)
                       (setq submitted t))))
            (codex-ide-slash-command-test--with-this-command 'corfu-complete
              (funcall exit-function "test" 'finished))))
        (should-not submitted)))))

(ert-deftest codex-ide-slash-command-completion-exit-can-suppress-submit ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            submitted)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (goto-char (codex-ide-session-input-start-marker session))
        (forward-char 5)
        (let* ((capf (codex-ide-slash-command-completion-at-point session))
               (exit-function (plist-get (nthcdr 3 capf) :exit-function)))
          (should exit-function)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'codex-ide-submit)
                     (lambda ()
                       (interactive)
                       (setq submitted t))))
            (let ((codex-ide-slash-command--suppress-completion-submit t))
              (codex-ide-slash-command-test--with-this-command
                  'minibuffer-choose-completion
                (funcall exit-function "test" 'finished)))))
        (should-not submitted)))))

(ert-deftest codex-ide-slash-command-complete-or-submit-completes-partial-and-submits ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            submitted)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/tes")
        (goto-char (codex-ide-session-input-start-marker session))
        (forward-char 4)
        (cl-letf (((symbol-function 'codex-ide-slash-command--complete-at-point)
                   (lambda ()
                     (delete-region
                      (1+ (codex-ide-session-input-start-marker session))
                      (codex-ide-slash-command--input-end-position session))
                     (insert "test")))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat function &rest args)
                     (apply function args)))
                  ((symbol-function 'codex-ide-submit)
                   (lambda ()
                     (interactive)
                     (setq submitted t))))
          (codex-ide-slash-command-complete-or-submit))
        (should (equal (codex-ide-slash-command--current-input session)
                       "/test"))
        (should submitted)))))

(ert-deftest codex-ide-slash-command-completion-exit-ignores-partial-command ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (codex-ide-session-mode)
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            submitted)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/tes")
        (goto-char (codex-ide-session-input-start-marker session))
        (forward-char 4)
        (let* ((capf (codex-ide-slash-command-completion-at-point session))
               (exit-function (plist-get (nthcdr 3 capf) :exit-function)))
          (should exit-function)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat function &rest args)
                       (apply function args)))
                    ((symbol-function 'codex-ide-submit)
                     (lambda ()
                       (interactive)
                       (setq submitted t))))
            (codex-ide-slash-command-test--with-this-command
                'minibuffer-choose-completion
              (funcall exit-function "tes" 'finished))))
        (should-not submitted)))))

(ert-deftest codex-ide-slash-command-completion-is-only-at-prompt-start ()
  (with-temp-buffer
    (let* ((codex-ide-slash-commands
            '(("menu" ignore "Open menu.")))
           (input-start (copy-marker (point)))
           (session (make-codex-ide-session
                     :buffer (current-buffer)
                     :input-start-marker input-start)))
      (insert "hello /m")
      (should-not (codex-ide-slash-command-completion-at-point session)))))

(ert-deftest codex-ide-submit-renders-slash-command-event-without-sending-prompt ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            (codex-ide-slash-command-test--called nil)
            active-during-command
            sent)
        (codex-ide-session-mode)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                   (lambda () session))
                  ((symbol-function 'codex-ide--send-turn-start)
                   (lambda (&rest _)
                     (setq sent t)))
                  ((symbol-function 'codex-ide-slash-command-test--command)
                   (lambda ()
                     (interactive)
                     (setq codex-ide-slash-command-test--called t
                           active-during-command
                           (codex-ide--input-prompt-active-p session))
                     (message "command message"))))
          (codex-ide--submit-prompt))
        (should codex-ide-slash-command-test--called)
        (should-not active-during-command)
        (should-not sent)
        (should (string-match-p
                 (regexp-quote "> /test")
                 (buffer-string)))
        (should (string-match-p
                 (regexp-quote "* Running slash-command")
                 (buffer-string)))
        (should (string-match-p
                 (regexp-quote
                  (concat "> /test\n\n\n"
                          "* Running slash-command\n"
                          "  └ codex-ide-slash-command-test--command\n"
                          "  └ command message\n"
                          "  └ Success\n\n\n> "))
                 (buffer-string)))
        (should-not (string-match-p
                     (regexp-quote "Submitted command:")
                     (buffer-string)))
        (should (codex-ide--input-prompt-active-p session))
        (should (string-empty-p (codex-ide--current-input session)))))))

(ert-deftest codex-ide-submit-slash-command-preserves-pending-local-images ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png")))
    (codex-ide-test-with-fixture project-dir
      (with-temp-buffer
        (let ((codex-ide-slash-commands
               '(("test" codex-ide-slash-command-test--command "Test command.")))
              (session (make-codex-ide-session
                        :buffer (current-buffer)
                        :directory default-directory
                        :thread-id "thread-1"
                        :status "idle"))
              (codex-ide-slash-command-test--called nil)
              sent)
          (codex-ide-session-mode)
          (setq-local codex-ide--session session)
          (codex-ide--insert-input-prompt session "/test")
          (codex-ide--add-pending-local-image session image-path)
          (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                     (lambda () session))
                    ((symbol-function 'codex-ide--send-turn-start)
                     (lambda (&rest _)
                       (setq sent t))))
            (codex-ide--submit-prompt))
          (should codex-ide-slash-command-test--called)
          (should-not sent)
          (should (equal (codex-ide--pending-local-images session)
                         (list image-path))))))))

(ert-deftest codex-ide-submit-running-slash-command-does-not-queue-pending-local-images ()
  (let* ((project-dir (codex-ide-test--make-temp-project))
         (image-path (codex-ide-test--make-project-file
                      project-dir
                      "screenshot.png"
                      "fake-png"))
         (codex-ide-running-submit-action 'queue))
    (codex-ide-test-with-fixture project-dir
      (with-temp-buffer
        (let ((codex-ide-slash-commands
               '(("test" codex-ide-slash-command-test--command "Test command.")))
              (session (make-codex-ide-session
                        :buffer (current-buffer)
                        :directory default-directory
                        :thread-id "thread-1"
                        :current-turn-id "turn-1"
                        :status "running"))
              (codex-ide-slash-command-test--called nil))
          (codex-ide-session-mode)
          (setq-local codex-ide--session session)
          (codex-ide--insert-input-prompt session "/test")
          (codex-ide--add-pending-local-image session image-path)
          (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                     (lambda () session)))
            (codex-ide--submit-prompt))
          (should codex-ide-slash-command-test--called)
          (should-not (codex-ide--queued-prompts session))
          (should (equal (codex-ide--pending-local-images session)
                         (list image-path))))))))

(ert-deftest codex-ide-submit-restores-prompt-when-slash-command-quits ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            quit-seen
            sent)
        (codex-ide-session-mode)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                   (lambda () session))
                  ((symbol-function 'codex-ide--send-turn-start)
                   (lambda (&rest _)
                     (setq sent t)))
                  ((symbol-function 'codex-ide-slash-command-test--command)
                   (lambda ()
                     (interactive)
                     (signal 'quit nil))))
          (condition-case nil
              (codex-ide--submit-prompt)
            (quit
             (setq quit-seen t))))
        (should quit-seen)
        (should-not sent)
        (should (string-match-p
                 (regexp-quote "* Running slash-command")
                 (buffer-string)))
        (should (string-match-p
                 (regexp-quote "  └ Interrupted")
                 (buffer-string)))
        (should (codex-ide--input-prompt-active-p session))
        (should (string-empty-p (codex-ide--current-input session)))))))

(ert-deftest codex-ide-submit-renders-slash-command-failure ()
  (codex-ide-test-with-fixture temporary-file-directory
    (with-temp-buffer
      (let ((codex-ide-slash-commands
             '(("test" codex-ide-slash-command-test--command "Test command.")))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :directory default-directory
                      :thread-id "thread-1"
                      :status "idle"))
            error-seen)
        (codex-ide-session-mode)
        (setq-local codex-ide--session session)
        (codex-ide--insert-input-prompt session "/test")
        (cl-letf (((symbol-function 'codex-ide--session-for-current-project)
                   (lambda () session))
                  ((symbol-function 'codex-ide-slash-command-test--command)
                   (lambda ()
                     (interactive)
                     (user-error "Nope"))))
          (condition-case nil
              (codex-ide--submit-prompt)
            (error
             (setq error-seen t))))
        (should error-seen)
        (should (string-match-p
                 (regexp-quote "  └ Failed: Nope")
                 (buffer-string)))
        (should (codex-ide--input-prompt-active-p session))
        (should (equal (codex-ide--current-input session) "/test"))))))

(ert-deftest codex-ide-slash-command-set-model-targets-current-session ()
  (with-temp-buffer
    (let ((session (make-codex-ide-session :buffer (current-buffer)))
          (codex-ide-model "gpt-5.4")
          (codex-ide-reasoning-effort "medium")
          (reads nil)
          message-text)
      (setq-local codex-ide--session session)
      (cl-letf (((symbol-function 'codex-ide-config-read-value)
                 (lambda (key &optional captured-session context)
                   (should (eq captured-session session))
                   (push (list key context) reads)
                   (pcase key
                     ('model "gpt-5.4-mini")
                     ('reasoning-effort "high"))))
                ((symbol-function 'codex-ide-config-read-scope)
                 (lambda (&rest _)
                   (error "Slash /model should not prompt for scope")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (setq message-text (apply #'format format-string args)))))
        (codex-ide-slash-command-set-model))
      (should (equal (nreverse reads)
                     '((model nil)
                       (reasoning-effort "gpt-5.4-mini"))))
      (should (equal codex-ide-model "gpt-5.4"))
      (should (equal codex-ide-reasoning-effort "medium"))
      (should (equal (codex-ide-config-effective-value 'model session)
                     "gpt-5.4-mini"))
      (should (equal (codex-ide-config-effective-value 'reasoning-effort session)
                     "high"))
      (should (equal message-text
                     "Codex model set to gpt-5.4-mini; reasoning effort set to high for this session.")))))

(ert-deftest codex-ide-slash-command-set-reasoning-targets-current-session ()
  (with-temp-buffer
    (let ((session (make-codex-ide-session :buffer (current-buffer)))
          (codex-ide-reasoning-effort "medium"))
      (setq-local codex-ide--session session)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (codex-ide-slash-command-set-reasoning-effort "high"))
      (should (equal codex-ide-reasoning-effort "medium"))
      (should (equal (codex-ide-config-effective-value 'reasoning-effort session)
                     "high")))))

(ert-deftest codex-ide-slash-command-toggle-fast-targets-current-session ()
  (with-temp-buffer
    (let ((session (make-codex-ide-session :buffer (current-buffer)))
          (codex-ide-fast "off"))
      (setq-local codex-ide--session session)
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (codex-ide-slash-command-toggle-fast)
        (should (equal codex-ide-fast "off"))
        (should (equal (codex-ide-config-effective-value 'fast session) "on"))
        (codex-ide-slash-command-toggle-fast))
      (should (equal codex-ide-fast "off"))
      (should (equal (codex-ide-config-effective-value 'fast session) "off")))))

(provide 'codex-ide-slash-command-tests)

;;; codex-ide-slash-command-tests.el ends here
