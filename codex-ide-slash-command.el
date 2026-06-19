;;; codex-ide-slash-command.el --- Slash command helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns the prompt slash-command registry, completion, and
;; dispatch helpers.  It intentionally does not own transcript mutation or
;; session mode setup; callers pass prompt/session state in at the boundary.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'codex-ide-config)
(require 'codex-ide-core)

(autoload 'codex-ide-session-buffer-list "codex-ide-session-buffer-list" nil t)
(autoload 'codex-ide-session-diff-open "codex-ide-diff-view" nil t)
(autoload 'codex-ide-loop-jump-or-create "codex-ide-loop" nil t)
(autoload 'codex-ide-status "codex-ide-status-mode" nil t)
(autoload 'codex-ide-submit "codex-ide-transcript" nil t)

(defgroup codex-ide-slash-command nil
  "Slash command support for Codex IDE prompts."
  :group 'codex-ide)

(defvar codex-ide-slash-command--suppress-completion-submit nil
  "Non-nil while slash command completion should not submit on exit.")

(defconst codex-ide-slash-command--loop-entry
  '("loop" codex-ide-loop-jump-or-create
    "Jump to or create this session's loop buffer.")
  "Default slash command entry for Codex loop buffers.")

(defcustom codex-ide-slash-commands
  `(("buffers" codex-ide-session-buffer-list "List live Codex session buffers.")
    ("diff" codex-ide-session-diff-open "Open the session diff view.")
    ("fast" codex-ide-slash-command-toggle-fast
     "Toggle fast mode for this session.")
    ,codex-ide-slash-command--loop-entry
    ("model" codex-ide-slash-command-set-model
     "Set the model and reasoning effort for this session.")
    ("reasoning" codex-ide-slash-command-set-reasoning-effort
     "Set the reasoning effort for this session.")
    ("sessions" codex-ide-status "Open the Codex session status buffer."))
  "Slash command registry.

Each entry is a list of the form (NAME COMMAND DESCRIPTION), where NAME is the
slash command name without its leading slash, COMMAND is an interactive command
symbol, and DESCRIPTION is shown in completion annotations."
  :type '(repeat (list (string :tag "Slash command name")
                       (function :tag "Interactive command")
                       (string :tag "Description")))
  :group 'codex-ide-slash-command)

(defun codex-ide-slash-command--ensure-core-loop-entry ()
  "Ensure `/loop' is present after reloading older core command defaults."
  (when (and (not (assoc "loop" codex-ide-slash-commands))
             (assoc "buffers" codex-ide-slash-commands)
             (assoc "diff" codex-ide-slash-commands)
             (assoc "sessions" codex-ide-slash-commands))
    (setq codex-ide-slash-commands
          (append codex-ide-slash-commands
                  (list codex-ide-slash-command--loop-entry)))))

(codex-ide-slash-command--ensure-core-loop-entry)

(defun codex-ide-slash-command--current-session ()
  "Return the current Codex session for a slash command."
  (or (and (boundp 'codex-ide--session) codex-ide--session)
      (codex-ide--get-default-session-for-current-buffer)
      (user-error "No Codex session available")))

(defun codex-ide-slash-command--apply-session-config (key value &optional session)
  "Apply config KEY VALUE to SESSION and report the change."
  (let ((session (or session (codex-ide-slash-command--current-session))))
    (codex-ide-config-apply key value 'this-session session)
    (message "%s"
             (codex-ide-config-format-apply-message
              key value 'this-session 1))))

;;;###autoload
(defun codex-ide-slash-command-set-model (&optional model reasoning-effort)
  "Set the Codex model and reasoning effort for the current session."
  (interactive)
  (let ((session (codex-ide-slash-command--current-session)))
    (codex-ide-set-model-and-reasoning-effort
     model
     reasoning-effort
     'this-session
     session)))

;;;###autoload
(defun codex-ide-slash-command-set-reasoning-effort (&optional value)
  "Set the reasoning effort for the current session."
  (interactive)
  (let* ((session (codex-ide-slash-command--current-session))
         (value (or value
                    (codex-ide-config-read-value 'reasoning-effort session))))
    (codex-ide-slash-command--apply-session-config
     'reasoning-effort value session)))

;;;###autoload
(defun codex-ide-slash-command-toggle-fast ()
  "Toggle fast mode for the current session."
  (interactive)
  (let* ((session (codex-ide-slash-command--current-session))
         (value (if (equal (codex-ide-config-effective-value 'fast session)
                           "on")
                    "off"
                  "on")))
    (codex-ide-slash-command--apply-session-config 'fast value session)))

(defun codex-ide-slash-command--entry-name (entry)
  "Return slash command ENTRY's name."
  (nth 0 entry))

(defun codex-ide-slash-command--entry-command (entry)
  "Return slash command ENTRY's command symbol."
  (nth 1 entry))

(defun codex-ide-slash-command--entry-description (entry)
  "Return slash command ENTRY's description."
  (nth 2 entry))

(defun codex-ide-slash-command-names ()
  "Return registered slash command names."
  (mapcar #'codex-ide-slash-command--entry-name codex-ide-slash-commands))

(defun codex-ide-slash-command-lookup (name)
  "Return the slash command entry for NAME, or nil."
  (cl-find name codex-ide-slash-commands
           :key #'codex-ide-slash-command--entry-name
           :test #'string=))

(defun codex-ide-slash-command--parse (prompt)
  "Parse PROMPT as a slash command.
Return nil when PROMPT does not begin with a slash after trimming outer
whitespace.  Otherwise return a plist with :name, :display, and :extra."
  (let ((text (string-trim prompt)))
    (when (string-prefix-p "/" text)
      (if (string-match "\\`/\\([^[:space:]]*\\)\\([[:space:]\n].*\\)?\\'" text)
          (let ((name (match-string 1 text))
                (extra (string-trim (or (match-string 2 text) ""))))
            (list :name name
                  :display (concat "/" name)
                  :extra extra))
        (list :name ""
              :display "/"
              :extra "")))))

(defun codex-ide-slash-command-prompt-p (prompt)
  "Return non-nil when PROMPT begins with a slash command marker."
  (and (codex-ide-slash-command--parse prompt) t))

(defun codex-ide-slash-command-exact-p (prompt)
  "Return non-nil when PROMPT is exactly a registered slash command."
  (when-let* ((parsed (codex-ide-slash-command--parse prompt)))
    (let ((name (plist-get parsed :name))
          (extra (plist-get parsed :extra)))
      (and (not (string-empty-p name))
           (string-empty-p extra)
           (codex-ide-slash-command-lookup name)))))

(defun codex-ide-slash-command-resolve-prompt (prompt)
  "Return the command entry for slash command PROMPT.
Return nil when PROMPT is not a slash command.  Signal a user error for
unrecognized slash commands or unsupported arguments."
  (when-let* ((parsed (codex-ide-slash-command--parse prompt)))
    (let* ((name (plist-get parsed :name))
           (display (plist-get parsed :display))
           (extra (plist-get parsed :extra))
           (entry (and (not (string-empty-p name))
                       (codex-ide-slash-command-lookup name))))
      (unless entry
        (user-error "Unrecognized command '%s'" display))
      (unless (string-empty-p extra)
        (user-error "Slash command '%s' does not accept arguments" display))
      entry)))

(defun codex-ide-slash-command-entry-command (entry)
  "Return slash command ENTRY's interactive command symbol."
  (codex-ide-slash-command--entry-command entry))

(defun codex-ide-slash-command-execute-entry (entry)
  "Execute slash command ENTRY interactively."
  (let* ((name (codex-ide-slash-command--entry-name entry))
         (command (codex-ide-slash-command--entry-command entry)))
    (unless (commandp command)
      (user-error "Slash command '/%s' is not currently available" name))
    (call-interactively command)))

(defun codex-ide-slash-command-dispatch-prompt (prompt)
  "Dispatch PROMPT as a slash command when applicable.
Return non-nil when PROMPT was a slash command.  Signal a user error for
unrecognized slash commands or unsupported arguments."
  (when-let* ((entry (codex-ide-slash-command-resolve-prompt prompt)))
    (codex-ide-slash-command-execute-entry entry)
    t))

(defun codex-ide-slash-command--input-end-position (session)
  "Return SESSION's active prompt input end position, or nil."
  (when-let* ((marker (and session
                           (codex-ide--session-metadata-get
                            session
                            :input-end-marker)))
              (buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer))
      (marker-position marker))))

(defun codex-ide-slash-command--input-start-position (session)
  "Return SESSION's active prompt input start position, or nil."
  (when-let* ((marker (and session
                           (codex-ide-session-input-start-marker session)))
              (buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (markerp marker)
               (eq (marker-buffer marker) buffer))
      (marker-position marker))))

(defun codex-ide-slash-command--current-input (session)
  "Return SESSION's active prompt input text, or nil."
  (when-let* ((start (codex-ide-slash-command--input-start-position session))
              (end (codex-ide-slash-command--input-end-position session))
              (buffer (and session (codex-ide-session-buffer session))))
    (when (<= start end)
      (with-current-buffer buffer
        (buffer-substring-no-properties start end)))))

(defun codex-ide-slash-command--submit-if-exact (session)
  "Submit SESSION when its active prompt is exactly a slash command."
  (let ((buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (codex-ide-slash-command-exact-p
                (or (codex-ide-slash-command--current-input session) "")))
      (with-current-buffer buffer
        (codex-ide-submit)))))

(defun codex-ide-slash-command--complete-at-point ()
  "Run slash command completion without using CAPF exit submission."
  (let ((codex-ide-slash-command--suppress-completion-submit t))
    (completion-at-point)))

;;;###autoload
(defun codex-ide-slash-command-complete-or-submit ()
  "Complete the active slash command or submit it when complete."
  (interactive)
  (let ((session (codex-ide-slash-command--current-session)))
    (if (codex-ide-slash-command-exact-p
         (or (codex-ide-slash-command--current-input session) ""))
        (codex-ide-submit)
      (if (codex-ide-slash-command-completion-at-point session)
          (progn
            (codex-ide-slash-command--complete-at-point)
            (run-at-time
             0 nil
             (lambda (target-session target-buffer)
               (when (buffer-live-p target-buffer)
                 (with-current-buffer target-buffer
                   (codex-ide-slash-command--submit-if-exact target-session))))
             session
             (current-buffer)))
        (codex-ide-submit)))))

(defun codex-ide-slash-command--completion-submit-command-p ()
  "Return non-nil when the current completion exit should submit."
  (memq this-command
        '(codex-ide-slash-command-complete-or-submit
          minibuffer-choose-completion
          corfu-insert)))

(defun codex-ide-slash-command--completion-exit (session _string status)
  "Maybe submit SESSION after slash command completion exits with STATUS."
  (when (and (not codex-ide-slash-command--suppress-completion-submit)
             (codex-ide-slash-command--completion-submit-command-p)
             (memq status '(exact finished sole)))
    (let ((buffer (and session (codex-ide-session-buffer session))))
      (when (buffer-live-p buffer)
        (run-at-time
         0 nil
         (lambda (target-session target-buffer)
           (when (buffer-live-p target-buffer)
             (with-current-buffer target-buffer
               (codex-ide-slash-command--submit-if-exact target-session))))
         session
         buffer)))))

(defun codex-ide-slash-command--completion-bounds (session)
  "Return completion bounds for SESSION's active slash command at point."
  (when-let* ((input-start (codex-ide-slash-command--input-start-position
                            session)))
    (let ((pos (point)))
      (when (and (<= input-start pos)
                 (< input-start (point-max))
                 (eq (char-after input-start) ?/))
        (let ((name-start (1+ input-start)))
          (when (and (>= pos name-start)
                     (save-excursion
                       (goto-char name-start)
                       (not (re-search-forward "[[:space:]\n]" pos t))))
            (cons name-start pos)))))))

(defun codex-ide-slash-command-completion-at-point (&optional session)
  "Return slash command completion data for SESSION at point."
  (setq session (or session
                    (and (boundp 'codex-ide--session) codex-ide--session)))
  (when-let* ((bounds (codex-ide-slash-command--completion-bounds session)))
    (let ((table (completion-table-dynamic
                  (lambda (_)
                    (codex-ide-slash-command-names)))))
      (list (car bounds)
            (cdr bounds)
            table
            :exclusive 'no
            :annotation-function
            (lambda (name)
              (when-let* ((entry (codex-ide-slash-command-lookup name))
                          (description
                           (codex-ide-slash-command--entry-description entry)))
                (concat "  " description)))
            :exit-function
            (lambda (string status)
              (codex-ide-slash-command--completion-exit
               session
               string
               status))))))

(provide 'codex-ide-slash-command)

;;; codex-ide-slash-command.el ends here
