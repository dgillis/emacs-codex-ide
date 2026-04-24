;;; codex-ide-config.el --- Session-aware config helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; Shared configuration semantics for Codex IDE.  This module owns descriptor
;; metadata, scope selection, session-local overrides, and effective config
;; lookup for settings that may differ between live sessions and future
;; sessions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-core)

(declare-function codex-ide--update-header-line "codex-ide-transcript" (&optional session))
(declare-function codex-ide--available-model-names "codex-ide-protocol" ())

(defvar codex-ide-model)
(defvar codex-ide-reasoning-effort)
(defvar codex-ide-approval-policy)
(defvar codex-ide-sandbox-mode)
(defvar codex-ide-personality)

(defconst codex-ide-config--other-choice "Other..."
  "Sentinel completion choice used to request freeform input.")

(defconst codex-ide-config--empty-choice "<empty>"
  "Sentinel completion choice used to clear an optional config value.")

(defconst codex-ide-config--completion-extra-properties
  '(:display-sort-function identity
    :cycle-sort-function identity)
  "Completion metadata used to preserve descriptor order in config prompts.")

(defconst codex-ide-config--scope-choices-in-session
  '(("This session" . this-session)
    ("All live sessions and future sessions" . all-sessions)))

(defconst codex-ide-config--scope-choices-outside-session
  '(("All live sessions and future sessions" . all-sessions)
    ("Future sessions only" . future-sessions)))

(defconst codex-ide-config--descriptors
  '((approval-policy
     :label "approval policy"
     :prompt "Approval policy"
     :choices ("untrusted" "on-failure" "on-request" "never")
     :global-var codex-ide-approval-policy
     :protocol-key approvalPolicy)
    (sandbox-mode
     :label "sandbox mode"
     :prompt "Sandbox mode"
     :choices ("read-only" "workspace-write" "danger-full-access")
     :global-var codex-ide-sandbox-mode
     :protocol-key sandbox)
    (personality
     :label "personality"
     :prompt "Personality"
     :choices ("none" "friendly" "pragmatic")
     :global-var codex-ide-personality
     :protocol-key personality)
    (model
     :label "model"
     :prompt "Model"
     :choices-function codex-ide--available-model-names
     :allow-custom t
     :allow-empty t
     :custom-prompt "Custom model: "
     :global-var codex-ide-model
     :protocol-key model)
    (reasoning-effort
     :label "reasoning effort"
     :prompt "Reasoning effort"
     :choices ("none" "minimal" "low" "medium" "high" "xhigh")
     :allow-empty t
     :global-var codex-ide-reasoning-effort
     :protocol-key effort))
  "Descriptor table for session-aware Codex IDE settings.")

(defun codex-ide-config--descriptor (key)
  "Return the descriptor plist for config KEY."
  (or (alist-get key codex-ide-config--descriptors)
      (error "Unknown Codex config key: %S" key)))

(defun codex-ide-config--label (key)
  "Return the human-readable label for config KEY."
  (plist-get (codex-ide-config--descriptor key) :label))

(defun codex-ide-config--prompt (key)
  "Return the interactive prompt text for config KEY."
  (plist-get (codex-ide-config--descriptor key) :prompt))

(defun codex-ide-config--choices (key)
  "Return completion choices for config KEY, if any."
  (plist-get (codex-ide-config--descriptor key) :choices))

(defun codex-ide-config--choices-function (key)
  "Return the dynamic choices function for config KEY, if any."
  (plist-get (codex-ide-config--descriptor key) :choices-function))

(defun codex-ide-config--allow-custom-p (key)
  "Return non-nil when config KEY supports arbitrary values."
  (plist-get (codex-ide-config--descriptor key) :allow-custom))

(defun codex-ide-config--allow-empty-p (key)
  "Return non-nil when config KEY supports clearing interactively."
  (plist-get (codex-ide-config--descriptor key) :allow-empty))

(defun codex-ide-config--custom-prompt (key)
  "Return the freeform input prompt for config KEY, if any."
  (plist-get (codex-ide-config--descriptor key) :custom-prompt))

(defun codex-ide-config--global-var (key)
  "Return the default variable symbol for config KEY."
  (plist-get (codex-ide-config--descriptor key) :global-var))

(defun codex-ide-config--protocol-key (key)
  "Return the protocol payload symbol for config KEY."
  (plist-get (codex-ide-config--descriptor key) :protocol-key))

(defun codex-ide-config-format-value (value)
  "Return a readable display string for config VALUE."
  (if (stringp value)
      value
    (format "%s" value)))

(defun codex-ide-config--live-sessions ()
  "Return all currently live non-query Codex sessions."
  (codex-ide--cleanup-dead-sessions)
  (seq-remove
   #'codex-ide--query-only-session-p
   (seq-filter #'codex-ide--live-session-p codex-ide--sessions)))

(defun codex-ide-config--session-overrides (&optional session)
  "Return session-local config overrides for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (and session
       (codex-ide--session-metadata-get session :config-overrides)))

(defun codex-ide-config--set-session-overrides (session overrides)
  "Store OVERRIDES as SESSION's config override plist."
  (codex-ide--session-metadata-put session :config-overrides overrides))

(defun codex-ide-config-session-value (key &optional session)
  "Return the session-local override for config KEY in SESSION."
  (plist-get (codex-ide-config--session-overrides session) key))

(defun codex-ide-config--session-value-bound-p (key &optional session)
  "Return non-nil when SESSION has an override for config KEY."
  (let ((overrides (codex-ide-config--session-overrides session))
        (found nil))
    (while overrides
      (when (eq (car overrides) key)
        (setq found t
              overrides nil))
      (when overrides
        (setq overrides (cddr overrides))))
    found))

(defun codex-ide-config-effective-value (key &optional session)
  "Return the effective value for config KEY in SESSION."
  (if (codex-ide-config--session-value-bound-p key session)
      (codex-ide-config-session-value key session)
    (symbol-value (codex-ide-config--global-var key))))

(defun codex-ide-config-set-session-value (key value &optional session)
  "Set SESSION's override for config KEY to VALUE."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((overrides (copy-sequence (or (codex-ide-config--session-overrides session)
                                      '()))))
    (setq overrides (plist-put overrides key value))
    (codex-ide-config--set-session-overrides session overrides))
  value)

(defun codex-ide-config-clear-session-value (key &optional session)
  "Clear SESSION's override for config KEY."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((overrides (copy-sequence (or (codex-ide-config--session-overrides session)
                                      '()))))
    (setq overrides (plist-put overrides key nil))
    (let ((normalized nil))
      (while overrides
        (when (cadr overrides)
          (setq normalized
                (plist-put normalized (car overrides) (cadr overrides))))
        (setq overrides (cddr overrides)))
      (codex-ide-config--set-session-overrides session normalized)))
  nil)

(defun codex-ide-config--refresh-session (session key)
  "Refresh UI after config KEY changed in SESSION."
  (when (buffer-live-p (codex-ide-session-buffer session))
    (codex-ide--update-header-line session))
  (codex-ide--run-session-event
   'config-changed
   session
   :key key
   :value (codex-ide-config-effective-value key session)))

(defun codex-ide-config-apply-to-session (key value &optional session)
  "Apply config KEY VALUE to SESSION only."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (if value
      (codex-ide-config-set-session-value key value session)
    (codex-ide-config-clear-session-value key session))
  (codex-ide-config--refresh-session session key)
  1)

(defun codex-ide-config-apply-to-all-sessions (key value)
  "Apply config KEY VALUE to all live sessions and future sessions."
  (set (codex-ide-config--global-var key) value)
  (let ((count 0))
    (dolist (session (codex-ide-config--live-sessions))
      (setq count (1+ count))
      (if value
          (codex-ide-config-set-session-value key value session)
        (codex-ide-config-clear-session-value key session))
      (codex-ide-config--refresh-session session key))
    count))

(defun codex-ide-config-apply-future-sessions (key value)
  "Apply config KEY VALUE only to future sessions."
  (set (codex-ide-config--global-var key) value)
  0)

(defun codex-ide-config-apply (key value scope &optional session)
  "Apply config KEY VALUE using SCOPE for SESSION."
  (pcase scope
    ('this-session
     (codex-ide-config-apply-to-session key value session))
    ('all-sessions
     (codex-ide-config-apply-to-all-sessions key value))
    ('future-sessions
     (codex-ide-config-apply-future-sessions key value))
    (_
     (error "Unsupported Codex config scope: %S" scope))))

(defun codex-ide-config-read-scope (&optional session)
  "Read the target scope for a session-aware config change.
When SESSION is nil and there are no live sessions, return `future-sessions'
without prompting."
  (let* ((session (or session (codex-ide--session-for-current-buffer)))
         (live-sessions (codex-ide-config--live-sessions))
         (completion-extra-properties
          codex-ide-config--completion-extra-properties))
    (cond
     (session
      (cdr
       (assoc
        (completing-read "Apply to: "
                         codex-ide-config--scope-choices-in-session
                         nil t nil nil
                         (caar codex-ide-config--scope-choices-in-session))
        codex-ide-config--scope-choices-in-session)))
     ((null live-sessions)
      'future-sessions)
     (t
      (cdr
       (assoc
       (completing-read "Apply to: "
                        codex-ide-config--scope-choices-outside-session
                        nil t nil nil
                        (caar codex-ide-config--scope-choices-outside-session))
        codex-ide-config--scope-choices-outside-session))))))

(defun codex-ide-config--resolve-choices (key)
  "Return available completion choices for config KEY."
  (or (codex-ide-config--choices key)
      (when-let ((fn (codex-ide-config--choices-function key)))
        (funcall fn))))

(defun codex-ide-config-read-value (key &optional session)
  "Read an interactive value for config KEY.
Descriptors may provide static choices, dynamic choices, custom input, and
explicit clearing semantics.  SESSION defaults to the session associated with
the current buffer."
  (let* ((session (or session (codex-ide--session-for-current-buffer)))
         (choices (codex-ide-config--resolve-choices key))
         (allow-custom (codex-ide-config--allow-custom-p key))
         (allow-empty (codex-ide-config--allow-empty-p key))
         (default (or (symbol-value (codex-ide-config--global-var key))
                      ""))
         (prompt (codex-ide-config-format-value-prompt
                  key
                  (codex-ide-config--prompt key)
                  session))
         (completion-extra-properties
          codex-ide-config--completion-extra-properties))
    (cond
     (choices
      (let* ((collection (append (when allow-empty
                                   (list codex-ide-config--empty-choice))
                                 choices
                                 (when allow-custom
                                   (list codex-ide-config--other-choice))))
             (choice (completing-read prompt collection nil t)))
        (cond
         ((equal choice codex-ide-config--empty-choice)
          "")
         ((equal choice codex-ide-config--other-choice)
          (read-string (or (codex-ide-config--custom-prompt key)
                           (format "%s: " (codex-ide-config--prompt key)))
                       default))
         (t
          choice))))
     (allow-custom
      (read-string (or (codex-ide-config--custom-prompt key) prompt)
                   default))
     (t
      (error "Config %S does not define interactive input semantics" key)))))

(defun codex-ide-config-apply-interactively (key value &optional session)
  "Apply session-aware config KEY VALUE with an interactive scope prompt.
SESSION defaults to the session associated with the current buffer."
  (let* ((session (or session (codex-ide--session-for-current-buffer)))
         (scope (codex-ide-config-read-scope session))
         (count (codex-ide-config-apply key value scope session)))
    (message "%s"
             (codex-ide-config-format-apply-message key value scope count))))

(defun codex-ide-config-format-apply-message (key value scope count)
  "Return a user-facing message for KEY VALUE applied with SCOPE to COUNT sessions."
  (let* ((label (capitalize (codex-ide-config--label key)))
         (verb (if value "set to" "cleared for"))
         (scope-text
          (pcase scope
            ('this-session "this session")
            ('all-sessions
             (format "%d live session%s and future sessions"
                     count
                     (if (= count 1) "" "s")))
            ('future-sessions "future sessions")
            (_ "the selected scope"))))
    (format "Codex %s %s %s"
            label
            verb
            (if value
                (format "%s for %s"
                        (codex-ide-config-format-value value)
                        scope-text)
              scope-text))))

(defun codex-ide-config-format-value-prompt (key base-prompt &optional session)
  "Return BASE-PROMPT annotated with effective and default values for KEY.
When SESSION is nil, annotate the prompt with only the default value."
  (let ((default (symbol-value (codex-ide-config--global-var key))))
    (if session
        (format "%s (effective = %s, default = %s): "
                base-prompt
                (codex-ide-config-format-value
                 (codex-ide-config-effective-value key session))
                (codex-ide-config-format-value default))
      (format "%s (default = %s): "
              base-prompt
              (codex-ide-config-format-value default)))))

(provide 'codex-ide-config)

;;; codex-ide-config.el ends here
