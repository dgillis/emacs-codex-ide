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

(declare-function codex-ide--update-header-line "codex-ide-header" (&optional session))
(declare-function codex-ide--available-model-names "codex-ide-protocol"
                  (&optional context session))
(declare-function codex-ide--reasoning-effort-options "codex-ide-protocol"
                  (&optional model session))

(defvar codex-ide-model)
(defvar codex-ide--model-list-interaction-cache)
(defvar codex-ide-fast)
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
     :applies-to-live-session t
     :protocol-key approvalPolicy)
    (sandbox-mode
     :label "sandbox mode"
     :prompt "Sandbox mode"
     :choices ("read-only" "workspace-write" "danger-full-access")
     :global-var codex-ide-sandbox-mode
     :applies-to-live-session t
     :protocol-key sandbox)
    (personality
     :label "personality"
     :prompt "Personality"
     :choices ("none" "friendly" "pragmatic")
     :global-var codex-ide-personality
     :applies-to-live-session t
     :protocol-key personality)
    (model
     :label "model"
     :prompt "Model"
     :choices-function codex-ide--available-model-names
     :allow-custom t
     :allow-empty t
     :nil-is-value t
     :custom-prompt "Custom model: "
     :global-var codex-ide-model
     :applies-to-live-session t
     :protocol-key model)
    (fast
     :label "fast"
     :prompt "Fast"
     :choices ("off" "on")
     :global-var codex-ide-fast
     :applies-to-live-session t
     :protocol-key serviceTier)
    (reasoning-effort
     :label "reasoning effort"
     :prompt "Reasoning effort"
     :choices-function codex-ide--reasoning-effort-options
     :fallback-choices ("none" "minimal" "low" "medium" "high" "xhigh")
     :global-var codex-ide-reasoning-effort
     :applies-to-live-session t
     :protocol-key effort))
  "Descriptor table for session-aware Codex IDE settings.")

(defvar codex-ide-config-history-limit 30
  "Maximum number of config snapshots retained in `codex-ide-config-history'.")

(defvar codex-ide-config-history nil
  "Recent config snapshots available for restore.
Each entry records the global and session-local state overwritten by one config
apply operation.")

(defvar codex-ide-config-presets
  '(("Max" . ( model "gpt-5.5"
               reasoning-effort "xhigh"
               fast "on"))
    ("Medium" . ( model "gpt-5.4"
                  reasoning-effort "medium"
                  fast "off"))
    ("Budget" . ( model "gpt-5.4-mini"
                  reasoning-effort "low"
                  fast "off"))
    ("Read-only" . ( approval-policy "on-request"
                     sandbox-mode "read-only"))
    ("Danger".  ( approval-policy "never"
                  sandbox-mode "danger-full-access")))
  "Named config presets.
Each entry is (NAME . PLIST), where PLIST maps config keys to values.  Omitted
keys are left unchanged when applying the preset.")

(defvar codex-ide-config--history-group nil
  "Pending grouped history entry for the active config menu interaction.")

(defun codex-ide-config--descriptor (key)
  "Return the descriptor plist for config KEY."
  (or (alist-get key codex-ide-config--descriptors)
      (error "Unknown Codex config key: %S" key)))

(defun codex-ide-config--keys ()
  "Return all known config keys in descriptor order."
  (mapcar #'car codex-ide-config--descriptors))

(defun codex-ide-config--plist-member-p (plist key)
  "Return non-nil when PLIST contains KEY."
  (let ((found nil))
    (while plist
      (when (eq (car plist) key)
        (setq found t
              plist nil))
      (when plist
        (setq plist (cddr plist))))
    found))

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

(defun codex-ide-config--fallback-choices (key)
  "Return fallback completion choices for config KEY, if any."
  (plist-get (codex-ide-config--descriptor key) :fallback-choices))

(defun codex-ide-config--allow-custom-p (key)
  "Return non-nil when config KEY supports arbitrary values."
  (plist-get (codex-ide-config--descriptor key) :allow-custom))

(defun codex-ide-config--allow-empty-p (key)
  "Return non-nil when config KEY supports clearing interactively."
  (plist-get (codex-ide-config--descriptor key) :allow-empty))

(defun codex-ide-config--custom-prompt (key)
  "Return the freeform input prompt for config KEY, if any."
  (plist-get (codex-ide-config--descriptor key) :custom-prompt))

(defun codex-ide-config--nil-is-value-p (key)
  "Return non-nil when nil is an explicit config value for KEY."
  (plist-get (codex-ide-config--descriptor key) :nil-is-value))

(defun codex-ide-config--global-var (key)
  "Return the default variable symbol for config KEY."
  (plist-get (codex-ide-config--descriptor key) :global-var))

(defun codex-ide-config-applies-to-live-session-p (key)
  "Return non-nil when config KEY affects future turns in a live session."
  (plist-get (codex-ide-config--descriptor key) :applies-to-live-session))

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

(defun codex-ide-config--global-values ()
  "Return a plist of current global config values."
  (let ((values nil))
    (dolist (key (codex-ide-config--keys))
      (setq values
            (plist-put values key
                       (symbol-value (codex-ide-config--global-var key)))))
    values))

(defun codex-ide-config--restore-global-values (values)
  "Restore global config VALUES."
  (dolist (key (codex-ide-config--keys))
    (set (codex-ide-config--global-var key)
         (plist-get values key))))

(defun codex-ide-config--affected-sessions (scope &optional session)
  "Return live sessions affected by config SCOPE and SESSION."
  (pcase scope
    ('this-session
     (when session
       (list session)))
    ('all-sessions
     (codex-ide-config--live-sessions))
    (_ nil)))

(defun codex-ide-config--snapshot-session (session)
  "Return restorable config state for SESSION."
  (list :session session
        :overrides (copy-sequence
                    (or (codex-ide-config--session-overrides session)
                        '()))))

(defun codex-ide-config--snapshot (scope &optional session)
  "Return restorable config state for SCOPE and SESSION."
  (list :scope scope
        :time (current-time)
        :restore-globals (memq scope '(all-sessions future-sessions))
        :globals (codex-ide-config--global-values)
        :sessions (mapcar #'codex-ide-config--snapshot-session
                          (codex-ide-config--affected-sessions scope session))))

(defun codex-ide-config--snapshot-state (snapshot)
  "Return the comparable restorable state in SNAPSHOT."
  (list :restore-globals (plist-get snapshot :restore-globals)
        :globals (plist-get snapshot :globals)
        :sessions (mapcar (lambda (entry)
                            (list :session (plist-get entry :session)
                                  :overrides (plist-get entry :overrides)))
                          (plist-get snapshot :sessions))))

(defun codex-ide-config--history-group-snapshot-scope (&optional session)
  "Return the broad snapshot scope for a grouped menu interaction."
  (if (or session (codex-ide-config--live-sessions))
      'all-sessions
    'future-sessions))

(defun codex-ide-config--history-group-snapshot (group)
  "Return a fresh snapshot for history GROUP."
  (codex-ide-config--snapshot
   (plist-get group :snapshot-scope)
   (plist-get group :session)))

(defun codex-ide-config--history-group-record-scope (group scope)
  "Return GROUP with config apply SCOPE recorded."
  (let ((scopes (plist-get group :scopes)))
    (if (and scope (not (memq scope scopes)))
        (plist-put group :scopes (append scopes (list scope)))
      group)))

(defun codex-ide-config--history-group-display-scope (group)
  "Return the display scope for grouped history GROUP."
  (let ((scopes (plist-get group :scopes)))
    (cond
     ((null scopes) 'mixed-scope)
     ((null (cdr scopes)) (car scopes))
     (t 'mixed-scope))))

(defun codex-ide-config--push-history (before after)
  "Record BEFORE and AFTER snapshots in `codex-ide-config-history'."
  (if codex-ide-config--history-group
      (let ((group (codex-ide-config--history-group-record-scope
                    codex-ide-config--history-group
                    (plist-get before :scope))))
        (setq codex-ide-config--history-group
              (plist-put group
                         :after
                         (codex-ide-config--history-group-snapshot group))))
    (codex-ide-config--push-history-entry
     (list :time (plist-get before :time)
           :scope (plist-get before :scope)
           :before before
           :after after))))

(defun codex-ide-config--push-history-entry (entry)
  "Push ENTRY onto `codex-ide-config-history' with limit trimming."
  (push entry codex-ide-config-history)
  (setq codex-ide-config-history
        (seq-take codex-ide-config-history codex-ide-config-history-limit)))

(defun codex-ide-config--merge-change-plists (base changes)
  "Merge CHANGES into BASE and return the resulting plist."
  (let ((merged (copy-sequence (or base '()))))
    (while changes
      (setq merged (plist-put merged (car changes) (cadr changes)))
      (setq changes (cddr changes)))
    merged))

(defun codex-ide-config-begin-history-group (&optional session interaction-id)
  "Start a grouped history entry for the current config interaction.
Return the group INTERACTION-ID, defaulting to the current time."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when codex-ide-config--history-group
    (codex-ide-config-commit-history-group))
  (let* ((interaction-id (or interaction-id (current-time)))
         (snapshot-scope
          (codex-ide-config--history-group-snapshot-scope session)))
    (setq codex-ide-config--history-group
          (list :interaction-id interaction-id
                :time interaction-id
                :origin 'menu-interaction
                :scopes nil
                :snapshot-scope snapshot-scope
                :session session
                :before (codex-ide-config--snapshot snapshot-scope session)
                :after nil))
    interaction-id))

(defun codex-ide-config-commit-history-group ()
  "Commit the pending grouped history entry, if any."
  (when codex-ide-config--history-group
    (let ((group codex-ide-config--history-group))
      (setq codex-ide-config--history-group nil)
      (let ((before (plist-get group :before))
            (after (or (plist-get group :after)
                       (plist-get group :before))))
        (when-let* ((changes
                     (codex-ide-config--history-menu-changed-values
                      before
                      after)))
          (codex-ide-config--push-history-entry
           (list :time (plist-get group :time)
                 :interaction-id (plist-get group :interaction-id)
                 :origin (plist-get group :origin)
                 :scope (codex-ide-config--history-group-display-scope group)
                 :scopes (plist-get group :scopes)
                 :before before
                 :after after
                 :changes changes)))))))

(defun codex-ide-config--snapshot-session-entry (snapshot session)
  "Return SESSION's entry in SNAPSHOT, if present."
  (seq-find (lambda (entry)
              (eq (plist-get entry :session) session))
            (plist-get snapshot :sessions)))

(defun codex-ide-config--snapshot-sessions (before after)
  "Return the session objects present in BEFORE or AFTER snapshots."
  (let ((sessions nil))
    (dolist (snapshot (list before after))
      (dolist (entry (plist-get snapshot :sessions))
        (let ((session (plist-get entry :session)))
          (cl-pushnew session sessions :test #'eq))))
    (nreverse sessions)))

(defun codex-ide-config--snapshot-session-override-state
    (snapshot session key)
  "Return SESSION's override state for KEY in SNAPSHOT.
The return value is (BOUND VALUE)."
  (let* ((entry (codex-ide-config--snapshot-session-entry snapshot session))
         (overrides (plist-get entry :overrides)))
    (list (codex-ide-config--plist-member-p overrides key)
          (plist-get overrides key))))

(defun codex-ide-config--snapshot-session-effective-value
    (snapshot session key)
  "Return SESSION's effective KEY value in SNAPSHOT."
  (let* ((state
          (codex-ide-config--snapshot-session-override-state
           snapshot session key))
         (bound (car state))
         (value (cadr state)))
    (if bound
        value
      (plist-get (plist-get snapshot :globals) key))))

(defun codex-ide-config--history-menu-changed-values (before after)
  "Return changed (KEY . VALUE) pairs between grouped menu snapshots."
  (let ((changes nil)
        (sessions (codex-ide-config--snapshot-sessions before after)))
    (dolist (key (codex-ide-config--keys))
      (let* ((before-globals (plist-get before :globals))
             (after-globals (plist-get after :globals))
             (global-changed
              (not (equal (plist-get before-globals key)
                          (plist-get after-globals key))))
             (changed-session nil)
             (session-changed
              (seq-some
               (lambda (session)
                 (let ((before-state
                        (codex-ide-config--snapshot-session-override-state
                         before session key))
                       (after-state
                        (codex-ide-config--snapshot-session-override-state
                         after session key)))
                   (when (or (not (eq (car before-state)
                                      (car after-state)))
                             (not (equal (cadr before-state)
                                         (cadr after-state))))
                     (setq changed-session session)
                     t)))
               sessions)))
        (when (or global-changed session-changed)
          (push (cons key
                      (if global-changed
                          (plist-get after-globals key)
                        (codex-ide-config--snapshot-session-effective-value
                         after changed-session key)))
                changes))))
    (nreverse changes)))

(defun codex-ide-config--history-values-for-scope (snapshot scope)
  "Return the config values in SNAPSHOT relevant to SCOPE."
  (pcase scope
    ('this-session
     (plist-get (car (plist-get snapshot :sessions)) :overrides))
    (_
     (plist-get snapshot :globals))))

(defun codex-ide-config--history-changed-values (entry)
  "Return a list of changed (KEY . VALUE) pairs for history ENTRY."
  (or (plist-get entry :changes)
      (if (or (eq (plist-get entry :origin) 'menu-interaction)
              (eq (plist-get entry :scope) 'menu-interaction))
          (codex-ide-config--history-menu-changed-values
           (plist-get entry :before)
           (plist-get entry :after))
        (let* ((scope (plist-get entry :scope))
               (before (codex-ide-config--history-values-for-scope
                        (plist-get entry :before) scope))
               (after (codex-ide-config--history-values-for-scope
                       (plist-get entry :after) scope))
               (changes nil))
          (dolist (key (codex-ide-config--keys))
            (let ((before-bound (codex-ide-config--plist-member-p before key))
                  (after-bound (codex-ide-config--plist-member-p after key))
                  (before-value (plist-get before key))
                  (after-value (plist-get after key)))
              (when (or (not (eq before-bound after-bound))
                        (not (equal before-value after-value)))
                (push (cons key after-value) changes))))
          (nreverse changes)))))

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

(defun codex-ide-config--default-value (key &optional session)
  "Return the default value for config KEY, using SESSION's buffer if available."
  (let ((variable (codex-ide-config--global-var key))
        (buffer (and session (codex-ide-session-buffer session))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (symbol-value variable))
      (symbol-value variable))))

(defun codex-ide-config-effective-value (key &optional session)
  "Return the effective value for config KEY in SESSION."
  (if (codex-ide-config--session-value-bound-p key session)
      (codex-ide-config-session-value key session)
    (codex-ide-config--default-value key session)))

(defun codex-ide-config-effective-reasoning-effort (&optional session)
  "Return SESSION's effective reasoning effort, defaulting to medium."
  (or (codex-ide-config-effective-value 'reasoning-effort session)
      "medium"))

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
    (let ((normalized nil))
      (while overrides
        (unless (eq (car overrides) key)
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
  (if (or value (codex-ide-config--nil-is-value-p key))
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
      (if (or value (codex-ide-config--nil-is-value-p key))
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
  (let* ((session (or session (codex-ide--get-default-session-for-current-buffer)))
         (before (codex-ide-config--snapshot scope session))
         (count
          (pcase scope
            ('this-session
             (codex-ide-config-apply-to-session key value session))
            ('all-sessions
             (codex-ide-config-apply-to-all-sessions key value))
            ('future-sessions
             (codex-ide-config-apply-future-sessions key value))
            (_
             (error "Unsupported Codex config scope: %S" scope)))))
    (codex-ide-config--push-history
     before
     (codex-ide-config--snapshot scope session))
    count))

(defun codex-ide-config-apply-values (values scope &optional session)
  "Apply config VALUES plist using SCOPE for SESSION.
Return the number of live sessions affected."
  (let* ((session (or session (codex-ide--get-default-session-for-current-buffer)))
         (before (codex-ide-config--snapshot scope session))
         (count 0))
    (dolist (key (codex-ide-config--keys))
      (when (codex-ide-config--plist-member-p values key)
        (setq count
              (max count
                   (pcase scope
                     ('this-session
                      (codex-ide-config-apply-to-session
                       key (plist-get values key) session))
                     ('all-sessions
                      (codex-ide-config-apply-to-all-sessions
                       key (plist-get values key)))
                     ('future-sessions
                      (codex-ide-config-apply-future-sessions
                       key (plist-get values key)))
                     (_
                      (error "Unsupported Codex config scope: %S" scope)))))))
    (codex-ide-config--push-history
     before
     (codex-ide-config--snapshot scope session))
    count))

(defun codex-ide-config--preset-values (preset)
  "Return the config values plist for PRESET."
  (let ((values (if (consp preset) (cdr preset) preset)))
    (unless (listp values)
      (error "Invalid Codex config preset: %S" preset))
    values))

(defun codex-ide-config-apply-preset (preset scope &optional session)
  "Apply config PRESET using SCOPE for SESSION."
  (codex-ide-config-apply-values
   (codex-ide-config--preset-values preset)
   scope
   session))

(defun codex-ide-config-format-preset (preset)
  "Return a compact display string for PRESET."
  (let* ((name (car preset))
         (values (codex-ide-config--preset-values preset))
         (summary
          (delq
           nil
           (mapcar
            (lambda (key)
              (when (codex-ide-config--plist-member-p values key)
                (format "%s=%s"
                        (codex-ide-config--label key)
                        (codex-ide-config-format-value
                         (plist-get values key)))))
            (codex-ide-config--keys)))))
    (if summary
        (format "%s  %s" name (mapconcat #'identity summary "; "))
      name)))

(defun codex-ide-config-read-preset ()
  "Read a config preset from the minibuffer."
  (unless codex-ide-config-presets
    (user-error "No Codex config presets configured"))
  (let* ((candidates
          (mapcar (lambda (preset)
                    (cons (codex-ide-config-format-preset preset)
                          preset))
                  codex-ide-config-presets))
         (completion-extra-properties
          codex-ide-config--completion-extra-properties))
    (cdr (assoc (completing-read "Apply config preset: "
                                 candidates nil t)
                candidates))))

(defun codex-ide-config--restore-snapshot (snapshot)
  "Restore config state from SNAPSHOT."
  (when (plist-get snapshot :restore-globals)
    (codex-ide-config--restore-global-values (plist-get snapshot :globals)))
  (dolist (entry (plist-get snapshot :sessions))
    (let ((session (plist-get entry :session)))
      (when (codex-ide-session-p session)
        (codex-ide-config--set-session-overrides
         session
         (copy-sequence (or (plist-get entry :overrides) '())))
        (dolist (key (codex-ide-config--keys))
          (codex-ide-config--refresh-session session key))))))

(defun codex-ide-config-restore-history-entry (entry)
  "Restore config state from history ENTRY."
  (interactive
   (list (codex-ide-config-read-history-entry)))
  (codex-ide-config--restore-snapshot (plist-get entry :before))
  (message "Restored Codex config snapshot: %s"
           (codex-ide-config-format-history-entry entry)))

(defun codex-ide-config-restore-last ()
  "Restore the most recent config snapshot and remove it from history."
  (interactive)
  (unless codex-ide-config-history
    (user-error "No Codex config history to restore"))
  (let ((entry (pop codex-ide-config-history)))
    (codex-ide-config--restore-snapshot (plist-get entry :before))
    (message "Restored previous Codex config: %s"
             (codex-ide-config-format-history-entry entry))))

(defun codex-ide-config-format-history-entry (entry)
  "Return a compact display string for history ENTRY."
  (let* ((time (plist-get entry :time))
         (scope (plist-get entry :scope))
         (changes (codex-ide-config--history-changed-values entry))
         (summary
          (if changes
              (mapconcat
               (lambda (change)
                 (format "%s=%s"
                         (codex-ide-config--label (car change))
                         (codex-ide-config-format-value (cdr change))))
               changes
               "; ")
            "(no changed values)")))
    (format "%s  %s  %s"
            (format-time-string "%Y-%m-%d %H:%M" time)
            (pcase scope
              ('this-session "this session")
              ('all-sessions "all live + future")
              ('future-sessions "future sessions")
              ('mixed-scope "mixed scope")
              ('menu-interaction "mixed scope")
              (_ (format "%S" scope)))
            summary)))

(defun codex-ide-config-read-history-entry ()
  "Read a config history entry from the minibuffer."
  (unless codex-ide-config-history
    (user-error "No Codex config history to restore"))
  (let* ((candidates
          (mapcar (lambda (entry)
                    (cons (codex-ide-config-format-history-entry entry)
                          entry))
                  codex-ide-config-history))
         (completion-extra-properties
          codex-ide-config--completion-extra-properties))
    (cdr (assoc (completing-read "Restore config snapshot: "
                                 candidates nil t)
                candidates))))

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

(defun codex-ide-config--resolve-input (key &optional context session)
  "Return interactive input metadata for config KEY.
CONTEXT is passed to dynamic choice functions together with SESSION."
  (if-let* ((choices (codex-ide-config--choices key)))
      (list :choices choices)
    (let* ((fn (codex-ide-config--choices-function key))
           (resolved (and fn (funcall fn context session))))
      (cond
       ((and (listp resolved) (keywordp (car resolved)))
        resolved)
       (resolved
        (list :choices resolved))
       (t
        (list :choices (codex-ide-config--fallback-choices key)))))))

(defun codex-ide-config-read-value (key &optional session context)
  "Read an interactive value for config KEY.
Descriptors may provide static choices, dynamic choices, custom input, and
explicit clearing semantics.  SESSION defaults to the session associated with
the current buffer.  CONTEXT supplies a value selected earlier in a combined
prompt, such as the model used to resolve reasoning effort choices."
  (let* ((session (or session (codex-ide--session-for-current-buffer)))
         (input (codex-ide-config--resolve-input key context session))
         (choices (plist-get input :choices))
         (allow-custom (codex-ide-config--allow-custom-p key))
         (allow-empty (codex-ide-config--allow-empty-p key))
         (configured-default
          (or (symbol-value (codex-ide-config--global-var key)) ""))
         (input-default-bound-p
          (codex-ide-config--plist-member-p input :default))
         (input-default (and input-default-bound-p
                             (plist-get input :default)))
         (prompt-default
          (if input-default-bound-p
              input-default
            (symbol-value (codex-ide-config--global-var key))))
         (prompt (codex-ide-config-format-value-prompt
                  key
                  (codex-ide-config--prompt key)
                  session
                  prompt-default))
         (completion-extra-properties
          codex-ide-config--completion-extra-properties))
    (cond
     (choices
      (let* ((collection (append (when allow-empty
                                   (list codex-ide-config--empty-choice))
                                 choices
                                 (when allow-custom
                                   (list codex-ide-config--other-choice))))
             (completion-default (and input-default
                                      (member input-default collection)
                                      input-default))
             (choice (completing-read
                      prompt collection nil t nil nil completion-default)))
        (cond
         ((equal choice codex-ide-config--empty-choice)
          "")
         ((equal choice codex-ide-config--other-choice)
          (read-string (or (codex-ide-config--custom-prompt key)
                           (format "%s: " (codex-ide-config--prompt key)))
                       configured-default))
         (t
          choice))))
     (allow-custom
      (read-string (or (codex-ide-config--custom-prompt key) prompt)
                   configured-default))
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

;;;###autoload
(defun codex-ide-set-model-and-reasoning-effort
    (&optional model reasoning-effort scope session)
  "Prompt for a Codex model, then prompt for reasoning effort, and apply both.
When MODEL is an empty string, clear the configured model.  SESSION defaults to
the session associated with the current buffer.  Interactively, prompt for the
  target scope after reading both values."
  (interactive)
  (let ((codex-ide--model-list-interaction-cache
         (make-hash-table :test #'equal)))
    (let* ((session (or session (codex-ide--session-for-current-buffer)))
           (model-choice
            (or model (codex-ide-config-read-value 'model session)))
           (model (and model-choice
                       (not (string-empty-p model-choice))
                       model-choice))
           (reasoning-effort
            (or reasoning-effort
                (codex-ide-config-read-value
                 'reasoning-effort session model-choice)))
           (scope (or scope (codex-ide-config-read-scope session)))
           (count (codex-ide-config-apply-values
                   (list 'model model
                         'reasoning-effort reasoning-effort)
                   scope
                   session)))
      (message "%s"
               (codex-ide-config-format-apply-values-message
                (list 'model model
                      'reasoning-effort reasoning-effort)
                scope
                count)))))

(defun codex-ide-config-format-apply-values-message (values scope count)
  "Return a user-facing message for config VALUES applied with SCOPE to COUNT.
VALUES is a plist keyed by Codex config keys."
  (let* ((changes
          (delq
           nil
           (mapcar
            (lambda (key)
              (when (codex-ide-config--plist-member-p values key)
                (let ((value (plist-get values key)))
                  (format "%s %s"
                          (codex-ide-config--label key)
                          (if value
                              (format "set to %s"
                                      (codex-ide-config-format-value value))
                            "cleared")))))
            (codex-ide-config--keys))))
         (scope-text
          (pcase scope
            ('this-session "this session")
            ('all-sessions
             (format "%d live session%s and future sessions"
                     count
                     (if (= count 1) "" "s")))
            ('future-sessions "future sessions")
            (_ "the selected scope"))))
    (format "Codex %s for %s."
            (mapconcat #'identity changes "; ")
            scope-text)))

(defun codex-ide-config-format-apply-message (key value scope count)
  "Return a user-facing message for KEY VALUE applied with SCOPE to COUNT sessions."
  (let* ((label (capitalize (codex-ide-config--label key)))
         (verb (if value "set to" "cleared for"))
         (live-session-note
          (unless (codex-ide-config-applies-to-live-session-p key)
            (pcase scope
              ('this-session
               " This will take effect when the live session is restarted or resumed.")
              ('all-sessions
               (when (> count 0)
                 " Changes for live sessions will take effect when each session is restarted or resumed.")))))
         (scope-text
          (pcase scope
            ('this-session "this session")
            ('all-sessions
             (format "%d live session%s and future sessions"
                     count
                     (if (= count 1) "" "s")))
            ('future-sessions "future sessions")
            (_ "the selected scope"))))
    (concat
     (format "Codex %s %s %s."
             label
             verb
             (if value
                 (format "%s for %s"
                         (codex-ide-config-format-value value)
                         scope-text)
               scope-text))
     live-session-note)))

(defun codex-ide-config-format-value-prompt
    (key base-prompt &optional session default)
  "Return BASE-PROMPT annotated with effective and default values for KEY.
When SESSION is nil, annotate the prompt with only DEFAULT.  DEFAULT falls
back to KEY's configured global value when omitted."
  (let ((default (or default
                     (symbol-value (codex-ide-config--global-var key)))))
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
