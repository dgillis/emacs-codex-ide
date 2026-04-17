;;; codex-ide-renderer.el --- Transcript rendering for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; Buffer rendering helpers for codex-ide transcript and status UI.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'json)
(require 'subr-x)
(require 'codex-ide-core)

(declare-function codex-ide--extract-error-text "codex-ide" (&rest values))
(declare-function codex-ide--classify-session-error "codex-ide" (&rest values))
(declare-function codex-ide--ensure-server-model-name "codex-ide" (&optional session))
(declare-function codex-ide--format-session-error-summary "codex-ide" (classification &optional prefix))
(declare-function codex-ide--server-model-name "codex-ide" (&optional session))
(declare-function codex-ide--sanitize-ansi-text "codex-ide" (text))
(declare-function codex-ide--session-for-current-project "codex-ide" ())
(declare-function codex-ide--strip-emacs-context-prefix "codex-ide" (text))
(declare-function codex-ide--sync-prompt-minor-mode "codex-ide" (&optional session))
(declare-function codex-ide--thread-read--item-kind "codex-ide" (item))
(declare-function codex-ide--thread-read--message-text "codex-ide" (item))
(declare-function codex-ide--thread-read-turns "codex-ide" (thread-read))
(declare-function codex-ide-log-message "codex-ide" (session format-string &rest args))

(defvar codex-ide-log-max-lines)
(defvar codex-ide-reasoning-effort)
(defvar codex-ide-resume-summary-turn-limit)
;; Cached to avoid repeated feature loading during streamed rendering.
(defvar codex-ide--markdown-display-mode-function-cache 'unset)

(defcustom codex-ide-render-markdown-during-streaming t
  "Whether to apply incremental markdown rendering while text streams.

When non-nil, codex-ide renders completed safe spans of assistant text as they
arrive.  It does not repeatedly re-render the full assistant message; unfinished
lines, pipe tables, and open fenced code blocks are left for a later streaming
pass or the final completion render."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-markdown-render-max-chars 30000
  "Maximum markdown span size to render with rich markdown.

When a single render span is larger than this many characters, codex-ide leaves
that span as plain text.  Set this to nil for no size limit, or 0 to disable
rich markdown rendering."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum characters"))
  :group 'codex-ide)

(defcustom codex-ide-command-output-fold-on-start nil
  "When non-nil, command output blocks start folded while output streams.

Streaming output is still collected and the output header line count updates.
Press RET on the output header to expand or fold the block."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-command-output-max-rendered-lines 10
  "Maximum command output lines to insert into the transcript buffer.

The full output still contributes to the output line count and item completion
bookkeeping, but only this many lines are inserted into the Emacs buffer.  Set
this to nil to render all command output lines.  When output exceeds this
limit, the transcript shows the most recent lines."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum lines"))
  :group 'codex-ide)

(defcustom codex-ide-command-output-max-rendered-chars 60000
  "Maximum command output characters to insert into the transcript buffer.

This is a second guard for extremely long individual lines.  Set this to nil to
render all command output characters, subject to
`codex-ide-command-output-max-rendered-lines'.  When output exceeds this limit,
the transcript shows the most recent characters."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum characters"))
  :group 'codex-ide)

(defface codex-ide-user-prompt-face
  '((((class color) (background light))
     :background "#f4f1e8")
    (((class color) (background dark))
     :background "#2d2a24"))
  "Face used to distinguish submitted and active user prompts."
  :group 'codex-ide)

(defface codex-ide-output-separator-face
  '((((class color) (background light))
     :foreground "#c7c1b4")
    (((class color) (background dark))
     :foreground "#5a554b"))
  "Face used for transcript separator rules."
  :group 'codex-ide)

(defface codex-ide-item-summary-face
  '((t :inherit font-lock-function-name-face))
  "Face used for item summary lines."
  :group 'codex-ide)

(defface codex-ide-item-detail-face
  '((t :inherit shadow))
  "Face used for item detail lines."
  :group 'codex-ide)

(defface codex-ide-command-output-face
  '((((class color) (background light))
     :inherit fixed-pitch
     :background "#ece8dd"
     :extend t)
    (((class color) (background dark))
     :inherit fixed-pitch
     :background "#1f2324"
     :extend t)
    (t
     :inherit fixed-pitch
     :extend t))
  "Face used for command output blocks."
  :group 'codex-ide)

(defface codex-ide-approval-header-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for inline approval request headers."
  :group 'codex-ide)

(defface codex-ide-approval-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for inline approval field labels."
  :group 'codex-ide)

(defface codex-ide-file-diff-header-face
  '((t :inherit font-lock-keyword-face))
  "Face used for file-change diff headers."
  :group 'codex-ide)

(defface codex-ide-file-diff-hunk-face
  '((t :inherit diff-hunk-header))
  "Face used for file-change diff hunk lines."
  :group 'codex-ide)

(defface codex-ide-file-diff-added-face
  '((t :inherit diff-added))
  "Face used for added lines in file-change diffs."
  :group 'codex-ide)

(defface codex-ide-file-diff-removed-face
  '((t :inherit diff-removed))
  "Face used for removed lines in file-change diffs."
  :group 'codex-ide)

(defface codex-ide-file-diff-context-face
  '((t :inherit fixed-pitch))
  "Face used for context lines in file-change diffs."
  :group 'codex-ide)

(defface codex-ide-header-line-face
  '((t :inherit (header-line font-lock-comment-face) :weight light :height 0.9))
  "Face used for the Codex session header line."
  :group 'codex-ide)

(defface codex-ide-status-running-face
  '((t :inherit mode-line-emphasis :weight bold))
  "Face used for a running Codex session in the mode line."
  :group 'codex-ide)

(defface codex-ide-status-idle-face
  '((t :inherit success :weight semibold))
  "Face used for an idle Codex session in the mode line."
  :group 'codex-ide)

(defface codex-ide-status-busy-face
  '((t :inherit warning :weight bold))
  "Face used for transitional Codex session states in the mode line."
  :group 'codex-ide)

(defface codex-ide-status-error-face
  '((t :inherit error :weight bold))
  "Face used for failed or disconnected Codex session states in the mode line."
  :group 'codex-ide)

(defconst codex-ide-log-marker-property 'codex-ide-log-marker
  "Text property storing the log marker for transcript text.")

(defconst codex-ide-agent-item-type-property 'codex-ide-agent-item-type
  "Text property storing the originating agent item type for transcript text.")

(defconst codex-ide-command-output-overlay-property
  'codex-ide-command-output-overlay
  "Text property storing a command output block overlay.")

(defvar codex-ide-command-output-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-ide-toggle-command-output-at-point)
    map)
  "Keymap active on command output headers and expanded output.")

(defvar codex-ide--current-transcript-log-marker nil
  "Marker for the log line associated with the transcript text being inserted.")

(defvar codex-ide--current-agent-item-type nil
  "Item type associated with the agent transcript text being inserted.")

(defun codex-ide--status-label (status)
  "Return a display label for STATUS."
  (pcase (and (stringp status) (downcase status))
    ("running" "Running")
    ("idle" "Idle")
    ("starting" "Starting")
    ("approval" "Approval")
    ("interrupting" "Interrupting")
    ("submitted" "Submitted")
    ("disconnected" "Disconnected")
    ((pred stringp) (capitalize status))
    (_ "Disconnected")))

(defun codex-ide--status-face (status)
  "Return the face to use for STATUS."
  (let ((status (and (stringp status) (downcase status))))
    (cond
     ((equal status "idle") 'codex-ide-status-idle-face)
     ((member status '("running" "submitted")) 'codex-ide-status-running-face)
     ((member status '("starting" "interrupting" "approval")) 'codex-ide-status-busy-face)
     ((or (member status '("failed" "error" "disconnected" "finished" "killed"))
          (and status
               (string-match-p (rx (or "exit" "exited" "abnormally")) status)))
      'codex-ide-status-error-face)
     (t 'codex-ide-status-busy-face))))

(defun codex-ide--mode-line-status (&optional session)
  "Return the current modeline status segment for SESSION."
  (setq session (or session (and (boundp 'codex-ide--session) codex-ide--session)))
  (when (codex-ide-session-p session)
    (let* ((status (or (codex-ide-session-status session) "disconnected"))
           (label (codex-ide--status-label status))
           (face (codex-ide--status-face status)))
      (concat
       " "
       (propertize "Codex" 'face 'mode-line-emphasis)
       ":"
       (propertize label 'face face)
       " "))))

(defun codex-ide--update-mode-line (&optional session)
  "Refresh the mode line indicator for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((buffer (and session (codex-ide-session-buffer session))))
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defun codex-ide--make-region-writable (start end)
  "Make the region from START to END writable."
  (when (< start end)
    (remove-text-properties start end
                            '(read-only t
                              rear-nonsticky (read-only)
                              front-sticky (read-only)))))

(defun codex-ide--current-agent-text-properties ()
  "Return text properties for agent-originated transcript text."
  (append
   (when (markerp codex-ide--current-transcript-log-marker)
     (list codex-ide-log-marker-property codex-ide--current-transcript-log-marker))
   (when (stringp codex-ide--current-agent-item-type)
     (list codex-ide-agent-item-type-property codex-ide--current-agent-item-type))))

(defun codex-ide--freeze-region (start end)
  "Make the region from START to END read-only."
  (when (< start end)
    (remove-text-properties start end
                            '(read-only nil
                              rear-nonsticky nil
                              front-sticky nil))
    (add-text-properties start end '(read-only t
                                     rear-nonsticky (read-only)
                                     front-sticky (read-only)))))

(defun codex-ide--append-to-buffer (buffer text &optional face properties)
  "Append TEXT to BUFFER as read-only transcript text.
When FACE is non-nil, apply it to the inserted text.
When PROPERTIES is non-nil, it should be a property list applied to the
inserted text."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (codex-ide--without-undo-recording
        (let ((inhibit-read-only t)
              (moving (= (point) (point-max)))
              start)
          (goto-char (point-max))
          (setq start (point))
          (insert text)
          (when (or face properties)
            (add-text-properties
             start
             (point)
             (append (when face (list 'face face))
                     properties)))
          (codex-ide--freeze-region start (point))
          (when moving
            (goto-char (point-max))))))))

(defun codex-ide--append-agent-text (buffer text &optional face properties)
  "Append agent-originated TEXT to BUFFER with FACE and PROPERTIES."
  (codex-ide--append-to-buffer
   buffer
   text
   face
   (append properties (codex-ide--current-agent-text-properties))))

(defun codex-ide--ensure-output-spacing (buffer)
  "Ensure BUFFER is ready for a new rendered output block."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (codex-ide--without-undo-recording
        (let ((inhibit-read-only t)
              start)
          (goto-char (point-max))
          (setq start (point))
          (cond
           ((= (point) (point-min)))
           ((and (eq (char-before (point)) ?\n)
                 (save-excursion
                   (forward-char -1)
                   (or (bobp)
                       (eq (char-before (point)) ?\n)))))
           ((eq (char-before (point)) ?\n)
            (insert "\n"))
           (t
            (insert "\n\n")))
          (codex-ide--freeze-region start (point)))))))

(defun codex-ide--output-separator-string ()
  "Return the separator rule used between transcript sections."
  (concat (make-string 72 ?-) "\n"))

(defun codex-ide--append-output-separator (buffer)
  "Append a transcript separator rule to BUFFER."
  (codex-ide--append-agent-text
   buffer
   (codex-ide--output-separator-string)
   'codex-ide-output-separator-face))

(defun codex-ide--restored-transcript-separator-string ()
  "Return the separator shown between restored history and the live prompt."
  (let* ((label "[End of restored session]")
         (width (length (string-trim-right (codex-ide--output-separator-string))))
         (padding (max 0 (- width (length label))))
         (left (/ padding 2))
         (right (- padding left)))
    (format "\n%s%s%s\n\n"
            (make-string left ?-)
            label
            (make-string right ?-))))

(defun codex-ide--append-restored-transcript-separator (buffer)
  "Append the restored-history boundary separator to BUFFER."
  (codex-ide--append-agent-text
   buffer
   (codex-ide--restored-transcript-separator-string)
   'codex-ide-output-separator-face))

(defun codex-ide--insert-pending-output-indicator (session &optional text)
  "Insert a temporary pending-output indicator for SESSION."
  (let ((buffer (codex-ide-session-buffer session))
        (indicator-text (or text "Working...\n")))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
          (let ((inhibit-read-only t)
                (moving (= (point) (point-max)))
                inserted-text
                start
                marker)
            (goto-char (point-max))
            (setq inserted-text
                  (concat (if (or (= (point) (point-min)) (bolp))
                              ""
                            "\n")
                          indicator-text))
            (setq start (point))
            (insert (propertize inserted-text 'face 'shadow))
            (setq marker (copy-marker start))
            (codex-ide--freeze-region start (point))
            (codex-ide--session-metadata-put
             session
             :pending-output-indicator-marker
             marker)
            (codex-ide--session-metadata-put
             session
             :pending-output-indicator-text
             inserted-text)
            (when moving
              (goto-char (point-max)))))))))

(defun codex-ide--clear-pending-output-indicator (session)
  "Remove SESSION's pending-output indicator, if it is still present."
  (when-let ((marker (codex-ide--session-metadata-get
                      session
                      :pending-output-indicator-marker)))
    (let ((buffer (marker-buffer marker))
          (indicator-text
           (or (codex-ide--session-metadata-get
                session
                :pending-output-indicator-text)
               "Working...\n")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (codex-ide--without-undo-recording
            (let ((inhibit-read-only t)
                  (moving (= (point) (point-max)))
                  (start (marker-position marker)))
              (when (and start
                         (<= start (point-max)))
                (save-excursion
                  (goto-char start)
                  (when (looking-at (regexp-quote indicator-text))
                    (delete-region start (match-end 0)))))
              (when moving
                (goto-char (point-max)))))))
      (set-marker marker nil))
    (codex-ide--session-metadata-put
     session
     :pending-output-indicator-marker
     nil)
    (codex-ide--session-metadata-put
     session
     :pending-output-indicator-text
     nil)))

(defun codex-ide--replace-pending-output-indicator (session text)
  "Replace SESSION's temporary pending-output indicator with TEXT."
  (codex-ide--clear-pending-output-indicator session)
  (codex-ide--insert-pending-output-indicator session text))

(defun codex-ide--delete-input-overlay (session)
  "Delete the active input overlay for SESSION, if any."
  (when-let ((overlay (codex-ide-session-input-overlay session)))
    (delete-overlay overlay)
    (setf (codex-ide-session-input-overlay session) nil)))

(defun codex-ide--style-user-prompt-region (start end)
  "Apply prompt styling to the user prompt region from START to END."
  (when (< start end)
    (add-text-properties start end '(face codex-ide-user-prompt-face))))

(defun codex-ide--format-compact-number (value)
  "Format numeric VALUE in a compact human-readable form."
  (cond
   ((not (numberp value)) "?")
   ((>= value 1000000)
    (format "%.1fM" (/ value 1000000.0)))
   ((>= value 1000)
    (format "%.1fk" (/ value 1000.0)))
   (t
    (number-to-string value))))

(defun codex-ide--format-token-usage-summary (token-usage)
  "Return a compact header summary for TOKEN-USAGE."
  (when-let* ((total (alist-get 'total token-usage))
              (window (alist-get 'modelContextWindow token-usage))
              (used (alist-get 'totalTokens total)))
    (let* ((remaining (max 0 (- window used)))
           (remaining-percent
            (if (> window 0)
                (/ (* 100.0 remaining) window)
              0.0))
           (last (or (alist-get 'last token-usage) total))
           (last-input (alist-get 'inputTokens last))
           (last-cached (alist-get 'cachedInputTokens last))
           (last-output (alist-get 'outputTokens last))
           (last-reasoning (alist-get 'reasoningOutputTokens last)))
      (string-join
       (delq nil
             (list
              (format "ctx: %s/%s"
                      (codex-ide--format-compact-number used)
                      (codex-ide--format-compact-number window))
              (format "left: %s (%.0f%%%%)"
                      (codex-ide--format-compact-number remaining)
                      remaining-percent)
              (when (numberp last-input)
                (format "last in:%s" (codex-ide--format-compact-number last-input)))
              (when (numberp last-cached)
                (format "cache:%s" (codex-ide--format-compact-number last-cached)))
              (when (numberp last-output)
                (format "out:%s" (codex-ide--format-compact-number last-output)))
              (when (numberp last-reasoning)
                (format "reason:%s" (codex-ide--format-compact-number last-reasoning)))))
       "  "))))

(defun codex-ide--format-reasoning-effort-summary (session)
  "Return a compact header summary for SESSION's reasoning effort."
  (when-let ((effort (or (codex-ide--session-metadata-get session :reasoning-effort)
                         codex-ide-reasoning-effort)))
    (format "effort:%s" effort)))

(defun codex-ide--format-model-summary (&optional session)
  "Return a compact header summary for SESSION's model."
  (let ((model (and session
                    (codex-ide--server-model-name session))))
    (unless model
      (codex-ide--ensure-server-model-name session))
    (when model
      (format "model:%s" model))))

(defun codex-ide--format-rate-limit-summary (rate-limits)
  "Return a compact header summary for RATE-LIMITS."
  (when-let* ((primary (alist-get 'primary rate-limits))
              (used-percent (alist-get 'usedPercent primary)))
    (format "quota: %s%%%% used%s"
            used-percent
            (if-let ((plan-type (alist-get 'planType rate-limits)))
                (format " (%s)" plan-type)
              ""))))

(defun codex-ide--update-header-line (&optional session)
  "Refresh the header line for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let ((buffer (codex-ide-session-buffer session)))
    (with-current-buffer buffer
      (let* ((context (with-current-buffer buffer
                        (codex-ide--get-active-buffer-context)))
             (focus (if context
                        (format "%s:%s"
                                (alist-get 'display-file context)
                                (alist-get 'line context))
                      "none"))
             (token-summary
              (codex-ide--format-token-usage-summary
               (codex-ide--session-metadata-get session :token-usage)))
             (rate-limit-summary
              (codex-ide--format-rate-limit-summary
               (codex-ide--session-metadata-get session :rate-limits)))
             (model-summary
              (codex-ide--format-model-summary session))
             (effort-summary
              (codex-ide--format-reasoning-effort-summary session)))
        (setq header-line-format
              (propertize
               (string-join
                (delq nil
                      (list
                       (format "focus: %s" focus)
                       model-summary
                       effort-summary
                       token-summary
                       rate-limit-summary))
                "  ")
               'face 'codex-ide-header-line-face)))
      (codex-ide--update-mode-line session))))

(defun codex-ide--parse-file-link-target (target)
  "Parse markdown file TARGET into (PATH LINE COLUMN), or nil."
  (let ((normalized
         (replace-regexp-in-string "\\\\/" "/" target t t)))
    (cond
     ((string-match "\\`\\(/[^#\n]+\\)#L\\([0-9]+\\)\\(?:C\\([0-9]+\\)\\)?\\'" normalized)
      (list (match-string 1 normalized)
            (string-to-number (match-string 2 normalized))
            (when-let ((column (match-string 3 normalized)))
              (string-to-number column))))
     ((string-match "\\`\\(/[^:\n]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'" normalized)
      (list (match-string 1 normalized)
            (string-to-number (match-string 2 normalized))
            (when-let ((column (match-string 3 normalized)))
              (string-to-number column))))
     ((string-prefix-p "/" normalized)
      (list normalized nil nil))
     (t nil))))

(defun codex-ide--open-file-link (_button)
  "Open the file link described by text properties at point."
  (interactive)
  (let ((path (get-text-property (point) 'codex-ide-path))
        (line (get-text-property (point) 'codex-ide-line))
        (column (get-text-property (point) 'codex-ide-column)))
    (unless (and path (file-exists-p path))
      (user-error "File does not exist: %s" (or path "unknown")))
    (find-file path)
    (goto-char (point-min))
    (when line
      (forward-line (1- line)))
    (when column
      (move-to-column (max 0 (1- column))))))

(defun codex-ide--clear-markdown-properties (start end)
  "Clear Codex markdown rendering properties between START and END."
  (let ((end-marker (copy-marker end t)))
    ;; Only remove properties from regions previously marked as markdown.
    ;; Clearing `face' across the whole agent-message span can wipe faces from
    ;; later non-markdown transcript entries like "* Ran ..." summaries.
    (save-excursion
      (goto-char start)
      (while (< (point) (marker-position end-marker))
        (let* ((pos (point))
               (next (min
                      (or (next-single-property-change
                           pos 'codex-ide-markdown nil (marker-position end-marker))
                          (marker-position end-marker))
                      (or (next-single-property-change
                           pos 'codex-ide-markdown-code-fontified nil
                           (marker-position end-marker))
                          (marker-position end-marker)))))
          (cond
           ((and (get-text-property pos 'codex-ide-markdown)
                 (get-text-property pos 'codex-ide-markdown-table-original))
            (let ((original (get-text-property
                             pos
                             'codex-ide-markdown-table-original)))
              (delete-region pos next)
              (goto-char pos)
              (insert original)))
           ((and (get-text-property pos 'codex-ide-markdown)
                 (get-text-property pos 'codex-ide-markdown-code-fontified))
            (remove-text-properties
             pos next
             '(mouse-face nil
               help-echo nil
               keymap nil
               category nil
               button nil
               action nil
               follow-link nil
               display nil
               codex-ide-path nil
               codex-ide-line nil
               codex-ide-column nil
               codex-ide-table-link nil
               codex-ide-markdown-table-original nil
               codex-ide-markdown-code-content nil
               codex-ide-markdown nil))
            (goto-char next))
           ((get-text-property pos 'codex-ide-markdown)
            (remove-text-properties
             pos next
             '(font-lock-face nil
               face nil
               mouse-face nil
               help-echo nil
               keymap nil
               category nil
               button nil
               action nil
               follow-link nil
               display nil
               codex-ide-path nil
               codex-ide-line nil
               codex-ide-column nil
               codex-ide-table-link nil
               codex-ide-markdown-table-original nil
               codex-ide-markdown-code-content nil
               codex-ide-markdown-code-fontified nil
               codex-ide-markdown nil))
            (goto-char next))
           (t
            (goto-char next))))))
    (set-marker end-marker nil)))

(defun codex-ide--normalize-markdown-link-label (label)
  "Return LABEL with markdown code delimiters stripped when present."
  (save-match-data
    (if (string-match "\\``\\([^`\n]+\\)`\\'" label)
        (match-string 1 label)
      label)))

(defun codex-ide--markdown-language-mode-candidates (language)
  "Return Emacs major mode functions for fenced code block LANGUAGE."
  (let* ((lang (downcase (string-trim (or language ""))))
         (modes
          (alist-get
           lang
           '(("bash" . (sh-mode))
             ("c" . (c-mode))
             ("c++" . (c++-mode))
             ("cpp" . (c++-mode))
             ("elisp" . (emacs-lisp-mode))
             ("emacs-lisp" . (emacs-lisp-mode))
             ("go" . (go-mode))
             ("java" . (java-mode))
             ("javascript" . (js-mode))
             ("js" . (js-mode))
             ("json" . (json-mode js-json-mode js-mode))
             ("python" . (python-mode))
             ("py" . (python-mode))
             ("ruby" . (ruby-mode))
             ("rust" . (rust-mode))
             ("shell" . (sh-mode))
             ("sh" . (sh-mode))
             ("typescript" . (typescript-mode js-mode))
             ("ts" . (typescript-mode js-mode))
             ("tsx" . (typescript-mode js-mode))
             ("yaml" . (yaml-mode conf-mode))
             ("yml" . (yaml-mode conf-mode)))
           nil nil #'string=)))
    (cl-remove-duplicates
     (cl-remove-if-not
      #'fboundp
      (append modes
              (unless (string-empty-p lang)
                (list (intern-soft (format "%s-mode" lang))))))
     :test #'eq)))

(defun codex-ide--markdown-language-mode (language)
  "Return an Emacs major mode function for fenced code block LANGUAGE."
  (car (codex-ide--markdown-language-mode-candidates language)))

(defvar codex-ide--font-lock-spec-cache (make-hash-table :test 'eq)
  "Cache of font-lock setup captured from major modes.")

(defconst codex-ide--cached-font-lock-variables
  '(font-lock-defaults
    font-lock-keywords
    font-lock-keywords-only
    font-lock-syntax-table
    font-lock-syntactic-face-function
    font-lock-syntactic-keywords
    font-lock-fontify-region-function
    font-lock-unfontify-region-function
    font-lock-extend-region-functions
    font-lock-extra-managed-props
    font-lock-multiline
    syntax-propertize-function)
  "Buffer-local variables copied from language modes for transcript fontification.")

(defun codex-ide--font-lock-spec-for-mode (mode)
  "Return cached font-lock setup for MODE.
The mode is invoked only when populating the cache.  Later callers reuse the
captured syntax table and font-lock variables without running the full major
mode again."
  (or (gethash mode codex-ide--font-lock-spec-cache)
      (let ((spec
             (with-temp-buffer
               (delay-mode-hooks
                 (funcall mode))
               (list
                :syntax-table (copy-syntax-table (syntax-table))
                :variables
                (mapcar (lambda (variable)
                          (list variable
                                (local-variable-p variable)
                                (when (boundp variable)
                                  (symbol-value variable))))
                        codex-ide--cached-font-lock-variables)))))
        (puthash mode spec codex-ide--font-lock-spec-cache)
        spec)))

(defun codex-ide--apply-font-lock-spec (spec)
  "Apply cached font-lock SPEC to the current buffer."
  (set-syntax-table (copy-syntax-table (plist-get spec :syntax-table)))
  (dolist (entry (plist-get spec :variables))
    (let ((variable (nth 0 entry))
          (localp (nth 1 entry))
          (value (nth 2 entry)))
      (when localp
        (set (make-local-variable variable) (copy-tree value))))))

(defun codex-ide--copy-code-font-lock-properties (source-buffer start end)
  "Copy font-lock properties from current buffer to SOURCE-BUFFER START END."
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let* ((next (next-property-change pos (current-buffer) (point-max)))
             (face (get-text-property pos 'face))
             (font-lock-face (get-text-property pos 'font-lock-face))
             (props (append
                     (when face (list 'face face))
                     (when font-lock-face
                       (list 'font-lock-face font-lock-face))))
             (target-start (+ start (1- pos)))
             (target-end (min end (+ start (1- next)))))
        (when props
          (with-current-buffer source-buffer
            (add-face-text-property
             target-start
             target-end
             (or face font-lock-face)
             'append)))
        (setq pos next)))))

(defun codex-ide--fontify-code-block-with-mode (source-buffer start end code language mode)
  "Apply MODE fontification for CODE into SOURCE-BUFFER between START and END."
  (or
   (condition-case nil
       (let ((spec (codex-ide--font-lock-spec-for-mode mode)))
         (with-temp-buffer
           (insert code)
           (codex-ide--apply-font-lock-spec spec)
           (font-lock-mode 1)
           (font-lock-ensure (point-min) (point-max))
           (codex-ide--copy-code-font-lock-properties
            source-buffer start end))
         t)
     (error nil))
   (condition-case nil
       (with-temp-buffer
         (insert code)
         (let ((buffer-file-name
                (format "codex-ide-snippet.%s"
                        (if (string-empty-p (string-trim (or language "")))
                            "txt"
                          (downcase (string-trim language))))))
           (delay-mode-hooks
             (funcall mode)))
         (font-lock-mode 1)
         (font-lock-ensure (point-min) (point-max))
         (codex-ide--copy-code-font-lock-properties
          source-buffer start end)
         t)
     (error nil))))

(defun codex-ide--fontify-code-block-region (start end language)
  "Apply syntax highlighting to region START END using LANGUAGE."
  (let ((source-buffer (current-buffer))
        (code (buffer-substring-no-properties start end)))
    (cl-some
     (lambda (mode)
       (codex-ide--fontify-code-block-with-mode
        source-buffer
        start
        end
        code
        language
        mode))
     (codex-ide--markdown-language-mode-candidates language))))

(defun codex-ide--render-fenced-code-blocks (start end)
  "Render fenced code blocks between START and END."
  (goto-char start)
  (while (re-search-forward "^[ \t]*```\\([^`\n]*\\)[ \t]*$" end t)
    (let* ((fence-start (match-beginning 0))
           (language (string-trim (or (match-string-no-properties 1) "")))
           (code-start (min (1+ (match-end 0)) end)))
      (when (and (< code-start end)
                 (re-search-forward "^[ \t]*```[ \t]*$" end t))
        (let* ((closing-start (match-beginning 0))
               (closing-end (min (if (eq (char-after (match-end 0)) ?\n)
                                     (1+ (match-end 0))
                                   (match-end 0))
                                 end)))
          (add-text-properties
           fence-start code-start
           '(display ""
             codex-ide-markdown t))
          (add-text-properties
           code-start closing-start
           '(codex-ide-markdown t
             codex-ide-markdown-code-content t))
          (add-face-text-property code-start closing-start 'fixed-pitch 'append)
          (when (and (< code-start closing-start)
                     (not (get-text-property
                           code-start
                           'codex-ide-markdown-code-fontified)))
            (codex-ide--fontify-code-block-region code-start closing-start language)
            (add-text-properties
             code-start closing-start
             '(codex-ide-markdown-code-fontified t)))
          (add-text-properties
           closing-start closing-end
           '(display ""
             codex-ide-markdown t))
          (goto-char closing-end))))))

(defun codex-ide--markdown-table-row-line-p (line)
  "Return non-nil when LINE looks like a markdown pipe table row."
  (string-match-p "\\`[ \t]*|.*|[ \t]*\\'" line))

(defun codex-ide--markdown-table-separator-line-p (line)
  "Return non-nil when LINE looks like a markdown table separator row."
  (string-match-p
   "\\`[ \t]*|[ \t]*:?-+:?[ \t]*\\(?:|[ \t]*:?-+:?[ \t]*\\)+|[ \t]*\\'"
   line))

(defun codex-ide--markdown-table-parse-row (line)
  "Split markdown pipe table LINE into trimmed cell strings."
  (let ((trimmed (string-trim line)))
    (mapcar #'string-trim
            (split-string
             (string-remove-prefix "|"
                                   (string-remove-suffix "|" trimmed))
             "|"))))

(defun codex-ide--markdown-line-region-end (&optional limit)
  "Return the current line end position, including a trailing newline when present.
When LIMIT is non-nil, do not move beyond it."
  (let* ((line-end (line-end-position))
         (newline-end (if (< line-end (point-max))
                          (1+ line-end)
                        line-end)))
    (min (or limit newline-end) newline-end)))

(defun codex-ide--markdown-table-column-alignments (separator)
  "Return column alignments parsed from markdown table SEPARATOR."
  (mapcar
   (lambda (cell)
     (let ((trimmed (string-trim cell)))
       (cond
        ((and (string-prefix-p ":" trimmed)
              (string-suffix-p ":" trimmed))
         'center)
        ((string-suffix-p ":" trimmed)
         'right)
        (t 'left))))
   (codex-ide--markdown-table-parse-row separator)))

(defconst codex-ide--markdown-table-inline-pattern
  (concat
   "\\(\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))\\)"
   "\\|`\\([^`\n]+\\)`"
   "\\|\\(\\*\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\*\\)"
   "\\|\\(__\\([^_\n ]\\(?:[^\n]*?[^_\n ]\\)?\\)__\\)"
   "\\|\\(\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\)"
   "\\|\\(_\\([^_\n ]\\(?:[^_\n]*[^_\n ]\\)?\\)_\\)")
  "Pattern for inline markdown supported inside rendered tables.")

(defun codex-ide--markdown-inline-word-char-p (char)
  "Return non-nil when CHAR is a word-like markdown delimiter neighbor."
  (and char
       (string-match-p "[[:alnum:]_]" (char-to-string char))))

(defun codex-ide--markdown-inline-underscore-boundary-p (text start end)
  "Return non-nil when underscores at START and END in TEXT are markdown delimiters."
  (and (not (codex-ide--markdown-inline-word-char-p
             (and (> start 0) (aref text (1- start)))))
       (not (codex-ide--markdown-inline-word-char-p
             (and (< end (length text)) (aref text end))))))

(defun codex-ide--markdown-table-render-cell (cell)
  "Return CELL rendered as visible table text."
  (let ((pos 0)
        (parts nil))
    (while (string-match codex-ide--markdown-table-inline-pattern cell pos)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        (when (> match-start pos)
          (push (substring cell pos match-start) parts))
        (cond
         ((match-beginning 2)
          (let* ((label (codex-ide--normalize-markdown-link-label
                         (match-string 2 cell)))
                 (target (match-string 3 cell))
                 (parsed (codex-ide--parse-file-link-target target)))
            (push
             (if parsed
                 (propertize
                  label
                  'face 'link
                  'mouse-face 'highlight
                  'help-echo target
                  'codex-ide-table-link t
                  'codex-ide-path (nth 0 parsed)
                  'codex-ide-line (nth 1 parsed)
                  'codex-ide-column (nth 2 parsed))
               (propertize
                label
                'face 'link
                'mouse-face 'highlight
                'help-echo target))
             parts)))
         ((match-beginning 4)
          (push (propertize (match-string 4 cell)
                            'face 'font-lock-keyword-face)
                parts))
         ((match-beginning 6)
          (push (propertize (match-string 6 cell) 'face 'bold) parts))
         ((match-beginning 8)
          (push
           (if (codex-ide--markdown-inline-underscore-boundary-p
                cell match-start match-end)
               (propertize (match-string 8 cell) 'face 'bold)
             (match-string 0 cell))
           parts))
         ((match-beginning 10)
          (push (propertize (match-string 10 cell) 'face 'italic) parts))
         ((match-beginning 12)
          (push
           (if (codex-ide--markdown-inline-underscore-boundary-p
                cell match-start match-end)
               (propertize (match-string 12 cell) 'face 'italic)
             (match-string 0 cell))
           parts)))
        (setq pos match-end)))
    (when (< pos (length cell))
      (push (substring cell pos) parts))
    (apply #'concat (nreverse parts))))

(defun codex-ide--markdown-region-unrendered-p (start end)
  "Return non-nil when START to END has no markdown-rendered text."
  (not (text-property-not-all start end 'codex-ide-markdown nil)))

(defun codex-ide--markdown-emphasis-delimiters-unrendered-p
    (span-start content-start content-end span-end)
  "Return non-nil when emphasis delimiters have not already been rendered."
  (and (codex-ide--markdown-region-unrendered-p span-start content-start)
       (codex-ide--markdown-region-unrendered-p content-end span-end)))

(defun codex-ide--markdown-emphasis-underscore-boundary-p (start end)
  "Return non-nil when underscores from START to END are markdown delimiters."
  (and (not (codex-ide--markdown-inline-word-char-p
             (char-before start)))
       (not (codex-ide--markdown-inline-word-char-p
             (char-after end)))))

(defun codex-ide--render-markdown-emphasis (start end pattern face &optional underscore)
  "Render markdown emphasis matching PATTERN with FACE between START and END.
PATTERN must capture the full marked span in group 2 and content in group 3.
When UNDERSCORE is non-nil, reject intraword underscore delimiters."
  (goto-char start)
  (while (re-search-forward pattern end t)
    (let ((span-start (match-beginning 2))
          (span-end (match-end 2))
          (content-start (match-beginning 3))
          (content-end (match-end 3)))
      (when (and (codex-ide--markdown-emphasis-delimiters-unrendered-p
                  span-start content-start content-end span-end)
                 (or (not underscore)
                     (codex-ide--markdown-emphasis-underscore-boundary-p
                      span-start span-end)))
        (let ((content-length (- content-end content-start)))
          (add-face-text-property content-start content-end face 'append)
          (delete-region content-end span-end)
          (delete-region span-start content-start)
          (goto-char (+ span-start content-length)))))))

(defun codex-ide--markdown-table-pad-cell (cell width alignment)
  "Return CELL padded to WIDTH using ALIGNMENT."
  (let* ((cell-width (string-width cell))
         (padding (max 0 (- width cell-width))))
    (pcase alignment
      ('right
       (concat (make-string padding ?\s) cell))
      ('center
       (let* ((left (/ padding 2))
              (right (- padding left)))
         (concat (make-string left ?\s)
                 cell
                 (make-string right ?\s))))
      (_
       (concat cell (make-string padding ?\s))))))

(defun codex-ide--markdown-table-format-row (cells widths alignments)
  "Return a propertized table row from CELLS using WIDTHS and ALIGNMENTS."
  (concat
   "| "
   (mapconcat
    (lambda (triple)
      (pcase-let ((`(,cell ,width ,alignment) triple))
        (codex-ide--markdown-table-pad-cell cell width alignment)))
    (cl-mapcar #'list cells widths alignments)
    " | ")
   " |\n"))

(defun codex-ide--markdown-table-separator-string (widths alignments)
  "Return a separator line for WIDTHS and ALIGNMENTS."
  (concat
   "|"
   (mapconcat
    (lambda (pair)
      (pcase-let ((`(,width ,alignment) pair))
        (let* ((visible-width (max 3 (+ width 2)))
               (inner-width (max 1 (- visible-width 2)))
               (dashes (make-string inner-width ?-)))
          (pcase alignment
            ('center (format ":%s:" dashes))
            ('right (format "-%s:" dashes))
            (_ (make-string visible-width ?-))))))
    (cl-mapcar #'list widths alignments)
    "|")
   "|\n"))

(defun codex-ide--markdown-table-leading-indentation (line)
  "Return indentation before the opening table pipe in LINE."
  (if (string-match "\\`\\([ \t]*\\)|" line)
      (match-string 1 line)
    ""))

(defun codex-ide--markdown-prefix-lines (text prefix)
  "Return TEXT with PREFIX added to each non-empty line."
  (if (string-empty-p prefix)
      text
    (mapconcat
     (lambda (line)
       (if (string-empty-p line)
           line
         (concat prefix line)))
     (split-string text "\n")
     "\n")))

(defun codex-ide--markdown-table-display-string (lines)
  "Return a rendered display string for markdown table LINES, or nil."
  (when (>= (length lines) 2)
    (let* ((header (car lines))
           (separator (cadr lines))
           (body (cddr lines)))
      (when (and (codex-ide--markdown-table-row-line-p header)
                 (codex-ide--markdown-table-separator-line-p separator))
        (let* ((indent (codex-ide--markdown-table-leading-indentation header))
               (alignments (codex-ide--markdown-table-column-alignments separator))
               (raw-rows (mapcar #'codex-ide--markdown-table-parse-row
                                 (cons header
                                       (seq-filter #'codex-ide--markdown-table-row-line-p
                                                   body))))
               (column-count (apply #'max (mapcar #'length raw-rows)))
               (normalized-alignments
                (append alignments
                        (make-list (max 0 (- column-count (length alignments)))
                                   'left)))
               (rendered-rows
                (mapcar
                 (lambda (row)
                   (append
                    (mapcar #'codex-ide--markdown-table-render-cell row)
                    (make-list (max 0 (- column-count (length row))) "")))
                 raw-rows))
               (widths
                (cl-loop for column from 0 below column-count
                         collect (apply #'max 1
                                        (mapcar (lambda (row)
                                                  (string-width (nth column row)))
                                                rendered-rows))))
               (table-text
                (concat
                 (codex-ide--markdown-table-format-row
                  (car rendered-rows)
                  widths
                  normalized-alignments)
                 (codex-ide--markdown-table-separator-string
                  widths
                  normalized-alignments)
                 (mapconcat
                  (lambda (row)
                    (codex-ide--markdown-table-format-row
                     row
                     widths
                     normalized-alignments))
                  (cdr rendered-rows)
                  "")))
               (table-text (codex-ide--markdown-prefix-lines table-text indent)))
          (add-face-text-property
           0 (length table-text) 'fixed-pitch 'append table-text)
          table-text)))))

(defun codex-ide--buttonize-markdown-table-links (start end)
  "Convert rendered file-link spans between START and END into buttons."
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'codex-ide-table-link nil end)
                      end)))
        (when (and (get-text-property pos 'codex-ide-table-link)
                   (get-text-property pos 'codex-ide-path))
          (make-text-button
           pos next
           'action #'codex-ide--open-file-link
           'follow-link t
           'help-echo (get-text-property pos 'help-echo)
           'face 'link
           'codex-ide-markdown t
           'codex-ide-path (get-text-property pos 'codex-ide-path)
           'codex-ide-line (get-text-property pos 'codex-ide-line)
           'codex-ide-column (get-text-property pos 'codex-ide-column)))
        (setq pos next)))))

(defun codex-ide--markdown-table-block-at-point (end &optional allow-trailing)
  "Return markdown table data at point as (START END LINES), or nil.
END bounds the scan region. When ALLOW-TRAILING is nil, require a line after
the table so streaming partial tables at point-max are not rendered yet."
  (let* ((header-start (line-beginning-position))
         (header-end (line-end-position))
         (header (buffer-substring-no-properties header-start header-end)))
    (when (and (not (get-text-property header-start 'codex-ide-markdown))
               (codex-ide--markdown-table-row-line-p header))
      (save-excursion
        (forward-line 1)
        (when (< (point) end)
          (let* ((separator-start (line-beginning-position))
                 (separator-end (line-end-position))
                 (separator
                  (buffer-substring-no-properties separator-start separator-end)))
            (when (and (not (get-text-property separator-start 'codex-ide-markdown))
                       (codex-ide--markdown-table-separator-line-p separator))
              (let ((lines (list header separator))
                    (block-end
                     (save-excursion
                       (goto-char separator-start)
                       (codex-ide--markdown-line-region-end end))))
                (forward-line 1)
                (while (and (< (point) end)
                            (let* ((row-start (line-beginning-position))
                                   (row-end (line-end-position))
                                   (row
                                    (buffer-substring-no-properties
                                     row-start row-end)))
                              (and (not (get-text-property
                                         row-start 'codex-ide-markdown))
                                   (codex-ide--markdown-table-row-line-p row))))
                  (let* ((row-start (line-beginning-position))
                         (row-end (line-end-position))
                         (row (buffer-substring-no-properties row-start row-end)))
                    (setq lines (append lines (list row))
                          block-end (codex-ide--markdown-line-region-end end)))
                  (forward-line 1))
                (when (or allow-trailing
                          (< block-end end))
                  (list header-start block-end lines))))))))))

(defun codex-ide--render-markdown-tables (start end &optional allow-trailing)
  "Render markdown pipe tables between START and END.
When ALLOW-TRAILING is nil, leave an unfinished trailing table unrendered."
  (let ((end-marker (copy-marker end t)))
    (goto-char start)
    (while (< (point) (marker-position end-marker))
      (if-let ((table (codex-ide--markdown-table-block-at-point
                       (marker-position end-marker)
                       allow-trailing)))
          (pcase-let ((`(,block-start ,block-end ,lines) table))
            (if-let ((rendered (codex-ide--markdown-table-display-string lines)))
                (let ((original (buffer-substring-no-properties
                                 block-start
                                 block-end)))
                  (goto-char block-start)
                  (delete-region block-start block-end)
                  (insert rendered)
                  (add-text-properties
                   block-start
                   (point)
                   `(codex-ide-markdown t
                     codex-ide-markdown-table-original ,original))
                  (codex-ide--buttonize-markdown-table-links block-start (point))
                  (goto-char (point)))
              (goto-char block-end)))
        (forward-line 1)))
    (set-marker end-marker nil)))

(defun codex-ide--render-markdown-region (start end &optional allow-trailing-tables)
  "Apply lightweight markdown rendering between START and END.
When ALLOW-TRAILING-TABLES is nil, do not render a trailing table that reaches
END; this keeps streamed partial tables from being reformatted on every delta."
  (codex-ide--without-undo-recording
    (save-excursion
      (let ((inhibit-read-only t)
            (end-marker (copy-marker end t)))
        (codex-ide--clear-markdown-properties start (marker-position end-marker))
        (goto-char start)
        (codex-ide--render-fenced-code-blocks
         start
         (marker-position end-marker))
        (goto-char start)
        (codex-ide--render-markdown-tables
         start
         (marker-position end-marker)
         allow-trailing-tables)
        (goto-char start)
        (while (re-search-forward
                "\\(\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))\\)"
                (marker-position end-marker)
                t)
          (unless (or (get-text-property (match-beginning 1) 'codex-ide-markdown)
                      (get-text-property (1- (match-end 1)) 'codex-ide-markdown))
            (let* ((match-start (match-beginning 1))
                   (match-end (match-end 1))
                   (label (match-string-no-properties 2))
                   (display-label (codex-ide--normalize-markdown-link-label label))
                   (target (match-string-no-properties 3))
                   (parsed (codex-ide--parse-file-link-target target)))
              (when parsed
                (make-text-button
                 match-start match-end
                 'action #'codex-ide--open-file-link
                 'follow-link t
                 'help-echo target
                 'face 'link
                 'codex-ide-markdown t
                 'codex-ide-path (nth 0 parsed)
                 'codex-ide-line (nth 1 parsed)
                 'codex-ide-column (nth 2 parsed))
                (add-text-properties
                 match-start match-end
                 `(display ,display-label))))))
        (goto-char start)
        (while (re-search-forward
                "`\\([^`\n]+\\)`"
                (marker-position end-marker)
                t)
          (unless (or (get-text-property (match-beginning 0) 'codex-ide-markdown)
                      (get-text-property (1- (match-end 0)) 'codex-ide-markdown))
            (let ((code-start (match-beginning 1))
                  (code-end (match-end 1)))
              (add-text-properties
               code-start code-end
               '(face font-lock-keyword-face
                 codex-ide-markdown t))
              (add-text-properties
               (match-beginning 0) code-start
               '(display ""
                 codex-ide-markdown t))
              (add-text-properties
               code-end (match-end 0)
               '(display ""
                 codex-ide-markdown t)))))
        (codex-ide--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^*]\\)\\(\\*\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\*\\)"
         'bold)
        (codex-ide--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^_]\\)\\(__\\([^_\n ]\\(?:[^\n]*?[^_\n ]\\)?\\)__\\)"
         'bold
         t)
        (codex-ide--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^*]\\)\\(\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\)"
         'italic)
        (codex-ide--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^_]\\)\\(_\\([^_\n ]\\(?:[^_\n]*[^_\n ]\\)?\\)_\\)"
         'italic
         t)
        (set-marker end-marker nil)))))

(defun codex-ide--markdown-region-over-size-limit-p (start end)
  "Return non-nil when START to END should stay plain for performance."
  (and (integerp codex-ide-markdown-render-max-chars)
       (or (<= codex-ide-markdown-render-max-chars 0)
           (> (- end start) codex-ide-markdown-render-max-chars))))

(defun codex-ide--maybe-render-markdown-region
    (start end &optional allow-trailing-tables)
  "Render markdown between START and END unless the region is too large.
Return non-nil when rendering was applied.  When rendering is skipped, remove
any existing Codex markdown properties from the region so the buffer remains
plain text."
  (if (codex-ide--markdown-region-over-size-limit-p start end)
      (progn
        (codex-ide--without-undo-recording
          (save-excursion
            (let ((inhibit-read-only t))
              (codex-ide--clear-markdown-properties start end))))
        nil)
    (codex-ide--render-markdown-region start end allow-trailing-tables)
    t))

(defun codex-ide--streaming-markdown-complete-line-limit (end)
  "Return the completed-line boundary at or before END."
  (save-excursion
    (goto-char end)
    (if (or (bobp) (bolp))
        (point)
      (line-beginning-position))))

(defun codex-ide--markdown-fence-line-p (line)
  "Return non-nil when LINE is a fenced-code delimiter."
  (string-match-p "\\`[ \t]*```[^`\n]*[ \t]*\\'" line))

(defun codex-ide--streaming-markdown-table-block-end (limit)
  "Return the raw markdown table block end at point, or nil.
LIMIT bounds the scan."
  (let* ((header-start (point))
         (header (buffer-substring-no-properties
                  header-start
                  (line-end-position))))
    (when (codex-ide--markdown-table-row-line-p header)
      (save-excursion
        (forward-line 1)
        (when (< (point) limit)
          (let ((separator (buffer-substring-no-properties
                            (point)
                            (line-end-position))))
            (when (codex-ide--markdown-table-separator-line-p separator)
              (forward-line 1)
              (while (and (< (point) limit)
                          (codex-ide--markdown-table-row-line-p
                           (buffer-substring-no-properties
                            (point)
                            (line-end-position))))
                (forward-line 1))
              (min (point) limit))))))))

(defun codex-ide--streaming-markdown-pending-table-header-p (line limit)
  "Return non-nil when LINE may be a table header awaiting more input.
LIMIT is the completed-line boundary for the current streaming pass."
  (and (codex-ide--markdown-table-row-line-p line)
       (save-excursion
         (forward-line 1)
         (or (>= (point) limit)
             (codex-ide--markdown-table-separator-line-p
              (buffer-substring-no-properties
               (point)
               (line-end-position)))))))

(defun codex-ide--streaming-markdown-segments (start limit)
  "Return stream-safe markdown segments from START to LIMIT.
The return value is (SEGMENTS NEXT), where SEGMENTS is a list of
\(START END ALLOW-TRAILING-TABLES) marker tuples and NEXT is a marker for the
next dirty position.  A trailing pipe table is rendered but kept dirty because
new rows may still arrive.  Open fenced code blocks stop the scan so the whole
block can be rendered once its closing fence arrives."
  (let ((segments nil)
        (segment-start start)
        (next-position limit)
        (stop nil))
    (save-excursion
      (goto-char start)
      (while (and (< (point) limit)
                  (not stop))
        (let* ((line-start (point))
               (line (buffer-substring-no-properties
                      line-start
                      (line-end-position))))
          (cond
           ((codex-ide--markdown-fence-line-p line)
            (when (< segment-start line-start)
              (push (list (copy-marker segment-start)
                          (copy-marker line-start)
                          nil)
                    segments))
            (let ((closing-end nil))
              (save-excursion
                (forward-line 1)
                (when (re-search-forward
                       "^[ \t]*```[ \t]*$"
                       limit
                       t)
                  (setq closing-end
                        (codex-ide--markdown-line-region-end limit))))
              (if closing-end
                  (progn
                    (push (list (copy-marker line-start)
                                (copy-marker closing-end)
                                nil)
                          segments)
                    (goto-char closing-end)
                    (setq segment-start closing-end))
                (setq next-position line-start
                      stop t))))
           ((let ((table-end
                   (codex-ide--streaming-markdown-table-block-end limit)))
              (when table-end
                (if (= table-end limit)
                    (progn
                      (when (< segment-start line-start)
                        (push (list (copy-marker segment-start)
                                    (copy-marker line-start)
                                    nil)
                              segments))
                      (push (list (copy-marker line-start)
                                  (copy-marker table-end)
                                  t)
                            segments)
                      (setq next-position line-start
                            stop t))
                  (goto-char table-end))
                t)))
           ((codex-ide--streaming-markdown-pending-table-header-p line limit)
            (when (< segment-start line-start)
              (push (list (copy-marker segment-start)
                          (copy-marker line-start)
                          nil)
                    segments))
            (setq next-position line-start
                  stop t))
           (t
            (forward-line 1))))))
    (unless stop
      (setq next-position limit)
      (when (< segment-start limit)
        (push (list (copy-marker segment-start)
                    (copy-marker limit)
                    nil)
              segments)))
    (list (nreverse segments) (copy-marker next-position))))

(defun codex-ide--render-current-agent-message-markdown-streaming
    (&optional session item-id)
  "Incrementally render stream-safe markdown for SESSION's current message."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (or (null item-id)
                   (equal item-id
                          (codex-ide-session-current-message-item-id session))))
      (when-let ((message-start
                  (codex-ide-session-current-message-start-marker session)))
        (when (eq (marker-buffer message-start) buffer)
          (with-current-buffer buffer
            (let* ((render-start-marker
                    (or (codex-ide--session-metadata-get
                         session
                         :agent-message-stream-render-start-marker)
                        (codex-ide--session-metadata-put
                         session
                         :agent-message-stream-render-start-marker
                         (copy-marker message-start))))
                   (render-start (marker-position render-start-marker))
                   (limit (codex-ide--streaming-markdown-complete-line-limit
                           (point-max))))
              (when (< render-start limit)
                (pcase-let ((`(,segments ,next-marker)
                             (codex-ide--streaming-markdown-segments
                              render-start
                              limit)))
                  (dolist (segment segments)
                    (let ((segment-start (marker-position (nth 0 segment)))
                          (segment-end (marker-position (nth 1 segment)))
                          (allow-trailing-tables (nth 2 segment)))
                      (when (< segment-start segment-end)
                        (codex-ide--maybe-render-markdown-region
                         segment-start
                         segment-end
                         allow-trailing-tables)))
                    (set-marker (nth 0 segment) nil)
                    (set-marker (nth 1 segment) nil))
                  (set-marker render-start-marker
                              (marker-position next-marker))
                  (set-marker next-marker nil))))))))))

(defun codex-ide--insert-input-prompt (&optional session initial-text)
  "Insert a writable `>' prompt for SESSION.
Optionally seed it with INITIAL-TEXT."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
          (let ((inhibit-read-only t)
                (moving (= (point) (point-max)))
                transcript-start
                prompt-start)
            (goto-char (point-max))
            (setq transcript-start (point))
            (unless (or (= (point) (point-min))
                        (bolp))
              (insert "\n"))
            (codex-ide--freeze-region transcript-start (point))
            (codex-ide--delete-input-overlay session)
            (setf (codex-ide-session-input-prompt-start-marker session)
                  (copy-marker (point)))
            (setq prompt-start (point))
            (insert (propertize "> " 'face 'codex-ide-user-prompt-face))
            (codex-ide--freeze-region prompt-start (point))
            (setf (codex-ide-session-input-start-marker session)
                  (copy-marker (point)))
            (codex-ide--reset-prompt-history-navigation session)
            (when initial-text
              (insert initial-text))
            (codex-ide--make-region-writable
             (marker-position (codex-ide-session-input-start-marker session))
             (point))
            (let ((overlay (make-overlay
                            (marker-position
                             (codex-ide-session-input-prompt-start-marker session))
                            (point-max)
                            buffer
                            nil
                            t)))
              (overlay-put overlay 'face 'codex-ide-user-prompt-face)
              (overlay-put overlay 'evaporate t)
              (setf (codex-ide-session-input-overlay session) overlay))
            (when moving
              (goto-char (point-max)))
            (codex-ide--sync-prompt-minor-mode session)))
        (codex-ide--discard-buffer-undo-history)))))

(defun codex-ide--input-prompt-active-p (&optional session)
  "Return non-nil when SESSION currently has an editable input prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (and session (codex-ide-session-buffer session)))
        (overlay (and session (codex-ide-session-input-overlay session)))
        (marker (and session (codex-ide-session-input-start-marker session))))
    (and (buffer-live-p buffer)
         (overlayp overlay)
         (eq (overlay-buffer overlay) buffer)
         (markerp marker)
         (eq (marker-buffer marker) buffer))))

(defun codex-ide--ensure-input-prompt (&optional session initial-text)
  "Insert an editable prompt for SESSION when one is not already active.
When INITIAL-TEXT is non-nil, seed a newly inserted prompt with it."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (when (and (string= (codex-ide-session-status session) "idle")
             (not (codex-ide-session-current-turn-id session))
             (not (codex-ide-session-output-prefix-inserted session))
             (not (codex-ide--input-prompt-active-p session)))
    (codex-ide--insert-input-prompt session initial-text)))

(defun codex-ide--current-input (&optional session)
  "Return the current editable input text for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session))
        (marker (codex-ide-session-input-start-marker session)))
    (unless (and (buffer-live-p buffer) marker)
      "")
    (with-current-buffer buffer
      (string-trim-right
       (buffer-substring-no-properties marker (point-max))))))

(defun codex-ide--replace-current-input (session text)
  "Replace SESSION's editable input region with TEXT."
  (let ((buffer (codex-ide-session-buffer session))
        (marker (codex-ide-session-input-start-marker session)))
    (unless (and (buffer-live-p buffer) marker)
      (user-error "No editable Codex prompt in this buffer"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char marker)
        (delete-region marker (point-max))
        (insert text)
        (goto-char (point-max))))))

(defun codex-ide--browse-prompt-history (direction)
  "Browse prompt history in DIRECTION for the current Codex session.
DIRECTION should be -1 for older history and 1 for newer history."
  (let* ((session (codex-ide--session-for-current-project))
         (history (or (codex-ide--project-persisted-get :prompt-history session)
                      '())))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (user-error "Prompt history is only available in the Codex session buffer"))
    (unless (codex-ide-session-input-start-marker session)
      (user-error "No editable Codex prompt in this buffer"))
    (unless history
      (user-error "No prompt history"))
    (let ((index (codex-ide-session-prompt-history-index session)))
      (when (null index)
        (setf (codex-ide-session-prompt-history-draft session)
              (codex-ide--current-input session)))
      (pcase direction
        (-1
         (cond
          ((null index)
           (setq index 0))
          ((>= index (1- (length history)))
           (user-error "End of prompt history"))
          (t
           (setq index (1+ index)))))
        (1
         (setq index (if (or (null index)
                             (<= index 0))
                         nil
                       (1- index)))))
      (if (null index)
          (progn
            (setf (codex-ide-session-prompt-history-index session) nil)
            (codex-ide--replace-current-input session ""))
        (setf (codex-ide-session-prompt-history-index session) index)
        (codex-ide--replace-current-input session (nth index history))))))

(defun codex-ide--goto-prompt-line (direction)
  "Move point to another user prompt line in DIRECTION.
DIRECTION should be -1 for a previous prompt line and 1 for a next prompt line."
  (let ((session (codex-ide--session-for-current-project)))
    (unless (eq (current-buffer) (codex-ide-session-buffer session))
      (user-error "Prompt-line navigation is only available in the Codex session buffer"))
    (save-match-data
      (beginning-of-line)
      (let ((found
             (pcase direction
               (-1
                (when (looking-at-p "> ")
                  (forward-line -1))
                (re-search-backward "^> " nil t))
               (1
                (when (looking-at-p "> ")
                  (forward-line 1))
                (re-search-forward "^> " nil t))
               (_
                (error "Unsupported prompt-line direction: %s" direction)))))
        (unless found
          (user-error (if (< direction 0)
                          "No earlier prompt line"
                        "No later prompt line")))
        (goto-char (match-end 0))))))

(defun codex-ide--begin-turn-display (&optional session context-summary quiet)
  "Freeze the current prompt and show immediate pending output for SESSION.
When CONTEXT-SUMMARY is non-nil, insert it beneath the submitted prompt.
When QUIET is non-nil, do not refresh SESSION's header line."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
          (let ((inhibit-read-only t)
                context-start
                spacing-start)
            (when-let ((start (codex-ide-session-input-prompt-start-marker session)))
              (codex-ide--style-user-prompt-region start (point-max))
              (codex-ide--freeze-region start (point-max))
              (when context-summary
                (setq context-start (point-max))
                (goto-char context-start)
                (insert "\n")
                (insert (propertize context-summary
                                    'face
                                    'codex-ide-item-detail-face))
                (codex-ide--freeze-region context-start (point))))
            (codex-ide--delete-input-overlay session)
            (codex-ide--sync-prompt-minor-mode session)
            (goto-char (point-max))
            (setq spacing-start (point))
            (insert "\n\n")
            (codex-ide--freeze-region spacing-start (point))
            (codex-ide--insert-pending-output-indicator session)
            (setf (codex-ide-session-output-prefix-inserted session) t
                  (codex-ide-session-status session) "running")
            (unless quiet
              (codex-ide--update-header-line session))))
        (codex-ide--discard-buffer-undo-history)))))

(defun codex-ide--shell-command-string (command)
  "Render COMMAND as a shell-like string."
  (cond
   ((stringp command) command)
   ((or (listp command) (vectorp command))
    (mapconcat (lambda (arg)
                 (if (stringp arg)
                     (shell-quote-argument arg)
                   (format "%s" arg)))
               (append command nil)
               " "))
   (t (format "%s" command))))

(defun codex-ide--command-argv (command)
  "Return COMMAND as an argv list when it can be parsed that way."
  (cond
   ((or (listp command) (vectorp command))
    (mapcar (lambda (arg)
              (if (stringp arg)
                  arg
                (format "%s" arg)))
            (append command nil)))
   ((stringp command)
    (codex-ide--split-shell-words command))))

(defun codex-ide--split-shell-words (command)
  "Split COMMAND into shell-like words.
This handles the simple quoting shapes emitted by command execution items
without interpreting shell metacharacters inside quoted strings."
  (let ((index 0)
        (length (length command))
        quote
        escaping
        in-word
        (word "")
        words)
    (while (< index length)
      (let ((char (aref command index)))
        (cond
         (escaping
          (setq word (concat word (char-to-string char))
                in-word t
                escaping nil))
         ((and quote (eq quote ?\'))
          (if (eq char quote)
              (setq quote nil)
            (setq word (concat word (char-to-string char))
                  in-word t)))
         ((eq char ?\\)
          (setq escaping t
                in-word t))
         (quote
          (if (eq char quote)
              (setq quote nil)
            (setq word (concat word (char-to-string char))
                  in-word t)))
         ((or (eq char ?\') (eq char ?\"))
          (setq quote char
                in-word t))
         ((memq char '(?\s ?\t ?\n))
          (when in-word
            (push word words)
            (setq word ""
                  in-word nil)))
         ((eq char ?|)
          (when in-word
            (push word words)
            (setq word ""
                  in-word nil))
          (push "|" words))
         (t
          (setq word (concat word (char-to-string char))
                in-word t))))
      (setq index (1+ index)))
    (when (or quote escaping)
      (setq words nil
            in-word nil
            word ""))
    (when in-word
      (push word words))
    (nreverse words)))

(defun codex-ide--split-shell-pipeline (command)
  "Split COMMAND on unquoted shell pipeline separators."
  (let ((index 0)
        (length (length command))
        quote
        escaping
        (part-start 0)
        parts)
    (while (< index length)
      (let ((char (aref command index)))
        (cond
         (escaping
          (setq escaping nil))
         ((and quote (eq quote ?\'))
          (when (eq char quote)
            (setq quote nil)))
         ((eq char ?\\)
          (setq escaping t))
         (quote
          (when (eq char quote)
            (setq quote nil)))
         ((or (eq char ?\') (eq char ?\"))
          (setq quote char))
         ((eq char ?|)
          (push (string-trim (substring command part-start index)) parts)
          (setq part-start (1+ index)))))
      (setq index (1+ index)))
    (unless (or quote escaping)
      (push (string-trim (substring command part-start)) parts)
      (nreverse parts))))

(defun codex-ide--shell-wrapper-inner-command (argv)
  "Return the shell script from shell wrapper ARGV, or nil."
  (when (and (>= (length argv) 3)
             (member (file-name-nondirectory (car argv))
                     '("bash" "sh" "zsh"))
             (member (cadr argv) '("-c" "-lc")))
    (nth 2 argv)))

(defun codex-ide--display-command-string (command)
  "Return the user-facing shell command string for COMMAND."
  (or (when-let* ((argv (codex-ide--command-argv command))
                  (inner (codex-ide--shell-wrapper-inner-command argv)))
        inner)
      (codex-ide--shell-command-string command)))

(defun codex-ide--display-command-argv (command)
  "Return argv for COMMAND after removing common shell wrappers."
  (let ((display-command (codex-ide--display-command-string command)))
    (or (codex-ide--split-shell-words display-command)
        (codex-ide--command-argv command))))

(defun codex-ide--sed-print-request (argv)
  "Parse a simple `sed -n' print request from ARGV.
Return (START END FILES), or nil when ARGV does not describe one."
  (when (and (consp argv)
             (string= (file-name-nondirectory (car argv)) "sed"))
    (let ((args (cdr argv))
          quiet
          script
          files
          unsupported)
      (while args
        (let ((arg (pop args)))
          (cond
           ((member arg '("-n" "--quiet" "--silent"))
            (setq quiet t))
           ((string= arg "-e")
            (if (or script (null args))
                (setq unsupported t)
              (setq script (pop args))))
           ((and (string-prefix-p "-e" arg)
                 (> (length arg) 2))
            (if script
                (setq unsupported t)
              (setq script (substring arg 2))))
           ((string-prefix-p "-" arg)
            (setq unsupported t))
           ((not script)
            (setq script arg))
           (t
            (push arg files)))))
      (when (and quiet
                 (not unsupported)
                 (stringp script)
                 (string-match "\\`[[:space:]]*\\([0-9]+\\)\\(?:,[[:space:]]*\\([0-9]+\\)\\)?p[[:space:]]*\\'"
                               script))
        (list (string-to-number (match-string 1 script))
              (if-let ((end (match-string 2 script)))
                  (string-to-number end)
                (string-to-number (match-string 1 script)))
              (nreverse files))))))

(defun codex-ide--nl-command-file (argv)
  "Return the file read by a simple `nl' command ARGV, or nil."
  (when (and (consp argv)
             (string= (file-name-nondirectory (car argv)) "nl"))
    (car (last (cl-remove-if (lambda (arg)
                               (string-prefix-p "-" arg))
                             (cdr argv))))))

(defun codex-ide--read-lines-summary (file start end)
  "Format a summary for reading FILE between START and END."
  (if (= start end)
      (format "Read %s (line %d)" file start)
    (format "Read %s (lines %d to %d)" file start end)))

(defun codex-ide--command-read-summary (command)
  "Return a semantic read summary for COMMAND, or nil."
  (let* ((display-command (codex-ide--display-command-string command))
         (argv (codex-ide--display-command-argv command))
         (sed-request (codex-ide--sed-print-request argv)))
    (cond
     ((and sed-request
           (= (length (nth 2 sed-request)) 1))
      (codex-ide--read-lines-summary
       (car (nth 2 sed-request))
       (nth 0 sed-request)
       (nth 1 sed-request)))
     ((and (stringp display-command)
           (string-match-p "|" display-command))
      (let ((parts (codex-ide--split-shell-pipeline display-command)))
        (when (= (length parts) 2)
          (let* ((left (codex-ide--split-shell-words (car parts)))
                 (right (codex-ide--split-shell-words (cadr parts)))
                 (file (codex-ide--nl-command-file left))
                 (request (codex-ide--sed-print-request right)))
            (when (and file request (null (nth 2 request)))
              (codex-ide--read-lines-summary
               file
               (nth 0 request)
               (nth 1 request))))))))))

(defconst codex-ide--rg-options-with-values
  '("-A" "-B" "-C" "-E" "-M" "-e" "-f" "-g" "-m" "-t" "-T"
    "--after-context" "--before-context" "--colors" "--context"
    "--context-separator" "--encoding" "--engine" "--field-context-separator"
    "--field-match-separator" "--file" "--files-from" "--glob"
    "--glob-case-insensitive" "--iglob"
    "--max-columns" "--max-count" "--max-depth" "--max-filesize"
    "--path-separator"
    "--pre" "--pre-glob" "--regexp" "--replace" "--sort"
    "--threads" "--type" "--type-add" "--type-clear" "--type-not")
  "Ripgrep options that consume the following argv element.")

(defun codex-ide--rg-search-request (argv)
  "Parse a simple ripgrep search from ARGV.
Return (PATTERN PATHS), or nil when ARGV does not describe a search."
  (when (and (consp argv)
             (member (file-name-nondirectory (car argv)) '("rg" "ripgrep")))
    (let ((args (cdr argv))
          pattern
          paths
          literal-args)
      (while args
        (let ((arg (pop args)))
          (cond
           (literal-args
            (if pattern
                (push arg paths)
              (setq pattern arg)))
           ((string= arg "--")
            (setq literal-args t))
           ((or (string= arg "-e")
                (string= arg "--regexp"))
            (when args
              (setq pattern (pop args))))
           ((string-prefix-p "--regexp=" arg)
            (setq pattern (substring arg (length "--regexp="))))
           ((member arg codex-ide--rg-options-with-values)
            (when args
              (pop args)))
           ((and (string-prefix-p "--" arg)
                 (string-match-p "=" arg)))
           ((string-prefix-p "-" arg))
           ((not pattern)
            (setq pattern arg))
           (t
            (push arg paths)))))
      (when (and (stringp pattern)
                 (not (string-empty-p pattern)))
        (list pattern (nreverse paths))))))

(defun codex-ide--search-summary (pattern paths)
  "Format a semantic search summary for PATTERN across PATHS."
  (format "Searched %s for %s"
          (codex-ide--search-locations-summary paths)
          (codex-ide--quote-summary-string pattern)))

(defun codex-ide--quote-summary-string (value)
  "Return VALUE quoted for a summary line."
  (format "\"%s\""
          (replace-regexp-in-string "\"" "\\\\\"" (or value "") t t)))

(defun codex-ide--search-locations-summary (paths)
  "Return a human-readable location summary for PATHS."
  (let ((paths (mapcar (lambda (path)
                         (if (string= path ".")
                             "current directory"
                           path))
                       paths)))
    (cond
     ((null paths) "current directory")
     ((null (cdr paths)) (car paths))
     ((<= (length paths) 3)
      (concat (string-join (butlast paths) ", ")
              " and "
              (car (last paths))))
     (t
      (format "%d locations" (length paths))))))

(defun codex-ide--count-search-output-hits (output)
  "Return a best-effort ripgrep hit count from OUTPUT."
  (when (stringp output)
    (let* ((lines (seq-filter
                   (lambda (line) (not (string-empty-p line)))
                   (split-string output "\n")))
           (numbered-lines
            (seq-filter
             (lambda (line)
               (string-match-p "\\(?:\\`\\|:\\)[0-9]+:" line))
             lines)))
      (length (or numbered-lines lines)))))

(defun codex-ide--format-hit-count (count)
  "Return a short summary for COUNT search hits."
  (format "found %d hit%s" count (if (= count 1) "" "s")))

(defun codex-ide--command-output-trimmed-end (output)
  "Return the end index of OUTPUT after trimming trailing whitespace."
  (let ((end (length output)))
    (while (and (> end 0)
                (memq (aref output (1- end))
                      '(?\s ?\t ?\n ?\r ?\f ?\v)))
      (setq end (1- end)))
    end))

(defun codex-ide--command-output-count-newlines (output end)
  "Return the number of newline characters in OUTPUT before END."
  (let ((count 0)
        (pos 0))
    (while (< pos end)
      (when (= (aref output pos) ?\n)
        (setq count (1+ count)))
      (setq pos (1+ pos)))
    count))

(defun codex-ide--command-output-line-count (output)
  "Return the display line count for command OUTPUT."
  (cond
   ((or (null output) (string-empty-p output)) 0)
   ((= (codex-ide--command-output-trimmed-end output) 0) 1)
   (t
    (1+ (codex-ide--command-output-count-newlines
         output
         (codex-ide--command-output-trimmed-end output))))))

(defun codex-ide--command-output-start-after-lines (output line-count)
  "Return the index after LINE-COUNT newline-terminated lines in OUTPUT."
  (let ((len (length output))
        (seen 0)
        (pos 0))
    (while (and (< pos len)
                (< seen line-count))
      (when (= (aref output pos) ?\n)
        (setq seen (1+ seen)))
      (setq pos (1+ pos)))
    pos))

(defun codex-ide--command-output-render-range (output)
  "Return the raw OUTPUT range to render into the transcript as (START . END)."
  (let ((start 0)
        (end (length output)))
    (when (integerp codex-ide-command-output-max-rendered-lines)
      (let* ((line-count (codex-ide--command-output-line-count output))
             (hidden-lines
              (max 0
                   (- line-count
                      (max 0 codex-ide-command-output-max-rendered-lines)))))
        (setq start
              (max start
                   (codex-ide--command-output-start-after-lines
                    output
                    hidden-lines)))))
    (when (integerp codex-ide-command-output-max-rendered-chars)
      (setq start
            (max start
                 (- end
                    (max 0 codex-ide-command-output-max-rendered-chars)))))
    (cons (min start end) end)))

(defun codex-ide--command-output-truncation-notice ()
  "Return the transcript notice inserted after truncated command output."
  "    ... transcript output truncated; showing latest output.\n")

(defun codex-ide--format-command-output-text (output &optional truncated)
  "Return prefixed display text for raw command OUTPUT."
  (when (and (stringp output) (not (string-empty-p output)))
    (let* ((ends-with-newline (string-suffix-p "\n" output))
           (body (if ends-with-newline
                     (substring output 0 -1)
                   output))
           (lines (split-string body "\n")))
      (concat
       (when truncated
         (codex-ide--command-output-truncation-notice))
       (mapconcat (lambda (line) (concat "    " line)) lines "\n")
       (if ends-with-newline "\n" "")))))

(defun codex-ide--command-output-header-prefix-text (overlay)
  "Return the non-action header text for command output OVERLAY."
  (let* ((line-count (overlay-get overlay :line-count))
         (visible-line-count (overlay-get overlay :visible-line-count))
         (truncated (overlay-get overlay :truncated))
         (line-label (if (= line-count 1) "line" "lines"))
         (complete (overlay-get overlay :complete)))
    (format "  └ output: %d %s%s%s "
            line-count
            line-label
            (if truncated
                (format ", showing last %d" visible-line-count)
              "")
            (if complete "" ", streaming"))))

(defun codex-ide--command-output-text (overlay)
  "Return the full output text for command output OVERLAY."
  (let* ((session (overlay-get overlay :session))
         (item-id (overlay-get overlay :item-id))
         (state (and session item-id
                     (codex-ide--item-state session item-id))))
    (or (plist-get state :output-text)
        (overlay-get overlay :output-fallback-text)
        "")))

(defun codex-ide--command-output-buffer-name (overlay)
  "Return the buffer name for full command output OVERLAY."
  (let* ((session (overlay-get overlay :session))
         (item-id (overlay-get overlay :item-id))
         (directory (and session (codex-ide-session-directory session)))
         (project (and directory
                       (file-name-nondirectory
                        (directory-file-name directory)))))
    (format "*codex-output[%s:%s]*"
            (or project "session")
            (or item-id "command"))))

(defun codex-ide--command-output-command-text (overlay)
  "Return the display command associated with command output OVERLAY."
  (let* ((session (overlay-get overlay :session))
         (item-id (overlay-get overlay :item-id))
         (state (and session item-id
                     (codex-ide--item-state session item-id)))
         (item (plist-get state :item))
         (command (and (listp item) (alist-get 'command item))))
    (and command
         (codex-ide--display-command-string command))))

(defun codex-ide--open-command-output-overlay (overlay)
  "Open full command output for OVERLAY in a separate buffer."
  (unless (overlayp overlay)
    (user-error "No command output at point"))
  (let* ((output (codex-ide--command-output-text overlay))
         (command (codex-ide--command-output-command-text overlay))
         (buffer (get-buffer-create
                  (codex-ide--command-output-buffer-name overlay))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when command
          (insert "$ " command "\n\n"))
        (insert output)
        (unless (or (string-empty-p output)
                    (string-suffix-p "\n" output))
          (insert "\n"))
        (goto-char (point-min))
        (special-mode)
        (setq-local buffer-undo-list t)
        (when (bound-and-true-p visual-line-mode)
          (visual-line-mode -1))
        (when (bound-and-true-p font-lock-mode)
          (font-lock-mode -1))))
    (pop-to-buffer buffer)))

(defun codex-ide--toggle-command-output-overlay (overlay)
  "Toggle command output OVERLAY.
Return non-nil when OVERLAY was toggled."
  (when (and (overlayp overlay)
             (buffer-live-p (overlay-buffer overlay)))
    (let ((folded (not (overlay-get overlay :folded))))
      (overlay-put overlay :folded folded)
      (overlay-put overlay 'invisible (and folded t))
      (codex-ide--set-command-output-header overlay)
      (codex-ide--set-command-output-body
       overlay
       (or (overlay-get overlay :display-text) ""))
      t)))

(defun codex-ide-open-command-output-at-point (&optional pos)
  "Open full command output for the output block at POS.
Return non-nil when a command output block was found."
  (interactive)
  (if-let ((overlay (codex-ide--command-output-overlay-at-point pos)))
      (progn
        (codex-ide--open-command-output-overlay overlay)
        t)
    (user-error "No command output at point")))

(defun codex-ide--insert-command-output-button (label action overlay help-echo)
  "Insert a command output button labeled LABEL invoking ACTION for OVERLAY."
  (let ((start (point)))
    (insert "[" label "]")
    (make-text-button
     start
     (point)
     'action (lambda (_button) (funcall action overlay))
     'follow-link t
     'help-echo help-echo
     codex-ide-command-output-overlay-property overlay)))

(defun codex-ide--insert-command-output-header (overlay)
  "Insert the command output header and action buttons for OVERLAY."
  (let ((prefix-start (point)))
    (insert (codex-ide--command-output-header-prefix-text overlay))
    (add-text-properties
     prefix-start
     (point)
     (list 'face 'codex-ide-item-detail-face
           'keymap codex-ide-command-output-map
           'help-echo "RET toggles command output"
           codex-ide-command-output-overlay-property overlay))
    (codex-ide--insert-command-output-button
     (if (overlay-get overlay :folded) "expand" "fold")
     #'codex-ide--toggle-command-output-overlay
     overlay
     "Toggle command output")
    (when (overlay-get overlay :truncated)
      (insert " ")
      (codex-ide--insert-command-output-button
       "full output"
       #'codex-ide--open-command-output-overlay
       overlay
       "Open full command output in a separate buffer"))
    (insert "\n")))

(defun codex-ide--set-command-output-header (overlay)
  "Refresh the visible header for command output OVERLAY."
  (let ((buffer (overlay-buffer overlay))
        (header-start (overlay-get overlay :header-start))
        (header-end (overlay-get overlay :header-end))
        (body-start (overlay-get overlay :body-start))
        (body-end (overlay-get overlay :body-end)))
    (when (and (buffer-live-p buffer)
               (markerp header-start)
               (markerp header-end)
               (markerp body-start)
               (markerp body-end))
      (with-current-buffer buffer
        (codex-ide--without-undo-recording
          (let ((inhibit-read-only t)
                (moving (= (point) (point-max)))
                (body-empty (= (marker-position body-start)
                               (marker-position body-end)))
                (start (marker-position header-start)))
            (goto-char start)
            (delete-region start (marker-position header-end))
            (codex-ide--insert-command-output-header overlay)
            (set-marker header-start start)
            (set-marker header-end (point))
            (set-marker body-start (point))
            (when body-empty
              (set-marker body-end (point)))
            (move-overlay overlay
                          (marker-position body-start)
                          (marker-position body-end))
            (codex-ide--freeze-region (marker-position header-start)
                                      (marker-position header-end))
            (when moving
              (goto-char (point-max)))))))))

(defun codex-ide--set-command-output-body (overlay display-text)
  "Refresh OVERLAY's visible body using DISPLAY-TEXT.
When OVERLAY is folded, remove the body text from the transcript buffer."
  (let ((buffer (overlay-buffer overlay))
        (body-start (overlay-get overlay :body-start))
        (body-end (overlay-get overlay :body-end)))
    (when (and (buffer-live-p buffer)
               (markerp body-start)
               (markerp body-end))
      (with-current-buffer buffer
        (let ((codex-ide--current-agent-item-type "commandExecution"))
          (codex-ide--without-undo-recording
            (let ((inhibit-read-only t)
                  (moving (= (point) (point-max)))
                  start)
              (delete-region (marker-position body-start)
                             (marker-position body-end))
              (goto-char (marker-position body-start))
              (setq start (point))
              (unless (overlay-get overlay :folded)
                (insert display-text)
                (add-text-properties
                 start
                 (point)
                 (append
                  (list 'face 'codex-ide-command-output-face
                        'keymap codex-ide-command-output-map
                        'help-echo "RET toggles command output"
                        codex-ide-command-output-overlay-property overlay)
                  (overlay-get overlay :body-properties)))
                (codex-ide--freeze-region start (point)))
              (set-marker body-end (point))
              (move-overlay overlay
                            (marker-position body-start)
                            (marker-position body-end))
              (when moving
                (goto-char (point-max))))))))))

(defun codex-ide--ensure-command-output-block (session item-id)
  "Return the command output overlay for ITEM-ID in SESSION, creating it."
  (let* ((state (codex-ide--item-state session item-id))
         (existing (plist-get state :command-output-overlay)))
    (if (and (overlayp existing) (buffer-live-p (overlay-buffer existing)))
        existing
      (let ((buffer (codex-ide-session-buffer session))
            overlay)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((codex-ide--current-agent-item-type "commandExecution"))
              (codex-ide--without-undo-recording
                (let ((inhibit-read-only t)
                      (moving (= (point) (point-max)))
                      (initial-folded codex-ide-command-output-fold-on-start)
                      header-start
                      header-end
                      body-start
                      body-end)
                  (goto-char (point-max))
                  (setq header-start (copy-marker (point)))
                  (setq overlay (make-overlay (point) (point) buffer nil nil))
                  (overlay-put overlay 'face 'codex-ide-command-output-face)
                  (overlay-put overlay codex-ide-command-output-overlay-property overlay)
                  (overlay-put overlay :session session)
                  (overlay-put overlay :item-id item-id)
                  (overlay-put overlay :header-start header-start)
                  (overlay-put overlay :display-text "")
                  (overlay-put overlay :line-count 0)
                  (overlay-put overlay :visible-line-count 0)
                  (overlay-put overlay :truncated nil)
                  (overlay-put overlay :folded initial-folded)
                  (overlay-put overlay :complete nil)
                  (overlay-put overlay 'invisible (and initial-folded t))
                  (overlay-put overlay :body-properties nil)
                  (codex-ide--insert-command-output-header overlay)
                  (setq header-end (copy-marker (point)))
                  (setq body-start (copy-marker (point)))
                  (setq body-end (copy-marker (point)))
                  (overlay-put overlay :header-end header-end)
                  (overlay-put overlay :body-start body-start)
                  (overlay-put overlay :body-end body-end)
                  (codex-ide--freeze-region (marker-position header-start)
                                            (marker-position header-end))
                  (when moving
                    (goto-char (point-max)))))))
          (codex-ide--put-item-state
           session
           item-id
           (plist-put state :command-output-overlay overlay))
          overlay)))))

(defun codex-ide--append-command-output-text (session item-id text)
  "Append command output TEXT for ITEM-ID in SESSION."
  (when (and (stringp text) (not (string-empty-p text)))
    (when-let ((overlay (codex-ide--ensure-command-output-block session item-id)))
      (let ((state-output-text
             (plist-get (codex-ide--item-state session item-id) :output-text))
            (previous (or (overlay-get overlay :output-fallback-text) ""))
            output-text
            visible-range
            visible-output
            display-text
            truncated)
        (setq output-text (or state-output-text (concat previous text))
              visible-range
              (codex-ide--command-output-render-range output-text)
              visible-output
              (substring output-text
                         (car visible-range)
                         (cdr visible-range))
              truncated (> (car visible-range) 0)
              display-text
              (or (codex-ide--format-command-output-text
                   visible-output
                   truncated)
                  (and truncated
                       (codex-ide--command-output-truncation-notice))
                  ""))
        (overlay-put overlay :output-fallback-text output-text)
        (let* ((line-count (codex-ide--command-output-line-count output-text))
               (visible-line-count
                (codex-ide--command-output-line-count visible-output)))
          (overlay-put overlay :line-count line-count)
          (overlay-put overlay :visible-line-count
                       (if truncated
                           (min visible-line-count line-count)
                         line-count))
          (overlay-put overlay :truncated truncated))
        (when (not (equal display-text
                          (overlay-get overlay :display-text)))
          (overlay-put overlay :display-text display-text)
          (overlay-put overlay
                       :body-properties
                       (codex-ide--current-agent-text-properties)))
        (codex-ide--set-command-output-header overlay)
        (codex-ide--set-command-output-body
         overlay
         (or (overlay-get overlay :display-text) ""))))))

(defun codex-ide--render-command-output-delta (session item-id delta)
  "Render streamed command output DELTA for ITEM-ID in SESSION."
  (codex-ide--append-command-output-text session item-id delta))

(defun codex-ide--complete-command-output-block (session item-id output)
  "Ensure command output for ITEM-ID is rendered and folded after completion."
  (let* ((state (codex-ide--item-state session item-id))
         (overlay (plist-get state :command-output-overlay)))
    (when (and (stringp output)
               (not (string-empty-p output))
               (not (and (overlayp overlay)
                         (buffer-live-p (overlay-buffer overlay)))))
      (codex-ide--append-command-output-text session item-id output)
      (setq state (codex-ide--item-state session item-id)
            overlay (plist-get state :command-output-overlay)))
    (when (and (overlayp overlay)
               (buffer-live-p (overlay-buffer overlay)))
      (overlay-put overlay :complete t)
      (overlay-put overlay :folded t)
      (overlay-put overlay 'invisible t)
      (codex-ide--set-command-output-header overlay)
      (codex-ide--set-command-output-body
       overlay
       (or (overlay-get overlay :display-text) "")))))

(defun codex-ide--command-output-overlay-at-point (&optional pos)
  "Return the command output overlay at POS, or nil."
  (let* ((pos (or pos (point)))
         (overlay (get-char-property pos codex-ide-command-output-overlay-property)))
    (cond
     ((overlayp overlay) overlay)
     ((and (> pos (point-min))
           (overlayp (get-char-property
                      (1- pos)
                      codex-ide-command-output-overlay-property)))
      (get-char-property (1- pos) codex-ide-command-output-overlay-property))
     (t nil))))

(defun codex-ide-toggle-command-output-at-point (&optional pos)
  "Toggle a command output block at POS.
Return non-nil when a command output block was found."
  (interactive)
  (when-let ((overlay (codex-ide--command-output-overlay-at-point pos)))
    (codex-ide--toggle-command-output-overlay overlay)))

(defun codex-ide--command-search-summary (command)
  "Return a semantic search summary for COMMAND, or nil."
  (when-let ((request (codex-ide--rg-search-request
                       (codex-ide--display-command-argv command))))
    (codex-ide--search-summary (car request) (cadr request))))

(defun codex-ide--command-summary (command)
  "Return the user-facing summary for shell COMMAND."
  (or (codex-ide--command-read-summary command)
      (codex-ide--command-search-summary command)
      "Ran command"))

(defun codex-ide--item-detail-line (text)
  "Format TEXT as an indented detail line."
  (format "  └ %s\n" text))

(defun codex-ide--append-shell-command-detail (buffer command)
  "Append COMMAND as an indented, shell-highlighted detail line to BUFFER."
  (when (and (stringp command)
             (not (string-empty-p command))
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (codex-ide--without-undo-recording
        (let ((inhibit-read-only t)
              (moving (= (point) (point-max)))
              start
              command-start
              command-end)
          (goto-char (point-max))
          (setq start (point))
          (insert "  $ ")
          (setq command-start (point))
          (insert command)
          (setq command-end (point))
          (insert "\n")
          (add-text-properties
           start
           (point)
           (append (list 'face 'codex-ide-item-detail-face)
                   (codex-ide--current-agent-text-properties)))
          (let ((inhibit-message t)
                (message-log-max nil))
            (codex-ide--fontify-code-block-region command-start command-end "sh"))
          (codex-ide--freeze-region start (point))
          (when moving
            (goto-char (point-max))))))))

(defun codex-ide--item-detail-block (text)
  "Format TEXT as a block of indented detail lines."
  (mapconcat (lambda (line)
               (codex-ide--item-detail-line
                (if (string-empty-p line) "" line)))
             (split-string text "\n")
             ""))

(defun codex-ide--file-change-diff-face (line)
  "Return the face to use for file-change diff LINE."
  (cond
   ((string-prefix-p "@@" line) 'codex-ide-file-diff-hunk-face)
   ((or (string-prefix-p "diff --git" line)
        (string-prefix-p "--- " line)
        (string-prefix-p "+++ " line)
        (string-prefix-p "index " line))
    'codex-ide-file-diff-header-face)
   ((string-prefix-p "+" line) 'codex-ide-file-diff-added-face)
   ((string-prefix-p "-" line) 'codex-ide-file-diff-removed-face)
   (t 'codex-ide-file-diff-context-face)))

(defun codex-ide--render-file-change-diff-text (buffer text)
  "Render file-change diff TEXT into BUFFER."
  (when (and (stringp text)
             (not (string-empty-p text)))
    (let ((trimmed (string-trim-right text)))
      (unless (string-empty-p trimmed)
        (codex-ide--append-agent-text
         buffer
         "diff:\n"
         'codex-ide-item-detail-face)
        (dolist (line (split-string trimmed "\n"))
          (codex-ide--append-agent-text
           buffer
           (concat line "\n")
           (codex-ide--file-change-diff-face line)))))))

(defun codex-ide--file-change-diff-text (item)
  "Extract a human-readable diff string from file-change ITEM."
  (let ((item-diff
         (or (alist-get 'diff item)
             (alist-get 'patch item)
             (alist-get 'output item)
             (alist-get 'text item))))
    (cond
     ((and (stringp item-diff)
           (not (string-empty-p item-diff)))
      item-diff)
     (t
      (string-join
       (delq nil
             (mapcar
              (lambda (change)
                (let ((path (alist-get 'path change))
                      (diff (or (alist-get 'diff change)
                                (alist-get 'patch change)
                                (alist-get 'output change)
                                (alist-get 'text change))))
                  (when (and (stringp diff)
                             (not (string-empty-p diff)))
                    (if (and path
                             (not (string-match-p
                                   (regexp-quote (format "+++ %s" path))
                                   diff)))
                        (format "diff -- %s\n%s" path diff)
                      diff))))
              (or (alist-get 'changes item) '())))
       "\n")))))

(defun codex-ide--summarize-item-start (item)
  "Build a one-line summary for ITEM start notifications."
  (let ((item-type (alist-get 'type item)))
    (pcase item-type
      ("commandExecution"
       (codex-ide--command-summary (alist-get 'command item)))
      ("webSearch"
       (let* ((action (alist-get 'action item))
              (action-type (alist-get 'type action))
              (queries (delq nil
                             (append (and (alist-get 'query action)
                                          (list (alist-get 'query action)))
                                     (alist-get 'queries action)
                                     (and (alist-get 'query item)
                                          (list (alist-get 'query item))))))
              (query-text (string-join queries " | ")))
         (pcase action-type
           ("openPage"
            (format "Opened page %s" (or (alist-get 'url action) "unknown page")))
           ("findInPage"
            (format "Searched in page %s for %s"
                    (or (alist-get 'url action) "unknown page")
                    (or (alist-get 'pattern action) "")))
           (_
            (if (string-empty-p query-text)
                "Searched the web"
              (format "Searched the web for %s" query-text))))))
      ("mcpToolCall"
       (format "Called %s/%s"
               (or (alist-get 'server item) "mcp")
               (or (alist-get 'tool item) "tool")))
      ("dynamicToolCall"
       (format "Called tool %s" (or (alist-get 'tool item) "tool")))
      ("collabToolCall"
       (format "Delegated with %s" (or (alist-get 'tool item) "collab tool")))
      ("fileChange"
       (let ((count (length (or (alist-get 'changes item) '()))))
         (format "Prepared %d file change%s" count (if (= count 1) "" "s"))))
      ("contextCompaction"
       "Compacted conversation context")
      ("imageView"
       (format "Viewed image %s" (or (alist-get 'path item) "")))
      ("enteredReviewMode"
       "Entered review mode")
      ("exitedReviewMode"
       "Exited review mode")
      (_ nil))))

(defun codex-ide--render-item-start-details (session item)
  "Render detail lines for ITEM in SESSION."
  (let ((buffer (codex-ide-session-buffer session))
        (item-type (alist-get 'type item)))
    (pcase item-type
      ("commandExecution"
       (unless (or (codex-ide--command-read-summary (alist-get 'command item))
                   (codex-ide--command-search-summary (alist-get 'command item)))
         (codex-ide--append-shell-command-detail
          buffer
          (codex-ide--display-command-string (alist-get 'command item))))
       (when-let ((cwd (alist-get 'cwd item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "cwd: %s" (abbreviate-file-name cwd)))
          'codex-ide-item-detail-face)))
      ("webSearch"
       (let* ((action (alist-get 'action item))
              (action-type (alist-get 'type action))
              (queries (delq nil
                             (append (alist-get 'queries action)
                                     (and (alist-get 'query action)
                                          (list (alist-get 'query action)))
                                     (and (alist-get 'query item)
                                          (list (alist-get 'query item)))))))
         (cond
          ((and (equal action-type "findInPage")
                (alist-get 'pattern action))
            (codex-ide--append-agent-text
             buffer
             (codex-ide--item-detail-line
              (format "pattern: %s" (alist-get 'pattern action)))
             'codex-ide-item-detail-face))
          ((> (length queries) 1)
           (dolist (query queries)
             (codex-ide--append-agent-text
              buffer
              (codex-ide--item-detail-line query)
              'codex-ide-item-detail-face))))))
      ("mcpToolCall"
       (when-let ((arguments (alist-get 'arguments item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "args: %s" (json-encode arguments)))
          'codex-ide-item-detail-face)))
      ("dynamicToolCall"
       (when-let ((arguments (alist-get 'arguments item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "args: %s" (json-encode arguments)))
          'codex-ide-item-detail-face)))
      ("fileChange"
       (dolist (change (or (alist-get 'changes item) '()))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line
           (format "%s %s"
                   (or (alist-get 'kind change) "change")
                   (or (alist-get 'path change) "unknown")))
          'codex-ide-item-detail-face)))
      ("imageView"
       (when-let ((path (alist-get 'path item)))
         (codex-ide--append-agent-text
          buffer
          (codex-ide--item-detail-line path)
          'codex-ide-item-detail-face))))))

(defun codex-ide--render-item-start (&optional session item)
  "Render a newly started ITEM for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((buffer (codex-ide-session-buffer session))
         (item-id (alist-get 'id item))
         (item-type (alist-get 'type item))
         (summary (codex-ide--summarize-item-start item))
         (existing-state (copy-sequence
                          (or (codex-ide--item-state session item-id) '()))))
    (let ((codex-ide--current-agent-item-type item-type))
      (when summary
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--clear-pending-output-indicator session)
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* %s\n" summary)
         'codex-ide-item-summary-face)
        (codex-ide--render-item-start-details session item)
        (let ((state existing-state))
          (setq state (plist-put state :type item-type))
          (setq state (plist-put state :item item))
          (setq state (plist-put state :summary summary))
          (setq state
                (plist-put
                 state
                 :search-request
                 (and (equal item-type "commandExecution")
                      (codex-ide--rg-search-request
                       (codex-ide--display-command-argv
                        (alist-get 'command item))))))
          (setq state (plist-put state :details-rendered t))
          (setq state (plist-put state :saw-output nil))
          (codex-ide--put-item-state session item-id state))
        (when-let ((pending-output
                    (plist-get (codex-ide--item-state session item-id)
                               :pending-output-text)))
          (codex-ide--render-command-output-delta session item-id pending-output)
          (codex-ide--put-item-state
           session
           item-id
           (plist-put (codex-ide--item-state session item-id)
                      :pending-output-text nil))))
      (when (and (not summary)
                 (equal item-type "reasoning"))
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session nil t))
        (codex-ide--replace-pending-output-indicator
         session
         "Reasoning...\n")))))

(defun codex-ide--render-plan-delta (&optional session params)
  "Render a plan delta PARAMS for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((delta (or (alist-get 'delta params) ""))
        (buffer (codex-ide-session-buffer session)))
    (let ((codex-ide--current-agent-item-type "plan"))
      (unless (string-empty-p delta)
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--clear-pending-output-indicator session)
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* Plan: %s\n" delta)
         'font-lock-doc-face)))))

(defun codex-ide--render-reasoning-delta (&optional session params)
  "Render a reasoning summary delta PARAMS for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((delta (or (alist-get 'delta params)
                   (alist-get 'text params)
                   ""))
        (buffer (codex-ide-session-buffer session)))
    (let ((codex-ide--current-agent-item-type "reasoning"))
      (unless (string-empty-p delta)
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--clear-pending-output-indicator session)
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* Reasoning: %s\n" delta)
         'shadow)))))

(defun codex-ide--render-item-completion (&optional session item)
  "Render any completion-only details for ITEM in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let* ((item-id (alist-get 'id item))
         (buffer (codex-ide-session-buffer session))
         (state (codex-ide--item-state session item-id))
         (item-type (alist-get 'type item))
         (status (alist-get 'status item)))
    (let ((codex-ide--current-agent-item-type item-type))
      (pcase item-type
        ("agentMessage"
         (codex-ide--render-current-agent-message-markdown session item-id t))
        ("commandExecution"
         (let* ((search-request (plist-get state :search-request))
                (output-text (or (plist-get state :output-text)
                                 (alist-get 'aggregatedOutput item)))
                (exit-code (alist-get 'exitCode item)))
           (codex-ide--complete-command-output-block session item-id output-text)
           (cond
            (search-request
             (when-let ((hit-count (or (codex-ide--count-search-output-hits
                                        output-text)
                                       (and (equal exit-code 1) 0))))
               (codex-ide--append-agent-text
                buffer
                (codex-ide--item-detail-line
                 (codex-ide--format-hit-count hit-count))
                'codex-ide-item-detail-face))
             (when (and (equal status "failed")
                        (not (equal exit-code 1)))
               (codex-ide--append-agent-text
                buffer
                (codex-ide--item-detail-line
                 (format "failed%s"
                         (if exit-code
                             (format " with exit code %s" exit-code)
                           "")))
                'error)))
            ((equal status "failed")
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line
             (format "failed%s"
                     (if exit-code
                         (format " with exit code %s" exit-code)
                       "")))
            'error))
            ((equal status "declined")
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line "declined")
            'warning)))))
        ("mcpToolCall"
         (when-let ((error-info (alist-get 'error item)))
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line
             (format "error: %s"
                     (or (alist-get 'message error-info) error-info)))
            'error)))
        ("dynamicToolCall"
         (when (eq (alist-get 'success item) :json-false)
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line "tool call failed")
            'error)))
        ("fileChange"
         (let ((diff-text (codex-ide--file-change-diff-text item))
               (streamed-diff (plist-get state :diff-text))
               (approval-rendered-items
                (codex-ide--session-metadata-get
                 session
                 :approval-file-change-diff-rendered-items)))
           (unless (or (plist-get state :approval-diff-rendered)
                       (and approval-rendered-items
                            (gethash item-id approval-rendered-items)))
             (codex-ide--render-file-change-diff-text
              buffer
              (if (and (stringp diff-text)
                       (not (string-empty-p diff-text)))
                  diff-text
                streamed-diff)))))
        ("exitedReviewMode"
         (when-let ((review (alist-get 'review item)))
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-block review)
            'codex-ide-item-detail-face)))))
    (codex-ide--clear-item-state session item-id)))

(defun codex-ide--ensure-agent-message-prefix (&optional session item-id)
  "Ensure the assistant message prefix has been inserted for ITEM-ID in SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (codex-ide-session-buffer session)))
    (unless (and (equal item-id (codex-ide-session-current-message-item-id session))
                 (codex-ide-session-current-message-prefix-inserted session))
      (unless (codex-ide-session-output-prefix-inserted session)
        (codex-ide--begin-turn-display session))
      (codex-ide--clear-pending-output-indicator session)
      (codex-ide--ensure-output-spacing buffer)
      (codex-ide--append-output-separator buffer)
      (codex-ide--append-agent-text buffer "\n")
      (setf (codex-ide-session-current-message-start-marker session)
            (with-current-buffer buffer
              (copy-marker (point-max))))
      (codex-ide--session-metadata-put
       session
       :agent-message-stream-render-start-marker
       (copy-marker (codex-ide-session-current-message-start-marker session)))
      (setf (codex-ide-session-current-message-item-id session) item-id
            (codex-ide-session-current-message-prefix-inserted session) t))))

(defun codex-ide--render-current-agent-message-markdown
    (&optional session item-id allow-trailing-tables)
  "Render the current assistant message for SESSION.
When ITEM-ID is non-nil, render only when it matches SESSION's current message."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (or (null item-id)
                   (equal item-id
                          (codex-ide-session-current-message-item-id session))))
      (when-let ((message-start
                  (codex-ide-session-current-message-start-marker session)))
        (when (eq (marker-buffer message-start) buffer)
          (with-current-buffer buffer
            (let* ((stream-marker
                    (codex-ide--session-metadata-get
                     session
                     :agent-message-stream-render-start-marker))
                   (render-start
                    (if (and (markerp stream-marker)
                             (eq (marker-buffer stream-marker) buffer))
                        (marker-position stream-marker)
                      (marker-position message-start))))
              (when (< render-start (point-max))
                (codex-ide--maybe-render-markdown-region
                 render-start
                 (point-max)
                 allow-trailing-tables))))
          (codex-ide--session-metadata-put
           session
           :agent-message-stream-render-start-marker
           nil))))))

(defun codex-ide--render-session-error (session values &optional prefix face)
  "Render session error VALUES for SESSION with PREFIX using FACE."
  (let* ((detail (apply #'codex-ide--extract-error-text values))
         (classification (apply #'codex-ide--classify-session-error values))
         (summary (codex-ide--format-session-error-summary classification prefix))
         (guidance (plist-get classification :guidance))
         (buffer (codex-ide-session-buffer session)))
    (codex-ide-log-message session "%s" summary)
    (unless (string-empty-p detail)
      (codex-ide-log-message session "  %s" detail))
    (when guidance
      (codex-ide-log-message session "%s" guidance))
    (setf (codex-ide-session-status session) "error")
    (codex-ide--update-header-line session)
    (codex-ide--clear-pending-output-indicator session)
    (codex-ide--append-to-buffer buffer (format "\n%s\n" summary) (or face 'error))
    (unless (string-empty-p detail)
      (let ((codex-ide--current-agent-item-type "error"))
        (codex-ide--append-agent-text
         buffer
         (codex-ide--item-detail-line detail)
         'codex-ide-item-detail-face)))
    (when guidance
      (codex-ide--append-to-buffer buffer (format "%s\n" guidance) (or face 'error)))
    classification))

(defun codex-ide--reopen-input-after-submit-error (&optional session prompt err)
  "Show ERR for SESSION and reopen a prompt seeded with PROMPT."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (setf (codex-ide-session-current-turn-id session) nil
        (codex-ide-session-current-message-item-id session) nil
        (codex-ide-session-current-message-prefix-inserted session) nil
        (codex-ide-session-current-message-start-marker session) nil
        (codex-ide-session-output-prefix-inserted session) nil
        (codex-ide-session-item-states session) (make-hash-table :test 'equal)
        (codex-ide-session-status session) "idle")
  (codex-ide--clear-pending-output-indicator session)
  (codex-ide--update-header-line session)
  (codex-ide--append-to-buffer
   (codex-ide-session-buffer session)
   (format "\n[Submit failed] %s\n\n" (error-message-string err))
   'error)
  (codex-ide--insert-input-prompt session prompt))

(defun codex-ide--finish-turn (&optional session closing-note)
  "Reset SESSION after a turn and reopen the prompt.
When CLOSING-NOTE is non-nil, append it before restoring the prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (setf (codex-ide-session-current-turn-id session) nil
          (codex-ide-session-current-message-item-id session) nil
          (codex-ide-session-current-message-prefix-inserted session) nil
          (codex-ide-session-current-message-start-marker session) nil
          (codex-ide-session-output-prefix-inserted session) nil
          (codex-ide-session-item-states session) (make-hash-table :test 'equal)
          (codex-ide-session-interrupt-requested session) nil
          (codex-ide-session-status session) "idle")
    (codex-ide--clear-pending-output-indicator session)
    (codex-ide--update-header-line session)
    (when closing-note
      (codex-ide--append-to-buffer buffer (format "\n%s\n" closing-note) 'warning))
    (codex-ide--append-to-buffer buffer "\n\n")
    (codex-ide--insert-input-prompt session)))

(defun codex-ide--thread-read-display-user-text (text)
  "Normalize stored user TEXT for transcript display."
  (when (stringp text)
    (let ((display-text (string-trim (codex-ide--strip-emacs-context-prefix text))))
      (unless (string-empty-p display-text)
        display-text))))

(defun codex-ide--thread-read-items (turn)
  "Return ordered transcript items for TURN."
  (or (alist-get 'items turn)
      (alist-get 'messages turn)
      []))

(defun codex-ide--append-restored-user-message (session text)
  "Append restored user TEXT to SESSION like a submitted prompt."
  (let ((buffer (codex-ide-session-buffer session))
        (display-text (codex-ide--thread-read-display-user-text text)))
    (when (and (buffer-live-p buffer)
               (stringp display-text)
               (not (string-empty-p display-text)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              prompt-start)
          (goto-char (point-max))
          (cond
           ((= (point) (point-min)))
           ((and (eq (char-before (point)) ?\n)
                 (save-excursion
                   (forward-char -1)
                   (or (bobp)
                       (eq (char-before (point)) ?\n)))))
           ((eq (char-before (point)) ?\n)
            (insert "\n"))
           (t
            (insert "\n\n")))
          (setq prompt-start (point))
          (insert (propertize "> " 'face 'codex-ide-user-prompt-face))
          (insert display-text)
          (codex-ide--style-user-prompt-region prompt-start (point))
          (codex-ide--freeze-region prompt-start (point))
          (insert "\n\n")
          (codex-ide--freeze-region prompt-start (point))))
      t)))

(defun codex-ide--append-restored-agent-message (session item)
  "Append restored agent ITEM to SESSION like live agent output."
  (let* ((buffer (codex-ide-session-buffer session))
         (item-id (or (alist-get 'id item) "restored-agent-message"))
         (text (codex-ide--thread-read--message-text item)))
    (when (and (buffer-live-p buffer)
               (stringp text)
               (not (string-empty-p (string-trim text))))
      (let ((codex-ide--current-agent-item-type "agentMessage"))
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-output-separator buffer)
        (codex-ide--append-agent-text buffer "\n")
        (setf (codex-ide-session-current-message-start-marker session)
              (with-current-buffer buffer
                (copy-marker (point-max))))
        (setf (codex-ide-session-current-message-item-id session) item-id
              (codex-ide-session-current-message-prefix-inserted session) t)
        (codex-ide--append-agent-text buffer text)
        (when-let ((start (codex-ide-session-current-message-start-marker session)))
          (with-current-buffer buffer
            (codex-ide--maybe-render-markdown-region start (point-max) t))))
      t)))

(defun codex-ide--replay-thread-read-turn (session turn)
  "Replay stored TURN into SESSION.
Return non-nil when any transcript content was restored."
  (let ((items (append (codex-ide--thread-read-items turn) nil))
        (restored nil))
    (dolist (item items restored)
      (pcase (codex-ide--thread-read--item-kind item)
        ('user
         (setq restored
               (or (codex-ide--append-restored-user-message
                    session
                    (codex-ide--thread-read--message-text item))
                   restored))
         (setf (codex-ide-session-output-prefix-inserted session) t
               (codex-ide-session-current-message-item-id session) nil
               (codex-ide-session-current-message-prefix-inserted session) nil
               (codex-ide-session-current-message-start-marker session) nil))
        ('assistant
         (setq restored
               (or (codex-ide--append-restored-agent-message session item)
                   restored)))
        (_ nil)))))

(defun codex-ide--restore-thread-read-transcript (&optional session thread-read)
  "Replay a stored transcript from THREAD-READ into SESSION.
Signal an error when THREAD-READ lacks replayable transcript items."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((limit (max 0 codex-ide-resume-summary-turn-limit))
         (turns (append (codex-ide--thread-read-turns thread-read) nil))
         (recent-turns (cond
                        ((<= limit 0) nil)
                        ((> (length turns) limit) (last turns limit))
                        (t turns)))
         (restored nil))
    (unless recent-turns
      (error "Stored thread has no replayable turns"))
    (dolist (turn recent-turns restored)
      (setq restored
            (or (codex-ide--replay-thread-read-turn session turn)
                restored)))
    (unless restored
      (error
       (concat
        "Stored thread transcript could not be replayed. "
        "Expected replayable userMessage/agentMessage turn items.")))
    (when restored
      (codex-ide--append-to-buffer (codex-ide-session-buffer session) "\n")
      (codex-ide--append-restored-transcript-separator
       (codex-ide-session-buffer session)))
    (setf (codex-ide-session-current-turn-id session) nil
          (codex-ide-session-current-message-item-id session) nil
          (codex-ide-session-current-message-prefix-inserted session) nil
          (codex-ide-session-current-message-start-marker session) nil
          (codex-ide-session-output-prefix-inserted session) nil
          (codex-ide-session-item-states session) (make-hash-table :test 'equal))
    restored))

(provide 'codex-ide-renderer)

;;; codex-ide-renderer.el ends here
