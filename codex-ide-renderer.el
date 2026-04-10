;;; codex-ide-renderer.el --- Transcript rendering for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; Buffer rendering helpers for codex-ide transcript and status UI.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'codex-ide-core)

(declare-function codex-ide--extract-error-text "codex-ide" (&rest values))
(declare-function codex-ide--classify-session-error "codex-ide" (&rest values))
(declare-function codex-ide--format-session-error-summary "codex-ide" (classification &optional prefix))
(declare-function codex-ide--sanitize-ansi-text "codex-ide" (text))
(declare-function codex-ide--session-for-current-project "codex-ide" ())
(declare-function codex-ide--strip-emacs-context-prefix "codex-ide" (text))
(declare-function codex-ide--sync-prompt-minor-mode "codex-ide" (&optional session))
(declare-function codex-ide--thread-read--item-kind "codex-ide" (item))
(declare-function codex-ide--thread-read--message-text "codex-ide" (item))
(declare-function codex-ide--thread-read-turns "codex-ide" (thread-read))
(declare-function codex-ide-log-message "codex-ide" (session format-string &rest args))

(defvar codex-ide-log-max-lines)
(defvar codex-ide-resume-summary-turn-limit)

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
          (goto-char (point-max)))))))

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
        (codex-ide--freeze-region start (point))))))

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
               (codex-ide--session-metadata-get session :rate-limits))))
        (setq header-line-format
              (propertize
               (string-join
                (delq nil
                      (list
                       (format "focus: %s" focus)
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
  (let ((pos start))
    ;; Only remove properties from regions previously marked as markdown.
    ;; Clearing `face' across the whole agent-message span can wipe faces from
    ;; later non-markdown transcript entries like "* Ran ..." summaries.
    (while (< pos end)
      (let ((next (next-single-property-change pos 'codex-ide-markdown nil end)))
        (when (get-text-property pos 'codex-ide-markdown)
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
             codex-ide-markdown nil)))
        (setq pos next)))))

(defun codex-ide--normalize-markdown-link-label (label)
  "Return LABEL with markdown code delimiters stripped when present."
  (save-match-data
    (if (string-match "\\``\\([^`\n]+\\)`\\'" label)
        (match-string 1 label)
      label)))

(defun codex-ide--markdown-language-mode (language)
  "Return an Emacs major mode function for fenced code block LANGUAGE."
  (let* ((lang (downcase (string-trim (or language ""))))
         (mode
          (alist-get
           lang
           '(("bash" . sh-mode)
             ("c" . c-mode)
             ("c++" . c++-mode)
             ("cpp" . c++-mode)
             ("elisp" . emacs-lisp-mode)
             ("emacs-lisp" . emacs-lisp-mode)
             ("go" . go-mode)
             ("java" . java-mode)
             ("javascript" . js-mode)
             ("js" . js-mode)
             ("json" . json-mode)
             ("python" . python-mode)
             ("py" . python-mode)
             ("ruby" . ruby-mode)
             ("rust" . rust-mode)
             ("shell" . sh-mode)
             ("sh" . sh-mode)
             ("typescript" . typescript-mode)
             ("ts" . typescript-mode)
             ("tsx" . tsx-ts-mode)
             ("yaml" . yaml-mode)
             ("yml" . yaml-mode))
           nil nil #'string=)))
    (cond
     ((and mode (fboundp mode))
      mode)
     ((string-empty-p lang)
      nil)
     (t
      (let ((candidate (intern-soft (format "%s-mode" lang))))
        (when (fboundp candidate)
          candidate))))))

(defun codex-ide--fontify-code-block-region (start end language)
  "Apply syntax highlighting to region START END using LANGUAGE."
  (when-let ((mode (codex-ide--markdown-language-mode language)))
    (let ((source-buffer (current-buffer))
          (code (buffer-substring-no-properties start end)))
      (with-temp-buffer
        (insert code)
        (funcall mode)
        (font-lock-ensure (point-min) (point-max))
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
                  (add-text-properties
                   target-start
                   target-end
                   props)))
              (setq pos next))))))))

(defun codex-ide--render-fenced-code-blocks (start end)
  "Render fenced code blocks between START and END."
  (goto-char start)
  (while (re-search-forward "^```\\([^`\n]*\\)[ \t]*$" end t)
    (let* ((fence-start (match-beginning 0))
           (language (string-trim (or (match-string-no-properties 1) "")))
           (code-start (min (1+ (match-end 0)) end)))
      (when (and (< code-start end)
                 (re-search-forward "^```[ \t]*$" end t))
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
           '(codex-ide-markdown t))
          (codex-ide--fontify-code-block-region code-start closing-start language)
          (add-text-properties
           closing-start closing-end
           '(display ""
             codex-ide-markdown t))
          (goto-char closing-end))))))

(defun codex-ide--render-markdown-region (start end)
  "Apply lightweight markdown rendering between START and END."
  (save-excursion
    (let ((inhibit-read-only t))
      (codex-ide--clear-markdown-properties start end)
      (goto-char start)
      (codex-ide--render-fenced-code-blocks start end)
      (goto-char start)
      (while (re-search-forward "\\(\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))\\)" end t)
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
      (while (re-search-forward "`\\([^`\n]+\\)`" end t)
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
               codex-ide-markdown t))))))))

(defun codex-ide--insert-input-prompt (&optional session initial-text)
  "Insert a writable `>' prompt for SESSION.
Optionally seed it with INITIAL-TEXT."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
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
          (codex-ide--sync-prompt-minor-mode session))))))

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

(defun codex-ide--begin-turn-display (&optional session context-summary)
  "Freeze the current prompt and show immediate pending output for SESSION.
When CONTEXT-SUMMARY is non-nil, insert it beneath the submitted prompt."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
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
          (setf (codex-ide-session-output-prefix-inserted session) t
                (codex-ide-session-status session) "running")
          (codex-ide--update-header-line session))))))

(defun codex-ide--shell-command-string (command)
  "Render COMMAND as a shell-like string."
  (cond
   ((stringp command) command)
   ((listp command)
    (mapconcat (lambda (arg)
                 (if (stringp arg)
                     (shell-quote-argument arg)
                   (format "%s" arg)))
               command
               " "))
   (t (format "%s" command))))

(defun codex-ide--item-detail-line (text)
  "Format TEXT as an indented detail line."
  (format "  └ %s\n" text))

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
       (format "Ran %s"
               (codex-ide--shell-command-string (alist-get 'command item))))
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
         (summary (codex-ide--summarize-item-start item)))
    (let ((codex-ide--current-agent-item-type item-type))
      (when summary
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
        (codex-ide--ensure-output-spacing buffer)
        (codex-ide--append-agent-text
         buffer
         (format "* %s\n" summary)
         'codex-ide-item-summary-face)
        (codex-ide--render-item-start-details session item)
        (codex-ide--put-item-state
         session
         item-id
         (list :type item-type
               :summary summary
               :details-rendered t
               :saw-output nil))))))

(defun codex-ide--render-plan-delta (&optional session params)
  "Render a plan delta PARAMS for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (let ((delta (or (alist-get 'delta params) ""))
        (buffer (codex-ide-session-buffer session)))
    (let ((codex-ide--current-agent-item-type "plan"))
      (unless (string-empty-p delta)
        (unless (codex-ide-session-output-prefix-inserted session)
          (codex-ide--begin-turn-display session))
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
        ("commandExecution"
         (cond
          ((equal status "failed")
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line
             (format "failed%s"
                     (if-let ((exit-code (alist-get 'exitCode item)))
                         (format " with exit code %s" exit-code)
                       "")))
            'error))
          ((equal status "declined")
           (codex-ide--append-agent-text
            buffer
            (codex-ide--item-detail-line "declined")
            'warning))))
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
               (streamed-diff (plist-get state :diff-text)))
           (codex-ide--render-file-change-diff-text
            buffer
            (if (and (stringp diff-text)
                     (not (string-empty-p diff-text)))
                diff-text
              streamed-diff))))
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
      (codex-ide--ensure-output-spacing buffer)
      (codex-ide--append-output-separator buffer)
      (codex-ide--append-agent-text buffer "\n")
      (setf (codex-ide-session-current-message-start-marker session)
            (copy-marker (with-current-buffer buffer (point-max))))
      (setf (codex-ide-session-current-message-item-id session) item-id
            (codex-ide-session-current-message-prefix-inserted session) t))))

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
              (copy-marker (with-current-buffer buffer (point-max))))
        (setf (codex-ide-session-current-message-item-id session) item-id
              (codex-ide-session-current-message-prefix-inserted session) t)
        (codex-ide--append-agent-text buffer text)
        (when-let ((start (codex-ide-session-current-message-start-marker session)))
          (with-current-buffer buffer
            (codex-ide--render-markdown-region start (point-max)))))
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
