;;; codex-ide-renderer.el --- View rendering for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; `codex-ide-renderer' owns the view layer for transcript presentation.
;; It is responsible for operations that act on explicit buffer state and
;; explicit inputs such as regions, markers, text properties, overlays, render
;; options, and insertion callbacks supplied by higher-level modules.  This
;; includes markdown rendering, prompt/transcript styling, separator formatting,
;; file-link activation, transcript block insertion helpers, command-output
;; presentation, approval/elicitation widgets, and lightweight status display
;; helpers.
;;
;; This file should stay usable as a view utility without requiring the rest of
;; codex-ide to be loaded.  In practice that means it should not depend on
;; session objects, app-server protocol payloads, project/session lookup
;; helpers, or higher-level controller modules.  It may depend on stock Emacs
;; libraries and buffer-local state, but callers should supply any session- or
;; application-specific meaning explicitly rather than having the renderer infer
;; it.
;;
;; Keep business logic out of this file.  Item interpretation, transcript
;; lifecycle, prompt management, insertion-position policy, error
;; classification, and replay decisions for stored thread data belong in
;; controller-oriented modules such as `codex-ide-transcript.el'.  When in
;; doubt, code belongs here only if it can be described as "take explicit
;; inputs plus current buffer state, then insert, update, or restyle the view".

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)

(defmacro codex-ide-renderer--without-undo-recording (&rest body)
  "Run BODY without recording undo entries in the current buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer-undo-list t))
     ,@body))

(defvar codex-ide--markdown-display-mode-function-cache 'unset)

(defvar codex-ide-renderer-link-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    map)
  "Keymap used for markdown file links.")

(define-key codex-ide-renderer-link-keymap
            (kbd "M-<return>")
            #'codex-ide-renderer-open-file-link-other-window)
(define-key codex-ide-renderer-link-keymap
            (kbd "C-M-j")
            #'codex-ide-renderer-open-file-link-other-window)

(defcustom codex-ide-renderer-render-markdown-during-streaming t
  "Whether to apply incremental markdown rendering while text streams."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-renderer-markdown-render-max-chars 30000
  "Maximum markdown span size to render with rich markdown."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum characters"))
  :group 'codex-ide)

(defcustom codex-ide-renderer-command-output-fold-on-start t
  "When non-nil, command output blocks start folded while output streams."
  :type 'boolean
  :group 'codex-ide)

(defcustom codex-ide-renderer-command-output-max-rendered-lines 10
  "Maximum command output lines to insert into the transcript buffer."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum lines"))
  :group 'codex-ide)

(defcustom codex-ide-renderer-command-output-max-rendered-chars 60000
  "Maximum command output characters to insert into the transcript buffer."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum characters"))
  :group 'codex-ide)

(defface codex-ide-user-prompt-face
  '((t :inherit default))
  "Face used to distinguish submitted and active user prompts."
  :group 'codex-ide)

(defface codex-ide-output-separator-face
  '((t))
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

(defconst codex-ide-prompt-start-property 'codex-ide-prompt-start
  "Text property used to mark the first character of a user prompt.")

(defface codex-ide-command-output-face
  '((t :inherit fixed-pitch :extend t))
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

(defconst codex-ide-item-result-overlay-property
  'codex-ide-item-result-overlay
  "Text property storing an expandable item-result overlay.")

(defconst codex-ide-command-output-overlay-property
  codex-ide-item-result-overlay-property
  "Compatibility alias for `codex-ide-item-result-overlay-property'.")

(defconst codex-ide-renderer--user-prompt-background-mix-light 0.07
  "Foreground mix fraction for prompt backgrounds on light themes.")

(defconst codex-ide-renderer--user-prompt-background-mix-dark 0.17
  "Foreground mix fraction for prompt backgrounds on dark themes.")

(defconst codex-ide-renderer--output-separator-foreground-mix-light 0.22
  "Foreground mix fraction for separators on light themes.")

(defconst codex-ide-renderer--output-separator-foreground-mix-dark 0.35
  "Foreground mix fraction for separators on dark themes.")

(defconst codex-ide-renderer--command-output-background-mix-light 0.09
  "Foreground mix fraction for command output backgrounds on light themes.")

(defconst codex-ide-renderer--command-output-background-mix-dark 0.12
  "Foreground mix fraction for command output backgrounds on dark themes.")

(defun codex-ide-renderer--color-defined-p (color)
  "Return non-nil when COLOR names a usable Emacs color."
  (and (stringp color)
       (ignore-errors
         (color-values color))))

(defun codex-ide-renderer--default-background-color ()
  "Return the current default background color, or a safe fallback."
  (let ((background (face-background 'default nil t)))
    (if (codex-ide-renderer--color-defined-p background)
        background
      "#000000")))

(defun codex-ide-renderer--default-foreground-color ()
  "Return the current default foreground color, or a safe fallback."
  (let ((foreground (face-foreground 'default nil t)))
    (if (codex-ide-renderer--color-defined-p foreground)
        foreground
      "#ffffff")))

(defun codex-ide-renderer--theme-dark-p ()
  "Return non-nil when the current default background is dark."
  (pcase-let ((`(,red ,green ,blue)
               (color-values (codex-ide-renderer--default-background-color))))
    (< (/ (+ red green blue) 3.0) (/ 65535.0 2))))

(defun codex-ide-renderer--blend-default-colors (amount)
  "Blend the default background toward the default foreground by AMOUNT."
  (let ((background (codex-ide-renderer--default-background-color))
        (foreground (codex-ide-renderer--default-foreground-color)))
    (pcase-let ((`(,fg-red ,fg-green ,fg-blue) (color-values foreground))
                (`(,bg-red ,bg-green ,bg-blue) (color-values background)))
      (format "#%02x%02x%02x"
              (round (/ (+ (* amount fg-red) (* (- 1 amount) bg-red)) 257.0))
              (round (/ (+ (* amount fg-green) (* (- 1 amount) bg-green)) 257.0))
              (round (/ (+ (* amount fg-blue) (* (- 1 amount) bg-blue)) 257.0))))))

(defun codex-ide-renderer--user-prompt-face-spec ()
  "Return the current face spec for `codex-ide-user-prompt-face'."
  `((t :inherit default
       :background
       ,(codex-ide-renderer--blend-default-colors
         (if (codex-ide-renderer--theme-dark-p)
             codex-ide-renderer--user-prompt-background-mix-dark
           codex-ide-renderer--user-prompt-background-mix-light)))))

(defun codex-ide-renderer--output-separator-face-spec ()
  "Return the current face spec for `codex-ide-output-separator-face'."
  `((t :foreground
       ,(codex-ide-renderer--blend-default-colors
         (if (codex-ide-renderer--theme-dark-p)
             codex-ide-renderer--output-separator-foreground-mix-dark
           codex-ide-renderer--output-separator-foreground-mix-light)))))

(defun codex-ide-renderer--command-output-face-spec ()
  "Return the current face spec for `codex-ide-command-output-face'."
  `((t :inherit fixed-pitch
       :background
       ,(codex-ide-renderer--blend-default-colors
         (if (codex-ide-renderer--theme-dark-p)
             codex-ide-renderer--command-output-background-mix-dark
           codex-ide-renderer--command-output-background-mix-light))
       :extend t)))

(defun codex-ide-renderer-refresh-theme-faces ()
  "Reapply theme-sensitive renderer face specs.

This clears any stale concrete color attributes that may otherwise survive
theme switches or file reloads in a live Emacs session."
  (face-spec-set 'codex-ide-user-prompt-face
                 (codex-ide-renderer--user-prompt-face-spec))
  (face-spec-set 'codex-ide-output-separator-face
                 (codex-ide-renderer--output-separator-face-spec))
  (face-spec-set 'codex-ide-command-output-face
                 (codex-ide-renderer--command-output-face-spec)))

(codex-ide-renderer-refresh-theme-faces)

(defun codex-ide-renderer-status-label (status)
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

(defun codex-ide-renderer-status-face (status)
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

(defun codex-ide-renderer-mode-line-status (&optional session)
  "Return the current modeline status segment for SESSION."
  (when session
    (let* ((status (if (fboundp 'codex-ide-session-status)
                       (or (codex-ide-session-status session) "disconnected")
                     "disconnected"))
           (label (codex-ide-renderer-status-label status))
           (face (codex-ide-renderer-status-face status)))
      (concat
       " "
       (propertize "Codex" 'face 'mode-line-emphasis)
       ":"
       (propertize label 'face face)
       " "))))

(defun codex-ide-renderer-make-region-writable (start end)
  "Make the region from START to END writable."
  (when (< start end)
    (remove-text-properties start end
                            '(read-only t
					rear-nonsticky (read-only)
					front-sticky (read-only)))))

(defun codex-ide-renderer-freeze-region (start end)
  "Make the region from START to END read-only."
  (when (< start end)
    (remove-text-properties start end
                            '(read-only nil
					rear-nonsticky nil
					front-sticky nil))
    (add-text-properties start end '(read-only t
					       rear-nonsticky (read-only)
					       front-sticky (read-only)))))

(defun codex-ide-renderer-insert-prompt-prefix ()
  "Insert the user prompt prefix."
  (insert
   (propertize
    "> "
    'face 'codex-ide-user-prompt-face
    'field 'codex-ide-prompt-prefix
    codex-ide-prompt-start-property t
    'read-only t
    'rear-nonsticky '(field read-only)
    'front-sticky '(field read-only))))

(defun codex-ide-renderer-line-has-prompt-start-p (&optional pos)
  "Return non-nil when POS is on a line beginning with a prompt."
  (save-excursion
    (when pos
      (goto-char pos))
    (beginning-of-line)
    (get-text-property (point) codex-ide-prompt-start-property)))

(defun codex-ide-renderer-style-user-prompt-region (start end)
  "Apply user prompt styling from START to END."
  (when (< start end)
    (add-text-properties start end '(face codex-ide-user-prompt-face))
    (save-excursion
      (goto-char start)
      (add-text-properties (line-beginning-position) (1+ (line-beginning-position))
                           `(,codex-ide-prompt-start-property t)))))

(defun codex-ide-renderer-insert-input-prompt (&optional initial-text separate-output-p)
  "Insert a writable prompt at point and return prompt markers.
When INITIAL-TEXT is non-nil, seed the editable region with it.
When SEPARATE-OUTPUT-P is non-nil, insert a blank line before the prompt and
return an active-boundary marker in the result plist.

Return a plist containing `:transcript-start', `:active-boundary',
`:prompt-start', and `:input-start' markers."
  (let (transcript-start
        active-boundary
        prompt-start
        input-start)
    (setq transcript-start (copy-marker (point)))
    (unless (or (= (point) (point-min))
                (bolp))
      (insert "\n"))
    (when separate-output-p
      (setq active-boundary (copy-marker (point)))
      (insert "\n"))
    (setq prompt-start (copy-marker (point)))
    (codex-ide-renderer-insert-prompt-prefix)
    (setq input-start (copy-marker (point)))
    (when initial-text
      (insert initial-text))
    (list :transcript-start transcript-start
          :active-boundary active-boundary
          :prompt-start prompt-start
          :input-start input-start)))

(defun codex-ide-renderer-insert-context-summary (text)
  "Insert context summary TEXT on its own line and return (START . END)."
  (let ((start (point)))
    (insert "\n")
    (insert (propertize text 'face 'codex-ide-item-detail-face))
    (cons start (point))))

(defun codex-ide-renderer-insert-running-input-list (text)
  "Insert running-input summary TEXT at point-max and return its markers.
Return a plist containing `:delete-start', `:boundary', and `:end' markers."
  (let (delete-start boundary end)
    (setq delete-start (copy-marker (point) t))
    (unless (or (= (point) (point-min))
                (bolp))
      (insert "\n"))
    (setq boundary (copy-marker (point)))
    (insert "\n")
    (insert (propertize text 'face 'codex-ide-item-detail-face))
    (setq end (copy-marker (point)))
    (list :delete-start delete-start
          :boundary boundary
          :end end)))

(defun codex-ide-renderer-insert-session-header (working-dir)
  "Insert the initial session header for WORKING-DIR and return (START . END)."
  (codex-ide-renderer-insert-read-only
   (format "Codex session for %s\n\n"
           (abbreviate-file-name working-dir))))

(defun codex-ide-renderer-output-separator-string ()
  "Return the separator rule used between transcript sections."
  (concat (make-string 72 ?-) "\n"))

(defun codex-ide-renderer-restored-transcript-separator-string ()
  "Return the separator used after restored transcript content."
  (let* ((label "[restored transcript]")
         (width (length (string-trim-right
                         (codex-ide-renderer-output-separator-string))))
         (available (max 0 (- width (length label) 2)))
         (left (/ available 2))
         (right (- available left)))
    (concat
     (make-string left ?-)
     " "
     label
     " "
     (make-string right ?-)
     "\n")))

(defun codex-ide-renderer--normalize-file-link-target (target)
  "Return file link TARGET with permissive outer wrappers removed."
  (string-trim
   (replace-regexp-in-string "\\\\/" "/" target t t)
   "[ <]+"
   "[ >]+"))

(defun codex-ide-renderer-parse-file-link-target (target)
  "Parse markdown file TARGET into (PATH LINE COLUMN), or nil."
  (let ((normalized
         (codex-ide-renderer--normalize-file-link-target target)))
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

(defun codex-ide-renderer-open-file-link (&optional _button)
  "Open the file link described by text properties at point."
  (interactive)
  (codex-ide-renderer--visit-file-link #'find-file))

(defun codex-ide-renderer-open-file-link-other-window (&optional _button)
  "Open the file link described by text properties at point in another window."
  (interactive)
  (codex-ide-renderer--visit-file-link #'find-file-other-window))

(defun codex-ide-renderer--visit-file-link (open-file-fn)
  "Open the file link at point using OPEN-FILE-FN."
  (let ((path (get-text-property (point) 'codex-ide-path))
        (line (get-text-property (point) 'codex-ide-line))
        (column (get-text-property (point) 'codex-ide-column)))
    (unless (and path (file-exists-p path))
      (user-error "File does not exist: %s" (or path "unknown")))
    (funcall open-file-fn path)
    (goto-char (point-min))
    (when line
      (forward-line (1- line)))
    (when column
      (move-to-column (max 0 (1- column))))))

(defun codex-ide-renderer-insert-action-button
    (label callback &optional help-echo keymap properties)
  "Insert a button labeled LABEL that invokes CALLBACK.
HELP-ECHO, KEYMAP, and PROPERTIES are applied to the created button."
  (let ((button-keymap (if keymap
                           (make-composed-keymap keymap button-map)
                         button-map)))
    (apply
     #'make-text-button
     (point)
     (progn (insert (format "[%s]" label)) (point))
     (append
      (list 'follow-link t
            'keymap button-keymap
            'help-echo (or help-echo label)
            'action (lambda (_button)
                      (funcall callback)))
      properties))))

(defun codex-ide-renderer-insert-approval-label (label)
  "Insert an emphasized approval field LABEL."
  (insert (propertize label 'face 'codex-ide-approval-label-face)))

(defun codex-ide-renderer-insert-approval-diff (text face-fn)
  "Insert approval diff TEXT using FACE-FN for each line."
  (let ((trimmed (and (stringp text) (string-trim-right text))))
    (when (and trimmed (not (string-empty-p trimmed)))
      (codex-ide-renderer-insert-approval-label "Proposed changes:")
      (insert "\n\n")
      (dolist (line (split-string trimmed "\n"))
        (insert (propertize (concat line "\n")
                            'face
                            (funcall face-fn line))))
      (insert "\n"))))

(cl-defun codex-ide-renderer-insert-item-result-header
    (overlay prefix-text toggle-fn open-fn
             &key keymap overlay-property toggle-help-echo
             toggle-button-help open-button-label open-button-help
             open-button-keymap)
  "Insert an expandable item-result header for OVERLAY using PREFIX-TEXT.
TOGGLE-FN and OPEN-FN receive OVERLAY when invoked."
  (let ((prefix-start (point)))
    (insert prefix-text)
    (add-text-properties
     prefix-start
     (point)
     (list 'face 'codex-ide-item-detail-face
           'keymap keymap
           'help-echo (or toggle-help-echo "RET toggles this section")
           overlay-property overlay))
    (codex-ide-renderer-insert-action-button
     (if (overlay-get overlay :folded) "expand" "fold")
     (lambda ()
       (funcall toggle-fn overlay))
     (or toggle-button-help "Toggle this section")
     keymap
     (list overlay-property overlay))
    (when open-button-label
      (insert " ")
      (codex-ide-renderer-insert-action-button
       open-button-label
       (lambda ()
         (funcall open-fn overlay))
       open-button-help
       open-button-keymap
       (list overlay-property overlay)))
    (insert "\n")))

(cl-defun codex-ide-renderer-insert-command-output-header
    (overlay prefix-text toggle-fn open-fn
             &key keymap overlay-property)
  "Insert a command-output header for OVERLAY using PREFIX-TEXT.
TOGGLE-FN and OPEN-FN receive OVERLAY when invoked."
  (codex-ide-renderer-insert-item-result-header
   overlay prefix-text toggle-fn open-fn
   :keymap keymap
   :overlay-property overlay-property
   :toggle-help-echo "RET toggles command output"
   :toggle-button-help "Toggle command output"
   :open-button-label (and (overlay-get overlay :truncated) "full output")
   :open-button-help "Open full command output in a separate buffer"))

(cl-defun codex-ide-renderer-insert-read-only
    (text &optional face properties)
  "Insert TEXT at point as frozen transcript text.
When FACE is non-nil, apply it to the inserted text.  PROPERTIES is appended
to the inserted region.  Return (START . END)."
  (let ((start (point)))
    (insert text)
    (unless (or face
                (plist-member properties 'face)
                (text-property-not-all 0 (length text) 'face nil text))
      (remove-text-properties start (point) '(face nil)))
    (when (or face properties)
      (add-text-properties
       start
       (point)
       (append (when face (list 'face face))
               properties)))
    (codex-ide-renderer-freeze-region start (point))
    (cons start (point))))

(defun codex-ide-renderer-insert-read-only-newlines (count)
  "Insert COUNT newline characters at point as frozen transcript text.
Return (START . END)."
  (codex-ide-renderer-insert-read-only (make-string count ?\n)))

(cl-defun codex-ide-renderer-append-to-buffer
    (text &key insertion-point face properties restore-point
          preserve-point move-point-to-end after-insert)
  "Insert frozen transcript TEXT into the current buffer.
When INSERTION-POINT is non-nil, insert there; otherwise insert at point.
When FACE is non-nil, apply it to the inserted text.  PROPERTIES is appended
to the inserted region.

When RESTORE-POINT is a marker, restore point to it and clear it after
insertion.  When PRESERVE-POINT is non-nil and RESTORE-POINT is nil, restore
point to its original location after insertion.  When MOVE-POINT-TO-END is
non-nil and neither restoration mode is used, move point to `point-max'.

When AFTER-INSERT is non-nil, call it with three arguments: START, END, and
the resolved insertion position.

Return (START . END)."
  (codex-ide-renderer--without-undo-recording
   (let* ((inhibit-read-only t)
          (resolved-insertion-point (or insertion-point (point)))
          (original-point (and preserve-point
                               (not (markerp restore-point))
                               (copy-marker (point) t)))
          range)
     (goto-char resolved-insertion-point)
     (setq range
           (codex-ide-renderer-insert-read-only text face properties))
     (when after-insert
       (funcall after-insert
                (car range)
                (cdr range)
                resolved-insertion-point))
     (cond
      ((markerp restore-point)
       (when (marker-buffer restore-point)
         (goto-char restore-point))
       (set-marker restore-point nil))
      (original-point
       (goto-char original-point)
       (set-marker original-point nil))
      (move-point-to-end
       (goto-char (point-max))))
     range)))

(defun codex-ide-renderer-insert-output-spacing ()
  "Insert the transcript spacing needed before a new output block.
Return (START . END)."
  (let ((start (point)))
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
    (codex-ide-renderer-freeze-region start (point))
    (cons start (point))))

(defun codex-ide-renderer-insert-output-separator (&optional properties)
  "Insert the standard transcript separator at point.
PROPERTIES is appended to the inserted region.  Return (START . END)."
  (codex-ide-renderer-insert-read-only
   (codex-ide-renderer-output-separator-string)
   'codex-ide-output-separator-face
   properties))

(defun codex-ide-renderer-insert-restored-transcript-separator (&optional properties)
  "Insert the restored-transcript separator at point.
PROPERTIES is appended to the inserted region.  Return (START . END)."
  (codex-ide-renderer-insert-read-only
   (codex-ide-renderer-restored-transcript-separator-string)
   'codex-ide-output-separator-face
   properties))

(defun codex-ide-renderer-insert-pending-indicator (text)
  "Insert pending-indicator TEXT at point and return (START . END INSERTED-TEXT)."
  (let* ((inserted-text
          (concat (if (or (= (point) (point-min)) (bolp))
                      ""
                    "\n")
                  text))
         (range (codex-ide-renderer-insert-read-only inserted-text 'shadow)))
    (list (car range) (cdr range) inserted-text)))

(defun codex-ide-renderer-delete-matching-text (start text)
  "Delete TEXT at START when it matches exactly.
Return non-nil when deletion occurred."
  (when (and start
             (<= start (point-max)))
    (save-excursion
      (goto-char start)
      (when (looking-at (regexp-quote text))
        (delete-region start (match-end 0))
        t))))

(cl-defun codex-ide-renderer-replace-region
    (start end text &optional face properties)
  "Replace the region from START to END with TEXT.
When FACE is non-nil, apply it to the inserted text.  PROPERTIES is appended
to the inserted region.  Return (START . END)."
  (delete-region start end)
  (goto-char start)
  (let ((insert-start (point)))
    (insert text)
    (when (or face properties)
      (add-text-properties
       insert-start
       (point)
       (append (when face (list 'face face))
               properties)))
    (cons insert-start (point))))

(defun codex-ide-renderer-replace-marker-region (start-marker end-marker text)
  "Replace text between START-MARKER and END-MARKER with TEXT.
Return (START . END) for the inserted text."
  (let ((range (codex-ide-renderer-replace-region
                (marker-position start-marker)
                (marker-position end-marker)
                text)))
    (set-marker end-marker (cdr range))
    range))

(cl-defun codex-ide-renderer-insert-item-result-body
    (display-text &key keymap overlay overlay-property properties face help-echo)
  "Insert DISPLAY-TEXT as a frozen expandable body.
KEYMAP is applied to the body text.  OVERLAY is attached via OVERLAY-PROPERTY.
PROPERTIES is appended to the inserted region.  Return (START . END)."
  (codex-ide-renderer-insert-read-only
   display-text
   (or face 'codex-ide-command-output-face)
   (append
    (list 'keymap keymap
          'help-echo (or help-echo "RET toggles this section")
          overlay-property overlay)
    properties)))

(cl-defun codex-ide-renderer-insert-command-output-body
    (display-text &key keymap overlay overlay-property properties)
  "Insert DISPLAY-TEXT as a frozen command-output body.
KEYMAP is applied to the body text.  OVERLAY is attached via OVERLAY-PROPERTY.
PROPERTIES is appended to the inserted region.  Return (START . END)."
  (codex-ide-renderer-insert-item-result-body
   display-text
   :keymap keymap
   :overlay overlay
   :overlay-property overlay-property
   :properties properties
   :face 'codex-ide-command-output-face
   :help-echo "RET toggles command output"))

(defun codex-ide-renderer-insert-shell-command-detail (command &optional properties)
  "Insert COMMAND as an indented shell-highlighted detail line.
PROPERTIES is appended to the inserted region.  Return (START . END)."
  (let ((start (point))
        command-start
        command-end)
    (insert "  $ ")
    (remove-text-properties start (point) '(face nil))
    (setq command-start (point))
    (insert command)
    (setq command-end (point))
    (insert "\n")
    (add-text-properties
     start
     (point)
     (append (list 'face 'codex-ide-item-detail-face)
             properties))
    (let ((inhibit-message t)
          (message-log-max nil))
      (codex-ide-renderer-fontify-code-block-region
       command-start command-end "sh"))
    (codex-ide-renderer-freeze-region start (point))
    (cons start (point))))

(defun codex-ide-renderer-insert-restored-user-message (text)
  "Insert restored user TEXT at point and return (START . END)."
  (codex-ide-renderer-insert-output-spacing)
  (let ((prompt-start (point)))
    (codex-ide-renderer-insert-prompt-prefix)
    (insert text)
    (codex-ide-renderer-style-user-prompt-region prompt-start (point))
    (codex-ide-renderer-freeze-region prompt-start (point))
    (insert "\n\n")
    (codex-ide-renderer-freeze-region prompt-start (point))
    (cons prompt-start (point))))

(defun codex-ide-renderer-insert-interactive-request-shell (title render-body)
  "Insert a standard interactive-request shell with TITLE.
RENDER-BODY is called to insert request-specific body content.
Return (START STATUS END RENDER-STATE)."
  (let ((start (point))
        render-state
        status
        end)
    (codex-ide-renderer-insert-output-spacing)
    (codex-ide-renderer-insert-output-separator)
    (codex-ide-renderer-insert-read-only "\n")
    (codex-ide-renderer-insert-read-only title 'codex-ide-approval-header-face)
    (codex-ide-renderer-insert-read-only "\n\n")
    (setq render-state (funcall render-body))
    (setq status (point))
    (setq end (point))
    (list start status end render-state)))

(defun codex-ide-renderer-insert-approval-resolution (label)
  "Insert a resolved approval LABEL line and return (START . END)."
  (let ((start (point)))
    (insert (propertize "Selected: "
                        'face
                        'codex-ide-approval-label-face))
    (insert label)
    (insert "\n")
    (cons start (point))))

(defun codex-ide-renderer-insert-approval-detail (detail &optional diff-face-fn)
  "Insert one formatted approval DETAIL at point.
When DETAIL is a diff block, use DIFF-FACE-FN to choose faces."
  (pcase (plist-get detail :kind)
    ('command
     (codex-ide-renderer-insert-approval-label "Run the following command?")
     (insert "\n\n    ")
     (insert (propertize (or (plist-get detail :text) "")
                         'face
                         'codex-ide-item-summary-face))
     (insert "\n\n"))
    ('diff
     (codex-ide-renderer-insert-approval-diff
      (plist-get detail :text)
      (or diff-face-fn #'ignore)))
    (_
     (when-let ((label (plist-get detail :label)))
       (codex-ide-renderer-insert-approval-label (format "%s: " label)))
     (insert (or (plist-get detail :text) ""))
     (insert "\n"))))

(defun codex-ide-renderer-insert-elicitation-text-field (default writable-ranges)
  "Insert a writable elicitation text field with DEFAULT.
Extend WRITABLE-RANGES and return (START END UPDATED-RANGES)."
  (let ((start nil)
        (end nil))
    (insert "    ")
    (setq start (copy-marker (point)))
    (when default
      (insert (format "%s" default)))
    (insert "\n")
    (setq end (copy-marker (point)))
    (push (cons start end) writable-ranges)
    (list start end writable-ranges)))

(defun codex-ide-renderer-insert-elicitation-choice-field
    (initial choices set-choice-fn button-keymap)
  "Insert an elicitation choice field at point.
INITIAL is the current display label.  CHOICES is an alist of label/value pairs.
SET-CHOICE-FN is called with LABEL and VALUE when a choice is selected.
BUTTON-KEYMAP is passed to the rendered action buttons.

Return (DISPLAY-START DISPLAY-END)."
  (let (display-start display-end)
    (insert "    Selected: ")
    (setq display-start (copy-marker (point)))
    (insert initial)
    (setq display-end (copy-marker (point) t))
    (insert "\n")
    (dolist (choice choices)
      (let ((label (car choice))
            (value (cdr choice)))
        (codex-ide-renderer-insert-action-button
         label
         (lambda ()
           (funcall set-choice-fn label value))
         nil
         button-keymap)
        (insert " ")))
    (insert "\n")
    (list display-start display-end)))

(defun codex-ide-renderer-insert-elicitation-field
    (prompt type initial choices writable-ranges set-choice-fn button-keymap)
  "Insert one elicitation field at point and return its view state.
PROMPT is the user-facing label.  TYPE is either `choice' or `text'.  INITIAL
is the initial display value or default field content.  CHOICES is used for
choice fields.  WRITABLE-RANGES is extended for text fields.  SET-CHOICE-FN
and BUTTON-KEYMAP are used for choice buttons.

Return a plist containing inserted markers and updated writable ranges."
  (codex-ide-renderer-insert-approval-label (concat prompt ":"))
  (insert "\n")
  (let ((result
         (pcase type
           ('choice
            (pcase-let ((`(,display-start ,display-end)
                         (codex-ide-renderer-insert-elicitation-choice-field
                          initial choices set-choice-fn button-keymap)))
              (list :display-start-marker display-start
                    :display-end-marker display-end
                    :writable-ranges writable-ranges)))
           (_
            (pcase-let ((`(,start ,end ,new-ranges)
                         (codex-ide-renderer-insert-elicitation-text-field
                          initial writable-ranges)))
              (list :start-marker start
                    :end-marker end
                    :writable-ranges new-ranges))))))
    (insert "\n")
    result))

(defun codex-ide-renderer--clear-markdown-properties (start end)
  "Clear Codex markdown rendering properties between START and END."
  (let ((end-marker (copy-marker end t)))
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

(defun codex-ide-renderer--normalize-markdown-link-label (label)
  "Return LABEL with markdown code delimiters stripped when present."
  (save-match-data
    (if (string-match "\\``\\([^`\n]+\\)`\\'" label)
        (match-string 1 label)
      label)))

(defun codex-ide-renderer--markdown-language-mode-candidates (language)
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

(defvar codex-ide-renderer--font-lock-spec-cache (make-hash-table :test 'eq)
  "Cache of font-lock setup captured from major modes.")

(defconst codex-ide-renderer--cached-font-lock-variables
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
    syntax-propertize-function))

(defun codex-ide-renderer--font-lock-spec-for-mode (mode)
  "Return cached font-lock setup for MODE."
  (or (gethash mode codex-ide-renderer--font-lock-spec-cache)
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
                        codex-ide-renderer--cached-font-lock-variables)))))
        (puthash mode spec codex-ide-renderer--font-lock-spec-cache)
        spec)))

(defun codex-ide-renderer--apply-font-lock-spec (spec)
  "Apply cached font-lock SPEC to the current buffer."
  (set-syntax-table (copy-syntax-table (plist-get spec :syntax-table)))
  (dolist (entry (plist-get spec :variables))
    (let ((variable (nth 0 entry))
          (localp (nth 1 entry))
          (value (nth 2 entry)))
      (when localp
        (set (make-local-variable variable) (copy-tree value))))))

(defun codex-ide-renderer--copy-code-font-lock-properties (source-buffer start end)
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

(defun codex-ide-renderer--fontify-code-block-with-mode (source-buffer start end code language mode)
  "Apply MODE fontification for CODE into SOURCE-BUFFER between START and END."
  (or
   (condition-case nil
       (let ((spec (codex-ide-renderer--font-lock-spec-for-mode mode)))
         (with-temp-buffer
           (insert code)
           (codex-ide-renderer--apply-font-lock-spec spec)
           (font-lock-mode 1)
           (font-lock-ensure (point-min) (point-max))
           (codex-ide-renderer--copy-code-font-lock-properties
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
         (codex-ide-renderer--copy-code-font-lock-properties
          source-buffer start end)
         t)
     (error nil))))

(defun codex-ide-renderer--fontify-code-block-region (start end language)
  "Apply syntax highlighting to region START END using LANGUAGE."
  (let ((source-buffer (current-buffer))
        (code (buffer-substring-no-properties start end)))
    (cl-some
     (lambda (mode)
       (codex-ide-renderer--fontify-code-block-with-mode
        source-buffer
        start
        end
        code
        language
        mode))
     (codex-ide-renderer--markdown-language-mode-candidates language))))

(defun codex-ide-renderer-fontify-code-block-region (start end language)
  "Apply syntax highlighting to region START END using LANGUAGE."
  (codex-ide-renderer--fontify-code-block-region start end language))

(defun codex-ide-renderer--render-fenced-code-blocks (start end)
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
            (codex-ide-renderer--fontify-code-block-region code-start closing-start language)
            (add-text-properties
             code-start closing-start
             '(codex-ide-markdown-code-fontified t)))
          (add-text-properties
           closing-start closing-end
           '(display ""
		     codex-ide-markdown t))
          (goto-char closing-end))))))

(defun codex-ide-renderer--markdown-table-row-line-p (line)
  "Return non-nil when LINE looks like a markdown pipe table row."
  (string-match-p "\\`[ \t]*|.*|[ \t]*\\'" line))

(defun codex-ide-renderer--markdown-table-separator-line-p (line)
  "Return non-nil when LINE looks like a markdown table separator row."
  (string-match-p
   "\\`[ \t]*|[ \t]*:?-+:?[ \t]*\\(?:|[ \t]*:?-+:?[ \t]*\\)+|[ \t]*\\'"
   line))

(defun codex-ide-renderer--markdown-table-parse-row (line)
  "Split markdown pipe table LINE into trimmed cell strings."
  (let ((trimmed (string-trim line)))
    (mapcar #'string-trim
            (split-string
             (string-remove-prefix "|"
                                   (string-remove-suffix "|" trimmed))
             "|"))))

(defun codex-ide-renderer--markdown-line-region-end (&optional limit)
  "Return the current line end position, including a trailing newline when present."
  (let* ((line-end (line-end-position))
         (newline-end (if (< line-end (point-max))
                          (1+ line-end)
                        line-end)))
    (min (or limit newline-end) newline-end)))

(defun codex-ide-renderer--markdown-table-column-alignments (separator)
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
   (codex-ide-renderer--markdown-table-parse-row separator)))

(defconst codex-ide-renderer--markdown-table-inline-pattern
  (concat
   "\\(\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))\\)"
   "\\|`\\([^`\n]+\\)`"
   "\\|\\(\\*\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\*\\)"
   "\\|\\(__\\([^_\n ]\\(?:[^\n]*?[^_\n ]\\)?\\)__\\)"
   "\\|\\(\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\)"
   "\\|\\(_\\([^_\n ]\\(?:[^_\n]*[^_\n ]\\)?\\)_\\)"))

(defconst codex-ide-renderer--markdown-link-pattern
  "\\(\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))\\)"
  "Pattern matching a simple inline markdown link `[label](target)`.")

(defconst codex-ide-renderer--markdown-inline-code-pattern
  "`\\([^`\n]+\\)`"
  "Pattern matching inline markdown code spans.")

(defconst codex-ide-renderer--markdown-bold-asterisk-pattern
  "\\(^\\|[^*]\\)\\(\\*\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\*\\)"
  "Pattern matching bold markdown emphasis delimited by `**`.")

(defconst codex-ide-renderer--markdown-bold-underscore-pattern
  "\\(^\\|[^_]\\)\\(__\\([^_\n ]\\(?:[^\n]*?[^_\n ]\\)?\\)__\\)"
  "Pattern matching bold markdown emphasis delimited by `__`.")

(defconst codex-ide-renderer--markdown-italic-asterisk-pattern
  "\\(^\\|[^*]\\)\\(\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\)"
  "Pattern matching italic markdown emphasis delimited by `*`.")

(defconst codex-ide-renderer--markdown-italic-underscore-pattern
  "\\(^\\|[^_]\\)\\(_\\([^_\n ]\\(?:[^_\n]*[^_\n ]\\)?\\)_\\)"
  "Pattern matching italic markdown emphasis delimited by `_`.")

(defun codex-ide-renderer--markdown-inline-word-char-p (char)
  "Return non-nil when CHAR is a word-like markdown delimiter neighbor."
  (and char
       (string-match-p "[[:alnum:]_]" (char-to-string char))))

(defun codex-ide-renderer--markdown-inline-underscore-boundary-p (text start end)
  "Return non-nil when underscores at START and END in TEXT are markdown delimiters."
  (and (not (codex-ide-renderer--markdown-inline-word-char-p
             (and (> start 0) (aref text (1- start)))))
       (not (codex-ide-renderer--markdown-inline-word-char-p
             (and (< end (length text)) (aref text end))))))

(defun codex-ide-renderer--markdown-table-render-cell (cell)
  "Return CELL rendered as visible table text."
  (let ((pos 0)
        (parts nil))
    (while (string-match codex-ide-renderer--markdown-table-inline-pattern cell pos)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        (when (> match-start pos)
          (push (substring cell pos match-start) parts))
        (cond
         ((match-beginning 2)
          (let* ((label (codex-ide-renderer--normalize-markdown-link-label
                         (match-string 2 cell)))
                 (target (match-string 3 cell))
                 (parsed (codex-ide-renderer-parse-file-link-target target)))
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
           (if (codex-ide-renderer--markdown-inline-underscore-boundary-p
                cell match-start match-end)
               (propertize (match-string 8 cell) 'face 'bold)
             (match-string 0 cell))
           parts))
         ((match-beginning 10)
          (push (propertize (match-string 10 cell) 'face 'italic) parts))
         ((match-beginning 12)
          (push
           (if (codex-ide-renderer--markdown-inline-underscore-boundary-p
                cell match-start match-end)
               (propertize (match-string 12 cell) 'face 'italic)
             (match-string 0 cell))
           parts)))
        (setq pos match-end)))
    (when (< pos (length cell))
      (push (substring cell pos) parts))
    (apply #'concat (nreverse parts))))

(defun codex-ide-renderer--markdown-region-unrendered-p (start end)
  "Return non-nil when START to END has no markdown-rendered text."
  (not (text-property-not-all start end 'codex-ide-markdown nil)))

(defun codex-ide-renderer--markdown-emphasis-delimiters-unrendered-p
    (span-start content-start content-end span-end)
  "Return non-nil when emphasis delimiters have not already been rendered."
  (and (codex-ide-renderer--markdown-region-unrendered-p span-start content-start)
       (codex-ide-renderer--markdown-region-unrendered-p content-end span-end)))

(defun codex-ide-renderer--markdown-emphasis-underscore-boundary-p (start end)
  "Return non-nil when underscores from START to END are markdown delimiters."
  (and (not (codex-ide-renderer--markdown-inline-word-char-p
             (char-before start)))
       (not (codex-ide-renderer--markdown-inline-word-char-p
             (char-after end)))))

(defun codex-ide-renderer--render-markdown-emphasis (start end pattern face &optional underscore)
  "Render markdown emphasis matching PATTERN with FACE between START and END."
  (goto-char start)
  (while (re-search-forward pattern end t)
    (let ((span-start (match-beginning 2))
          (span-end (match-end 2))
          (content-start (match-beginning 3))
          (content-end (match-end 3)))
      (when (and (codex-ide-renderer--markdown-emphasis-delimiters-unrendered-p
                  span-start content-start content-end span-end)
                 (or (not underscore)
                     (codex-ide-renderer--markdown-emphasis-underscore-boundary-p
                      span-start span-end)))
        (let ((content-length (- content-end content-start)))
          (add-face-text-property content-start content-end face 'append)
          (delete-region content-end span-end)
          (delete-region span-start content-start)
          (goto-char (+ span-start content-length)))))))

(defun codex-ide-renderer--markdown-table-pad-cell (cell width alignment)
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

(defun codex-ide-renderer--markdown-table-format-row (cells widths alignments)
  "Return a propertized table row from CELLS using WIDTHS and ALIGNMENTS."
  (concat
   "| "
   (mapconcat
    (lambda (triple)
      (pcase-let ((`(,cell ,width ,alignment) triple))
        (codex-ide-renderer--markdown-table-pad-cell cell width alignment)))
    (cl-mapcar #'list cells widths alignments)
    " | ")
   " |\n"))

(defun codex-ide-renderer--markdown-table-separator-string (widths alignments)
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

(defun codex-ide-renderer--markdown-table-leading-indentation (line)
  "Return indentation before the opening table pipe in LINE."
  (if (string-match "\\`\\([ \t]*\\)|" line)
      (match-string 1 line)
    ""))

(defun codex-ide-renderer--markdown-prefix-lines (text prefix)
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

(defun codex-ide-renderer--markdown-table-display-string (lines)
  "Return a rendered display string for markdown table LINES, or nil."
  (when (>= (length lines) 2)
    (let* ((header (car lines))
           (separator (cadr lines))
           (body (cddr lines)))
      (when (and (codex-ide-renderer--markdown-table-row-line-p header)
                 (codex-ide-renderer--markdown-table-separator-line-p separator))
        (let* ((indent (codex-ide-renderer--markdown-table-leading-indentation header))
               (alignments (codex-ide-renderer--markdown-table-column-alignments separator))
               (raw-rows (mapcar #'codex-ide-renderer--markdown-table-parse-row
                                 (cons header
                                       (seq-filter #'codex-ide-renderer--markdown-table-row-line-p
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
                    (mapcar #'codex-ide-renderer--markdown-table-render-cell row)
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
                 (codex-ide-renderer--markdown-table-format-row
                  (car rendered-rows)
                  widths
                  normalized-alignments)
                 (codex-ide-renderer--markdown-table-separator-string
                  widths
                  normalized-alignments)
                 (mapconcat
                  (lambda (row)
                    (codex-ide-renderer--markdown-table-format-row
                     row
                     widths
                     normalized-alignments))
                  (cdr rendered-rows)
                  "")))
               (table-text (codex-ide-renderer--markdown-prefix-lines table-text indent)))
          (add-face-text-property
           0 (length table-text) 'fixed-pitch 'append table-text)
          table-text)))))

(defun codex-ide-renderer--buttonize-markdown-table-links (start end)
  "Convert rendered file-link spans between START and END into buttons."
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'codex-ide-table-link nil end)
                      end)))
        (when (and (get-text-property pos 'codex-ide-table-link)
                   (get-text-property pos 'codex-ide-path))
          (make-text-button
           pos next
           'action #'codex-ide-renderer-open-file-link
           'follow-link t
           'keymap codex-ide-renderer-link-keymap
           'help-echo (get-text-property pos 'help-echo)
           'face 'link
           'codex-ide-markdown t
           'codex-ide-path (get-text-property pos 'codex-ide-path)
           'codex-ide-line (get-text-property pos 'codex-ide-line)
           'codex-ide-column (get-text-property pos 'codex-ide-column)))
        (setq pos next)))))

(defun codex-ide-renderer--markdown-table-block-at-point (end &optional allow-trailing)
  "Return markdown table data at point as (START END LINES), or nil."
  (let* ((header-start (line-beginning-position))
         (header-end (line-end-position))
         (header (buffer-substring-no-properties header-start header-end)))
    (when (and (not (get-text-property header-start 'codex-ide-markdown))
               (codex-ide-renderer--markdown-table-row-line-p header))
      (save-excursion
        (forward-line 1)
        (when (< (point) end)
          (let* ((separator-start (line-beginning-position))
                 (separator-end (line-end-position))
                 (separator
                  (buffer-substring-no-properties separator-start separator-end)))
            (when (and (not (get-text-property separator-start 'codex-ide-markdown))
                       (codex-ide-renderer--markdown-table-separator-line-p separator))
              (let ((lines (list header separator))
                    (block-end
                     (save-excursion
                       (goto-char separator-start)
                       (codex-ide-renderer--markdown-line-region-end end))))
                (forward-line 1)
                (while (and (< (point) end)
                            (let* ((row-start (line-beginning-position))
                                   (row-end (line-end-position))
                                   (row
                                    (buffer-substring-no-properties
                                     row-start row-end)))
                              (and (not (get-text-property
                                         row-start 'codex-ide-markdown))
                                   (codex-ide-renderer--markdown-table-row-line-p row))))
                  (let* ((row-start (line-beginning-position))
                         (row-end (line-end-position))
                         (row (buffer-substring-no-properties row-start row-end)))
                    (setq lines (append lines (list row))
                          block-end (codex-ide-renderer--markdown-line-region-end end)))
                  (forward-line 1))
                (when (or allow-trailing
                          (< block-end end))
                  (list header-start block-end lines))))))))))

(defun codex-ide-renderer--render-markdown-tables (start end &optional allow-trailing)
  "Render markdown pipe tables between START and END."
  (let ((end-marker (copy-marker end t)))
    (goto-char start)
    (while (< (point) (marker-position end-marker))
      (if-let ((table (codex-ide-renderer--markdown-table-block-at-point
                       (marker-position end-marker)
                       allow-trailing)))
          (pcase-let ((`(,block-start ,block-end ,lines) table))
            (if-let ((rendered (codex-ide-renderer--markdown-table-display-string lines)))
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
                  (codex-ide-renderer--buttonize-markdown-table-links block-start (point))
                  (goto-char (point)))
              (goto-char block-end)))
        (forward-line 1)))
    (set-marker end-marker nil)))

(cl-defun codex-ide-renderer-render-markdown-region (start end &optional allow-trailing-tables)
  "Apply lightweight markdown rendering between START and END."
  (codex-ide-renderer--without-undo-recording
   (save-excursion
     (let ((inhibit-read-only t)
           (end-marker (copy-marker end t)))
       (codex-ide-renderer--clear-markdown-properties start (marker-position end-marker))
       (goto-char start)
       (codex-ide-renderer--render-fenced-code-blocks
        start
        (marker-position end-marker))
       (goto-char start)
       (codex-ide-renderer--render-markdown-tables
        start
        (marker-position end-marker)
        allow-trailing-tables)
       (goto-char start)
       (while (re-search-forward
               codex-ide-renderer--markdown-link-pattern
               (marker-position end-marker)
               t)
         (unless (or (get-text-property (match-beginning 1) 'codex-ide-markdown)
                     (get-text-property (1- (match-end 1)) 'codex-ide-markdown))
           (let* ((match-start (match-beginning 1))
                  (match-end (match-end 1))
                  (label (match-string-no-properties 2))
                  (display-label (codex-ide-renderer--normalize-markdown-link-label label))
                  (target (match-string-no-properties 3))
                  (parsed (codex-ide-renderer-parse-file-link-target target)))
             (when parsed
               (make-text-button
                match-start match-end
                'action #'codex-ide-renderer-open-file-link
                'follow-link t
                'keymap codex-ide-renderer-link-keymap
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
               codex-ide-renderer--markdown-inline-code-pattern
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
       (codex-ide-renderer--render-markdown-emphasis
        start
        (marker-position end-marker)
        codex-ide-renderer--markdown-bold-asterisk-pattern
        'bold)
       (codex-ide-renderer--render-markdown-emphasis
        start
        (marker-position end-marker)
        codex-ide-renderer--markdown-bold-underscore-pattern
        'bold
        t)
       (codex-ide-renderer--render-markdown-emphasis
        start
        (marker-position end-marker)
        codex-ide-renderer--markdown-italic-asterisk-pattern
        'italic)
       (codex-ide-renderer--render-markdown-emphasis
        start
        (marker-position end-marker)
        codex-ide-renderer--markdown-italic-underscore-pattern
        'italic
        t)
       (set-marker end-marker nil)))))

(defun codex-ide-renderer--markdown-region-over-size-limit-p (start end)
  "Return non-nil when START to END should stay plain for performance."
  (and (integerp codex-ide-renderer-markdown-render-max-chars)
       (or (<= codex-ide-renderer-markdown-render-max-chars 0)
           (> (- end start) codex-ide-renderer-markdown-render-max-chars))))

(defun codex-ide-renderer-maybe-render-markdown-region (start end &optional allow-trailing-tables)
  "Render markdown between START and END unless the region is too large."
  (if (codex-ide-renderer--markdown-region-over-size-limit-p start end)
      (progn
        (codex-ide-renderer--without-undo-recording
         (save-excursion
           (let ((inhibit-read-only t))
             (codex-ide-renderer--clear-markdown-properties start end))))
        nil)
    (codex-ide-renderer-render-markdown-region start end allow-trailing-tables)
    t))

(defun codex-ide-renderer--streaming-markdown-complete-line-limit (end)
  "Return the completed-line boundary at or before END."
  (save-excursion
    (goto-char end)
    (if (or (bobp) (bolp))
        (point)
      (line-beginning-position))))

(defun codex-ide-renderer--markdown-fence-line-p (line)
  "Return non-nil when LINE is a fenced-code delimiter."
  (string-match-p "\\`[ \t]*```[^`\n]*[ \t]*\\'" line))

(defun codex-ide-renderer--streaming-markdown-table-block-end (limit)
  "Return the raw markdown table block end at point, or nil."
  (let* ((header-start (point))
         (header (buffer-substring-no-properties
                  header-start
                  (line-end-position))))
    (when (codex-ide-renderer--markdown-table-row-line-p header)
      (save-excursion
        (forward-line 1)
        (when (< (point) limit)
          (let ((separator (buffer-substring-no-properties
                            (point)
                            (line-end-position))))
            (when (codex-ide-renderer--markdown-table-separator-line-p separator)
              (forward-line 1)
              (while (and (< (point) limit)
                          (codex-ide-renderer--markdown-table-row-line-p
                           (buffer-substring-no-properties
                            (point)
                            (line-end-position))))
                (forward-line 1))
              (min (point) limit))))))))

(defun codex-ide-renderer--streaming-markdown-pending-table-header-p (line limit)
  "Return non-nil when LINE may be a table header awaiting more input."
  (and (codex-ide-renderer--markdown-table-row-line-p line)
       (save-excursion
         (forward-line 1)
         (or (>= (point) limit)
             (codex-ide-renderer--markdown-table-separator-line-p
              (buffer-substring-no-properties
               (point)
               (line-end-position)))))))

(defun codex-ide-renderer--streaming-markdown-segments (start limit)
  "Return stream-safe markdown segments from START to LIMIT."
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
           ((codex-ide-renderer--markdown-fence-line-p line)
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
                        (codex-ide-renderer--markdown-line-region-end limit))))
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
                   (codex-ide-renderer--streaming-markdown-table-block-end limit)))
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
           ((codex-ide-renderer--streaming-markdown-pending-table-header-p line limit)
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

(defun codex-ide-renderer-render-markdown-streaming (start end &optional state-marker)
  "Incrementally render stream-safe markdown from START to END.
When STATE-MARKER is non-nil, it tracks the next dirty position."
  (let* ((render-start (if (and (markerp state-marker)
                                (marker-buffer state-marker))
                           (marker-position state-marker)
                         start))
         (limit (codex-ide-renderer--streaming-markdown-complete-line-limit end)))
    (when (< render-start limit)
      (pcase-let ((`(,segments ,next-marker)
                   (codex-ide-renderer--streaming-markdown-segments
                    render-start
                    limit)))
        (dolist (segment segments)
          (let ((segment-start (marker-position (nth 0 segment)))
                (segment-end (marker-position (nth 1 segment)))
                (allow-trailing-tables (nth 2 segment)))
            (when (< segment-start segment-end)
              (codex-ide-renderer-maybe-render-markdown-region
               segment-start
               segment-end
               allow-trailing-tables)))
          (set-marker (nth 0 segment) nil)
          (set-marker (nth 1 segment) nil))
        (when (markerp state-marker)
          (set-marker state-marker (marker-position next-marker)))
        (prog1 (marker-position next-marker)
          (set-marker next-marker nil))))))

(provide 'codex-ide-renderer)

;;; codex-ide-renderer.el ends here
