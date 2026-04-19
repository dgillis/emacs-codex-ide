;;; codex-ide-renderer.el --- View rendering for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; `codex-ide-renderer' owns the view layer for transcript presentation.
;; It is responsible for operations that act on explicit buffer state and
;; explicit inputs such as regions, markers, text properties, overlays, and
;; render options.  This includes markdown rendering, prompt/transcript styling,
;; separator formatting, file-link activation, and lightweight status display
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
;; lifecycle, prompt management, error classification, and replay of stored
;; thread data belong in controller-oriented modules such as
;; `codex-ide-transcript.el'.  When in doubt, code belongs here only if it can
;; be described as "take this input and current buffer state, then render or
;; restyle the view".

;;; Code:

(require 'button)
(require 'cl-lib)
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
  '((((class color) (background light))
     :inherit default
     :background "#f4f1e8")
    (((class color) (background dark))
     :inherit default
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

(defconst codex-ide-prompt-start-property 'codex-ide-prompt-start
  "Text property used to mark the first character of a user prompt.")

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
    codex-ide-prompt-start-property t)))

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

(defun codex-ide-renderer-parse-file-link-target (target)
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

(defun codex-ide-renderer-open-file-link (_button)
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
                "\\(\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))\\)"
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
        (codex-ide-renderer--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^*]\\)\\(\\*\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\*\\)"
         'bold)
        (codex-ide-renderer--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^_]\\)\\(__\\([^_\n ]\\(?:[^\n]*?[^_\n ]\\)?\\)__\\)"
         'bold
         t)
        (codex-ide-renderer--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^*]\\)\\(\\*\\([^*\n ]\\(?:[^*\n]*[^*\n ]\\)?\\)\\*\\)"
         'italic)
        (codex-ide-renderer--render-markdown-emphasis
         start
         (marker-position end-marker)
         "\\(^\\|[^_]\\)\\(_\\([^_\n ]\\(?:[^_\n]*[^_\n ]\\)?\\)_\\)"
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
