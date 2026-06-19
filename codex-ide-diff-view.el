;;; codex-ide-diff-view.el --- Diff buffer views for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns dedicated diff-buffer presentation for Codex file changes.
;;
;; It turns Codex-provided patch text into a standalone `codex-ide-diff-mode'
;; buffer derived from `codex-ide-section-mode' and displays that buffer using
;; codex-ide's normal window policy.  Keeping this separate from transcript
;; control lets the transcript layer stay focused on when a diff should be
;; offered while this module owns how the diff is shown.

;;; Code:

(require 'cl-lib)
(require 'codex-ide-diff-data)
(require 'codex-ide-diff-model)
(require 'codex-ide-section)
(require 'codex-ide-utils)
(require 'subr-x)

(declare-function codex-ide-display-buffer "codex-ide-window"
                  (buffer &optional action &rest args))
(declare-function codex-ide--session-for-current-project "codex-ide-session" ())
(declare-function codex-ide-session-buffer "codex-ide-core" (session))
(declare-function codex-ide-session-current-turn-id "codex-ide-core" (session))
(declare-function codex-ide-session-directory "codex-ide-core" (session))
(declare-function codex-ide-session-p "codex-ide-core" (object))
(declare-function codex-ide-diff-data-combined-turn-diff-text
                  "codex-ide-diff-data" (session &optional turn-id))
(declare-function codex-ide-diff-data-turn-id-at-point
                  "codex-ide-diff-data" (session &optional point buffer))

(defvar codex-ide--display-buffer-other-window-pop-up-action)

(defvar codex-ide-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map codex-ide-section-mode-map)
    (define-key map (kbd "C-c TAB") #'codex-ide-diff-toggle-file-at-point)
    (define-key map (kbd "C-c C-a") #'codex-ide-diff-collapse-all-files)
    (define-key map (kbd "C-c C-e") #'codex-ide-diff-expand-all-files)
    (define-key map (kbd "RET") #'codex-ide-diff-goto-source-at-point)
    (define-key map (kbd "<return>") #'codex-ide-diff-goto-source-at-point)
    map)
  "Keymap used in standalone Codex diff buffers.")

(define-derived-mode codex-ide-diff-mode codex-ide-section-mode "Codex-Diff"
  "Major mode for standalone Codex diff buffers.

* \\<codex-ide-diff-mode-map>\\[codex-ide-diff-toggle-file-at-point] toggles the file diff at point.

* \\[codex-ide-diff-collapse-all-files] collapses all file diffs.

* \\[codex-ide-diff-expand-all-files] expands all file diffs.

* \\[codex-ide-diff-goto-source-at-point] jumps to source for the diff line at point.")

(defvar-local codex-ide-diff--raw-text nil
  "Raw diff text backing the current Codex diff buffer.")

(defvar-local codex-ide-diff--display-text nil
  "Display-normalized diff text backing the current Codex diff buffer.")

(defvar-local codex-ide-diff--directory nil
  "Directory used to resolve source paths in the current Codex diff buffer.")

(defvar-local codex-ide-session-diff--session nil
  "Codex session associated with the current session diff buffer.")

(defvar-local codex-ide-session-diff-source 'live
  "Diff source shown by the current session diff buffer.
The value is one of `live', `transcript', or `pinned'.")

(defvar-local codex-ide-session-diff--turn-id nil
  "Turn id selected by the current session diff buffer, when any.")

(defvar-local codex-ide-session-diff--header-line nil
  "Rendered header line for the current session diff buffer.")

(defface codex-ide-diff-added-snippet-face
  '((t :inherit codex-ide-file-diff-added-face :extend nil))
  "Face used for inline added snippets that must not extend to line end."
  :group 'codex-ide)

(defface codex-ide-diff-removed-snippet-face
  '((t :inherit codex-ide-file-diff-removed-face :extend nil))
  "Face used for inline removed snippets that must not extend to line end."
  :group 'codex-ide)

(defvar codex-ide-session-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map codex-ide-diff-mode-map)
    (define-key map (kbd "g") #'codex-ide-session-diff-refresh)
    (define-key map (kbd "l") #'codex-ide-session-diff-follow-live)
    (define-key map (kbd "t") #'codex-ide-session-diff-follow-transcript)
    (define-key map (kbd "p") #'codex-ide-session-diff-pin-current-turn)
    map)
  "Keymap used in canonical Codex session diff buffers.")

(define-derived-mode codex-ide-session-diff-mode codex-ide-diff-mode
  "Codex-Session-Diff"
  "Major mode for a canonical Codex session diff buffer.

* \\<codex-ide-session-diff-mode-map>\\[codex-ide-session-diff-follow-live] shows the latest or currently running turn.

* \\[codex-ide-session-diff-follow-transcript] follows the turn at point in the session transcript.

* \\[codex-ide-session-diff-pin-current-turn] pins the diff buffer to the turn at point in the session transcript.

* \\[codex-ide-session-diff-refresh] refreshes the current diff source."
  (setq-local header-line-format
              '((:eval codex-ide-session-diff--header-line)))
  (setq-local mode-line-process
              '("[" (:eval (symbol-name codex-ide-session-diff-source)) "]")))

(defvar codex-ide-diff-inline-body-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'codex-ide-diff-goto-source-at-point)
    (define-key map (kbd "<return>") #'codex-ide-diff-goto-source-at-point)
    map)
  "Keymap used on expanded inline Codex diff body text.")

(defun codex-ide-diff--title (diff-text)
  "Return a compact title for DIFF-TEXT."
  (when (string-match
         (rx line-start
             "diff --"
             (? "git")
             " "
             (+ nonl))
         diff-text)
    (let ((line (string-trim (match-string 0 diff-text))))
      (cond
       ((string-match (rx line-start "diff --git "
                          (? "a/")
                          (group (+ (not (any " \n")))))
                      line)
        (match-string 1 line))
       ((string-match (rx line-start "diff -- " (group (+ nonl))) line)
        (match-string 1 line))
       (t line)))))

(defun codex-ide-diff--generated-buffer-name (diff-text)
  "Return a fresh buffer name suitable for DIFF-TEXT."
  (generate-new-buffer-name
   (format "*codex diff: %s*"
           (or (codex-ide-diff--title diff-text)
               "changes"))))

(defun codex-ide-diff-buffer-name-for-session (session-buffer)
  "Return the diff buffer name for SESSION-BUFFER."
  (format "%s-diff"
          (if (bufferp session-buffer)
              (buffer-name session-buffer)
            session-buffer)))

(defun codex-ide-diff-combined-buffer-name-for-session (session-buffer)
  "Return the combined-turn diff buffer name for SESSION-BUFFER."
  (format "%s-turn-diff"
          (if (bufferp session-buffer)
              (buffer-name session-buffer)
            session-buffer)))

(defun codex-ide-diff--line-face (line)
  "Return the face to use for diff LINE."
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

(defun codex-ide-diff--file-sections ()
  "Return top-level file sections in the current diff buffer."
  (cl-remove-if-not
   (lambda (section)
     (eq (codex-ide-section-type section) 'file))
   codex-ide-section--root-sections))

(defun codex-ide-diff--plural (count singular plural)
  "Return SINGULAR or PLURAL for COUNT."
  (if (= count 1) singular plural))

(defun codex-ide-diff--summary-heading (stats)
  "Return the top summary heading for file STATS."
  (let* ((file-count (length stats))
         (added (cl-loop for stat in stats
                         sum (or (plist-get stat :added) 0)))
         (removed (cl-loop for stat in stats
                           sum (or (plist-get stat :removed) 0))))
    (format "%d %s changed, %d %s(+), %d %s(-)"
            file-count
            (codex-ide-diff--plural file-count "file" "files")
            added
            (codex-ide-diff--plural added "insertion" "insertions")
            removed
            (codex-ide-diff--plural removed "deletion" "deletions"))))

(defun codex-ide-diff--section-heading (text &optional face properties)
  "Return TEXT styled as a Codex diff section heading.
When FACE is non-nil, combine it with `bold'.  PROPERTIES are additional text
properties to apply to the heading."
  (let ((heading (copy-sequence text)))
    (dotimes (index (length heading))
      (let ((existing-face (get-text-property index 'face heading)))
        (put-text-property
         index
         (1+ index)
         'face
         (delq nil
               (append (list 'bold face)
                       (if (listp existing-face)
                           existing-face
                         (list existing-face))))
         heading)))
    (when properties
      (add-text-properties 0 (length heading) properties heading))
    heading))

(defun codex-ide-diff--file-stat-segment (count prefix face)
  "Return a propertized file heading stat segment for COUNT."
  (unless (zerop count)
    (propertize (format " %s%d" prefix count) 'face face)))

(defun codex-ide-diff--stat-bar (added removed max-changed)
  "Return a compact proportional stat bar for ADDED and REMOVED lines."
  (let* ((changed (+ added removed))
         (width 36)
         (bar-width (cond
                     ((<= changed 0) 0)
                     ((<= changed width) changed)
                     (t
                      (max 1 (ceiling (* changed width)
                                      (max 1 max-changed))))))
         (added-width (if (> changed 0)
                          (round (* bar-width
                                    (/ (float added) changed)))
                        0))
         (removed-width (- bar-width added-width)))
    (concat (make-string added-width ?+)
            (make-string removed-width ?-))))

(defun codex-ide-diff--insert-stat-line
    (stat path-width changed-width max-changed)
  "Insert one summary line for STAT using PATH-WIDTH and CHANGED-WIDTH."
  (let* ((path (plist-get stat :path))
         (added (or (plist-get stat :added) 0))
         (removed (or (plist-get stat :removed) 0))
         (changed (or (plist-get stat :changed) 0))
         (bar (codex-ide-diff--stat-bar added removed max-changed))
         (added-part (cl-position ?+ bar :test #'char-equal))
         (removed-part (cl-position ?- bar :test #'char-equal)))
    (insert (format (format "%%-%ds | %%%dd " path-width changed-width)
                    path
                    changed))
    (when added-part
      (insert (propertize (substring bar added-part (or removed-part))
                          'face 'codex-ide-diff-added-snippet-face)))
    (when removed-part
      (insert (propertize (substring bar removed-part)
                          'face 'codex-ide-diff-removed-snippet-face)))
    (insert "\n")))

(defun codex-ide-diff--render-summary (files &optional directory)
  "Render a collapsed summary section for parsed FILES."
  (let* ((stats (mapcar (lambda (file)
                          (codex-ide-diff-model-file-stats file directory))
                        files))
         (path-width (cl-loop for stat in stats
                              maximize (length (plist-get stat :path))))
         (max-changed (cl-loop for stat in stats
                               maximize (or (plist-get stat :changed) 0)))
         (changed-width (length (number-to-string (max 0 max-changed)))))
    (codex-ide-section-insert
     'summary
     stats
     (codex-ide-diff--section-heading
      (codex-ide-diff--summary-heading stats))
     (lambda (_section)
       (dolist (stat stats)
         (codex-ide-diff--insert-stat-line
          stat
          path-width
          changed-width
          max-changed)))
     nil)))

(defun codex-ide-diff--file-section-at-point ()
  "Return the file section containing point, or nil."
  (let ((section (codex-ide-section-containing-point)))
    (while (and section
                (not (eq (codex-ide-section-type section) 'file)))
      (setq section (codex-ide-section-parent section)))
    section))

(defun codex-ide-diff-collapse-all-files ()
  "Collapse all file sections in the current Codex diff buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-diff-mode)
    (user-error "Not in a Codex diff buffer"))
  (let ((count 0))
    (dolist (section (codex-ide-diff--file-sections))
      (unless (codex-ide-section-hidden section)
        (codex-ide-section-hide section)
        (setq count (1+ count))))
    (unless (> count 0)
      (message "No file diffs to collapse"))
    count))

(defun codex-ide-diff-expand-all-files ()
  "Expand all collapsed file sections in the current Codex diff buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-diff-mode)
    (user-error "Not in a Codex diff buffer"))
  (dolist (section (codex-ide-diff--file-sections))
    (codex-ide-section-show section)))

(defun codex-ide-diff-toggle-file-at-point ()
  "Toggle the file diff section at point."
  (interactive)
  (unless (derived-mode-p 'codex-ide-diff-mode)
    (user-error "Not in a Codex diff buffer"))
  (let ((section (codex-ide-diff--file-section-at-point)))
    (unless section
      (user-error "No file diff at point"))
    (codex-ide-section-toggle section)))

(defun codex-ide-session-diff-buffer-name-for-session (session-buffer)
  "Return the canonical session diff buffer name for SESSION-BUFFER."
  (format "%s-session-diff"
          (if (bufferp session-buffer)
              (buffer-name session-buffer)
            session-buffer)))

(defun codex-ide-diff--line-index-at-point ()
  "Return the zero-based line index at point in the current buffer."
  (1- (line-number-at-pos)))

(defun codex-ide-diff--insert-line (indexed-line &optional face)
  "Insert INDEXED-LINE with diff styling and source-jump metadata.
When FACE is non-nil, use it instead of deriving a face from the line text."
  (let ((index (car indexed-line))
        (line (cdr indexed-line)))
    (insert (propertize
             line
             'face (or face (codex-ide-diff--line-face line))
             'keymap codex-ide-diff-inline-body-map
             'help-echo "RET jumps to source"
             'codex-ide-diff-line-index index))
    (insert "\n")))

(defun codex-ide-diff--file-heading (file &optional directory)
  "Return a section heading for parsed diff FILE."
  (let* ((path (plist-get file :path))
         (old-path (plist-get file :old-path))
         (stats (codex-ide-diff-model-file-stats file directory))
         (added (or (plist-get stats :added) 0))
         (removed (or (plist-get stats :removed) 0)))
    (concat
     (if (and old-path (not (equal old-path path)))
         (format "%s -> %s" old-path path)
       path)
     (codex-ide-diff--file-stat-segment
      added
      "+"
      'codex-ide-diff-added-snippet-face)
     (codex-ide-diff--file-stat-segment
      removed
      "-"
      'codex-ide-diff-removed-snippet-face))))

(defun codex-ide-diff--render-file (file &optional directory)
  "Render parsed diff FILE as a section."
  (codex-ide-section-insert
   'file
   file
   (codex-ide-diff--section-heading
    (codex-ide-diff--file-heading file directory))
   (lambda (_section)
     (let ((body-only-side
            (codex-ide-diff-model-body-only-side file directory)))
       (dolist (line (plist-get file :header-lines))
         (unless (codex-ide-diff-model-ordinary-file-header-line-p (cdr line))
           (codex-ide-diff--insert-line
            line
            (pcase body-only-side
              ('added 'codex-ide-file-diff-added-face)
              ('removed 'codex-ide-file-diff-removed-face)
              (_ nil))))))
     (dolist (hunk (plist-get file :hunks))
       (let ((header (plist-get hunk :header)))
         (codex-ide-section-insert
          'hunk
          hunk
          (codex-ide-diff--section-heading
           (cdr header)
           nil
           (list 'codex-ide-diff-line-index (car header)))
          (lambda (_hunk-section)
            (dolist (line (plist-get hunk :lines))
              (codex-ide-diff--insert-line line)))))))))

(defun codex-ide-diff--section-identity (section)
  "Return a stable identity for diff SECTION across rerenders."
  (pcase (codex-ide-section-type section)
    ('summary 'summary)
    ('file
     (list 'file (plist-get (codex-ide-section-value section) :path)))
    ('hunk
     (list 'hunk
           (cdr (plist-get (codex-ide-section-value section) :header))))
    (_ (codex-ide-section-type section))))

(defun codex-ide-diff--file-section-path-p (path)
  "Return non-nil when PATH identifies a top-level diff file section."
  (and (consp path)
       (null (cdr path))
       (consp (car path))
       (eq (caar path) 'file)))

(defun codex-ide-diff-fold-new-file-sections-when-any-file-folded-p
    (initial-state)
  "Return non-nil when new file sections should be folded.
INITIAL-STATE is the section view state captured before rerendering.  This
default policy folds newly added file sections once at least one existing file
section was folded before the update."
  (cl-some
   (lambda (entry)
     (and (codex-ide-diff--file-section-path-p (car entry))
          (cdr entry)))
   (alist-get 'hidden initial-state)))

(defcustom codex-ide-diff-new-file-section-fold-predicate
  #'codex-ide-diff-fold-new-file-sections-when-any-file-folded-p
  "Predicate deciding whether newly added diff file sections start folded.
The function is called with the section view state captured before a diff
buffer rerender.  When it returns non-nil, file sections that were not present
in the captured state are folded after the rerender."
  :type 'function
  :group 'codex-ide)

(defun codex-ide-diff--initial-file-section-paths (initial-state)
  "Return file section paths recorded in INITIAL-STATE."
  (let (paths)
    (dolist (entry (alist-get 'hidden initial-state))
      (when (codex-ide-diff--file-section-path-p (car entry))
        (push (car entry) paths)))
    paths))

(defun codex-ide-diff--apply-new-file-section-defaults (initial-state)
  "Apply default fold state to file sections absent from INITIAL-STATE."
  (when (and (functionp codex-ide-diff-new-file-section-fold-predicate)
             (funcall codex-ide-diff-new-file-section-fold-predicate
                      initial-state))
    (let ((initial-paths
           (codex-ide-diff--initial-file-section-paths initial-state)))
      (dolist (section (codex-ide-diff--file-sections))
        (let ((path (codex-ide-section-path
                     section
                     #'codex-ide-diff--section-identity)))
          (unless (member path initial-paths)
            (codex-ide-section-hide section)))))))

(defun codex-ide-session-diff--empty-message-p (text)
  "Return non-nil when TEXT is a session diff empty-state message."
  (and (stringp text)
       (string-match-p (rx line-start "# " (+ nonl) "\n"
                           "# Press ")
                       text)))

(defun codex-ide-session-diff--header-segment (text &optional face)
  "Return TEXT propertized for a session diff header segment.
When FACE is non-nil, combine it with `codex-ide-header-line-face'."
  (propertize text
              'face (if face
                        (list face 'codex-ide-header-line-face)
                      'codex-ide-header-line-face)))

(defun codex-ide-session-diff--header-diffstat (display-text &optional directory)
  "Return a propertized diffstat segment for DISPLAY-TEXT."
  (let* ((files (and (stringp display-text)
                     (codex-ide-diff-model-group-files-by-path
                      (codex-ide-diff-model-parse-files display-text))))
         (file-count (length files)))
    (if (zerop file-count)
        (codex-ide-session-diff--header-segment "no changes")
      (let* ((stats (mapcar (lambda (file)
                              (codex-ide-diff-model-file-stats file directory))
                            files))
             (added (cl-loop for stat in stats
                             sum (or (plist-get stat :added) 0)))
             (removed (cl-loop for stat in stats
                               sum (or (plist-get stat :removed) 0))))
        (concat
         (codex-ide-session-diff--header-segment
          (format "%d %s "
                  file-count
                  (codex-ide-diff--plural file-count "file" "files")))
         (codex-ide-session-diff--header-segment
          (format "+%d" added)
          'codex-ide-file-diff-added-face)
         (codex-ide-session-diff--header-segment " ")
         (codex-ide-session-diff--header-segment
          (format "-%d" removed)
          'codex-ide-file-diff-removed-face))))))

(defun codex-ide-session-diff--source-label (source)
  "Return a user-facing label for session diff SOURCE."
  (pcase source
    ('live "Live diff")
    ('pinned "Pinned diff")
    ('transcript "Turn-at-point diff")
    (_ (capitalize (format "%s" source)))))

(defun codex-ide-session-diff--source-detail (session source turn-id)
  "Return a user-facing detail string for SESSION diff SOURCE and TURN-ID."
  (pcase source
    ('live
     (if (and session (codex-ide-session-current-turn-id session))
         "running turn"
       "latest turn"))
    ('pinned
     (if turn-id
         (format "turn %s" (codex-ide-short-turn-id turn-id))
       "no pinned turn"))
    ('transcript
     (if turn-id
         (format "turn %s" (codex-ide-short-turn-id turn-id))
       "no turn at point"))
    (_ nil)))

(defun codex-ide-session-diff--format-header-line
    (session source turn-id display-text &optional directory)
  "Return the session diff header line for SESSION SOURCE TURN-ID DISPLAY-TEXT."
  (let ((segments
         (delq nil
               (list
                (codex-ide-session-diff--header-segment
                 (codex-ide-session-diff--source-label source))
                (when-let* ((detail
                             (codex-ide-session-diff--source-detail
                              session
                              source
                              turn-id)))
                  (codex-ide-session-diff--header-segment detail))
                (and (eq source 'transcript)
                     (codex-ide-session-diff--header-segment "follows point"))
                (codex-ide-session-diff--header-diffstat
                 display-text
                 directory)))))
    (concat
     (codex-ide-session-diff--header-segment " ")
     (mapconcat #'identity
                segments
                (codex-ide-session-diff--header-segment " | ")))))

(defun codex-ide-diff--render-text-1 (raw-text display-text directory)
  "Render RAW-TEXT and DISPLAY-TEXT in the current Codex diff buffer."
  (let ((files (codex-ide-diff-model-group-files-by-path
                (codex-ide-diff-model-parse-files display-text))))
    (setq-local codex-ide-diff--raw-text raw-text)
    (setq-local codex-ide-diff--display-text display-text)
    (setq-local codex-ide-diff--directory directory)
    (codex-ide-section-reset)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if files
          (progn
            (codex-ide-diff--render-summary files directory)
            (insert "\n")
            (dolist (file files)
              (codex-ide-diff--render-file file directory)))
        (insert (if (codex-ide-session-diff--empty-message-p display-text)
                    (propertize (string-trim-right display-text)
                                'face 'font-lock-comment-face)
                  (string-trim-right display-text)))
        (insert "\n"))
      (setq-local buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(defun codex-ide-diff--render-text (raw-text display-text directory)
  "Render RAW-TEXT and DISPLAY-TEXT while preserving matching section state."
  (let ((initial-state
         (codex-ide-section-capture-view-state
          #'codex-ide-diff--section-identity)))
    (prog1 (codex-ide-diff--render-text-1 raw-text display-text directory)
      (codex-ide-section-restore-view-state
       initial-state
       #'codex-ide-diff--section-identity)
      (codex-ide-diff--apply-new-file-section-defaults initial-state))))

(defun codex-ide-diff-goto-source (diff-text line-index &optional directory)
  "Jump from DIFF-TEXT LINE-INDEX to the corresponding source location.
DIRECTORY is used to resolve relative diff paths."
  (let* ((location (codex-ide-diff-model-source-location-for-line
                    diff-text
                    line-index))
         (path (plist-get location :path))
         (line (plist-get location :line)))
    (unless (and path line)
      (user-error "No source location for this diff line"))
    (let ((file (expand-file-name path (or directory default-directory))))
      (unless (file-exists-p file)
        (user-error "Source file does not exist: %s" file))
      (find-file-other-window file)
      (goto-char (point-min))
      (forward-line (1- line))
      (back-to-indentation)
      (point))))

(defun codex-ide-diff-goto-source-at-point (&optional pos)
  "Jump to the source location corresponding to the diff line at POS."
  (interactive)
  (let* ((pos (or pos (point)))
         (property-pos (if (> pos (point-min)) (1- pos) pos))
         (overlay (or (get-char-property pos 'codex-ide-diff-overlay)
                      (get-char-property property-pos 'codex-ide-diff-overlay)))
         (rendered-line-index
          (or (get-text-property pos 'codex-ide-diff-line-index)
              (get-text-property property-pos 'codex-ide-diff-line-index)))
         (body-start (and (overlayp overlay)
                          (overlay-get overlay :body-start)))
         (diff-text (or (and (overlayp overlay)
                             (overlay-get overlay :result-full-text))
                        codex-ide-diff--raw-text
                        (and (overlayp overlay)
                             (overlay-get overlay :display-text))
                        codex-ide-diff--display-text
                        (buffer-substring-no-properties
                         (point-min)
                         (point-max))))
         (directory (or (and (overlayp overlay)
                             (overlay-get overlay :directory))
                        codex-ide-diff--directory
                        default-directory))
         (line-index (cond
                      ((integerp rendered-line-index) rendered-line-index)
                      ((and (markerp body-start)
                            (eq (marker-buffer body-start)
                                (current-buffer)))
                       (save-excursion
                         (goto-char pos)
                         (count-lines (marker-position body-start)
                                      (line-beginning-position))))
                      (t
                       (save-excursion
                         (goto-char pos)
                         (codex-ide-diff--line-index-at-point))))))
    (codex-ide-diff-goto-source diff-text line-index directory)))

(cl-defun codex-ide-diff-open-buffer
    (diff-text &optional buffer-name directory &key (select nil select-specified-p))
  "Display DIFF-TEXT in a dedicated `codex-ide-diff-mode' buffer.
When BUFFER-NAME is non-nil, reuse that buffer.
DIRECTORY is used as the buffer's `default-directory' for source jumps.
When SELECT is non-nil, select the displayed diff window.  When SELECT is
omitted, use `codex-ide-display-buffer' default selection behavior.
Return the created buffer."
  (unless (and (stringp diff-text)
               (not (string-empty-p (string-trim diff-text))))
    (user-error "No diff text available"))
  (let* ((display-text
          (codex-ide-diff-data-display-text diff-text directory))
         (buffer (if buffer-name
                     (get-buffer-create buffer-name)
                   (generate-new-buffer
                    (codex-ide-diff--generated-buffer-name display-text)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (when directory
          (setq-local default-directory (file-name-as-directory directory)))
        (codex-ide-diff-mode)
        (codex-ide-diff--render-text diff-text display-text directory)))
    (if select-specified-p
        (codex-ide-display-buffer
         buffer
         codex-ide--display-buffer-other-window-pop-up-action
         :select select)
      (codex-ide-display-buffer
       buffer
       codex-ide--display-buffer-other-window-pop-up-action))
    buffer))

(defun codex-ide-session-diff--empty-message (_source _turn-id message)
  "Return empty-state text for MESSAGE."
  (string-join
   (delq nil
         (list (format "# %s" message)
               (concat "# "
                       (substitute-command-keys
                        "Press \\[describe-mode] for help."))))
   "\n"))

(defun codex-ide-session-diff--target-turn-id (source)
  "Return the turn id to render under SOURCE."
  (pcase source
    ('live nil)
    ((or 'transcript 'pinned) codex-ide-session-diff--turn-id)
    (_ nil)))

(defun codex-ide-session-diff--session-buffer-turn-id (session)
  "Return SESSION's turn id at point in its session buffer, if any."
  (when-let* ((session-buffer (and session (codex-ide-session-buffer session))))
    (when (buffer-live-p session-buffer)
      (with-current-buffer session-buffer
        (codex-ide-diff-data-turn-id-at-point
         session
         (point)
         session-buffer)))))

(defun codex-ide-session-diff--render (session source turn-id)
  "Render SESSION diff for SOURCE and TURN-ID in the current buffer."
  (let* ((diff-text
          (if (and (not (eq source 'live))
                   (not turn-id))
              (codex-ide-session-diff--empty-message
               source
               nil
               (pcase source
                 ('transcript "No prompt at transcript position")
                 ('pinned "No pinned turn selected")
                 (_ "No turn selected")))
            (condition-case err
                (codex-ide-diff-data-combined-turn-diff-text session turn-id)
              (user-error
               (codex-ide-session-diff--empty-message
                source
                turn-id
                (error-message-string err))))))
         (directory (and session (codex-ide-session-directory session)))
         (display-text
          (codex-ide-diff-data-display-text diff-text directory)))
    (setq-local codex-ide-session-diff--header-line
                (codex-ide-session-diff--format-header-line
                 session
                 source
                 turn-id
                 display-text
                 directory))
    (let ((inhibit-read-only t))
      (when directory
        (setq-local default-directory (file-name-as-directory directory)))
      (codex-ide-diff--render-text diff-text display-text directory))))

(defun codex-ide-session-diff-refresh (&optional buffer)
  "Refresh BUFFER, or the current canonical session diff buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (unless (eq major-mode 'codex-ide-session-diff-mode)
        (user-error "Not in a Codex session diff buffer"))
      (unless (codex-ide-session-p codex-ide-session-diff--session)
        (user-error "No Codex session associated with this diff buffer"))
      (codex-ide-session-diff--render
       codex-ide-session-diff--session
       codex-ide-session-diff-source
       (codex-ide-session-diff--target-turn-id
        codex-ide-session-diff-source)))))

(defun codex-ide-session-diff--buffer-for-session (session)
  "Return the existing canonical diff buffer for SESSION, if any."
  (when-let* ((session-buffer (and session (codex-ide-session-buffer session))))
    (get-buffer (codex-ide-session-diff-buffer-name-for-session
                 session-buffer))))

(defun codex-ide-session-diff--kill-for-session-buffer ()
  "Kill the canonical diff buffer associated with the current session buffer."
  (when (and (boundp 'codex-ide--session)
             (codex-ide-session-p codex-ide--session))
    (let ((session codex-ide--session))
      (when-let* ((diff-buffer
                   (codex-ide-session-diff--buffer-for-session session)))
        (when (buffer-live-p diff-buffer)
          (with-current-buffer diff-buffer
            (when (eq codex-ide-session-diff--session session)
              (kill-buffer diff-buffer))))))))

(defun codex-ide-session-diff--install-session-kill-hook (session)
  "Arrange for SESSION's canonical diff buffer to die with its session buffer."
  (when-let* ((session-buffer (and session (codex-ide-session-buffer session))))
    (when (buffer-live-p session-buffer)
      (with-current-buffer session-buffer
        (add-hook 'kill-buffer-hook
                  #'codex-ide-session-diff--kill-for-session-buffer
                  nil
                  t)))))

;;;###autoload
(defun codex-ide-session-diff-open (&optional session)
  "Open or reuse the canonical session diff buffer for SESSION."
  (interactive)
  (let* ((session (or session (codex-ide--session-for-current-project)))
         (session-buffer (and session (codex-ide-session-buffer session))))
    (unless session
      (user-error "No Codex session available"))
    (let ((buffer (get-buffer-create
                   (codex-ide-session-diff-buffer-name-for-session
                    (or session-buffer "*codex*")))))
      (with-current-buffer buffer
        (unless (eq major-mode 'codex-ide-session-diff-mode)
          (codex-ide-session-diff-mode))
        (setq-local codex-ide-session-diff--session session)
        (setq-local codex-ide-session-diff-source
                    (or codex-ide-session-diff-source 'live))
        (codex-ide-session-diff-refresh buffer))
      (codex-ide-session-diff--install-session-kill-hook session)
      (codex-ide-display-buffer
       buffer
       codex-ide--display-buffer-other-window-pop-up-action)
      buffer)))

(defun codex-ide-session-diff-follow-live ()
  "Show the latest or currently running turn in this session diff buffer."
  (interactive)
  (setq-local codex-ide-session-diff-source 'live)
  (setq-local codex-ide-session-diff--turn-id nil)
  (codex-ide-session-diff-refresh))

(defun codex-ide-session-diff-follow-transcript (&optional turn-id)
  "Show TURN-ID in this session diff buffer and follow transcript selection."
  (interactive)
  (setq-local codex-ide-session-diff-source 'transcript)
  (setq-local codex-ide-session-diff--turn-id
              (or turn-id
                  (codex-ide-session-diff--session-buffer-turn-id
                   codex-ide-session-diff--session)
                  codex-ide-session-diff--turn-id))
  (codex-ide-session-diff-refresh))

(defun codex-ide-session-diff-pin-current-turn (&optional turn-id)
  "Pin this session diff buffer to TURN-ID."
  (interactive)
  (setq-local codex-ide-session-diff-source 'pinned)
  (setq-local codex-ide-session-diff--turn-id
              (or turn-id
                  (codex-ide-session-diff--session-buffer-turn-id
                   codex-ide-session-diff--session)
                  codex-ide-session-diff--turn-id))
  (codex-ide-session-diff-refresh))

(defun codex-ide-session-diff-transcript-point-changed
    (session turn-id)
  "Notify SESSION's canonical diff buffer that transcript point is at TURN-ID."
  (when-let* ((buffer (codex-ide-session-diff--buffer-for-session session)))
    (with-current-buffer buffer
      (when (and (eq codex-ide-session-diff-source 'transcript)
                 (not (equal codex-ide-session-diff--turn-id turn-id)))
        (setq-local codex-ide-session-diff--turn-id turn-id)
        (codex-ide-session-diff-refresh buffer)))))

(defun codex-ide-session-diff-note-session-updated (session)
  "Refresh SESSION's canonical diff buffer when its source should update."
  (when-let* ((buffer (codex-ide-session-diff--buffer-for-session session)))
    (with-current-buffer buffer
      (when (or (eq codex-ide-session-diff-source 'live)
                (and (eq codex-ide-session-diff-source 'transcript)
                     (equal codex-ide-session-diff--turn-id
                            (codex-ide-session-current-turn-id session))))
        (codex-ide-session-diff-refresh buffer)))))

;;;###autoload
(defun codex-ide-diff-open-combined-turn-buffer (&optional session turn-id)
  "Open the combined diff for SESSION TURN-ID in a standalone diff buffer.
When called interactively with nil TURN-ID, use the last transcript turn at or
above point.  Otherwise, when TURN-ID is nil, prefer the running turn and
otherwise use the most recent completed turn."
  (interactive
   (let ((session (codex-ide--session-for-current-project)))
     (list session
           (codex-ide-diff-data-turn-id-at-point
            session
            (point)
            (current-buffer)))))
  (let* ((session (or session (codex-ide--session-for-current-project)))
         (buffer (and session (codex-ide-session-buffer session)))
         (diff-text (codex-ide-diff-data-combined-turn-diff-text
                     session
                     turn-id)))
    (unless session
      (user-error "No Codex session available"))
    (codex-ide-diff-open-buffer
     diff-text
     (codex-ide-diff-combined-buffer-name-for-session
      (or buffer "*codex*"))
     (and session (codex-ide-session-directory session)))))

(provide 'codex-ide-diff-view)

;;; codex-ide-diff-view.el ends here
