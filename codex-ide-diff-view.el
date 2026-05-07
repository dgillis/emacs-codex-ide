;;; codex-ide-diff-view.el --- Diff buffer views for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns dedicated diff-buffer presentation for Codex file changes.
;;
;; It turns Codex-provided patch text into a standalone `diff-mode' buffer and
;; displays that buffer using codex-ide's normal window policy.  Keeping this
;; separate from transcript control lets the transcript layer stay focused on
;; when a diff should be offered while this module owns how the diff is shown.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'codex-ide-diff-data)
(require 'subr-x)

(declare-function codex-ide-display-buffer "codex-ide-window"
                  (buffer &optional action))
(declare-function codex-ide--session-for-current-project "codex-ide-session" ())
(declare-function codex-ide-session-buffer "codex-ide-core" (session))
(declare-function codex-ide-session-directory "codex-ide-core" (session))
(declare-function codex-ide-diff-data-combined-turn-diff-text
                  "codex-ide-diff-data" (session &optional turn-id))
(declare-function codex-ide-diff-data-turn-id-at-point
                  "codex-ide-diff-data" (session &optional point buffer))

(defvar codex-ide--display-buffer-other-window-pop-up-action)

(defvar codex-ide-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map diff-mode-map)
    (define-key map (kbd "RET") #'codex-ide-diff-goto-source-at-point)
    (define-key map (kbd "<return>") #'codex-ide-diff-goto-source-at-point)
    map)
  "Keymap used in standalone Codex diff buffers.")

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

(defun codex-ide-diff--strip-path-prefix (path)
  "Return PATH without a leading diff-side prefix."
  (cond
   ((not (stringp path)) nil)
   ((or (string-prefix-p "a/" path)
        (string-prefix-p "b/" path))
    (substring path 2))
   (t path)))

(defun codex-ide-diff--parse-new-start (hunk-line)
  "Return the new-file start line from HUNK-LINE, or nil."
  (when (string-match
         (rx line-start "@@"
             (+ (not (any "+")))
             "+"
             (group (+ digit)))
         hunk-line)
    (string-to-number (match-string 1 hunk-line))))

(defun codex-ide-diff--source-location-for-line (diff-text line-index)
  "Return source location for zero-based LINE-INDEX in DIFF-TEXT.
The returned value is a plist containing `:path' and `:line', or nil when the
diff line has no corresponding source location."
  (let ((lines (split-string diff-text "\n"))
        current-path
        current-new-line
        location)
    (cl-loop
     for line in lines
     for index from 0
     until (> index line-index)
     do
     (cond
      ((string-match
        (rx line-start "diff --git " (? "a/")
            (+ (not (any " \n")))
            (+ space) (? "b/")
            (group (+ (not (any " \n")))))
        line)
       (setq current-path (codex-ide-diff--strip-path-prefix
                           (match-string 1 line)))
       (setq current-new-line nil)
       (when (= index line-index)
         (setq location nil)))
      ((string-match
        (rx line-start "+++" (+ space)
            (group (+ (not (any "\n")))))
        line)
       (let ((path (match-string 1 line)))
         (unless (equal path "/dev/null")
           (setq current-path (codex-ide-diff--strip-path-prefix path))))
       (when (= index line-index)
         (setq location nil)))
      ((string-prefix-p "@@" line)
       (setq current-new-line
             (or (codex-ide-diff--parse-new-start line)
                 current-new-line))
       (when (= index line-index)
         (setq location nil)))
      ((and current-path current-new-line
            (not (string-prefix-p "\\ No newline" line)))
       (let ((target-line current-new-line))
         (cond
          ((string-prefix-p "+" line)
           (setq current-new-line (1+ current-new-line)))
          ((string-prefix-p "-" line)
           nil)
          (t
           (setq current-new-line (1+ current-new-line))))
         (when (= index line-index)
           (setq location
                 (list :path current-path
                       :line (max 1 target-line))))))))
    location))

(defun codex-ide-diff--line-index-at-point ()
  "Return the zero-based line index at point in the current buffer."
  (1- (line-number-at-pos)))

(defun codex-ide-diff-goto-source (diff-text line-index &optional directory)
  "Jump from DIFF-TEXT LINE-INDEX to the corresponding source location.
DIRECTORY is used to resolve relative diff paths."
  (let* ((location (codex-ide-diff--source-location-for-line
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
         (overlay (get-char-property pos 'codex-ide-diff-overlay))
         (body-start (and (overlayp overlay)
                          (overlay-get overlay :body-start)))
         (diff-text (or (and (overlayp overlay)
                             (overlay-get overlay :display-text))
                        (buffer-substring-no-properties
                         (point-min)
                         (point-max))))
         (directory (or (and (overlayp overlay)
                             (overlay-get overlay :directory))
                        default-directory))
         (line-index (if (and (markerp body-start)
                              (eq (marker-buffer body-start)
                                  (current-buffer)))
                         (save-excursion
                           (goto-char pos)
                           (count-lines (marker-position body-start)
                                        (line-beginning-position)))
                       (save-excursion
                         (goto-char pos)
                         (codex-ide-diff--line-index-at-point)))))
    (codex-ide-diff-goto-source diff-text line-index directory)))

(defun codex-ide-diff-open-buffer (diff-text &optional buffer-name directory)
  "Display DIFF-TEXT in a dedicated `diff-mode' buffer.
When BUFFER-NAME is non-nil, reuse that buffer.
DIRECTORY is used as the buffer's `default-directory' for source jumps.
Return the created buffer."
  (unless (and (stringp diff-text)
               (not (string-empty-p (string-trim diff-text))))
    (user-error "No diff text available"))
  (let ((buffer (if buffer-name
                    (get-buffer-create buffer-name)
                  (generate-new-buffer
                   (codex-ide-diff--generated-buffer-name diff-text)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (when directory
          (setq-local default-directory (file-name-as-directory directory)))
        (erase-buffer)
        (insert (string-trim-right diff-text))
        (insert "\n")
        (diff-mode)
        (use-local-map codex-ide-diff-mode-map)
        (setq-local buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))))
    (codex-ide-display-buffer
     buffer
     codex-ide--display-buffer-other-window-pop-up-action)
    buffer))

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
