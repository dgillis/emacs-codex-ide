;;; codex-ide-diff-model.el --- Parsed diff model helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns the parsed representation of Codex diff text.  It keeps
;; section parsing, body-only file classification, diff stats, and source
;; location mapping separate from buffer rendering.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun codex-ide-diff-model-strip-path-prefix (path)
  "Return PATH without a leading diff-side prefix."
  (cond
   ((not (stringp path)) nil)
   ((or (string-prefix-p "a/" path)
        (string-prefix-p "b/" path))
    (substring path 2))
   (t path)))

(defun codex-ide-diff-model-line-file-start-p
    (lines index &optional git-only)
  "Return non-nil when LINES at INDEX starts a file diff.
When GIT-ONLY is non-nil, only recognize `diff --git' headers."
  (let ((line (nth index lines))
        (next (nth (1+ index) lines)))
    (or (and line (string-prefix-p "diff --git " line))
        (and (not git-only)
             line next
             (string-prefix-p "--- " line)
             (string-prefix-p "+++ " next)))))

(defun codex-ide-diff-model-path-from-diff-git-line (line)
  "Return the new path from a diff --git LINE, or nil."
  (when (string-match
         (rx line-start "diff --git " (? "a/")
             (+ (not (any " \n")))
             (+ space) (? "b/")
             (group (+ (not (any " \n")))))
         line)
    (codex-ide-diff-model-strip-path-prefix (match-string 1 line))))

(defun codex-ide-diff-model-path-from-header-line (line)
  "Return the path from a --- or +++ header LINE, or nil."
  (when (string-match
         (rx line-start (or "---" "+++") (+ space)
             (group (+ (not (any "\t\n")))))
         line)
    (let ((path (match-string 1 line)))
      (unless (equal path "/dev/null")
        (codex-ide-diff-model-strip-path-prefix path)))))

(defun codex-ide-diff-model--parse-file-section (lines start)
  "Parse a file diff from LINES starting at START.
Return a cons of the parsed file plist and the next line index."
  (let ((index start)
        (line-count (length lines))
        header-lines
        hunks
        path
        old-path
        old-null
        new-null
        (git-block (string-prefix-p "diff --git " (nth start lines))))
    (while (and (< index line-count)
                (not (and (> index start)
                          (codex-ide-diff-model-line-file-start-p
                           lines
                           index
                           git-block)))
                (not (string-prefix-p "@@" (nth index lines))))
      (let ((line (nth index lines)))
        (push (cons index line) header-lines)
        (cond
         ((string-prefix-p "diff --git " line)
          (setq path (or (codex-ide-diff-model-path-from-diff-git-line line)
                         path)))
         ((string-prefix-p "--- " line)
          (setq old-null
                (or old-null
                    (string-match-p (rx line-start "---" (+ space) "/dev/null")
                                    line)))
          (setq old-path (or (codex-ide-diff-model-path-from-header-line line)
                             old-path)))
         ((string-prefix-p "+++ " line)
          (setq new-null
                (or new-null
                    (string-match-p (rx line-start "+++" (+ space) "/dev/null")
                                    line)))
          (setq path (or (codex-ide-diff-model-path-from-header-line line)
                         path))))
        (setq index (1+ index))))
    (while (and (< index line-count)
                (not (codex-ide-diff-model-line-file-start-p
                      lines
                      index
                      git-block)))
      (let ((line (nth index lines)))
        (if (string-prefix-p "@@" line)
            (let ((hunk-header (cons index line))
                  body-lines)
              (setq index (1+ index))
              (while (and (< index line-count)
                          (not (codex-ide-diff-model-line-file-start-p
                                lines
                                index
                                git-block))
                          (not (string-prefix-p "@@" (nth index lines))))
                (push (cons index (nth index lines)) body-lines)
                (setq index (1+ index)))
              (push (list :header hunk-header
                          :lines (nreverse body-lines))
                    hunks))
          (push (cons index line) header-lines)
          (setq index (1+ index)))))
    (cons (list :path (or path old-path "changes")
                :old-path old-path
                :old-null old-null
                :new-null new-null
                :body-only (null hunks)
                :header-lines (nreverse header-lines)
                :hunks (nreverse hunks))
          index)))

(defun codex-ide-diff-model-parse-files (diff-text)
  "Return parsed file sections from DIFF-TEXT."
  (let ((lines (split-string diff-text "\n"))
        (index 0)
        files)
    (while (< index (length lines))
      (if (codex-ide-diff-model-line-file-start-p lines index)
          (let ((parsed (codex-ide-diff-model--parse-file-section lines index)))
            (push (car parsed) files)
            (setq index (cdr parsed)))
        (setq index (1+ index))))
    (nreverse files)))

(defun codex-ide-diff-model-group-files-by-path (files)
  "Return FILES grouped by display path while preserving hunk order."
  (let (grouped)
    (dolist (file files)
      (let* ((path (plist-get file :path))
             (existing
              (cl-find path grouped
                       :key (lambda (candidate)
                              (plist-get candidate :path))
                       :test #'equal)))
        (if existing
            (setf (plist-get existing :hunks)
                  (append (plist-get existing :hunks)
                          (plist-get file :hunks)))
          (push (copy-tree file) grouped))))
    (nreverse grouped)))

(defun codex-ide-diff-model-ordinary-file-header-line-p (line)
  "Return non-nil when LINE is redundant in the section diff view."
  (or (string-prefix-p "diff --git " line)
      (string-prefix-p "--- " line)
      (string-prefix-p "+++ " line)
      (string-prefix-p "index " line)))

(defun codex-ide-diff-model-body-only-line-count (file)
  "Return visible body line count for body-only FILE."
  (let ((count 0))
    (dolist (indexed-line (plist-get file :header-lines))
      (unless (codex-ide-diff-model-ordinary-file-header-line-p
               (cdr indexed-line))
        (setq count (1+ count))))
    count))

(defun codex-ide-diff-model-body-only-side (file &optional directory)
  "Return the side represented by body-only FILE, either `added' or `removed'."
  (when (or (plist-get file :body-only)
            (null (plist-get file :hunks)))
    (cond
     ((plist-get file :old-null) 'added)
     ((plist-get file :new-null) 'removed)
     ((let ((path (plist-get file :path)))
        (and (stringp path)
             (not (string-empty-p path))
             (file-exists-p
              (expand-file-name path (or directory default-directory)))))
      'added)
     (t 'removed))))

(defun codex-ide-diff-model-file-stats (file &optional directory)
  "Return a plist summarizing additions and deletions in parsed FILE."
  (let ((added 0)
        (removed 0))
    (pcase (codex-ide-diff-model-body-only-side file directory)
      ('added
       (setq added (codex-ide-diff-model-body-only-line-count file)))
      ('removed
       (setq removed (codex-ide-diff-model-body-only-line-count file))))
    (dolist (hunk (plist-get file :hunks))
      (dolist (indexed-line (plist-get hunk :lines))
        (let ((line (cdr indexed-line)))
          (cond
           ((and (string-prefix-p "+" line)
                 (not (string-prefix-p "+++" line)))
            (setq added (1+ added)))
           ((and (string-prefix-p "-" line)
                 (not (string-prefix-p "---" line)))
            (setq removed (1+ removed)))))))
    (list :path (plist-get file :path)
          :added added
          :removed removed
          :changed (+ added removed))))

(defun codex-ide-diff-model--parse-new-start (hunk-line)
  "Return the new-file start line from HUNK-LINE, or nil."
  (when (string-match
         (rx line-start "@@"
             (+ (not (any "+")))
             "+"
             (group (+ digit)))
         hunk-line)
    (string-to-number (match-string 1 hunk-line))))

(defun codex-ide-diff-model-source-location-for-line (diff-text line-index)
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
       (setq current-path (codex-ide-diff-model-strip-path-prefix
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
           (setq current-path (codex-ide-diff-model-strip-path-prefix path))
           (setq current-new-line 1)))
       (when (= index line-index)
         (setq location nil)))
      ((string-prefix-p "@@" line)
       (setq current-new-line
             (or (codex-ide-diff-model--parse-new-start line)
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

(provide 'codex-ide-diff-model)

;;; codex-ide-diff-model.el ends here
