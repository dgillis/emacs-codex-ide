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

(require 'diff-mode)
(require 'codex-ide-diff-data)
(require 'subr-x)

(declare-function codex-ide-display-buffer "codex-ide-window"
                  (buffer &optional action))
(declare-function codex-ide--session-for-current-project "codex-ide-session" ())
(declare-function codex-ide-session-buffer "codex-ide-core" (session))
(declare-function codex-ide-diff-data-combined-turn-diff-text
                  "codex-ide-diff-data" (session &optional turn-id))

(defvar codex-ide--display-buffer-other-window-pop-up-action)

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

(defun codex-ide-diff-open-buffer (diff-text &optional buffer-name)
  "Display DIFF-TEXT in a dedicated `diff-mode' buffer.
When BUFFER-NAME is non-nil, reuse that buffer.
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
        (erase-buffer)
        (insert (string-trim-right diff-text))
        (insert "\n")
        (diff-mode)
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
When TURN-ID is nil, prefer the running turn and otherwise use the most recent
completed turn."
  (interactive)
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
      (or buffer "*codex*")))))

(provide 'codex-ide-diff-view)

;;; codex-ide-diff-view.el ends here
