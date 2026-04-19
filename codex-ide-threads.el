;;; codex-ide-threads.el --- Stored thread selection helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns logic for presenting stored Codex threads to the user and
;; turning raw app-server thread metadata into completion candidates.
;;
;; Responsibilities kept here:
;;
;; - Formatting timestamps and previews for thread choices.
;; - Filtering thread lists for resume/continue flows.
;; - Building completion candidates and affixation metadata.
;; - Selecting the latest or an explicitly chosen thread.
;;
;; This code is intentionally separate from protocol transport and session
;; lifecycle.  The protocol module fetches raw thread data; the session module
;; decides when resume flows should happen; this module turns the raw data into
;; UI-level selection behavior.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'codex-ide-core)
(require 'codex-ide-context)
(require 'codex-ide-protocol)
(require 'codex-ide-utils)

(defun codex-ide--format-thread-updated-at (updated-at)
  "Format UPDATED-AT for thread labels."
  (cond
   ((numberp updated-at)
    (codex-ide-human-time-ago updated-at))
   ((stringp updated-at)
    (or (codex-ide-human-time-ago updated-at)
        updated-at))
   (t "")))

(defun codex-ide--thread-choice-preview (value)
  "Format thread preview VALUE for completion labels."
  (let* ((text (or value ""))
         (stripped (codex-ide--strip-emacs-context-prefix text)))
    (string-trim
     (if (and (stringp text)
              (codex-ide--leading-emacs-context-prefix-p text)
              (equal stripped text))
         ""
       stripped))))

(defun codex-ide--thread-choice-short-id (thread)
  "Return a short id for THREAD."
  (let ((thread-id (alist-get 'id thread)))
    (substring thread-id 0 (min 8 (length thread-id)))))

(defun codex-ide--thread-choice-candidates (threads)
  "Return completion candidates alist for THREADS."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (thread threads)
      (let* ((raw (or (alist-get 'name thread)
                      (alist-get 'preview thread)
                      "Untitled"))
             (preview (codex-ide--thread-choice-preview raw))
             (candidate (if (string-empty-p preview) "Untitled" preview)))
        (puthash candidate (1+ (gethash candidate counts 0)) counts)))
    (mapcar
     (lambda (thread)
       (let* ((raw (or (alist-get 'name thread)
                       (alist-get 'preview thread)
                       "Untitled"))
              (preview (codex-ide--thread-choice-preview raw))
              (candidate (if (string-empty-p preview) "Untitled" preview)))
         (cons (if (> (gethash candidate counts 0) 1)
                   (format "%s [%s]" candidate
                           (codex-ide--thread-choice-short-id thread))
                 candidate)
               thread)))
     threads)))

(defun codex-ide--thread-choice-affixation (candidates choices)
  "Return affixation data for CANDIDATES using CHOICES."
  (mapcar
   (lambda (candidate)
     (let ((thread (cdr (assoc candidate choices))))
       (list candidate
             (format "%s " (codex-ide--format-thread-updated-at
                            (alist-get 'createdAt thread)))
             (format " [%s]" (codex-ide--thread-choice-short-id thread)))))
   candidates))

(defun codex-ide--thread-list-data (&optional session omit-thread-id)
  "Return thread list data using SESSION.
When OMIT-THREAD-ID is non-nil, exclude that thread from the result."
  (seq-remove
   (lambda (thread)
     (equal (alist-get 'id thread) omit-thread-id))
   (codex-ide--list-threads session)))

(defun codex-ide--pick-thread (&optional session omit-thread-id)
  "Prompt to select a thread for the current working directory using SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (unless session
    (error "No Codex session available"))
  (let* ((working-dir (codex-ide-session-directory session))
         (threads (codex-ide--thread-list-data session omit-thread-id))
         (choices (codex-ide--thread-choice-candidates threads))
         (completion-extra-properties
          `(:affixation-function
            ,(lambda (candidates)
               (codex-ide--thread-choice-affixation candidates choices))
            :display-sort-function identity
            :cycle-sort-function identity)))
    (unless choices
      (user-error "%s for %s"
                  (if omit-thread-id
                      "No other Codex threads found"
                    "No Codex threads found")
                  (abbreviate-file-name working-dir)))
    (cdr (assoc (completing-read "Resume Codex thread: " choices nil t)
                choices))))

(defun codex-ide--latest-thread (&optional session)
  "Return the most recent thread for the current working directory using SESSION."
  (car (codex-ide--list-threads session)))

(provide 'codex-ide-threads)

;;; codex-ide-threads.el ends here
