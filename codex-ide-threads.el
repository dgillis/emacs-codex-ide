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

(defun codex-ide--agent-root-thread (session current-thread-id)
  "Return the root agent thread above CURRENT-THREAD-ID using SESSION."
  (let ((seen (make-hash-table :test #'equal))
        (thread-id current-thread-id)
        (thread nil))
    (while thread-id
      (when (gethash thread-id seen)
        (error "Invalid agent tree: cyclic parent for thread %s" thread-id))
      (puthash thread-id t seen)
      (setq thread (alist-get 'thread
                              (codex-ide--read-thread session thread-id nil)))
      (unless (and (listp thread)
                   (stringp (alist-get 'id thread)))
        (error "Unable to read Codex thread %s" thread-id))
      (let ((parent-thread-id (alist-get 'parentThreadId thread)))
        (setq thread-id
              (and (stringp parent-thread-id)
                   (not (string-empty-p parent-thread-id))
                   parent-thread-id))))
    thread))

(defun codex-ide--agent-descendant-filter-unsupported-error-p (err)
  "Return non-nil when ERR means descendant thread filtering is unsupported."
  (let ((message (downcase (error-message-string err))))
    (and (string-match-p "ancestor.?thread.?id" message)
         (string-match-p
          "invalid request\\|unknown field\\|invalid params\\|unsupported"
          message))))

(defun codex-ide--agent-tree-threads (session current-thread-id)
  "Return root and spawned descendants for CURRENT-THREAD-ID using SESSION."
  (let* ((root (codex-ide--agent-root-thread session current-thread-id))
         (root-id (alist-get 'id root))
         (descendants
          (condition-case err
              (codex-ide--list-descendant-threads session root-id)
            (error
             (unless (codex-ide--agent-descendant-filter-unsupported-error-p err)
               (signal (car err) (cdr err)))
             (user-error
              "This app-server cannot list agent descendants; update the Codex CLI")))))
    (cons root
          (seq-remove (lambda (thread)
                        (equal (alist-get 'id thread) root-id))
                      descendants))))

(defun codex-ide--agent-thread-depth (thread root-id threads-by-id)
  "Return THREAD's nesting depth below ROOT-ID using THREADS-BY-ID."
  (if (equal (alist-get 'id thread) root-id)
      0
    (let ((parent-id (alist-get 'parentThreadId thread))
          (depth 0)
          (seen (make-hash-table :test #'equal)))
      (while (and (stringp parent-id)
                  (not (string-empty-p parent-id))
                  (not (equal parent-id root-id))
                  (not (gethash parent-id seen)))
        (puthash parent-id t seen)
        (setq depth (1+ depth)
              parent-id (alist-get 'parentThreadId
                                   (gethash parent-id threads-by-id))))
      (if (equal parent-id root-id)
          (1+ depth)
        depth))))

(defun codex-ide--agent-thread-name (thread root-id)
  "Return THREAD's display name relative to ROOT-ID."
  (if (equal (alist-get 'id thread) root-id)
      "Main"
    (or (seq-find (lambda (value)
                    (and (stringp value)
                         (not (string-empty-p value))))
                  (list (alist-get 'agentNickname thread)
                        (alist-get 'agentRole thread)
                        (codex-ide--thread-choice-preview
                         (alist-get 'preview thread))))
        "Agent")))

(defun codex-ide--agent-thread-choice-candidates (threads)
  "Return completion candidates for agent THREADS."
  (let* ((root-id (alist-get 'id (car threads)))
         (threads-by-id (make-hash-table :test #'equal))
         (counts (make-hash-table :test #'equal)))
    (dolist (thread threads)
      (puthash (alist-get 'id thread) thread threads-by-id))
    (dolist (thread threads)
      (let* ((depth (codex-ide--agent-thread-depth thread root-id threads-by-id))
             (candidate (concat (make-string (* 2 depth) ?\s)
                                (codex-ide--agent-thread-name thread root-id))))
        (puthash candidate (1+ (gethash candidate counts 0)) counts)))
    (mapcar
     (lambda (thread)
       (let* ((depth (codex-ide--agent-thread-depth thread root-id threads-by-id))
              (candidate (concat (make-string (* 2 depth) ?\s)
                                 (codex-ide--agent-thread-name thread root-id))))
         (cons (if (> (gethash candidate counts 0) 1)
                   (format "%s [%s]" candidate
                           (codex-ide--thread-choice-short-id thread))
                 candidate)
               thread)))
     threads)))

(defun codex-ide--agent-thread-status (thread)
  "Return THREAD's normalized status string."
  (let ((status (alist-get 'status thread)))
    (cond
     ((stringp status) status)
     ((listp status) (alist-get 'type status))
     (t nil))))

(defun codex-ide--agent-thread-choice-affixation (candidates choices)
  "Return affixation data for agent CANDIDATES using CHOICES."
  (mapcar
   (lambda (candidate)
     (let* ((thread (cdr (assoc candidate choices)))
            (parts (delq nil
                         (list (and (stringp (alist-get 'agentRole thread))
                                    (alist-get 'agentRole thread))
                               (codex-ide--agent-thread-status thread)
                               (codex-ide--thread-choice-short-id thread)))))
       (list candidate "" (format " [%s]" (string-join parts ", ")))))
   candidates))

(defun codex-ide--pick-agent-thread (session current-thread-id)
  "Prompt for a thread in CURRENT-THREAD-ID's agent tree using SESSION."
  (let* ((threads (codex-ide--agent-tree-threads session current-thread-id))
         (choices (codex-ide--agent-thread-choice-candidates threads))
         (completion-extra-properties
          `(:affixation-function
            ,(lambda (candidates)
               (codex-ide--agent-thread-choice-affixation candidates choices))
            :display-sort-function identity
            :cycle-sort-function identity)))
    (when (< (length threads) 2)
      (user-error "No Codex subagents found"))
    (cdr (assoc (completing-read "Switch to agent: " choices nil t)
                choices))))

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
