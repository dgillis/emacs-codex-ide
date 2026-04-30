;;; codex-ide-diff-data.el --- Diff data helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns diff-text normalization and lookup for Codex file-change
;; items and turns.  It deliberately does not display buffers; view concerns
;; live in `codex-ide-diff-view.el'.

;;; Code:

(require 'seq)
(require 'codex-ide-core)
(require 'codex-ide-protocol)
(require 'subr-x)

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

(defun codex-ide--combine-diff-texts (texts)
  "Return TEXTS combined into one diff string, or nil when empty."
  (let ((normalized
         (delq nil
               (mapcar (lambda (text)
                         (when (and (stringp text)
                                    (not (string-empty-p (string-trim text))))
                           (string-trim-right text)))
                       texts))))
    (when normalized
      (string-join normalized "\n\n"))))

(defun codex-ide--current-turn-diff-entry (session)
  "Return the tracked combined-diff entry for SESSION's latest submitted turn."
  (codex-ide--session-metadata-get session :current-turn-diff-entry))

(defun codex-ide--set-current-turn-diff-entry (session entry)
  "Store combined-diff ENTRY for SESSION's latest submitted turn."
  (codex-ide--session-metadata-put session :current-turn-diff-entry entry))

(defun codex-ide--register-submitted-turn-prompt (session prompt)
  "Track PROMPT as SESSION's latest submitted prompt."
  (let* ((existing (or (codex-ide--current-turn-diff-entry session) '()))
         (entry (list :prompt prompt
                      :status (if (codex-ide-session-current-turn-id session)
                                  'running
                                'pending)
                      :turn-id (codex-ide-session-current-turn-id session)
                      :file-change-items
                      (copy-tree (plist-get existing :file-change-items)))))
    (codex-ide--set-current-turn-diff-entry session entry)))

(defun codex-ide--mark-current-turn-diff-started (session turn-id)
  "Mark SESSION's tracked combined-diff entry as running for TURN-ID."
  (let* ((existing (or (codex-ide--current-turn-diff-entry session) '()))
         (entry (list :prompt (plist-get existing :prompt)
                      :status 'running
                      :turn-id turn-id
                      :file-change-items
                      (copy-tree (plist-get existing :file-change-items)))))
    (codex-ide--set-current-turn-diff-entry session entry)))

(defun codex-ide--mark-current-turn-diff-completed (session)
  "Mark SESSION's tracked combined-diff entry as completed."
  (when-let ((existing (codex-ide--current-turn-diff-entry session)))
    (codex-ide--set-current-turn-diff-entry
     session
     (plist-put (copy-tree existing) :status 'completed))))

(defun codex-ide--put-current-turn-file-change
    (session item-id &optional item diff-delta)
  "Update SESSION's tracked file-change ITEM-ID with ITEM and DIFF-DELTA."
  (let* ((entry (or (codex-ide--current-turn-diff-entry session)
                    (list :prompt nil
                          :status (if (codex-ide-session-current-turn-id session)
                                      'running
                                    'pending)
                          :turn-id (codex-ide-session-current-turn-id session)
                          :file-change-items nil)))
         (items (copy-tree (plist-get entry :file-change-items)))
         (existing (seq-find
                    (lambda (candidate)
                      (equal (plist-get candidate :item-id) item-id))
                    items))
         (final-diff (and item (codex-ide--file-change-diff-text item)))
         (updated (list :item-id item-id
                        :item (or item (plist-get existing :item))
                        :diff-text (cond
                                    ((and (stringp final-diff)
                                          (not (string-empty-p final-diff)))
                                     final-diff)
                                    ((stringp diff-delta)
                                     (concat (or (plist-get existing :diff-text) "")
                                             diff-delta))
                                    (t
                                     (plist-get existing :diff-text))))))
    (setq items
          (if existing
              (mapcar (lambda (candidate)
                        (if (equal (plist-get candidate :item-id) item-id)
                            updated
                          candidate))
                      items)
            (append items (list updated))))
    (codex-ide--set-current-turn-diff-entry
     session
     (plist-put entry :file-change-items items))))

(defun codex-ide--current-turn-diff-texts (session)
  "Return normalized diff texts for SESSION's tracked active turn."
  (when-let ((entry (codex-ide--current-turn-diff-entry session)))
    (delq nil
          (mapcar (lambda (item-entry)
                    (let* ((item (plist-get item-entry :item))
                           (item-diff (and item
                                           (codex-ide--file-change-diff-text
                                            item))))
                      (if (and (stringp item-diff)
                               (not (string-empty-p item-diff)))
                          item-diff
                        (plist-get item-entry :diff-text))))
                  (plist-get entry :file-change-items)))))

(defun codex-ide--current-turn-combined-diff-text (session)
  "Return combined diff text for SESSION's tracked active turn, or nil."
  (codex-ide--combine-diff-texts
   (codex-ide--current-turn-diff-texts session)))

(defun codex-ide--turn-file-change-diff-texts (turn)
  "Return file-change diff texts from TURN."
  (delq nil
        (mapcar (lambda (item)
                  (when (equal (alist-get 'type item) "fileChange")
                    (codex-ide--file-change-diff-text item)))
                (append (codex-ide--thread-read-items turn) nil))))

(defun codex-ide--read-turn-combined-diff-text (session &optional turn-id)
  "Return combined diff text for TURN-ID in SESSION's thread history.
When TURN-ID is nil, use the most recent stored turn."
  (let* ((thread-id (codex-ide-session-thread-id session))
         (thread-read (and thread-id
                           (codex-ide--read-thread session thread-id t)))
         (turns (and thread-read
                     (append (codex-ide--thread-read-turns thread-read) nil)))
         (turn (if turn-id
                   (seq-find (lambda (candidate)
                               (equal (alist-get 'id candidate) turn-id))
                             turns)
                 (car (last turns)))))
    (codex-ide--combine-diff-texts
     (and turn (codex-ide--turn-file-change-diff-texts turn)))))

(defun codex-ide-diff-data-combined-turn-diff-text (session &optional turn-id)
  "Return combined file-change diff text for SESSION TURN-ID.
When TURN-ID is nil, prefer the active running turn and otherwise use the most
recent stored turn."
  (unless session
    (error "No Codex session available"))
  (let* ((tracked-entry (codex-ide--current-turn-diff-entry session))
         (tracked-turn-id (plist-get tracked-entry :turn-id))
         (tracked-diff-text (and tracked-turn-id
                                 (codex-ide--current-turn-combined-diff-text
                                  session)))
         (diff-text
          (cond
           ((and turn-id
                 (equal turn-id (codex-ide-session-current-turn-id session)))
            tracked-diff-text)
           ((and turn-id
                 tracked-turn-id
                 (equal turn-id tracked-turn-id))
            (or tracked-diff-text
                (codex-ide--read-turn-combined-diff-text session turn-id)))
           (turn-id
            (codex-ide--read-turn-combined-diff-text session turn-id))
           ((codex-ide-session-current-turn-id session)
            (or tracked-diff-text
                (codex-ide--read-turn-combined-diff-text
                 session
                 (codex-ide-session-current-turn-id session))))
           (tracked-turn-id
            (or tracked-diff-text
                (codex-ide--read-turn-combined-diff-text session tracked-turn-id)))
           (t
            (codex-ide--read-turn-combined-diff-text session)))))
    (unless (and (stringp diff-text)
                 (not (string-empty-p (string-trim diff-text))))
      (user-error "No diffs found for the target prompt"))
    diff-text))

(provide 'codex-ide-diff-data)

;;; codex-ide-diff-data.el ends here
