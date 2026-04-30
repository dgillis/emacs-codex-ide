;;; codex-ide-diff-data.el --- Diff data helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns diff-text normalization and lookup for Codex file-change
;; items and turns.  It deliberately does not display buffers; view concerns
;; live in `codex-ide-diff-view.el'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'codex-ide-core)
(require 'codex-ide-protocol)
(require 'subr-x)

(defun codex-ide--apply-patch-file-paths (text)
  "Return file paths mentioned by an apply-patch TEXT."
  (when (stringp text)
    (let (paths)
      (dolist (line (split-string text "\n"))
        (when (string-match
               (rx line-start
                   "*** "
                   (or "Add" "Update" "Delete")
                   " File: "
                   (group (+ nonl))
                   line-end)
               line)
          (push (match-string 1 line) paths)))
      (nreverse paths))))

(defun codex-ide--apply-patch-file-diff-header (kind path move-to)
  "Return unified diff file header for apply-patch KIND PATH MOVE-TO."
  (pcase kind
    ("Add" (format "--- /dev/null\n+++ b/%s" path))
    ("Delete" (format "--- a/%s\n+++ /dev/null" path))
    (_ (format "--- a/%s\n+++ b/%s" path (or move-to path)))))

(defun codex-ide--apply-patch-diff-text (text)
  "Convert apply-patch TEXT to a unified-diff-like string, or nil."
  (when (and (stringp text)
             (string-match-p (rx line-start "*** Begin Patch") text))
    (let (sections kind path move-to body)
      (cl-labels
          ((flush-section
             ()
             (when path
               (push
		(string-join
                 (cons (codex-ide--apply-patch-file-diff-header
			kind
			path
			move-to)
                       (nreverse body))
                 "\n")
		sections))
             (setq kind nil
                   path nil
                   move-to nil
                   body nil)))
        (dolist (line (split-string text "\n"))
          (cond
           ((string-match
             (rx line-start
                 "*** "
                 (group (or "Add" "Update" "Delete"))
                 " File: "
                 (group (+ nonl))
                 line-end)
             line)
            (flush-section)
            (setq kind (match-string 1 line)
                  path (match-string 2 line)))
           ((and path
                 (string-match
                  (rx line-start "*** Move to: " (group (+ nonl)) line-end)
                  line))
            (setq move-to (match-string 1 line)))
           ((or (string-match-p
                 (rx line-start "*** " (or "Begin Patch" "End Patch") line-end)
                 line)
                (string-match-p
                 (rx line-start "*** End of File" line-end)
                 line))
            nil)
           (path
            (push line body))))
        (flush-section)
        (when sections
          (string-join (nreverse sections) "\n\n"))))))

(defun codex-ide--diff-text-has-file-header-p (diff path)
  "Return non-nil when DIFF already contains a file header for PATH."
  (or (string-match-p (rx line-start "diff --") diff)
      (and path
           (string-match-p
            (regexp-quote (format "+++ %s" path))
            diff))
      (and path
           (string-match-p
            (regexp-quote (format "+++ b/%s" path))
            diff))
      (and path
           (string-match-p
            (regexp-quote (format "*** Update File: %s" path))
            diff))
      (and path
           (string-match-p
            (regexp-quote (format "*** Add File: %s" path))
            diff))
      (and path
           (string-match-p
            (regexp-quote (format "*** Delete File: %s" path))
            diff))))

(defun codex-ide--file-change-display-path (path diff)
  "Return the best display path for a file-change PATH and DIFF."
  (if (and (stringp path)
           (equal path "patch"))
      (or (car (codex-ide--apply-patch-file-paths diff))
          path)
    path))

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
      (or (codex-ide--apply-patch-diff-text item-diff)
          item-diff))
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
                    (let ((display-path
                           (codex-ide--file-change-display-path path diff))
                          (normalized-diff
                           (or (codex-ide--apply-patch-diff-text diff)
                               diff)))
                      (if (and display-path
                               (not (codex-ide--diff-text-has-file-header-p
                                     normalized-diff
                                     display-path)))
                          (format "diff -- %s\n%s" display-path normalized-diff)
                        normalized-diff)))))
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

(defun codex-ide--turn-start-index (session)
  "Return SESSION's transcript turn-start index."
  (codex-ide--session-metadata-get session :turn-start-index))

(defun codex-ide--set-pending-turn-start-marker (session marker)
  "Store MARKER as SESSION's pending transcript turn-start marker."
  (codex-ide--session-metadata-put session :pending-turn-start-marker marker))

(defun codex-ide--pending-turn-start-marker (session)
  "Return SESSION's pending transcript turn-start marker."
  (codex-ide--session-metadata-get session :pending-turn-start-marker))

(defun codex-ide--record-turn-start (session turn-id marker)
  "Record TURN-ID as starting at MARKER in SESSION's transcript."
  (when (and session
             (stringp turn-id)
             (not (string-empty-p turn-id))
             (markerp marker)
             (marker-buffer marker))
    (let* ((index (copy-tree (codex-ide--turn-start-index session)))
           (entry (list :turn-id turn-id
                        :marker (copy-marker marker nil))))
      (setq index
            (cons entry
                  (seq-remove
                   (lambda (candidate)
                     (equal (plist-get candidate :turn-id) turn-id))
                   index)))
      (codex-ide--session-metadata-put session :turn-start-index index))))

(defun codex-ide--record-pending-turn-start (session turn-id)
  "Bind SESSION's pending transcript turn-start marker to TURN-ID."
  (when-let ((marker (codex-ide--pending-turn-start-marker session)))
    (codex-ide--record-turn-start session turn-id marker)
    (codex-ide--set-pending-turn-start-marker session nil)))

(defun codex-ide-diff-data-turn-id-at-point (session &optional point buffer)
  "Return the last SESSION turn id at or before POINT in BUFFER.
When POINT or BUFFER is nil, use the current point and buffer."
  (let ((point (or point (point)))
        (buffer (or buffer (current-buffer)))
        best)
    (dolist (entry (codex-ide--turn-start-index session))
      (let ((marker (plist-get entry :marker)))
        (when (and (markerp marker)
                   (eq (marker-buffer marker) buffer)
                   (<= (marker-position marker) point)
                   (or (not best)
                       (> (marker-position marker)
                          (marker-position (plist-get best :marker)))))
          (setq best entry))))
    (plist-get best :turn-id)))

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

(defun codex-ide--restored-thread-read (session)
  "Return SESSION's last restored thread-read payload, if any."
  (codex-ide--session-metadata-get session :restored-thread-read))

(defun codex-ide--set-restored-thread-read (session thread-read)
  "Store THREAD-READ as SESSION's last restored thread-read payload."
  (codex-ide--session-metadata-put session :restored-thread-read thread-read))

(defun codex-ide--turn-file-change-diff-texts (turn)
  "Return file-change diff texts from TURN."
  (delq nil
        (mapcar (lambda (item)
                  (when (equal (alist-get 'type item) "fileChange")
                    (codex-ide--file-change-diff-text item)))
                (append (codex-ide--thread-read-items turn) nil))))

(defun codex-ide--thread-read-combined-diff-text (thread-read &optional turn-id)
  "Return combined diff text for TURN-ID in THREAD-READ.
When TURN-ID is nil, use the most recent stored turn."
  (let* ((turns (and thread-read
                     (append (codex-ide--thread-read-turns thread-read) nil)))
         (turn (if turn-id
                   (seq-find (lambda (candidate)
                               (equal (alist-get 'id candidate) turn-id))
                             turns)
                 (car (last turns)))))
    (codex-ide--combine-diff-texts
     (and turn (codex-ide--turn-file-change-diff-texts turn)))))

(defun codex-ide--thread-read-thread-id (thread-read)
  "Return the thread id from THREAD-READ, if present."
  (alist-get 'id (alist-get 'thread thread-read)))

(defun codex-ide--matching-restored-thread-read (session)
  "Return SESSION's restored thread-read when it matches the current thread."
  (let* ((thread-id (codex-ide-session-thread-id session))
         (thread-read (codex-ide--restored-thread-read session))
         (restored-thread-id (and thread-read
                                  (codex-ide--thread-read-thread-id
                                   thread-read))))
    (when (and thread-read
               (or (not thread-id)
                   (not restored-thread-id)
                   (equal thread-id restored-thread-id)))
      thread-read)))

(defun codex-ide--read-turn-combined-diff-text (session &optional turn-id)
  "Return combined diff text for TURN-ID in SESSION's thread history.
When TURN-ID is nil, use the most recent stored turn."
  (let* ((thread-id (codex-ide-session-thread-id session))
         (thread-read (and thread-id
                           (ignore-errors
                             (codex-ide--read-thread session thread-id t))))
         (diff-text
          (codex-ide--thread-read-combined-diff-text thread-read turn-id)))
    (or diff-text
        (codex-ide--thread-read-combined-diff-text
         (codex-ide--matching-restored-thread-read session)
         turn-id))))

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
