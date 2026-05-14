;;; codex-ide-approvals-data.el --- Approval state helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns the session-local data model for approval-like interactive
;; requests.  It deliberately does not render UI or send protocol responses.
;; Callers store approval records here and query them by lifecycle state, turn,
;; and id.

;;; Code:

(require 'cl-lib)
(require 'codex-ide-core)

(defconst codex-ide-approvals-data--store-key :approvals
  "Session metadata key for approval records.")

(defconst codex-ide-approvals-data--legacy-pending-key :pending-approvals
  "Legacy session metadata key used before approval records had statuses.")

(defconst codex-ide-approvals-data-heavy-view-keys
  '(:start-marker :status-marker :end-marker :fields)
  "View properties cleared after the transcript no longer needs live state.

Markers track buffer edits and keep references to their buffers.  Elicitation
fields also contain markers and mutable value cells.  Keeping them while an
approval is pending is necessary for button/input behavior; after resolution the
transcript text is already updated, so the canonical data record can retain the
domain decision without carrying these live buffer objects indefinitely.")

(defun codex-ide-approvals-data--store (&optional session)
  "Return SESSION's approval store, creating it when necessary."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (or (codex-ide--session-metadata-get session codex-ide-approvals-data--store-key)
      (codex-ide--session-metadata-put
       session
       codex-ide-approvals-data--store-key
       (make-hash-table :test 'equal))))

(defun codex-ide-approvals-data--legacy-record (id approval)
  "Return a lifecycle-aware approval record for legacy APPROVAL keyed by ID."
  (let ((view nil)
        (record (copy-sequence approval)))
    (dolist (key codex-ide-approvals-data-heavy-view-keys)
      (when (plist-member record key)
        (setq view (plist-put view key (plist-get record key)))
        (setq record (plist-put record key nil))))
    (append
     (list :id id
           :status 'pending
           :turn-id nil
           :created-at nil
           :resolved-at nil
           :decision nil
           :result nil
           :view view)
     record)))

(defun codex-ide-approvals-data-migrate-legacy-pending (session)
  "Copy SESSION's legacy pending approvals into the canonical approval store.

This keeps live reloads tolerant of buffers that still have `:pending-approvals'
metadata from an older version.  The legacy metadata is left untouched because
older loaded functions may still hold references to it until reload completes."
  (let ((legacy (codex-ide--session-metadata-get
                 session
                 codex-ide-approvals-data--legacy-pending-key)))
    (when (hash-table-p legacy)
      (let ((store (codex-ide-approvals-data--store session)))
        (maphash
         (lambda (id approval)
           (unless (gethash id store)
             (puthash id
                      (codex-ide-approvals-data--legacy-record id approval)
                      store)))
         legacy)))))

(defun codex-ide-approvals-data-store (&optional session)
  "Return SESSION's canonical approval store.

The returned hash table is the low-level backing store.  Prefer query and
mutation helpers in this module when possible."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (codex-ide-approvals-data-migrate-legacy-pending session)
  (codex-ide-approvals-data--store session))

(defun codex-ide-approvals-data-get (session id)
  "Return SESSION approval record with request ID, or nil."
  (gethash id (codex-ide-approvals-data-store session)))

(cl-defun codex-ide-approvals-data-add
    (session id kind params &key turn-id view metadata created-at)
  "Add a pending approval record for SESSION request ID.

KIND identifies the approval family.  PARAMS are the protocol request params.
TURN-ID should be captured at request creation time rather than inferred during
resolution.  VIEW contains render-only state such as markers and input field
state.  METADATA is appended for future non-view extension fields."
  (let ((record (append
                 (list :id id
                       :kind kind
                       :status 'pending
                       :params params
                       :turn-id turn-id
                       :created-at (or created-at (current-time))
                       :resolved-at nil
                       :decision nil
                       :result nil
                       :view view)
                 metadata)))
    (puthash id record (codex-ide-approvals-data-store session))
    record))

(defun codex-ide-approvals-data-view (approval)
  "Return APPROVAL's render-only view plist."
  (plist-get approval :view))

(defun codex-ide-approvals-data-view-get (approval key)
  "Return APPROVAL view value for KEY."
  (plist-get (codex-ide-approvals-data-view approval) key))

(defun codex-ide-approvals-data--matches-p (approval status turn-id kind)
  "Return non-nil when APPROVAL matches STATUS, TURN-ID, and KIND filters."
  (and (or (eq status :any)
           (eq (plist-get approval :status) status))
       (or (eq turn-id :any)
           (equal (plist-get approval :turn-id) turn-id))
       (or (eq kind :any)
           (eq (plist-get approval :kind) kind))))

(defun codex-ide-approvals-data--record-less-p (left right)
  "Return non-nil when approval record LEFT should sort before RIGHT."
  (let ((left-created (plist-get left :created-at))
        (right-created (plist-get right :created-at)))
    (cond
     ((and left-created right-created
           (not (equal left-created right-created)))
      (time-less-p left-created right-created))
     (left-created t)
     (right-created nil)
     (t
      (string< (format "%s" (plist-get left :id))
               (format "%s" (plist-get right :id)))))))

(cl-defun codex-ide-approvals-data-list
    (session &key (status :any) (turn-id :any) (kind :any))
  "Return SESSION approval records matching optional STATUS, TURN-ID, and KIND.

Records are returned in creation order so grouped approval UI can present a
stable progression independent of hash-table iteration order."
  (let (records)
    (maphash
     (lambda (_id approval)
       (when (codex-ide-approvals-data--matches-p approval status turn-id kind)
         (push approval records)))
     (codex-ide-approvals-data-store session))
    (sort records #'codex-ide-approvals-data--record-less-p)))

(cl-defun codex-ide-approvals-data-count
    (session &key (status :any) (turn-id :any) (kind :any))
  "Return count of SESSION approval records matching filters."
  (length (codex-ide-approvals-data-list
           session
           :status status
           :turn-id turn-id
           :kind kind)))

(defun codex-ide-approvals-data-pending-list (session)
  "Return SESSION approvals whose lifecycle status is `pending'."
  (codex-ide-approvals-data-list session :status 'pending))

(defun codex-ide-approvals-data-pending-p (session)
  "Return non-nil when SESSION has unresolved approvals."
  (> (codex-ide-approvals-data-count session :status 'pending) 0))

(defun codex-ide-approvals-data--clear-view (approval)
  "Return APPROVAL with heavyweight live view state cleared.

The transcript resolution line is inserted before this is called, so markers and
elicitation field state are no longer needed for resolved approval behavior."
  (plist-put approval :view nil))

(cl-defun codex-ide-approvals-data-resolve
    (session id status &key decision result resolved-at clear-view)
  "Mark SESSION approval ID as resolved with STATUS.

DECISION stores the user-facing/raw decision value.  RESULT stores the protocol
response payload sent back to Codex.  When CLEAR-VIEW is non-nil, remove
marker-heavy view state from the record after resolution."
  (let ((approval (codex-ide-approvals-data-get session id)))
    (when approval
      (setq approval
            (plist-put
             (plist-put
              (plist-put
               (plist-put approval :status status)
               :decision decision)
              :result result)
             :resolved-at (or resolved-at (current-time))))
      (when clear-view
        (setq approval (codex-ide-approvals-data--clear-view approval)))
      (puthash id approval (codex-ide-approvals-data-store session))
      approval)))

(provide 'codex-ide-approvals-data)

;;; codex-ide-approvals-data.el ends here
