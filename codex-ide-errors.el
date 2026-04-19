;;; codex-ide-errors.el --- Error parsing and recovery for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module centralizes codex-ide's error handling policy.
;;
;; The responsibilities here are deliberately narrow:
;;
;; - Normalize arbitrary payloads into readable error strings.
;; - Classify session, protocol, process, and notification failures into a
;;   small set of UX-oriented categories.
;; - Format consistent user-facing summaries and guidance strings.
;; - Recover transcript/session presentation after failures that interrupt a
;;   visible turn.
;;
;; Keeping this logic in its own module matters because many higher-level
;; modules need the same judgement calls but should not each invent their own
;; copies.  Session/process lifecycle code needs startup and shutdown
;; classification, protocol code needs structured response handling, and the
;; transcript controller needs notification-specific formatting and recovery.
;;
;; This file intentionally does not own the transport itself and does not own
;; transcript rendering.  It produces normalized information and invokes the
;; transcript's turn-finishing primitive only when cleanup of an interrupted
;; visible turn is required.

;;; Code:

(require 'subr-x)
(require 'codex-ide-core)

(declare-function codex-ide--finish-turn "codex-ide-transcript"
                  (&optional session closing-note))

(defun codex-ide--stringify-error-payload (value)
  "Return a readable string representation for error VALUE."
  (cond
   ((stringp value) value)
   ((null value) "")
   (t (format "%S" value))))

(defun codex-ide--extract-error-text (&rest values)
  "Return a compact string built from VALUES."
  (string-join
   (delq nil
         (mapcar
          (lambda (value)
            (let ((text (string-trim (codex-ide--stringify-error-payload value))))
              (unless (string-empty-p text)
                text)))
          values))
   "\n"))

(defun codex-ide--sanitize-ansi-text (text)
  "Return TEXT with ANSI escape sequences removed."
  (replace-regexp-in-string
   "\x1b\\[[0-9;?]*[ -/]*[@-~]"
   ""
   (or text "")))

(defun codex-ide--alist-get-safe (key value)
  "Return KEY from VALUE when VALUE is an alist."
  (when (listp value)
    (alist-get key value)))

(defun codex-ide--notification-error-info (params)
  "Return normalized error info plist from notification PARAMS."
  (let* ((error (or (codex-ide--alist-get-safe 'error params) params))
         (details (or (codex-ide--alist-get-safe 'details error)
                      (codex-ide--alist-get-safe 'additionalDetails error)
                      (codex-ide--alist-get-safe 'detail error)))
         (http-status (or (codex-ide--alist-get-safe 'httpStatus error)
                          (codex-ide--alist-get-safe 'status error)
                          (codex-ide--alist-get-safe 'statusCode error)))
         (message (or (codex-ide--alist-get-safe 'message error)
                      (codex-ide--alist-get-safe 'message params)))
         (retry-delay (or (codex-ide--alist-get-safe 'retryDelayMs error)
                          (codex-ide--alist-get-safe 'retryDelayMs params)))
         (will-retry (not (memq (or (codex-ide--alist-get-safe 'willRetry error)
                                    (codex-ide--alist-get-safe 'willRetry params))
                                '(nil :json-false)))))
    `((message . ,message)
      (details . ,details)
      (http-status . ,http-status)
      ,@(when retry-delay
          `((retry-delay-ms . ,retry-delay)))
      (will-retry . ,will-retry)
      (turn-id . ,(or (codex-ide--alist-get-safe 'turnId params)
                      (codex-ide--alist-get-safe 'turnId error))))))

(defun codex-ide--notification-error-display-detail (info)
  "Return a single display-oriented detail string from INFO."
  (codex-ide--extract-error-text
   (alist-get 'details info)
   (alist-get 'message info)))

(defun codex-ide--notification-error-message (info)
  "Return the primary readable notification error message from INFO."
  (or (let ((message (string-trim
                      (codex-ide--stringify-error-payload
                       (alist-get 'message info)))))
        (unless (string-empty-p message)
          message))
      "Codex reported an error"))

(defun codex-ide--notification-error-additional-details (info)
  "Return supplemental detail lines from INFO."
  (delq nil
        (list
         (when-let ((status (alist-get 'http-status info)))
           (format "HTTP status: %s" status))
         (when-let ((delay (alist-get 'retry-delay-ms info)))
           (format "Retry delay: %sms" delay))
         (let ((details (string-trim
                         (codex-ide--stringify-error-payload
                          (alist-get 'details info)))))
           (unless (string-empty-p details)
             (format "additionalDetails: %s" details))))))

(defun codex-ide--classify-session-error (&rest values)
  "Return a classification plist for VALUES describing a session error."
  (let* ((detail (downcase (codex-ide--extract-error-text values)))
         (classification
          (cond
           ((or (string-match-p "unauthorized\\|authentication\\|api key" detail)
                (string-match-p "403\\|forbidden" detail))
            '(:kind auth
              :summary "Codex authentication failed."
              :guidance "Run `codex login` and retry."))
           ((or (string-match-p "429\\|rate limit" detail)
                (string-match-p "quota" detail))
            '(:kind rate-limit
              :summary "Codex is rate limited."
              :guidance "Wait for quota to recover, then retry."))
           ((or (string-match-p "timed out\\|timeout" detail)
                (string-match-p "econnreset\\|broken pipe" detail))
            '(:kind network
              :summary "Connection interrupted"
              :guidance "Retry after the Codex process or network stabilizes."))
           ((or (string-match-p "exec: .* not found" detail)
                (string-match-p "searching for program" detail)
                (string-match-p "no such file or directory" detail)
                (string-match-p "codex_home does not exist" detail))
            '(:kind startup
              :summary "Codex startup failed."
              :guidance "Check the configured Codex CLI path and environment."))
           (t
            '(:kind generic
              :summary "Codex request failed"
              :guidance nil)))))
    classification))

(defun codex-ide--format-session-error-summary (classification &optional prefix)
  "Format a one-line summary from CLASSIFICATION and optional PREFIX."
  (string-join
   (delq nil
         (list prefix
               (plist-get classification :summary)))
   ": "))

(defun codex-ide--format-session-error-message (classification detail &optional prefix)
  "Format a full message string from CLASSIFICATION and DETAIL with PREFIX."
  (string-join
   (delq nil
         (list (codex-ide--format-session-error-summary classification prefix)
               (unless (string-empty-p detail)
                 detail)
               (plist-get classification :guidance)))
   "\n"))

(defun codex-ide--recover-from-session-error (session classification)
  "Reset SESSION after a recoverable error using CLASSIFICATION."
  (when (and (memq (plist-get classification :kind)
                   '(auth rate-limit generic startup))
             (or (codex-ide-session-current-turn-id session)
                 (codex-ide-session-output-prefix-inserted session)))
    (codex-ide--session-metadata-put session :last-retry-notice nil)
    (codex-ide--finish-turn
     session
     (format "[%s]"
             (or (plist-get classification :summary)
                 "Codex request failed")))))

(provide 'codex-ide-errors)

;;; codex-ide-errors.el ends here
