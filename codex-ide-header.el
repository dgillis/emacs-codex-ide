;;; codex-ide-header.el --- Session header line UI for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns compact session-buffer header line formatting and refresh
;; behavior.  Transcript and session controllers update session metadata, then
;; call these helpers to present the current model, quota, context, and focus.

;;; Code:

(require 'subr-x)
(require 'codex-ide-core)
(require 'codex-ide-protocol)
(require 'codex-ide-renderer)

(declare-function codex-ide-config-effective-value "codex-ide-config" (key &optional session))
(declare-function codex-ide-config-effective-reasoning-effort "codex-ide-config" (&optional session))

(defcustom codex-ide-live-usage-refresh-delay 0.1
  "Seconds to coalesce visible usage refreshes during streaming turns."
  :type 'number
  :group 'codex-ide)

(defvar codex-ide-header-extra-summary-functions nil
  "Hook of functions that return extra header summaries for a session.
Each function receives one argument, the `codex-ide-session' object, and should
return a propertized string or nil.")

(defun codex-ide--update-mode-line (&optional session)
  "Refresh the mode line indicator for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let* ((buffer (and session (codex-ide-session-buffer session))))
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defun codex-ide--run-live-usage-refresh (session)
  "Refresh visible usage UI for SESSION after a coalesced timer fires."
  (codex-ide--session-metadata-put session :live-usage-refresh-timer nil)
  (when-let* ((buffer (and session (codex-ide-session-buffer session))))
    (when (and (buffer-live-p buffer)
               (get-buffer-window-list buffer nil t))
      (codex-ide--update-header-line session)
      (force-window-update buffer)
      (redisplay))))

(defun codex-ide--cancel-live-usage-refresh (&optional session)
  "Cancel SESSION's pending live usage refresh timer, if any."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let* ((timer (and session
                          (codex-ide--session-metadata-get
                           session
                           :live-usage-refresh-timer))))
    (when (timerp timer)
      (cancel-timer timer))
    (codex-ide--session-metadata-put session :live-usage-refresh-timer nil)))

(defun codex-ide--schedule-live-usage-refresh (&optional session)
  "Schedule a coalesced visible usage refresh for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when (and session
             (not (timerp (codex-ide--session-metadata-get
                           session
                           :live-usage-refresh-timer))))
    (codex-ide--session-metadata-put
     session
     :live-usage-refresh-timer
     (run-at-time
      codex-ide-live-usage-refresh-delay
      nil
      #'codex-ide--run-live-usage-refresh
      session))))

(defun codex-ide--format-compact-number (value)
  "Format numeric VALUE in a compact human-readable form."
  (cond
   ((not (numberp value)) "?")
   ((>= value 1000000)
    (format "%.1fM" (/ value 1000000.0)))
   ((>= value 1000)
    (format "%.1fk" (/ value 1000.0)))
   (t
    (number-to-string value))))

(defun codex-ide--format-token-usage-context-summary (token-usage)
  "Return the context portion of the header summary for TOKEN-USAGE."
  (when-let* ((total (alist-get 'total token-usage))
              (last (or (alist-get 'last token-usage) total))
              (window (alist-get 'modelContextWindow token-usage))
              (used (alist-get 'totalTokens last)))
    (format "Context: %s/%s%s"
            (codex-ide--format-compact-number used)
            (codex-ide--format-compact-number window)
            (if-let* ((total-used (alist-get 'totalTokens total)))
                (format " (%s total)"
                        (codex-ide--format-compact-number total-used))
              ""))))

(defun codex-ide--format-model-summary (&optional session)
  "Return a compact header summary for SESSION's model."
  (let ((model (and session
                    (or (let ((configured
                               (codex-ide-config-effective-value
                                'model
                                session)))
                          (and (stringp configured)
                               (not (string-empty-p configured))
                               configured))
                        (codex-ide--server-model-name session))))
        (effort (and session
                     (codex-ide-config-effective-reasoning-effort session)))
        (fast (and session
                   (equal (codex-ide-config-effective-value 'fast session)
                          "on"))))
    (unless model
      (codex-ide--ensure-server-model-name session))
    (when model
      (let ((details (delq nil
                           (list effort
                                 (and fast "fast")))))
        (format "Model: %s%s"
                model
                (if details
                    (format " (%s)" (string-join details " + "))
                  ""))))))

(defun codex-ide--format-rate-limit-window-label (window)
  "Return a compact label for rate limit WINDOW."
  (when-let* ((minutes (alist-get 'windowDurationMins window)))
    (cond
     ((= minutes 10080) "wk")
     ((and (> minutes 0) (= (mod minutes 1440) 0))
      (format "%sd" (/ minutes 1440)))
     ((and (> minutes 0) (= (mod minutes 60) 0))
      (format "%sh" (/ minutes 60)))
     (t
      (format "%sm" minutes)))))

(defun codex-ide--format-rate-limit-reset (window)
  "Return a compact reset label for rate limit WINDOW."
  (when-let* ((resets-at (alist-get 'resetsAt window)))
    (let* ((reset-time (seconds-to-time resets-at))
           (reset-date (format-time-string "%Y-%m-%d" reset-time))
           (today (format-time-string "%Y-%m-%d" (current-time))))
      (if (equal reset-date today)
          (format-time-string "%H:%M" reset-time)
        (format "%s%s"
                (format-time-string "%b" reset-time)
                (string-to-number
                 (format-time-string "%d" reset-time)))))))

(defun codex-ide--format-rate-limit-window-summary (window &optional raw-header)
  "Return a compact header summary for one rate limit WINDOW.
When RAW-HEADER is non-nil, escape percent signs for `header-line-format'."
  (when-let* ((label (codex-ide--format-rate-limit-window-label window))
              (percent (alist-get 'usedPercent window)))
    (let ((percent-format (if raw-header "%s%%%%" "%s%%")))
      (if-let* ((reset (codex-ide--format-rate-limit-reset window)))
          (format "%s→%s" (format percent-format percent) reset)
        (format "%s/%s" (format percent-format percent) label)))))

(defun codex-ide--format-rate-limit-summary (rate-limits &optional raw-header)
  "Return a compact header summary for RATE-LIMITS.
When RAW-HEADER is non-nil, escape percent signs for `header-line-format'."
  (let* ((primary (alist-get 'primary rate-limits))
         (secondary (alist-get 'secondary rate-limits))
         (windows (delq nil
                        (list (and primary
                                   (codex-ide--format-rate-limit-window-summary
                                    primary
                                    raw-header))
                              (and secondary
                                   (codex-ide--format-rate-limit-window-summary
                                    secondary
                                    raw-header))))))
    (when windows
      (format "Quota: %s%s%s"
              (string-join windows " ")
              (if-let* ((plan-type (alist-get 'planType rate-limits)))
                  (format " (%s)" plan-type)
                "")
              (if-let* ((reached (alist-get 'rateLimitReachedType rate-limits)))
                  (format " limit:%s" reached)
                "")))))

(defun codex-ide--header-extra-summaries (session)
  "Return extra header summaries for SESSION."
  (delq nil
        (mapcar
         (lambda (function)
           (when (functionp function)
             (condition-case err
                 (funcall function session)
               (error
                (message "Codex header summary failed: %s"
                         (error-message-string err))
                nil))))
         codex-ide-header-extra-summary-functions)))

(defun codex-ide--update-header-line (&optional session)
  "Refresh the header line for SESSION."
  (setq session (or session (codex-ide--get-default-session-for-current-buffer)))
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (with-current-buffer buffer
      (let* ((context (with-current-buffer buffer
                        (codex-ide--get-active-buffer-context)))
             (focus (if context
                        (format "Focus: %s"
                                (alist-get 'buffer-name context))
                      "Focus: none"))
             (token-context-summary
              (codex-ide--format-token-usage-context-summary
               (codex-ide--session-metadata-get session :token-usage)))
             (rate-limit-summary
              (codex-ide--format-rate-limit-summary
               (codex-ide--session-metadata-get session :rate-limits)
               t))
             (model-summary
              (codex-ide--format-model-summary session)))
        (setq header-line-format
              (propertize
               (concat
                " "
                (string-join
                 (append
                  (delq nil
                        (list
                         focus
                         model-summary
                         rate-limit-summary
                         token-context-summary))
                  (codex-ide--header-extra-summaries session))
                 " | "))
               'face 'codex-ide-header-line-face)))
      (codex-ide--update-mode-line session))))

(provide 'codex-ide-header)

;;; codex-ide-header.el ends here
