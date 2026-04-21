;;; codex-ide-log.el --- Log buffer management for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns codex-ide's log buffers and stderr capture.
;;
;; The transcript buffer is the user-facing conversation view.  The log buffer
;; is the implementation-facing diagnostic stream.  It records JSON-RPC traffic,
;; process lifecycle events, and stderr output in a form that is useful during
;; debugging and test assertions but should remain separate from normal
;; transcript rendering.
;;
;; Keeping log concerns isolated avoids a common failure mode in the old
;; monolith: session/process code, transcript code, and stderr formatting all
;; modified the same region of the file.  With this split, lifecycle code asks
;; for log services and the log module handles buffer creation, trimming, and
;; stderr chunk normalization.

;;; Code:

(require 'subr-x)
(require 'codex-ide-core)
(require 'codex-ide-errors)

(defvar codex-ide-logging-enabled)
(defvar codex-ide-log-max-lines)

(define-derived-mode codex-ide-log-mode special-mode "Codex-IDE-Log"
  "Major mode for Codex IDE log buffers."
  (buffer-disable-undo)
  (setq-local truncate-lines t))

(defun codex-ide--log-buffer-name-from-session-buffer-name (buffer-name)
  "Return a log buffer name derived from transcript BUFFER-NAME."
  (if (string-match "\\`\\(.*\\)\\*\\'" buffer-name)
      (concat (match-string 1 buffer-name) "-log*")
    (concat buffer-name "-log")))

(defun codex-ide--query-log-buffer-name (session)
  "Return the computed query-session log buffer name for SESSION."
  (codex-ide--append-buffer-name-suffix
   (format "*%s-log[%s]-query*"
           codex-ide-buffer-name-prefix
           (codex-ide--project-name (codex-ide-session-directory session)))
   (and (integerp (codex-ide-session-name-suffix session))
        (> (codex-ide-session-name-suffix session) 0)
        (codex-ide-session-name-suffix session))))

(defun codex-ide--log-buffer-name (session)
  "Return the computed log buffer name for SESSION."
  (if (codex-ide-session-query-only session)
      (codex-ide--query-log-buffer-name session)
    (codex-ide--log-buffer-name-from-session-buffer-name
     (if (buffer-live-p (codex-ide-session-buffer session))
         (buffer-name (codex-ide-session-buffer session))
       (codex-ide--session-buffer-name
        (codex-ide-session-directory session)
        (codex-ide-session-name-suffix session))))))

(defun codex-ide--initialize-log-buffer (buffer directory)
  "Prepare BUFFER for logging for DIRECTORY."
  (with-current-buffer buffer
    (codex-ide-log-mode)
    (setq-local default-directory directory)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Codex log for %s\n\n"
                      (abbreviate-file-name directory))))))

(defun codex-ide--ensure-log-buffer (session)
  "Return SESSION's log buffer, creating it when needed."
  (when codex-ide-logging-enabled
    (or (get-buffer (codex-ide--log-buffer-name session))
        (let* ((directory (codex-ide-session-directory session))
               (buffer (get-buffer-create (codex-ide--log-buffer-name session))))
          (codex-ide--initialize-log-buffer buffer directory)
          buffer))))

(defun codex-ide--trim-log-buffer ()
  "Trim the current log buffer to `codex-ide-log-max-lines'."
  (when (and (integerp codex-ide-log-max-lines)
             (> codex-ide-log-max-lines 0))
    (save-excursion
      (goto-char (point-min))
      (forward-line codex-ide-log-max-lines)
      (when (< (point) (point-max))
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point)))))))

(defun codex-ide-log-message (session format-string &rest args)
  "Append a formatted log line for SESSION using FORMAT-STRING and ARGS."
  (when-let ((buffer (codex-ide--ensure-log-buffer session)))
    (let ((text (apply #'format format-string args)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (marker nil))
          (goto-char (point-max))
          (setq marker (copy-marker (point)))
          (insert (format "[%s] %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S")
                          text))
          (codex-ide--trim-log-buffer)
          marker)))))

(defun codex-ide--kill-log-buffer (session)
  "Kill SESSION's currently computed log buffer, if live."
  (when-let ((buffer (get-buffer (codex-ide--log-buffer-name session))))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun codex-ide--stderr-filter (process chunk)
  "Append stderr CHUNK from PROCESS to the owning session log."
  (when-let ((session (process-get process 'codex-session)))
    (let* ((sanitized (codex-ide--sanitize-ansi-text chunk))
           (pending (concat (or (codex-ide--session-metadata-get session :stderr-partial) "")
                            sanitized))
           (lines (split-string pending "\n"))
           (complete-lines (butlast lines))
           (partial (car (last lines))))
      (codex-ide--session-metadata-put
       session
       :stderr-tail
       (let* ((previous-tail (or (codex-ide--session-metadata-get session :stderr-tail) ""))
              (combined-tail (concat previous-tail sanitized)))
         (if (> (length combined-tail) 4000)
             (substring combined-tail (- (length combined-tail) 4000))
           combined-tail)))
      (codex-ide--session-metadata-put session :stderr-partial partial)
      (when complete-lines
        (dolist (line complete-lines)
          (unless (string-empty-p line)
            (codex-ide-log-message session "stderr: %s" line)))))))

(defun codex-ide--discard-process-buffer (process)
  "Detach and kill any buffer associated with PROCESS."
  (when process
    (let ((buffer (ignore-errors (process-buffer process))))
      (when buffer
        (ignore-errors (set-process-buffer process nil))
        (when (buffer-live-p buffer)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer)))))))

(provide 'codex-ide-log)

;;; codex-ide-log.el ends here
