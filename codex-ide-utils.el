;;; codex-ide-utils.el --- Shared utility helpers for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Small shared helpers that do not belong in larger feature modules.

;;; Code:

(require 'subr-x)

(defun codex-ide--human-time-normalize (timestamp)
  "Return TIMESTAMP as an Emacs time value, or nil when it cannot be parsed."
  (cond
   ((null timestamp) nil)
   ((numberp timestamp)
    ;; App/server payloads may use Unix seconds or milliseconds.
    (seconds-to-time
     (if (> timestamp 100000000000)
         (/ timestamp 1000.0)
       timestamp)))
   ((stringp timestamp)
    (let ((trimmed (string-trim timestamp)))
      (cond
       ((string-match-p "\\`[0-9]+\\'" trimmed)
        (codex-ide--human-time-normalize (string-to-number trimmed)))
       (t
        (or (ignore-errors (parse-iso8601-time-string trimmed))
            (ignore-errors (date-to-time trimmed)))))))
   (t timestamp)))

(defun codex-ide-human-time-ago (timestamp)
  "Return a compact human-readable relative time string for TIMESTAMP."
  (when-let ((time (codex-ide--human-time-normalize timestamp)))
    (let* ((seconds (max 0 (floor (float-time (time-since time))))))
      (cond
       ((< seconds 60)
        "just now")
       ((< seconds 3600)
        (format "%d minute%s ago"
                (/ seconds 60)
                (if (= (/ seconds 60) 1) "" "s")))
       ((< seconds (* 3600 24))
        (format "%d hour%s ago"
                (/ seconds 3600)
                (if (= (/ seconds 3600) 1) "" "s")))
       ((< seconds (* 3600 24 7))
        (format "%d day%s ago"
                (/ seconds (* 3600 24))
                (if (= (/ seconds (* 3600 24)) 1) "" "s")))
       ((< seconds (* 3600 24 14))
        "last week")
       (t
        (format-time-string "%Y-%m-%d" time))))))

(provide 'codex-ide-utils)

;;; codex-ide-utils.el ends here
