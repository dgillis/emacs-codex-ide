;;; codex-ide-utils-tests.el --- Tests for codex-ide utility helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-utils'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-ide-utils)

(ert-deftest codex-ide-human-time-ago-renders-recent-relative-times ()
  (let ((now (encode-time 0 0 12 17 4 2026)))
    (cl-letf (((symbol-function 'time-since)
               (lambda (time)
                 (time-subtract now time))))
    (should (equal (codex-ide-human-time-ago (encode-time 30 59 11 17 4 2026))
                   "just now"))
    (should (equal (codex-ide-human-time-ago (encode-time 0 55 11 17 4 2026))
                   "5 minutes ago"))
    (should (equal (codex-ide-human-time-ago (encode-time 0 10 10 17 4 2026))
                   "1 hour ago"))
    (should (equal (codex-ide-human-time-ago (encode-time 0 0 12 15 4 2026))
                   "2 days ago"))
    (should (equal (codex-ide-human-time-ago (encode-time 0 0 12 8 4 2026))
                   "last week")))))

(ert-deftest codex-ide-human-time-ago-normalizes-numeric-and-iso8601-strings ()
  (let* ((now (encode-time 0 0 12 17 4 2026))
         (five-minutes-ago (encode-time 0 55 11 17 4 2026)))
    (cl-letf (((symbol-function 'time-since)
               (lambda (time)
                 (time-subtract now time))))
    (should (equal (codex-ide-human-time-ago
                    (number-to-string (floor (float-time five-minutes-ago))))
                   (codex-ide-human-time-ago
                    (floor (float-time five-minutes-ago)))))
    (should (equal (codex-ide-human-time-ago
                    (format-time-string "%Y-%m-%dT%H:%M:%S%z" five-minutes-ago))
                   (codex-ide-human-time-ago five-minutes-ago))))))

(provide 'codex-ide-utils-tests)

;;; codex-ide-utils-tests.el ends here
