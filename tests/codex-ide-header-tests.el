;;; codex-ide-header-tests.el --- Tests for quota header formatting -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Codex IDE session header.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide-header)

(ert-deftest codex-ide-header-shows-time-for-overnight-quota-reset ()
  (let* ((system-time-locale "C")
         (now (encode-time 0 0 21 20 10 2026))
         (same-day-reset (encode-time 0 0 23 20 10 2026))
         (overnight-reset (encode-time 0 0 2 21 10 2026))
         (window `((usedPercent . 49)
                   (windowDurationMins . 300)
                   (resetsAt . ,(floor (float-time same-day-reset))))))
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (should (equal (codex-ide--format-rate-limit-window-summary window)
                     "49%→23:00"))
      (setf (alist-get 'resetsAt window) (floor (float-time overnight-reset)))
      (should (equal (codex-ide--format-rate-limit-window-summary window)
                     "49%→02:00 Oct21"))
      (should (equal (codex-ide--format-rate-limit-window-summary window t)
                     "49%%→02:00 Oct21")))))

(provide 'codex-ide-header-tests)

;;; codex-ide-header-tests.el ends here
