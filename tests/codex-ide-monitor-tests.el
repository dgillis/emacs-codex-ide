;;; codex-ide-monitor-tests.el --- Tests for session monitor layout -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-monitor'.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-monitor)

(defmacro codex-ide-monitor-test-with-sessions (bindings &rest body)
  "Create live test sessions named by BINDINGS, then run BODY.
BINDINGS is a list of symbols.  BODY also receives a `sessions' binding in the
same order."
  (declare (indent 1) (debug t))
  (let ((root-dir (make-symbol "root-dir"))
        (project-dir (make-symbol "project-dir")))
    `(let* ((,root-dir (codex-ide-test--make-temp-project))
            (,project-dir (expand-file-name "project" ,root-dir)))
       (make-directory ,project-dir t)
       (codex-ide-test-with-fixture ,root-dir
         (codex-ide-test-with-fake-processes
          (let ,(mapcar (lambda (binding) `(,binding nil)) bindings)
            (let ((default-directory ,project-dir))
              ,@(mapcar
                 (lambda (binding)
                   `(setq ,binding (codex-ide--create-process-session)))
                 bindings))
            (let ((sessions (list ,@bindings)))
              ,@body)))))))

(ert-deftest codex-ide-monitor-focused-session-prefers-current-session-buffer ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c)
    (with-current-buffer (codex-ide-session-buffer session-b)
      (should (eq (codex-ide-monitor--focused-session sessions)
                  session-b)))))

(ert-deftest codex-ide-monitor-focused-session-falls-back-to-most-recent-session ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c)
    (setf (codex-ide-session-created-at session-a) 10
          (codex-ide-session-created-at session-b) 30
          (codex-ide-session-created-at session-c) 20)
    (with-temp-buffer
      (should (eq (codex-ide-monitor--focused-session sessions)
                  session-b)))))

(ert-deftest codex-ide-monitor-rail-sessions-excludes-focused-and-uses-default-limit ()
  (codex-ide-monitor-test-with-sessions
      (session-a session-b session-c session-d session-e)
    (should (equal (codex-ide-monitor--rail-sessions sessions session-b)
                   (list session-a session-c session-d)))))

(ert-deftest codex-ide-monitor-default-sessions-use-most-recent-activity ()
  (codex-ide-monitor-test-with-sessions
      (session-a session-b session-c session-d session-e)
    (setf (codex-ide-session-created-at session-a) 10
          (codex-ide-session-created-at session-b) 50
          (codex-ide-session-created-at session-c) 30
          (codex-ide-session-created-at session-d) 40
          (codex-ide-session-created-at session-e) 20)
    (should (equal (codex-ide-monitor--default-sessions)
                   (list session-b session-d session-c session-e)))))

(ert-deftest codex-ide-monitor-layout-defaults-to-recent-four-sessions ()
  (codex-ide-monitor-test-with-sessions
      (session-a session-b session-c session-d session-e)
    (setf (codex-ide-session-created-at session-a) 10
          (codex-ide-session-created-at session-b) 50
          (codex-ide-session-created-at session-c) 30
          (codex-ide-session-created-at session-d) 40
          (codex-ide-session-created-at session-e) 20)
    (save-window-excursion
      (codex-ide-monitor-layout)
      (should (eq (window-buffer (selected-window))
                  (codex-ide-session-buffer session-b)))
      (should-not (get-buffer-window (codex-ide-session-buffer session-a) nil))
      (dolist (session (list session-b session-c session-d session-e))
        (should (get-buffer-window (codex-ide-session-buffer session) nil))))))

(ert-deftest codex-ide-monitor-layout-errors-without-live-sessions ()
  (codex-ide-test-with-fixture (codex-ide-test--make-temp-project)
    (let ((codex-ide--sessions nil))
      (should-error (codex-ide-monitor-layout) :type 'user-error))))

(ert-deftest codex-ide-monitor-layout-displays-single-session-without-rail ()
  (codex-ide-monitor-test-with-sessions (session-a)
    (save-window-excursion
      (with-current-buffer (codex-ide-session-buffer session-a)
        (codex-ide-monitor-layout))
      (should (= (length (window-list nil 'no-minibuf)) 1))
      (should (eq (window-buffer (selected-window))
                  (codex-ide-session-buffer session-a))))))

(ert-deftest codex-ide-monitor-layout-displays-main-and-rail-buffers ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c)
    (setf (codex-ide-session-created-at session-a) 10
          (codex-ide-session-created-at session-b) 30
          (codex-ide-session-created-at session-c) 20)
    (with-current-buffer (codex-ide-session-buffer session-c)
      (insert "tail marker"))
    (save-window-excursion
      (codex-ide-monitor-layout)
      (should (eq (window-buffer (selected-window))
                  (codex-ide-session-buffer session-b)))
      (should (get-buffer-window (codex-ide-session-buffer session-a) nil))
      (should (get-buffer-window (codex-ide-session-buffer session-b) nil))
      (should (get-buffer-window (codex-ide-session-buffer session-c) nil))
      (let ((rail-window (get-buffer-window (codex-ide-session-buffer session-c) nil)))
        (should rail-window)
        (should (= (window-point rail-window)
                   (with-current-buffer (codex-ide-session-buffer session-c)
                     (point-max))))))))

(ert-deftest codex-ide-monitor-tail-window-bottom-aligns-buffer-end ()
  (save-window-excursion
    (delete-other-windows)
    (let ((buffer (get-buffer-create " *codex-ide-monitor-tail-window*")))
      (unwind-protect
          (let ((window (selected-window))
                near-end-start)
            (with-current-buffer buffer
              (erase-buffer)
              (dotimes (line 80)
                (insert (format "line %02d\n" line))))
            (set-window-buffer window buffer)
            (with-current-buffer buffer
              (setq near-end-start
                    (save-excursion
                      (goto-char (point-max))
                      (forward-line -3)
                      (point)))
              (set-window-start window near-end-start t)
              (set-window-point window (point-max)))
            (codex-ide-monitor--tail-window window)
            (redisplay t)
            (with-current-buffer buffer
              (should (= (window-point window) (point-max)))
              (should (>= (window-end window t) (point-max)))
              (should (< (window-start window) near-end-start))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest codex-ide-monitor-tail-window-resumes-tail-following ()
  (codex-ide-monitor-test-with-sessions (session-a)
    (save-window-excursion
      (delete-other-windows)
      (let ((window (selected-window))
            tail-point)
        (with-current-buffer (codex-ide-session-buffer session-a)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dotimes (line 80)
              (insert (format "line %02d\n" line)))
            (codex-ide--insert-input-prompt session-a nil))
          (setq tail-point (codex-ide--input-end-position session-a))
          (setq-local codex-ide-session-mode--last-point (point-min)
                      codex-ide-session-mode--last-window-start (point-min)))
        (set-window-buffer window (codex-ide-session-buffer session-a))
        (set-window-parameter window 'codex-ide-tail-follow-suspended t)
        (codex-ide-monitor--tail-window window)
        (redisplay t)
        (should-not
         (window-parameter window 'codex-ide-tail-follow-suspended))
        (with-current-buffer (codex-ide-session-buffer session-a)
          (should (= (window-point window) tail-point))
          (should (= codex-ide-session-mode--last-point tail-point))
          (should (= codex-ide-session-mode--last-window-start
                     (window-start window))))))))

(ert-deftest codex-ide-monitor-layout-tails-main-after-final-split-size ()
  (codex-ide-monitor-test-with-sessions (session-a session-b)
    (save-window-excursion
      (delete-other-windows)
      (let ((full-width (window-total-width (selected-window)))
            main-tail-width)
        (cl-letf* ((original-tail-window
                    (symbol-function 'codex-ide-monitor--tail-window))
                   ((symbol-function 'codex-ide-monitor--tail-window)
                    (lambda (window)
                      (when (eq (window-buffer window)
                                (codex-ide-session-buffer session-a))
                        (setq main-tail-width (window-total-width window)))
                      (funcall original-tail-window window))))
          (codex-ide-monitor-layout session-a))
        (should main-tail-width)
        (should (< main-tail-width full-width))))))

(ert-deftest codex-ide-monitor-layout-splits-rail-windows-evenly ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c session-d)
    (setf (codex-ide-session-created-at session-a) 40
          (codex-ide-session-created-at session-b) 30
          (codex-ide-session-created-at session-c) 20
          (codex-ide-session-created-at session-d) 10)
    (save-window-excursion
      (codex-ide-monitor-layout)
      (let ((heights
             (mapcar
              (lambda (session)
                (window-total-height
                 (get-buffer-window (codex-ide-session-buffer session) nil)))
              (list session-b session-c session-d))))
        (should (= (length heights) 3))
        (should (<= (- (apply #'max heights)
                       (apply #'min heights))
                    1))))))

(ert-deftest codex-ide-monitor-promote-session-rebuilds-layout-around-selected-session ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c)
    (save-window-excursion
      (with-current-buffer (codex-ide-session-buffer session-a)
        (codex-ide-monitor-layout))
      (select-window (get-buffer-window (codex-ide-session-buffer session-c) nil))
      (codex-ide-monitor-promote-session)
      (should (eq (window-buffer (selected-window))
                  (codex-ide-session-buffer session-c))))))

(ert-deftest codex-ide-monitor-promote-rail-key-promotes-indexed-session ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c session-d)
    (setf (codex-ide-session-created-at session-a) 40
          (codex-ide-session-created-at session-b) 30
          (codex-ide-session-created-at session-c) 20
          (codex-ide-session-created-at session-d) 10)
    (save-window-excursion
      (codex-ide-monitor-layout)
      (let ((command (lookup-key codex-ide-session-mode-map (kbd "C-c 2"))))
        (dolist (key '("C-c 1" "C-c 2" "C-c 3"))
          (should (commandp (lookup-key codex-ide-session-mode-map
                                        (kbd key)))))
        (call-interactively command))
      (should (eq (window-buffer (selected-window))
                  (codex-ide-session-buffer session-c))))))

(ert-deftest codex-ide-monitor-layout-for-sessions-displays-only-selected-sessions ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c session-d)
    (save-window-excursion
      (codex-ide-monitor-layout-for-sessions
       (list session-d session-b session-c)
       session-b)
      (should (eq (window-buffer (selected-window))
                  (codex-ide-session-buffer session-b)))
      (should-not (get-buffer-window (codex-ide-session-buffer session-a) nil))
      (should (get-buffer-window (codex-ide-session-buffer session-c) nil))
      (should (get-buffer-window (codex-ide-session-buffer session-d) nil)))))

(ert-deftest codex-ide-monitor-layout-for-sessions-displays-all-selected-sessions ()
  (codex-ide-monitor-test-with-sessions
      (session-a session-b session-c session-d session-e session-f)
    (save-window-excursion
      (codex-ide-monitor-layout-for-sessions
       (list session-a session-b session-c session-d session-e session-f)
       session-a)
      (should (= (length (window-list nil 'no-minibuf)) 6))
      (dolist (session (list session-a session-b session-c session-d
                             session-e session-f))
        (should (get-buffer-window (codex-ide-session-buffer session) nil))))))

(ert-deftest codex-ide-monitor-promote-rail-key-preserves-explicit-session-scope ()
  (codex-ide-monitor-test-with-sessions (session-a session-b session-c session-d)
    (save-window-excursion
      (codex-ide-monitor-layout-for-sessions
       (list session-a session-b session-c)
       session-a)
      (call-interactively
       (lookup-key codex-ide-session-mode-map (kbd "C-c 1")))
      (should (eq (window-buffer (selected-window))
                  (codex-ide-session-buffer session-b)))
      (should (get-buffer-window (codex-ide-session-buffer session-a) nil))
      (should (get-buffer-window (codex-ide-session-buffer session-c) nil))
      (should-not (get-buffer-window (codex-ide-session-buffer session-d) nil)))))

(ert-deftest codex-ide-monitor-promote-session-errors-outside-live-session ()
  (codex-ide-monitor-test-with-sessions (session-a session-b)
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (should-error (codex-ide-monitor-promote-session)
                      :type 'user-error)))))

;;; codex-ide-monitor-tests.el ends here
