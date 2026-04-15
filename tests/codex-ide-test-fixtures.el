;;; codex-ide-test-fixtures.el --- Shared fixtures for codex-ide tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared helpers for codex-ide ERT suites.

;;; Code:

(require 'cl-lib)

(setq load-prefer-newer t)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(cl-defstruct (codex-ide-test-process
               (:constructor codex-ide-test-process-create))
  live
  plist
  sent-strings)

(defconst codex-ide-test--root-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root used by the codex-ide test suite.")

(defun codex-ide-test--process-put (process key value)
  "Store VALUE at KEY on fake PROCESS."
  (setf (codex-ide-test-process-plist process)
        (plist-put (codex-ide-test-process-plist process) key value))
  value)

(defun codex-ide-test--process-get (process key)
  "Return KEY from fake PROCESS."
  (plist-get (codex-ide-test-process-plist process) key))

(defun codex-ide-test--cleanup-buffers (buffers-before)
  "Kill buffers created after BUFFERS-BEFORE."
  (dolist (buffer (buffer-list))
    (unless (memq buffer buffers-before)
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer))))))

(defmacro codex-ide-test-with-fixture (directory &rest body)
  "Run BODY in an isolated codex-ide fixture rooted at DIRECTORY."
  (declare (indent 1) (debug t))
	  `(let* ((default-directory (file-name-as-directory ,directory))
	          (buffers-before (buffer-list))
	          (codex-ide--cli-available nil)
	          (codex-ide--sessions nil)
	          (codex-ide--active-buffer-contexts (make-hash-table :test 'equal))
	          (codex-ide--active-buffer-objects (make-hash-table :test 'equal))
	          (codex-ide-persisted-project-state (make-hash-table :test 'equal))
          (codex-ide--session-metadata (make-hash-table :test 'eq))
          (codex-ide-session-baseline-prompt nil)
          (codex-ide-new-session-split nil)
          (codex-ide-want-mcp-bridge 'prompt)
          (codex-ide-new-session-split nil)
          (codex-ide-want-mcp-bridge 'prompt)
          (codex-ide-enable-emacs-tool-bridge nil))
     (unwind-protect
         (progn ,@body)
       (when codex-ide-track-active-buffer-mode
         (codex-ide-track-active-buffer-mode -1))
       (codex-ide-test--cleanup-buffers buffers-before))))

(defmacro codex-ide-test-with-fake-processes (&rest body)
  "Run BODY with process primitives redirected to fake process objects."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'make-process)
              (lambda (&rest plist)
                (codex-ide-test-process-create
                 :live t
                 :plist (list :make-process-spec plist)
                 :sent-strings nil)))
             ((symbol-function 'make-pipe-process)
              (lambda (&rest plist)
                (codex-ide-test-process-create
                 :live t
                 :plist (list :make-pipe-process-spec plist)
                 :sent-strings nil)))
             ((symbol-function 'process-live-p)
              (lambda (process)
                (and (codex-ide-test-process-p process)
                     (codex-ide-test-process-live process))))
             ((symbol-function 'delete-process)
              (lambda (process)
                (setf (codex-ide-test-process-live process) nil)
                nil))
             ((symbol-function 'process-put)
              #'codex-ide-test--process-put)
             ((symbol-function 'process-get)
              #'codex-ide-test--process-get)
             ((symbol-function 'process-send-string)
              (lambda (process string)
                (setf (codex-ide-test-process-sent-strings process)
                      (append (codex-ide-test-process-sent-strings process)
                              (list string)))))
             ((symbol-function 'set-process-query-on-exit-flag)
              (lambda (&rest _) nil))
             ((symbol-function 'accept-process-output)
              (lambda (&rest _) nil)))
     ,@body))

(defun codex-ide-test--make-temp-project ()
  "Create and return a temporary project directory."
  (make-temp-file "codex-ide-tests-" t))

(defun codex-ide-test--make-project-file (directory name contents)
  "Create file NAME with CONTENTS under DIRECTORY and return its path."
  (let ((path (expand-file-name name directory)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert contents))
    path))

(provide 'codex-ide-test-fixtures)

;;; codex-ide-test-fixtures.el ends here
