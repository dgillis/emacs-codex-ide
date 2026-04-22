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

(defun codex-ide-test--log-buffer-name (session)
  "Return the computed log buffer name for SESSION."
  (if (codex-ide-session-query-only session)
      (codex-ide--append-buffer-name-suffix
       (format "*%s-log[%s]-query*"
               codex-ide-buffer-name-prefix
               (file-name-nondirectory
                (directory-file-name (codex-ide-session-directory session))))
       (and (integerp (codex-ide-session-name-suffix session))
            (> (codex-ide-session-name-suffix session) 0)
            (codex-ide-session-name-suffix session)))
    (replace-regexp-in-string
     "\\*$" "-log*"
     (if (buffer-live-p (codex-ide-session-buffer session))
         (buffer-name (codex-ide-session-buffer session))
       (codex-ide--session-buffer-name
        (codex-ide-session-directory session)
        (codex-ide-session-name-suffix session))))))

(defun codex-ide-test--log-buffer (session)
  "Return SESSION's computed log buffer, if any."
  (get-buffer (codex-ide-test--log-buffer-name session)))

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
          (codex-ide-logging-enabled t)
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
  `(let ((old-make-process (symbol-function 'make-process))
         (old-make-pipe-process (symbol-function 'make-pipe-process))
         (old-process-live-p (symbol-function 'process-live-p))
         (old-delete-process (symbol-function 'delete-process))
         (old-process-put (symbol-function 'process-put))
         (old-process-get (symbol-function 'process-get))
         (old-process-send-string (symbol-function 'process-send-string))
         (old-set-process-query-on-exit-flag
          (symbol-function 'set-process-query-on-exit-flag))
         (old-accept-process-output (symbol-function 'accept-process-output)))
     (unwind-protect
         (progn
           (fset 'make-process
                 (lambda (&rest plist)
                   (codex-ide-test-process-create
                    :live t
                    :plist (list :make-process-spec plist)
                    :sent-strings nil)))
           (fset 'make-pipe-process
                 (lambda (&rest plist)
                   (codex-ide-test-process-create
                    :live t
                    :plist (list :make-pipe-process-spec plist)
                    :sent-strings nil)))
           (fset 'process-live-p
                 (lambda (process)
                   (and (codex-ide-test-process-p process)
                        (codex-ide-test-process-live process))))
           (fset 'delete-process
                 (lambda (process)
                   (setf (codex-ide-test-process-live process) nil)
                   nil))
           (fset 'process-put #'codex-ide-test--process-put)
           (fset 'process-get #'codex-ide-test--process-get)
           (fset 'process-send-string
                 (lambda (process string)
                   (setf (codex-ide-test-process-sent-strings process)
                         (append (codex-ide-test-process-sent-strings process)
                                 (list string)))))
           (fset 'set-process-query-on-exit-flag
                 (lambda (&rest _) nil))
           (fset 'accept-process-output
                 (lambda (&rest _) nil))
           ,@body)
       (fset 'make-process old-make-process)
       (fset 'make-pipe-process old-make-pipe-process)
       (fset 'process-live-p old-process-live-p)
       (fset 'delete-process old-delete-process)
       (fset 'process-put old-process-put)
       (fset 'process-get old-process-get)
       (fset 'process-send-string old-process-send-string)
       (fset 'set-process-query-on-exit-flag
             old-set-process-query-on-exit-flag)
       (fset 'accept-process-output old-accept-process-output))))

(defvar codex-ide-test--fake-process-primitives-prewarmed nil
  "Whether fake process primitive rebinding has been prewarmed.")

(defun codex-ide-test--prewarm-fake-process-primitives ()
  "Eagerly pay Emacs's first-time process primitive rebinding cost.

This avoids attributing one-time subr rebinding overhead to the first test that
uses `codex-ide-test-with-fake-processes'."
  (unless codex-ide-test--fake-process-primitives-prewarmed
    (setq codex-ide-test--fake-process-primitives-prewarmed t)
    (codex-ide-test-with-fake-processes nil)))

(codex-ide-test--prewarm-fake-process-primitives)

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
