;;; -*- lexical-binding: t -*-

;;; Commentary:

;; Shows debug info about running codex buffers.

;;; Code:
(require 'codex-ide)
(require 'codex-ide-core)
(require 'seq)
(require 'subr-x)

(defun codex-ide-debug-info--active-buffer-name (directory)
  "Return the active non-Codex buffer name for DIRECTORY, or nil."
  (let ((buffer (gethash (codex-ide--normalize-directory directory)
                         codex-ide--active-buffer-objects)))
    (when (buffer-live-p buffer)
      (buffer-name buffer))))

(defun codex-ide-debug-info--active-buffer-context (directory)
  "Return the tracked active buffer context for DIRECTORY, or nil."
  (gethash (codex-ide--normalize-directory directory)
           codex-ide--active-buffer-contexts))

(defun codex-ide-debug-info--format-value (value)
  "Return VALUE formatted for debug display."
  (if (stringp value)
      value
    (prin1-to-string value)))

(defun codex-ide-debug-info--context-lines (context)
  "Return nested bullet lines for CONTEXT."
  (if context
      (mapcar (lambda (entry)
                (format "    - %s: %s"
                        (car entry)
                        (codex-ide-debug-info--format-value (cdr entry))))
              context)
    '("    - nil")))

(defun codex-ide-debug-info--project-lines (directory)
  "Return formatted debug lines for DIRECTORY."
  (let* ((sessions (codex-ide--sessions-for-directory directory t))
         (active-session (codex-ide--last-active-session-for-directory directory))
         (active-session-buffer (when-let ((buffer (and active-session
                                                       (codex-ide-session-buffer active-session))))
                                  (when (buffer-live-p buffer)
                                    (buffer-name buffer))))
         (active-buffer (codex-ide-debug-info--active-buffer-name directory))
         (active-buffer-context (codex-ide-debug-info--active-buffer-context directory)))
    (append
     (list (format "Project: %s" (codex-ide--project-name directory))
           (format "  - %d active session%s"
                   (length sessions)
                   (if (= (length sessions) 1) "" "s"))
           (format "  - Active session: %S" active-session-buffer)
           (format "  - Active buffer: %S" active-buffer)
           "  - Active buffer context:")
     (codex-ide-debug-info--context-lines active-buffer-context))))

(defun codex-ide-debug-info--message ()
  "Return the formatted Codex IDE debug info message."
  (let ((directories (codex-ide--live-session-directories)))
    (if directories
        (string-join
         (append
          '("Codex IDE Debug Info")
          (seq-mapcat
           (lambda (directory)
             (append '("")
                     (codex-ide-debug-info--project-lines directory)))
           directories))
         "\n")
      "Codex IDE Debug Info\n\nNo active sessions")))

;;;###autoload
(defun codex-ide-show-debug-info ()
  "Show a minibuffer summary of live Codex IDE session state."
  (interactive)
  (message "%s" (codex-ide-debug-info--message)))


(provide 'codex-ide-debug-info)

;; Local Variables:
;; read-symbol-shorthands: (("my/" . "codex-ide-debug-info-"))
;; End:
;;; codex-ide-debug-info ends here
