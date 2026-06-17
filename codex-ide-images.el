;;; codex-ide-images.el --- Local image input helpers for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands and small helpers for attaching local image files to the current
;; Codex prompt.  The regular prompt submit path sends them as app-server
;; `localImage' input items.

;;; Code:

(require 'subr-x)
(require 'codex-ide-session)

(declare-function codex-ide-session-buffer "codex-ide-core" (session))
(declare-function codex-ide--add-pending-local-image "codex-ide-transcript"
                  (session path &optional temporary))
(declare-function codex-ide--ensure-input-prompt "codex-ide-transcript"
                  (&optional session initial-text))
(declare-function codex-ide--ensure-session-for-current-project "codex-ide-session" ())

(defconst codex-ide--clipboard-png-class
  (format "%cclass PNGf%c" #x00ab #x00bb)
  "AppleScript PNG pasteboard class literal.")

(defun codex-ide--normalize-image-file (path)
  "Return expanded PATH after validating it is a readable file."
  (let ((expanded (expand-file-name path)))
    (unless (and (file-readable-p expanded)
                 (not (file-directory-p expanded)))
      (user-error "Image file is not readable: %s" path))
    expanded))

(defun codex-ide--read-image-file ()
  "Read an image file path for Codex local image input."
  (codex-ide--normalize-image-file
   (read-file-name "Image file: " nil nil t)))

(defun codex-ide--clipboard-image-osascript-args (path)
  "Return `osascript' arguments that write the clipboard PNG image to PATH."
  (list
   "-e" (format "set outputPath to POSIX file %S" path)
   "-e" (format "set imageData to the clipboard as %s"
                codex-ide--clipboard-png-class)
   "-e" "set fileRef to open for access outputPath with write permission"
   "-e" "try"
   "-e" "set eof of fileRef to 0"
   "-e" "write imageData to fileRef"
   "-e" "close access fileRef"
   "-e" "on error errMsg number errNum"
   "-e" "try"
   "-e" "close access fileRef"
   "-e" "end try"
   "-e" "error errMsg number errNum"
   "-e" "end try"))

(defun codex-ide--clipboard-image-error-message (details)
  "Return a user-facing clipboard image error for DETAILS."
  (if (string-match-p "\\(-1700\\|expected type\\|clipboard\\)" details)
      "Clipboard does not contain a PNG image"
    (format "Failed to save clipboard image: %s" details)))

(defun codex-ide--save-clipboard-image ()
  "Save the macOS clipboard PNG image to a temporary file and return its path."
  (unless (eq system-type 'darwin)
    (user-error "Clipboard image submission currently supports macOS only"))
  (unless (executable-find "osascript")
    (user-error "Cannot find osascript for clipboard image submission"))
  (let ((path (make-temp-file "codex-ide-clipboard-" nil ".png"))
        (buffer (generate-new-buffer " *codex-ide-clipboard-image*")))
    (unwind-protect
        (let ((exit-code
               (apply #'call-process
                      "osascript"
                      nil
                      buffer
                      nil
                      (codex-ide--clipboard-image-osascript-args path))))
          (if (and (integerp exit-code)
                   (zerop exit-code))
              path
            (delete-file path)
            (let ((details (string-trim
                            (with-current-buffer buffer
                              (buffer-string)))))
              (user-error "%s"
                          (codex-ide--clipboard-image-error-message
                           details)))))
      (kill-buffer buffer))))

(defun codex-ide--attach-image (path &optional temporary)
  "Attach local image PATH to the current Codex prompt."
  (let ((session (codex-ide--ensure-session-for-current-project)))
    (with-current-buffer (codex-ide-session-buffer session)
      (codex-ide--ensure-input-prompt session)
      (codex-ide--add-pending-local-image session path temporary))
    (message "Attached image: %s" (file-name-nondirectory path))))

;;;###autoload
(defun codex-ide-submit-image (path)
  "Attach local image file PATH to the current Codex prompt."
  (interactive (list (codex-ide--read-image-file)))
  (codex-ide--attach-image (codex-ide--normalize-image-file path)))

;;;###autoload
(defun codex-ide-submit-clipboard-image ()
  "Attach the macOS clipboard image to the current Codex prompt."
  (interactive)
  (codex-ide--attach-image (codex-ide--save-clipboard-image) t))

(provide 'codex-ide-images)

;;; codex-ide-images.el ends here
