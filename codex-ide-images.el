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
                  (session path))
(declare-function codex-ide--ensure-input-prompt "codex-ide-transcript"
                  (&optional session initial-text))
(declare-function codex-ide--ensure-session-for-current-project "codex-ide-session" ())
(declare-function codex-ide--local-image-temp-prefix "codex-ide-transcript"
                  (&optional session))

(defconst codex-ide--clipboard-image-swift-helper-source
  (string-join
   '("import AppKit"
     "import Foundation"
     ""
     "func fail(_ message: String) -> Never {"
     "    if let data = (message + \"\\n\").data(using: .utf8) {"
     "        FileHandle.standardError.write(data)"
     "    }"
     "    exit(2)"
     "}"
     ""
     "let arguments = CommandLine.arguments"
     "guard arguments.count >= 2 else { fail(\"missing output path\") }"
     "let outputPath = arguments[1]"
     "let outputURL = URL(fileURLWithPath: outputPath)"
     "let pasteboard = NSPasteboard.general"
     "let imageExtensions: Set<String> = ["
     "    \"png\", \"jpg\", \"jpeg\", \"gif\", \"heic\", \"heif\","
     "    \"webp\", \"tiff\", \"tif\", \"bmp\""
     "]"
     ""
     "if let objects = pasteboard.readObjects("
     "    forClasses: [NSURL.self],"
     "    options: [.urlReadingFileURLsOnly: true]"
     ") {"
     "    for object in objects {"
     "        guard let url = object as? URL, url.isFileURL else { continue }"
     "        let path = url.path"
     "        guard FileManager.default.isReadableFile(atPath: path) else { continue }"
     "        let ext = url.pathExtension.lowercased()"
     "        if imageExtensions.contains(ext) || NSImage(contentsOf: url) != nil {"
     "            print(path)"
     "            exit(0)"
     "        }"
     "    }"
     "}"
     ""
     "if let data = pasteboard.data(forType: .png), data.count > 0 {"
     "    do {"
     "        try data.write(to: outputURL, options: .atomic)"
     "        print(outputPath)"
     "        exit(0)"
     "    } catch {"
     "        fail(\"failed to write PNG data: \\(error)\")"
     "    }"
     "}"
     ""
     "func writePNG(_ image: NSImage) -> Bool {"
     "    guard let tiff = image.tiffRepresentation,"
     "          let rep = NSBitmapImageRep(data: tiff),"
     "          let png = rep.representation(using: .png, properties: [:]) else {"
     "        return false"
     "    }"
     "    do {"
     "        try png.write(to: outputURL, options: .atomic)"
     "        print(outputPath)"
     "        return true"
     "    } catch {"
     "        fail(\"failed to write converted image data: \\(error)\")"
     "    }"
     "}"
     ""
     "if let data = pasteboard.data(forType: .tiff),"
     "   data.count > 0,"
     "   let image = NSImage(data: data),"
     "   writePNG(image) {"
     "    exit(0)"
     "}"
     ""
     "if let image = NSImage(pasteboard: pasteboard), writePNG(image) {"
     "    exit(0)"
     "}"
     ""
     "fail(\"clipboard does not contain an image\")")
   "\n")
  "Swift helper source used to extract macOS clipboard images.")

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

(defun codex-ide--clipboard-image-swift-helper-path ()
  "Return the generated Swift clipboard image helper path."
  (let ((path (expand-file-name
               "codex-ide-clipboard-image-helper.swift"
               temporary-file-directory)))
    (unless (and (file-exists-p path)
                 (with-temp-buffer
                   (insert-file-contents path)
                   (equal (buffer-string)
                          codex-ide--clipboard-image-swift-helper-source)))
      (with-temp-file path
        (insert codex-ide--clipboard-image-swift-helper-source)))
    path))

(defun codex-ide--save-clipboard-image-with-swift (path)
  "Save the macOS clipboard image to PATH using Swift.
Return the path to attach.  This may be an existing image file path when the
pasteboard contains file URLs."
  (let ((buffer (generate-new-buffer " *codex-ide-clipboard-image-swift*")))
    (unwind-protect
        (let ((exit-code
               (call-process
                "swift"
                nil
                buffer
                nil
                (codex-ide--clipboard-image-swift-helper-path)
                path)))
          (if (and (integerp exit-code)
                   (zerop exit-code))
              (let ((attached-path
                     (string-trim
                      (with-current-buffer buffer
                        (buffer-string)))))
                (unless (and (stringp attached-path)
                             (not (string-empty-p attached-path))
                             (file-readable-p attached-path))
                  (user-error "Clipboard image helper did not return a readable image path"))
                (unless (equal (expand-file-name attached-path)
                               (expand-file-name path))
                  (ignore-errors
                    (delete-file path)))
                attached-path)
            (ignore-errors
              (delete-file path))
            (let ((details (string-trim
                            (with-current-buffer buffer
                              (buffer-string)))))
              (user-error "%s"
                          (if (string-empty-p details)
                              "Clipboard does not contain an image"
                            details)))))
      (kill-buffer buffer))))

(defun codex-ide--save-clipboard-image (session)
  "Save the macOS clipboard image for SESSION and return its path."
  (unless (eq system-type 'darwin)
    (user-error "Clipboard image submission currently supports macOS only"))
  (unless (executable-find "swift")
    (user-error
     "Clipboard image submission requires the macOS Swift command line tool"))
  (let ((path (make-temp-file (codex-ide--local-image-temp-prefix session)
                              nil
                              ".png")))
    (codex-ide--save-clipboard-image-with-swift path)))

(defun codex-ide--attach-image (path)
  "Attach local image PATH to the current Codex prompt."
  (let ((session (codex-ide--ensure-session-for-current-project)))
    (with-current-buffer (codex-ide-session-buffer session)
      (codex-ide--ensure-input-prompt session)
      (codex-ide--add-pending-local-image session path))
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
  (let ((session (codex-ide--ensure-session-for-current-project)))
    (codex-ide--attach-image (codex-ide--save-clipboard-image session))))

(provide 'codex-ide-images)

;;; codex-ide-images.el ends here
