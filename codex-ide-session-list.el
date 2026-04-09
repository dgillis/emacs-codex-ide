;;; codex-ide-session-list.el --- Shared tabulated session list UI -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared `tabulated-list-mode' helpers used by Codex session list buffers.

;;; Code:

(require 'codex-ide)
(require 'hl-line)
(require 'tabulated-list)

(defface codex-ide-session-list-primary-face
  '((t :inherit default :weight semibold))
  "Face used for the primary column in Codex list views."
  :group 'codex-ide)

(defface codex-ide-session-list-secondary-face
  '((t :inherit shadow))
  "Face used for secondary columns in Codex list views."
  :group 'codex-ide)

(defface codex-ide-session-list-id-face
  '((t :inherit font-lock-constant-face))
  "Face used for thread and identifier columns in Codex list views."
  :group 'codex-ide)

(defface codex-ide-session-list-status-face
  '((t :inherit font-lock-keyword-face :weight semibold))
  "Face used for status columns in Codex list views."
  :group 'codex-ide)

(defface codex-ide-session-list-time-face
  '((t :inherit font-lock-doc-face))
  "Face used for timestamp columns in Codex list views."
  :group 'codex-ide)

(defface codex-ide-session-list-current-row-face
  '((((class color) (background light))
     :background "#f2f5e9")
    (((class color) (background dark))
     :background "#2a3126"))
  "Face used to highlight the current row in Codex list views."
  :group 'codex-ide)

(defvar-local codex-ide-session-list--entries-function nil
  "Function that returns `tabulated-list-entries' for the current buffer.")

(defvar-local codex-ide-session-list--visit-function nil
  "Function called with the current row id when visiting an entry.")

(defvar codex-ide-session-list-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-session-list-mode'.")

(set-keymap-parent codex-ide-session-list-mode-map tabulated-list-mode-map)
(define-key codex-ide-session-list-mode-map (kbd "RET") #'codex-ide-session-list-visit)

(define-derived-mode codex-ide-session-list-mode tabulated-list-mode "Codex-Session-List"
  "Parent mode for Codex session list buffers.")

(defun codex-ide-session-list-cell (text face)
  "Return TEXT propertized with FACE for a tabulated list cell."
  (propertize (or text "") 'face face))

(defun codex-ide-session-list--tabulated-entries ()
  "Return tabulated list entries for the current buffer."
  (unless (functionp codex-ide-session-list--entries-function)
    (error "No Codex session list entries function configured"))
  (funcall codex-ide-session-list--entries-function))

(defun codex-ide-session-list-visit ()
  "Visit the row at point."
  (interactive)
  (unless (functionp codex-ide-session-list--visit-function)
    (user-error "Nothing to visit in this list"))
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No list entry at point"))
    (funcall codex-ide-session-list--visit-function id)))

(defun codex-ide-session-list--setup
    (buffer-name mode format entries-function visit-function
                 &optional sort-key setup-function)
  "Create and return a session list buffer.
BUFFER-NAME names the buffer.  MODE is the major mode function to call.
FORMAT is assigned to `tabulated-list-format'.  ENTRIES-FUNCTION computes
the rows, and VISIT-FUNCTION handles `RET'.  SORT-KEY initializes
`tabulated-list-sort-key' when non-nil.  SETUP-FUNCTION is called in the
buffer before the first render when non-nil."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (funcall mode)
      (setq tabulated-list-format format
            tabulated-list-padding 2
            tabulated-list-sort-key sort-key
            hl-line-face 'codex-ide-session-list-current-row-face
            codex-ide-session-list--entries-function entries-function
            codex-ide-session-list--visit-function visit-function
            tabulated-list-entries #'codex-ide-session-list--tabulated-entries)
      (hl-line-mode 1)
      (when (functionp setup-function)
        (funcall setup-function))
      (tabulated-list-init-header)
      (tabulated-list-print t))
    buffer))

(provide 'codex-ide-session-list)

;;; codex-ide-session-list.el ends here
