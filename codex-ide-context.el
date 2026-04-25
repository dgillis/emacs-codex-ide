;;; codex-ide-context.el --- Prompt context composition for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns the Emacs-context payload that codex-ide attaches to prompts
;; and the related prompt-history persistence.
;;
;; In practice this includes four tightly related concerns:
;;
;; - Formatting the one-time session baseline prompt.
;; - Formatting the per-prompt editor context block derived from active buffers
;;   and selected regions.
;; - Detecting and stripping those context blocks when replaying or previewing
;;   stored thread text.
;; - Building the structured payload that transcript/session code submits to the
;;   app-server.
;;
;; Keeping this logic separate from transcript rendering and session lifecycle
;; makes the dependency direction clearer: transcript code asks for a prompt
;; payload, but it does not need to know how editor context is discovered or
;; serialized.

;;; Code:

(require 'subr-x)
(require 'codex-ide-core)

(defvar codex-ide-session-baseline-prompt)

(defconst codex-ide--session-context-open-tag "[Emacs session context]")
(defconst codex-ide--session-context-close-tag "[/Emacs session context]")
(defconst codex-ide--prompt-context-open-tag "[Emacs prompt context]")
(defconst codex-ide--prompt-context-close-tag "[/Emacs prompt context]")
(defconst codex-ide--discarded-buffer-context-message
  "Codex buffer context is being discarded since the buffer does not exist.")
(defconst codex-ide--selection-text-limit 400
  "Maximum number of selection characters to include in context payloads.")

(defconst codex-ide--selection-summary-text-limit 12
  "Maximum selection text length to render directly in context summaries.")

(cl-defun codex-ide--make-buffer-context (&optional buffer &key working-dir)
  "Build Codex context for BUFFER or the current buffer.
When WORKING-DIR is nil, infer the project directory from BUFFER."
  (when-let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when-let ((working-dir (codex-ide--normalize-directory
                                 (or working-dir
                                     (codex-ide--get-working-directory)))))
          (let ((file-path (buffer-file-name)))
            `((file . ,(and file-path (expand-file-name file-path)))
              (buffer-name . ,(buffer-name))
              (point . ,(point))
              (line . ,(line-number-at-pos))
              (column . ,(current-column))
              (project-dir . ,working-dir))))))))

(defun codex-ide--buffer-context-ambient-project-p (context &optional working-dir)
  "Return non-nil when CONTEXT should be tracked as ambient project context.
When WORKING-DIR is non-nil, require CONTEXT to belong to that project."
  (let* ((project-dir (codex-ide--normalize-directory
                       (or working-dir
                           (alist-get 'project-dir context))))
         (file (alist-get 'file context)))
    (and project-dir
         file
         (file-in-directory-p file (file-name-as-directory project-dir))
         (string= (alist-get 'project-dir context) project-dir))))

(defun codex-ide--buffer-selection-context (&optional buffer)
  "Return BUFFER's active region bounds as an alist, or nil.
The return value contains 1-based line numbers and 0-based columns."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (append
           `((start . ,start)
             (end . ,end)
             (start-line . ,(line-number-at-pos start))
             (start-column . ,(save-excursion
                                (goto-char start)
                                (current-column)))
             (end-line . ,(line-number-at-pos end))
             (end-column . ,(save-excursion
                              (goto-char end)
                              (current-column))))
           (when (<= (- end start) codex-ide--selection-text-limit)
             `((text . ,(buffer-substring-no-properties start end))))))))))

(defun codex-ide--buffer-context-display-file (context)
  "Return the display label for CONTEXT."
  (let* ((file (alist-get 'file context))
         (buffer-name (alist-get 'buffer-name context))
         (project-dir (alist-get 'project-dir context))
         (project-dir-path (and project-dir (file-name-as-directory project-dir))))
    (cond
     ((not file)
      (format "[buffer] %s" buffer-name))
     ((and project-dir-path
           (file-in-directory-p file project-dir-path))
      (file-relative-name file project-dir-path))
     (t
      (abbreviate-file-name file)))))

(defun codex-ide--format-session-context ()
  "Format the one-time session baseline prompt block."
  (when-let ((prompt (and (stringp codex-ide-session-baseline-prompt)
                          (string-trim codex-ide-session-baseline-prompt))))
    (unless (string-empty-p prompt)
      (format (concat "%s\n"
                      "Take the following into account in this prompt and all following ones:\n"
                      "%s\n"
                      "%s\n")
              codex-ide--session-context-open-tag
              prompt
              codex-ide--session-context-close-tag))))

(defun codex-ide--format-buffer-context (context)
  "Format CONTEXT for insertion into a Codex prompt."
  (let ((selection (alist-get 'selection context)))
    (format (concat "%s\n"
                    "Last file/buffer focused in Emacs: %s\n"
                    "Buffer: %s\n"
                    "Cursor: point %s, line %s, column %s\n"
                    "%s"
                    "%s\n")
            codex-ide--prompt-context-open-tag
            (codex-ide--buffer-context-display-file context)
            (alist-get 'buffer-name context)
            (alist-get 'point context)
            (alist-get 'line context)
            (alist-get 'column context)
            (if selection
                (format (concat "Selected region start: %s\n"
                                "Selected region end: %s\n"
                                "Selected region text: %s\n")
                        (alist-get 'start selection)
                        (alist-get 'end selection)
                        (or (alist-get 'text selection) ""))
              "")
            codex-ide--prompt-context-close-tag)))

(defun codex-ide--format-buffer-context-summary (context)
  "Return a compact transcript summary line for CONTEXT."
  (let ((selection (alist-get 'selection context)))
    (string-join
     (delq nil
           (list
            (format "Context: %s %s:%s"
                    (alist-get 'buffer-name context)
                    (alist-get 'line context)
                    (alist-get 'column context))
            (when selection
              (let ((text (alist-get 'text selection)))
                (format "selection=%s"
                        (if (and text
                                 (< (length text) codex-ide--selection-summary-text-limit))
                            (replace-regexp-in-string
                             "\n" "\\\\n"
                             (prin1-to-string text)
                             t t)
                          (format "%s:%s-%s:%s"
                                  (alist-get 'start-line selection)
                                  (alist-get 'start-column selection)
                                  (alist-get 'end-line selection)
                                  (alist-get 'end-column selection))))))))
     " ")))

(defun codex-ide--format-discarded-buffer-context ()
  "Return a prompt-context block for a discarded stale buffer context."
  (format "%s\n%s\n%s\n"
          codex-ide--prompt-context-open-tag
          codex-ide--discarded-buffer-context-message
          codex-ide--prompt-context-close-tag))

(defun codex-ide--context-with-selected-region (context &optional buffer)
  "Return CONTEXT augmented with BUFFER's active region, when present."
  (if-let ((selection (codex-ide--buffer-selection-context buffer)))
      (append context `((selection . ,selection)))
    context))

(defun codex-ide--context-buffer-resolution (&optional working-dir)
  "Resolve the live buffer to use for prompt context in WORKING-DIR.
Return an alist containing either `(buffer . BUFFER)' or `(discarded . t)'."
  (let ((working-dir (codex-ide--normalize-directory
                      (or working-dir
                          (codex-ide--get-working-directory)))))
    (cond
     ((buffer-live-p codex-ide--prompt-origin-buffer)
      `((buffer . ,codex-ide--prompt-origin-buffer)))
     (codex-ide--prompt-origin-buffer
      `((discarded . t)))
     (t
      (let ((tracked-buffer (and working-dir
                                 (gethash working-dir
                                          codex-ide--active-buffer-objects))))
        (cond
         ((buffer-live-p tracked-buffer)
          `((buffer . ,tracked-buffer)))
         ((when-let ((inferred (codex-ide--infer-recent-file-buffer)))
            (puthash working-dir inferred codex-ide--active-buffer-objects)
            `((buffer . ,inferred))))
         ((and working-dir
               (gethash working-dir codex-ide--active-buffer-contexts))
          (remhash working-dir codex-ide--active-buffer-contexts)
          (remhash working-dir codex-ide--active-buffer-objects)
          `((discarded . t)))
         (t nil)))))))

(defun codex-ide--push-prompt-history (session prompt)
  "Record PROMPT in SESSION history."
  (let ((trimmed (string-trim-right prompt)))
    (unless (string-empty-p trimmed)
      (codex-ide--project-persisted-put
       :prompt-history
       (cons trimmed
             (delete trimmed
                     (copy-sequence
                      (or (codex-ide--project-persisted-get :prompt-history session)
                          '()))))
       session)
      (codex-ide--reset-prompt-history-navigation session))))

(defun codex-ide--context-payload-for-prompt ()
  "Return context payload metadata for the current prompt, or nil."
  (let* ((working-dir (codex-ide--get-working-directory))
         (resolution (codex-ide--context-buffer-resolution working-dir))
         (context-buffer (alist-get 'buffer resolution)))
    (cond
     (context-buffer
      (when-let ((context (codex-ide--make-buffer-context
                           context-buffer
                           :working-dir working-dir)))
        (unless codex-ide--prompt-origin-buffer
          (puthash working-dir context codex-ide--active-buffer-contexts))
        (let* ((context-with-selection
                (codex-ide--context-with-selected-region
                 context
                 context-buffer))
               (formatted-context
                (codex-ide--format-buffer-context context-with-selection))
               (context-summary
                (codex-ide--format-buffer-context-summary context-with-selection)))
          `((formatted . ,formatted-context)
            (summary . ,context-summary)))))
     ((alist-get 'discarded resolution)
      `((formatted . ,(codex-ide--format-discarded-buffer-context))
        (summary . ,codex-ide--discarded-buffer-context-message))))))

(defun codex-ide--compose-turn-payload (prompt)
  "Build prompt payload metadata for PROMPT in the current working directory."
  (let* ((context-payload (codex-ide--context-payload-for-prompt))
         (context-prefix (alist-get 'formatted context-payload))
         (session (codex-ide--get-default-session-for-current-buffer))
         (session-prefix (unless (codex-ide--session-metadata-get session :session-context-sent)
                           (codex-ide--format-session-context)))
         (prompt-prefix (unless (codex-ide--leading-emacs-context-prefix-p prompt)
                          context-prefix))
         (full-prompt (string-join (delq nil (list session-prefix prompt-prefix prompt))
                                   "\n\n")))
    `((context-summary . ,(alist-get 'summary context-payload))
      (included-session-context . ,(and session-prefix t))
      (input . [((type . "text")
                 (text . ,full-prompt))]))))

(defun codex-ide--compose-turn-input (prompt)
  "Build `turn/start' input items for PROMPT."
  (alist-get 'input (codex-ide--compose-turn-payload prompt)))

(defun codex-ide--strip-leading-context-block (text open-tag close-tag)
  "Remove a leading context block delimited by OPEN-TAG and CLOSE-TAG from TEXT."
  (if (and (stringp text)
           (string-prefix-p open-tag text)
           (string-match (regexp-quote close-tag) text))
      (string-trim-left (substring text (match-end 0)))
    text))

(defun codex-ide--strip-emacs-context-prefix (text)
  "Remove any leading Emacs session or prompt context block from TEXT."
  (let ((stripped text)
        (changed t))
    (while changed
      (setq changed nil)
      (dolist (tags `((,codex-ide--session-context-open-tag . ,codex-ide--session-context-close-tag)
                      (,codex-ide--prompt-context-open-tag . ,codex-ide--prompt-context-close-tag)
                      ("[Emacs context]" . "[/Emacs context]")))
        (let ((next (codex-ide--strip-leading-context-block
                     stripped
                     (car tags)
                     (cdr tags))))
          (unless (equal next stripped)
            (setq stripped next
                  changed t)))))
    stripped))

(defun codex-ide--leading-emacs-context-prefix-p (text)
  "Return non-nil when TEXT begins with a known Emacs context prefix marker."
  (and (stringp text)
       (or (string-prefix-p codex-ide--session-context-open-tag text)
           (string-prefix-p codex-ide--prompt-context-open-tag text)
           (string-prefix-p "[Emacs context]" text))))

(provide 'codex-ide-context)

;;; codex-ide-context.el ends here
