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
                    "Cursor: line %s, column %s\n"
                    "%s"
                    "%s\n")
            codex-ide--prompt-context-open-tag
            (alist-get 'display-file context)
            (alist-get 'buffer-name context)
            (alist-get 'line context)
            (alist-get 'column context)
            (if selection
                (format "Selected region: line %s, column %s to line %s, column %s\n"
                        (alist-get 'start-line selection)
                        (alist-get 'start-column selection)
                        (alist-get 'end-line selection)
                        (alist-get 'end-column selection))
              "")
            codex-ide--prompt-context-close-tag)))

(defun codex-ide--format-buffer-context-summary (context)
  "Return a compact transcript summary line for CONTEXT."
  (let ((selection (alist-get 'selection context)))
    (string-join
     (delq nil
           (list
            (format "Context: file=%S" (alist-get 'display-file context))
            (format "buffer=%S" (alist-get 'buffer-name context))
            (format "line=%s" (alist-get 'line context))
            (format "column=%s" (alist-get 'column context))
            (when selection
              (format "selection=%S"
                      (format "%s:%s-%s:%s"
                              (alist-get 'start-line selection)
                              (alist-get 'start-column selection)
                              (alist-get 'end-line selection)
                              (alist-get 'end-column selection))))))
     " ")))

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
  (let ((working-dir (codex-ide--get-working-directory)))
    (when-let* ((context-buffer (or codex-ide--prompt-origin-buffer
                                    (codex-ide--get-active-buffer-object)))
                (context (or (and codex-ide--prompt-origin-buffer
                                  (codex-ide--make-explicit-buffer-context
                                   codex-ide--prompt-origin-buffer
                                   working-dir))
                             (codex-ide--get-active-buffer-context))))
      (let* ((context-with-selection
              (codex-ide--context-with-selected-region
               context
               context-buffer))
             (formatted-context
              (codex-ide--format-buffer-context context-with-selection))
             (context-summary
              (codex-ide--format-buffer-context-summary context-with-selection)))
        `((formatted . ,formatted-context)
          (summary . ,context-summary))))))

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
