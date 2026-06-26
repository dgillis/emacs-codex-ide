;;; codex-ide-rollout.el --- Codex rollout JSONL storage adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module reads Codex rollout JSONL files and converts their storage-level
;; event schema into the item shapes consumed by transcript replay.
;;
;; Rollout files are a persisted storage detail, separate from the app-server
;; JSON-RPC interface.  Keeping that parsing here isolates restore code from
;; storage-schema drift.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'codex-ide-protocol)

(defun codex-ide-rollout--alist-get-any (keys alist)
  "Return the first non-nil value for one of KEYS in ALIST."
  (seq-some (lambda (key) (alist-get key alist)) keys))

(defun codex-ide-rollout--json-read-string-safe (text)
  "Read JSON TEXT as an alist, returning nil on failure."
  (when (stringp text)
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (json-false :json-false))
          (json-read-from-string text))
      (error nil))))

(defun codex-ide-rollout--call-arguments (payload)
  "Return decoded arguments from rollout call PAYLOAD."
  (let ((arguments (codex-ide-rollout--alist-get-any
                    '(arguments argument) payload)))
    (cond
     ((stringp arguments)
      (or (codex-ide-rollout--json-read-string-safe arguments)
          arguments))
     (t arguments))))

(defun codex-ide-rollout--message-item (payload)
  "Convert a rollout message PAYLOAD into a transcript item."
  (when (member (alist-get 'role payload) '("assistant" assistant))
    (when-let* ((text (codex-ide--thread-read--message-text payload)))
      (unless (string-empty-p (string-trim text))
        `((type . "agentMessage")
          ,@(when-let* ((id (alist-get 'id payload)))
              `((id . ,id)))
          (text . ,text)
          ,@(when-let* ((phase (alist-get 'phase payload)))
              `((phase . ,phase))))))))

(defun codex-ide-rollout--function-call-item (payload)
  "Convert a rollout function-call PAYLOAD into a transcript item."
  (let* ((call-id (codex-ide-rollout--alist-get-any
                   '(call_id call-id callId) payload))
         (name (codex-ide-rollout--alist-get-any '(name tool) payload))
         (namespace (codex-ide-rollout--alist-get-any
                     '(namespace server) payload))
         (arguments (codex-ide-rollout--call-arguments payload))
         (command (and (equal name "exec_command")
                       (listp arguments)
                       (codex-ide-rollout--alist-get-any
                        '(cmd command) arguments))))
    (cond
     (command
      `((type . "commandExecution")
        (id . ,call-id)
        (command . ,command)
        (aggregatedOutput . nil)
        (status . nil)
        ,@(when-let* ((cwd (codex-ide-rollout--alist-get-any
                           '(workdir cwd) arguments)))
            `((cwd . ,cwd)))))
     ((and (stringp namespace)
           (string-prefix-p "mcp__" namespace)
           (stringp name)
           (not (string-empty-p name)))
      `((type . "mcpToolCall")
        (id . ,call-id)
        (server . ,namespace)
        (tool . ,name)
        (result . nil)
        (status . nil)
        ,@(when arguments
            `((arguments . ,arguments)))))
     (t nil))))

(defun codex-ide-rollout--custom-tool-call-item (payload)
  "Convert a rollout custom-tool-call PAYLOAD into a transcript item."
  (let ((call-id (codex-ide-rollout--alist-get-any
                  '(call_id call-id callId) payload))
        (name (codex-ide-rollout--alist-get-any '(name tool) payload))
        (input (codex-ide-rollout--alist-get-any '(input arguments) payload)))
    (when (equal name "apply_patch")
      (when (stringp input)
        `((type . "fileChange")
          (id . ,call-id)
          (status . nil)
          (changes . (((path . "patch")
                       (kind . "modified")
                       (diff . ,input)))))))))

(defun codex-ide-rollout--exec-command-output (output)
  "Return normalized command OUTPUT details from rollout storage.
The stored tool result is often an envelope containing tool metadata followed by
an \"Output:\" line.  Return a plist with :output and, when available, :exit-code."
  (let ((normalized-output output)
        exit-code)
    (when (and (stringp output)
               (string-prefix-p "Chunk ID: " output))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (when (re-search-forward
               "^Process exited with code \\([-0-9]+\\)$" nil t)
          (setq exit-code (string-to-number (match-string 1))))
        (goto-char (point-min))
        (when (re-search-forward "^Output:\n" nil t)
          (setq normalized-output
                (buffer-substring-no-properties (point) (point-max))))))
    (list :output normalized-output
          :exit-code exit-code)))

(defun codex-ide-rollout--complete-call-item (item output)
  "Update rollout-derived ITEM with completion OUTPUT."
  (pcase (alist-get 'type item)
    ("commandExecution"
     (let ((details (codex-ide-rollout--exec-command-output output)))
       (setf (alist-get 'aggregatedOutput item) (plist-get details :output))
       (when-let* ((exit-code (plist-get details :exit-code)))
         (setf (alist-get 'exitCode item) exit-code)))
     (setf (alist-get 'status item) "completed"))
    ("fileChange"
     (setf (alist-get 'status item) "completed"))
    (_
     (setf (alist-get 'result item) output)
     (setf (alist-get 'status item) "completed")))
  item)

(defun codex-ide-rollout--turn-boundary-line-p (line boundary-type)
  "Return non-nil when LINE is an event message for BOUNDARY-TYPE."
  (and (string-match-p "\"type\":\"event_msg\"" line)
       (string-match-p
        (format "\"type\":\"%s\"" (regexp-quote boundary-type))
        line)))

(defconst codex-ide-rollout--recent-turn-initial-bytes
  (* 96 1024 1024)
  "Initial tail size used when reading limited rollout turns.")

(defun codex-ide-rollout--recent-turn-lines-in-current-buffer (limit)
  "Return (LINES . COUNT) for the most recent LIMIT completed turns."
  (let ((turns nil)
        (current-lines nil)
        (collecting nil)
        (count 0))
    (goto-char (point-max))
    (while (and (< count limit)
                (> (point) (point-min)))
      (forward-line -1)
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
        (cond
         ((and (not collecting)
               (codex-ide-rollout--turn-boundary-line-p
                line
                "task_complete"))
          (setq collecting t
                current-lines (list line)))
         (collecting
          (push line current-lines)
          (when (codex-ide-rollout--turn-boundary-line-p
                 line
                 "task_started")
            (push current-lines turns)
            (setq current-lines nil
                  collecting nil
                  count (1+ count)))))))
    (cons (apply #'append turns) count)))

(defun codex-ide-rollout--drop-partial-leading-line ()
  "Drop the first partial line from a tail chunk."
  (goto-char (point-min))
  (unless (eobp)
    (delete-region
     (point-min)
     (min (point-max)
          (save-excursion
            (forward-line 1)
            (point))))))

(defun codex-ide-rollout--recent-turn-lines (path limit)
  "Return raw JSONL lines for the most recent LIMIT completed turns in PATH."
  (when (and (integerp limit) (> limit 0))
    (let* ((file-size (file-attribute-size (file-attributes path)))
           (chunk-size (min file-size
                            codex-ide-rollout--recent-turn-initial-bytes))
           lines
           count)
      (while (and (or (null count) (< count limit))
                  (< chunk-size file-size))
        (with-temp-buffer
          (let ((start (max 0 (- file-size chunk-size))))
            (let ((coding-system-for-read 'utf-8-unix))
              (insert-file-contents path nil start file-size))
            (when (> start 0)
              (codex-ide-rollout--drop-partial-leading-line)))
          (let ((result
                 (codex-ide-rollout--recent-turn-lines-in-current-buffer
                  limit)))
            (setq lines (car result)
                  count (cdr result))))
        (when (< count limit)
          (setq chunk-size (min file-size (* chunk-size 2)))))
      (when (or (null count) (< count limit))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents path))
          (let ((result
                 (codex-ide-rollout--recent-turn-lines-in-current-buffer
                  limit)))
            (setq lines (car result)
                  count (cdr result)))))
      lines)))

(defun codex-ide-rollout--turn-render-items-from-current-buffer ()
  "Return renderable per-turn items from the current JSONL buffer."
  (let ((turns nil)
        (current-items nil)
        (current-active nil)
        (items-by-call-id (make-hash-table :test 'equal)))
    (condition-case nil
        (progn
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
                   (entry (codex-ide-rollout--json-read-string-safe line))
                   (entry-type (alist-get 'type entry))
                   (payload (alist-get 'payload entry))
                   (payload-type (and (listp payload)
                                      (alist-get 'type payload))))
              (cond
               ((and (equal entry-type "event_msg")
                     (equal payload-type "task_started"))
                (setq current-items nil)
                (setq current-active t)
                (clrhash items-by-call-id))
               ((and (equal entry-type "event_msg")
                     (equal payload-type "task_complete"))
                (when current-active
                  (push (nreverse current-items) turns))
                (setq current-items nil)
                (setq current-active nil)
                (clrhash items-by-call-id))
               ((and current-active
                     (equal entry-type "response_item")
                     (equal payload-type "message"))
                (when-let* ((item (codex-ide-rollout--message-item payload)))
                  (push item current-items)))
               ((and current-active
                     (equal entry-type "response_item")
                     (equal payload-type "function_call"))
                (let ((item (codex-ide-rollout--function-call-item payload)))
                  (when item
                    (push item current-items)
                    (when-let* ((call-id (alist-get 'id item)))
                      (puthash call-id item items-by-call-id)))))
               ((and current-active
                     (equal entry-type "response_item")
                     (equal payload-type "custom_tool_call"))
                (let ((item (codex-ide-rollout--custom-tool-call-item payload)))
                  (when item
                    (push item current-items)
                    (when-let* ((call-id (alist-get 'id item)))
                      (puthash call-id item items-by-call-id)))))
               ((and current-active
                     (equal entry-type "response_item")
                     (member payload-type '("function_call_output"
                                            "custom_tool_call_output")))
                (when-let* ((call-id (codex-ide-rollout--alist-get-any
                                      '(call_id call-id callId) payload))
                            (item (gethash call-id items-by-call-id)))
                  (codex-ide-rollout--complete-call-item
                   item
                   (or (codex-ide-rollout--alist-get-any
                        '(output result) payload)
                       ""))))))
            (forward-line 1)))
      (error nil))
    (nreverse turns)))

(defun codex-ide-rollout-turn-render-items (path &optional limit)
  "Return renderable per-turn items read from rollout JSONL PATH.
When LIMIT is a positive integer, only parse the most recent LIMIT completed
turns."
  (when (and (stringp path)
             (file-readable-p path))
    (with-temp-buffer
      (if (and (integerp limit) (> limit 0))
          (dolist (line (codex-ide-rollout--recent-turn-lines path limit))
            (insert line "\n"))
        (insert-file-contents path))
      (codex-ide-rollout--turn-render-items-from-current-buffer))))

(provide 'codex-ide-rollout)

;;; codex-ide-rollout.el ends here
