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
    (when-let ((text (codex-ide--thread-read--message-text payload)))
      (unless (string-empty-p (string-trim text))
        `((type . "agentMessage")
          ,@(when-let ((id (alist-get 'id payload)))
              `((id . ,id)))
          (text . ,text)
          ,@(when-let ((phase (alist-get 'phase payload)))
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
        ,@(when-let ((cwd (codex-ide-rollout--alist-get-any
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

(defun codex-ide-rollout--complete-call-item (item output)
  "Update rollout-derived ITEM with completion OUTPUT."
  (pcase (alist-get 'type item)
    ("commandExecution"
     (setf (alist-get 'aggregatedOutput item) output)
     (setf (alist-get 'status item) "completed"))
    ("fileChange"
     (setf (alist-get 'status item) "completed"))
    (_
     (setf (alist-get 'result item) output)
     (setf (alist-get 'status item) "completed")))
  item)

(defun codex-ide-rollout-turn-render-items (path)
  "Return renderable per-turn items read from rollout JSONL PATH."
  (when (and (stringp path)
             (file-readable-p path))
    (let ((turns nil)
          (current-items nil)
          (current-active nil)
          (items-by-call-id (make-hash-table :test 'equal)))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents path)
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
                  (when-let ((item (codex-ide-rollout--message-item payload)))
                    (push item current-items)))
                 ((and current-active
                       (equal entry-type "response_item")
                       (equal payload-type "function_call"))
                  (let ((item (codex-ide-rollout--function-call-item payload)))
                    (when item
                      (push item current-items)
                      (when-let ((call-id (alist-get 'id item)))
                        (puthash call-id item items-by-call-id)))))
                 ((and current-active
                       (equal entry-type "response_item")
                       (equal payload-type "custom_tool_call"))
                  (let ((item (codex-ide-rollout--custom-tool-call-item payload)))
                    (when item
                      (push item current-items)
                      (when-let ((call-id (alist-get 'id item)))
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
      (nreverse turns))))

(provide 'codex-ide-rollout)

;;; codex-ide-rollout.el ends here
