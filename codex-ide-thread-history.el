;;; codex-ide-thread-history.el --- Stored thread history helpers for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis

;;; Commentary:

;; This module owns normalization of stored thread history into the renderable
;; item shapes used by transcript replay and history-oriented commands.
;;
;; App-server thread reads may omit storage-level render details such as
;; apply_patch calls.  Rollout JSONL files can supply those details.  Keep the
;; storage merge here so transcript rendering and non-rendering history lookups
;; answer "what happened in this turn?" from the same normalized data.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'codex-ide-protocol)
(require 'codex-ide-rollout)

(defun codex-ide--stored-item-with-id (item fallback-id)
  "Return stored ITEM with FALLBACK-ID when it lacks an id."
  (let ((copy (copy-tree item)))
    (if (alist-get 'id copy)
        copy
      (append copy `((id . ,fallback-id))))))

(defun codex-ide--normalized-stored-render-item (item)
  "Return ITEM normalized for replay through live rendering primitives."
  (let ((copy (copy-tree item)))
    (when (and (equal (alist-get 'type copy) "commandExecution")
               (not (alist-get 'aggregatedOutput copy))
               (alist-get 'output copy))
      (push (cons 'aggregatedOutput (alist-get 'output copy)) copy))
    copy))

(defun codex-ide--merge-restored-turn-items (turn rollout-items)
  "Return TURN with ROLLOUT-ITEMS merged into its item list."
  (if (not rollout-items)
      turn
    (let ((copy (copy-tree turn))
          (items (append (codex-ide--thread-read-items turn) nil))
          (merged nil)
          (inserted nil)
          (rollout-has-assistant
           (seq-some
            (lambda (item)
              (eq (codex-ide--thread-read--item-kind item) 'assistant))
            rollout-items)))
      (if rollout-has-assistant
          (setq merged
                (append
                 (seq-filter
                  (lambda (item)
                    (eq (codex-ide--thread-read--item-kind item) 'user))
                  items)
                 rollout-items))
        (dolist (item items)
          (when (and (not inserted)
                     (eq (codex-ide--thread-read--item-kind item) 'assistant))
            (setq merged (append (reverse rollout-items) merged))
            (setq inserted t))
          (push item merged))
        (unless inserted
          (setq merged (append (reverse rollout-items) merged)))
        (setq merged (nreverse merged)))
      (setf (alist-get 'items copy) merged)
      copy)))

(defun codex-ide--thread-read-with-rollout-render-items (thread-read &optional limit)
  "Return THREAD-READ augmented with renderable rollout storage items.
When LIMIT is non-nil, request only that many recent rollout turns."
  (if (and (integerp limit) (<= limit 0))
      thread-read
    (let* ((thread (alist-get 'thread thread-read))
           (path (alist-get 'path thread))
           (rollout-turns (codex-ide-rollout-turn-render-items path limit)))
      (if (not rollout-turns)
          thread-read
        (let* ((copy (copy-tree thread-read))
               (copy-thread (alist-get 'thread copy))
               (turns (append (codex-ide--thread-read-turns copy) nil))
               (rollout-turns
                (nthcdr (max 0 (- (length rollout-turns) (length turns)))
                        rollout-turns))
               (turns
                (if (and (integerp limit)
                         (> limit 0)
                         (> (length turns) (length rollout-turns)))
                    (last turns (length rollout-turns))
                  turns))
               (merged-turns
                (cl-loop for turn in turns
                         for rollout-items in rollout-turns
                         collect (codex-ide--merge-restored-turn-items
                                  turn
                                  rollout-items))))
          (if (alist-get 'turns copy)
              (setf (alist-get 'turns copy) merged-turns)
            (setf (alist-get 'turns copy-thread) merged-turns))
          copy)))))

(provide 'codex-ide-thread-history)

;;; codex-ide-thread-history.el ends here
