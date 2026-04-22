;;; codex-ide-integration-tests.el --- Real Codex integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Opt-in tests that exercise codex-ide against a real Codex app-server.  These
;; tests intentionally avoid exact LLM response assertions and instead validate
;; Emacs-visible behavior, sentinel tokens, and UI invariants.

;;; Code:

(require 'ert)
(require 'codex-ide-integration-fixtures)
(require 'codex-ide-session-buffer-list)

(ert-deftest codex-ide-integration-starts-real-session ()
  (codex-ide-integration--log "test starts-real-session: begin")
  (codex-ide-integration-with-session
    (codex-ide-integration--log
     "test starts-real-session: asserting session basics")
    (should (codex-ide-session-p session))
    (should (process-live-p (codex-ide-session-process session)))
    (should (buffer-live-p (codex-ide-session-buffer session)))
    (should (stringp (codex-ide-session-thread-id session)))
    (should-not (string-empty-p (codex-ide-session-thread-id session)))
    (should (string= (codex-ide-session-status session) "idle"))
    (with-current-buffer (codex-ide-session-buffer session)
      (should (derived-mode-p 'codex-ide-session-mode))
      (should (codex-ide--input-prompt-active-p session))
      (should (string-match-p "Codex session for" (buffer-string)))))
  (codex-ide-integration--log "test starts-real-session: done"))

(ert-deftest codex-ide-integration-submits-prompt-and-renders-response ()
  (codex-ide-integration--log "test submits-prompt: begin")
  (codex-ide-integration-with-session
    (let* ((token (codex-ide-integration--token "CODEX_IDE_IT_PING"))
           (prompt (format "Reply with one short sentence containing the exact token %s. Do not edit files."
                           token))
           (buffer (codex-ide-session-buffer session)))
      (codex-ide-integration--log
       "test submits-prompt: submitting prompt token=%s buffer=%S"
       token
       (buffer-name buffer))
      (with-current-buffer buffer
        (codex-ide--replace-current-input session prompt)
        (codex-ide--submit-prompt))
      (codex-ide-integration--log
       "test submits-prompt: waiting for token to appear in agent response")
      (codex-ide-integration--wait-until
       (lambda ()
         (>= (codex-ide-integration--buffer-match-count
              buffer
              (regexp-quote token))
             2))
       nil
       (format "agent response token %s\n%s"
               token
               (codex-ide-integration--failure-context session)))
      (codex-ide-integration--log
       "test submits-prompt: token observed; waiting for idle")
      (codex-ide-integration--wait-for-session-idle session)
      (codex-ide-integration--log
       "test submits-prompt: asserting rendered transcript and empty input")
      (with-current-buffer buffer
        (should (string-match-p (regexp-quote prompt) (buffer-string)))
        (should (>= (codex-ide-integration--buffer-match-count
                     buffer
                     (regexp-quote token))
                    2))
        (should (codex-ide--input-prompt-active-p session))
        (should (equal (codex-ide--current-input session) "")))))
  (codex-ide-integration--log "test submits-prompt: done"))

(ert-deftest codex-ide-integration-session-buffer-list-shows-live-session ()
  (codex-ide-integration--log "test session-buffer-list: begin")
  (codex-ide-integration-with-session
    (let* ((token (codex-ide-integration--token "CODEX_IDE_IT_LIST"))
           (prompt (format "Reply with the exact token %s and no file edits."
                           token))
           (session-buffer-name (buffer-name (codex-ide-session-buffer session))))
      (codex-ide-integration--log
       "test session-buffer-list: submitting prompt token=%s buffer=%S"
       token
       session-buffer-name)
      (with-current-buffer (codex-ide-session-buffer session)
        (codex-ide--replace-current-input session prompt)
        (codex-ide--submit-prompt))
      (codex-ide-integration--log
       "test session-buffer-list: waiting for token to appear in agent response")
      (codex-ide-integration--wait-until
       (lambda ()
         (>= (codex-ide-integration--buffer-match-count
              (codex-ide-session-buffer session)
              (regexp-quote token))
             2))
       nil
       (format "agent response token %s\n%s"
               token
               (codex-ide-integration--failure-context session)))
      (codex-ide-integration--log
       "test session-buffer-list: token observed; waiting for idle")
      (codex-ide-integration--wait-for-session-idle session)
      (codex-ide-integration--log
       "test session-buffer-list: opening live session buffer list")
      (codex-ide-session-buffer-list)
      (with-current-buffer "*Codex Session Buffers*"
        (codex-ide-integration--log
         "test session-buffer-list: asserting table contents")
        (should (derived-mode-p 'codex-ide-session-buffer-list-mode))
        (should (string-match-p (regexp-quote session-buffer-name)
                                (buffer-string)))
        (should (seq-some
                 (lambda (entry)
                   (eq (car entry) session))
                 (funcall tabulated-list-entries)))
        (should (string-match-p "Reply with the exact token"
                                (buffer-string)))
        (should (string-match-p "Idle" (buffer-string))))))
  (codex-ide-integration--log "test session-buffer-list: done"))

(ert-deftest codex-ide-integration-renders-markdown-code-response ()
  (codex-ide-integration--log "test markdown-code: begin")
  (codex-ide-integration-with-session
    (let* ((token (codex-ide-integration--token "CODEX_IDE_IT_CODE"))
           (prompt (format "Reply with only a fenced elisp code block containing this exact token in a comment: %s. Do not edit files."
                           token))
           (buffer (codex-ide-integration-submit-prompt-and-wait-for-token
                    session
                    prompt
                    token
                    "markdown-code")))
      (codex-ide-integration--log
       "test markdown-code: asserting rendered markdown properties")
      (should (codex-ide-integration--buffer-has-markdown-match-p
               buffer
               (regexp-quote token)))))
  (codex-ide-integration--log "test markdown-code: done"))

(ert-deftest codex-ide-integration-prompt-history-after-real-prompts ()
  (codex-ide-integration--log "test prompt-history: begin")
  (codex-ide-integration-with-session
    (let* ((first-token (codex-ide-integration--token "CODEX_IDE_IT_HISTORY_A"))
           (second-token (codex-ide-integration--token "CODEX_IDE_IT_HISTORY_B"))
           (first-prompt (format "Reply with the exact token %s and no file edits."
                                 first-token))
           (second-prompt (format "Reply with the exact token %s and no file edits."
                                  second-token)))
      (codex-ide-integration-submit-prompt-and-wait-for-token
       session
       first-prompt
       first-token
       "prompt-history first")
      (codex-ide-integration-submit-prompt-and-wait-for-token
       session
       second-prompt
       second-token
       "prompt-history second")
      (codex-ide-integration--log
       "test prompt-history: browsing prompt history")
      (with-current-buffer (codex-ide-session-buffer session)
        (codex-ide--browse-prompt-history -1)
        (should (string= (codex-ide--current-input session) second-prompt))
        (codex-ide--browse-prompt-history -1)
        (should (string= (codex-ide--current-input session) first-prompt))
        (codex-ide--browse-prompt-history 1)
        (should (string= (codex-ide--current-input session) second-prompt))
        (codex-ide--browse-prompt-history 1)
        (should (string= (codex-ide--current-input session) "")))))
  (codex-ide-integration--log "test prompt-history: done"))

(provide 'codex-ide-integration-tests)

;;; codex-ide-integration-tests.el ends here
