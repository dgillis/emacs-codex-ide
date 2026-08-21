;;; codex-ide-math-preview-tests.el --- Tests for Codex math previews -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage for optional LaTeX previews in Codex transcript buffers.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)

(defmacro codex-ide-math-preview-test-with-message-buffer (&rest body)
  "Run BODY in a temporary current-assistant-message buffer.

BODY may refer to the lexical variable `session'."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (let* ((codex-ide--session-metadata (make-hash-table :test 'eq))
            (session (make-codex-ide-session
                      :buffer (current-buffer)
                      :current-message-item-id "msg-1"
                      :current-message-prefix-inserted t
                      :item-states (make-hash-table :test 'equal))))
       (setf (codex-ide-session-current-message-start-marker session)
             (copy-marker (point-min)))
       (codex-ide--session-metadata-put
        session
        :agent-message-stream-render-start-marker
        (copy-marker (point-min)))
       ,@body)))

(ert-deftest codex-ide-math-preview-is-disabled-by-default ()
  (with-temp-buffer
    (let ((codex-ide-math-preview-auto-enable nil))
      (codex-ide-session-mode)
      (should-not codex-ide-math-preview-mode))))

(ert-deftest codex-ide-math-preview-missing-texfrag-does-not-break-auto-enable ()
  (let ((original-require (symbol-function 'require))
        (codex-ide-math-preview-auto-enable t))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'texfrag)
                     nil
                   (funcall original-require feature filename noerror)))))
      (with-temp-buffer
        (codex-ide-session-mode)
        (should-not codex-ide-math-preview-mode)))))

(ert-deftest codex-ide-math-preview-enables-and-configures-texfrag ()
  (let ((original-require (symbol-function 'require))
        (mode-calls nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'texfrag)
                     t
                   (funcall original-require feature filename noerror))))
              ((symbol-function 'texfrag-mode)
               (lambda (arg)
                 (push arg mode-calls)
                 (setq texfrag-mode (> arg 0)))))
      (with-temp-buffer
        (codex-ide-session-mode)
        (setq-local texfrag-mode nil
                    texfrag-comments-only t
                    texfrag-equation-filter nil
                    texfrag-frag-alist nil
                    texfrag-LaTeX-frag-alist
                    '(("\\$" "\\$" "$" "$"))
                    texfrag-markdown-preview-image-links t
                    texfrag-scale nil)
        (codex-ide-math-preview-mode 1)
        (should codex-ide-math-preview-mode)
        (should codex-ide-math-preview--owns-texfrag-mode)
        (should-not texfrag-comments-only)
        (should (eq texfrag-equation-filter #'identity))
        (should-not texfrag-markdown-preview-image-links)
        (should (= texfrag-scale codex-ide-math-preview-scale))
        (should (eq (cadr (caar texfrag-frag-alist))
                    #'codex-ide-math-preview--texfrag-fragment-p))
        (should (assq 'texfrag-mode minor-mode-overriding-map-alist))
        (let ((preview-directory
               codex-ide-math-preview--temporary-directory))
          (should (file-directory-p preview-directory))
          (codex-ide-math-preview-mode -1)
          (should-not (file-exists-p preview-directory)))
        (should-not (assq 'texfrag-mode minor-mode-overriding-map-alist))
        (should (local-variable-p 'texfrag-comments-only))
        (should texfrag-comments-only)
        (should (equal mode-calls '(-1 1)))))))

(ert-deftest codex-ide-math-preview-preserves-existing-texfrag-mode-state ()
  (let ((original-require (symbol-function 'require))
        (mode-calls nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'texfrag)
                     t
                   (funcall original-require feature filename noerror))))
              ((symbol-function 'texfrag-mode)
               (lambda (arg)
                 (push arg mode-calls)
                 (setq texfrag-mode (> arg 0)))))
      (with-temp-buffer
        (codex-ide-session-mode)
        (setq-local texfrag-mode t
                    texfrag-comments-only 'original
                    texfrag-equation-filter 'original-filter
                    texfrag-frag-alist nil
                    texfrag-LaTeX-frag-alist
                    '(("\\$" "\\$" "$" "$"))
                    texfrag-markdown-preview-image-links t
                    texfrag-scale 2.0
                    texfrag-subdir "existing-texfrag-directory")
        (codex-ide-math-preview-mode 1)
        (should-not codex-ide-math-preview--owns-texfrag-mode)
        (should-not codex-ide-math-preview--temporary-directory)
        (codex-ide-math-preview-mode -1)
        (should texfrag-mode)
        (should (eq texfrag-comments-only 'original))
        (should (eq texfrag-equation-filter 'original-filter))
        (should (= texfrag-scale 2.0))
        (should (equal texfrag-subdir "existing-texfrag-directory"))
        (should-not mode-calls)))))

(ert-deftest codex-ide-math-preview-eligibility-excludes-non-prose-text ()
  (with-temp-buffer
    (let ((agent-property
           (list codex-ide-agent-item-type-property "agentMessage")))
      (insert (apply #'propertize "$x$" agent-property))
      (let ((plain-start (point-min)))
        (should
         (codex-ide-math-preview--eligible-position-p plain-start "$"))
        (put-text-property
         plain-start (1+ plain-start) 'codex-ide-markdown-inline-code t)
        (should-not
         (codex-ide-math-preview--eligible-position-p plain-start "$")))
      (let ((code-start (point)))
        (insert (apply #'propertize "$y$" agent-property))
        (put-text-property
         code-start (1+ code-start) 'codex-ide-markdown-code-content t)
        (should-not
         (codex-ide-math-preview--eligible-position-p code-start "$")))
      (let ((tool-start (point)))
        (insert
         (propertize "$z$" codex-ide-agent-item-type-property "commandExecution"))
        (should-not
         (codex-ide-math-preview--eligible-position-p tool-start "$")))
      (let ((escaped-start (point)))
        (insert (apply #'propertize "\\$q$" agent-property))
        (should-not
         (codex-ide-math-preview--eligible-position-p
          (1+ escaped-start)
          "$"))))))

(ert-deftest codex-ide-renderer-labels-inline-code-for-math-exclusion ()
  (with-temp-buffer
    (insert "Code `$x$` and prose $y$.")
    (codex-ide-renderer-render-markdown-region (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "$x$")
    (should (get-text-property
             (match-beginning 0)
             'codex-ide-markdown-inline-code))
    (search-forward "$y$")
    (should-not (get-text-property
                 (match-beginning 0)
                 'codex-ide-markdown-inline-code))))

(ert-deftest codex-ide-final-agent-render-runs-math-hook-but-streaming-does-not ()
  (codex-ide-math-preview-test-with-message-buffer
   (let* ((calls nil)
          (codex-ide-transcript-agent-message-rendered-hook
           (list
            (lambda (buffer start end)
              (push (list buffer start end
                          (buffer-substring-no-properties start end))
                    calls)))))
     (insert
      (propertize "$x^2$\n"
                  codex-ide-agent-item-type-property
                  "agentMessage"))
     (codex-ide--render-current-agent-message-markdown-streaming
      session
      "msg-1")
     (should-not calls)
     (codex-ide--render-current-agent-message-markdown session "msg-1" t)
     (should (= (length calls) 1))
     (pcase-let ((`(,buffer ,start ,end ,text) (car calls)))
       (should (eq buffer (current-buffer)))
       (should (= start (point-min)))
       (should (= end (point-max)))
       (should (equal text "$x^2$\n"))))))

(ert-deftest codex-ide-math-preview-coalesces-completed-message-regions ()
  (with-temp-buffer
    (insert (make-string 30 ?x))
    (setq-local codex-ide-math-preview-mode t)
    (let ((timer-count 0)
          (cancelled nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _args)
                   (setq timer-count (1+ timer-count))
                   (intern (format "math-timer-%d" timer-count))))
                ((symbol-function 'timerp)
                 (lambda (value)
                   (and (symbolp value)
                        (string-prefix-p "math-timer-" (symbol-name value)))))
                ((symbol-function 'cancel-timer)
                 (lambda (timer)
                   (setq cancelled timer))))
        (codex-ide-math-preview-request (current-buffer) 10 15)
        (codex-ide-math-preview-request (current-buffer) 4 20)
        (should (= (marker-position
                    codex-ide-math-preview--dirty-start-marker)
                   4))
        (should (= (marker-position
                    codex-ide-math-preview--dirty-end-marker)
                   20))
        (should (eq cancelled 'math-timer-1))
        (should (eq codex-ide-math-preview--render-timer 'math-timer-2))))))

(ert-deftest codex-ide-math-preview-render-pending-consumes-dirty-region ()
  (with-temp-buffer
    (insert (make-string 20 ?x))
    (setq-local codex-ide-math-preview-mode t
                codex-ide-math-preview--dirty-start-marker (copy-marker 3)
                codex-ide-math-preview--dirty-end-marker (copy-marker 12 t))
    (let ((rendered nil)
          (texfrag-running nil))
      (cl-letf (((symbol-function 'codex-ide-math-preview--render-region)
                 (lambda (start end)
                   (setq rendered (cons start end)))))
        (codex-ide-math-preview--render-pending (current-buffer))
        (should (equal rendered '(3 . 12)))
        (should-not codex-ide-math-preview--dirty-start-marker)
        (should-not codex-ide-math-preview--dirty-end-marker)))))

(ert-deftest codex-ide-math-preview-render-region-skips-tex-when-no-math ()
  (with-temp-buffer
    (insert "No math here")
    (let ((rendered nil)
          (cleared nil))
      (cl-letf (((symbol-function 'texfrag-search-forward-fragment)
                 (lambda (_end) nil))
                ((symbol-function 'texfrag-region)
                 (lambda (start end)
                   (setq rendered (cons start end))))
                ((symbol-function 'texfrag-clearout-region)
                 (lambda (start end)
                   (setq cleared (cons start end)))))
        (codex-ide-math-preview--render-region (point-min) (point-max))
        (should-not rendered)
        (should (equal cleared (cons (point-min) (point-max))))))))

(provide 'codex-ide-math-preview-tests)

;;; codex-ide-math-preview-tests.el ends here
