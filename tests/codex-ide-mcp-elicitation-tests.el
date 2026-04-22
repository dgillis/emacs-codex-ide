;;; codex-ide-mcp-elicitation-tests.el --- Tests for codex-ide MCP elicitation -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for MCP elicitation helpers and codex-ide integration.

;;; Code:

(require 'ert)
(require 'json)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)

(ert-deftest codex-ide-mcp-elicitation-capabilities-include-form-and-url ()
  (should
   (equal (codex-ide-mcp-elicitation-capabilities)
          '((elicitation . ((form . ())
                            (url . ())))))))

(ert-deftest codex-ide-mcp-elicitation-form-request-collects-supported-values ()
  (let ((completions '("submit" "false" "Option B"))
        (inputs '("Ada" "7")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (pop completions)))
              ((symbol-function 'read-from-minibuffer)
               (lambda (&rest _)
                 (pop inputs))))
      (should
       (equal
        (codex-ide-mcp-elicitation-handle-request
         '((message . "Need settings")
           (requestedSchema
            . ((type . "object")
               (properties
                . ((name . ((type . "string")
                            (title . "Name")))
                   (count . ((type . "integer")
                             (title . "Count")))
                   (enabled . ((type . "boolean")
                               (title . "Enabled")))
                   (choice . ((type . "string")
                              (title . "Choice")
                              (enum . ("a" "b"))
                              (enumNames . ("Option A" "Option B"))))))
               (required . ("name" "count" "enabled" "choice"))))))
        '((action . "accept")
          (content . ((name . "Ada")
                      (count . 7)
                      (enabled . :json-false)
                      (choice . "b")))))))))

(ert-deftest codex-ide-mcp-elicitation-normalizes-nested-request-payload ()
  (let ((completions '("submit"))
        (inputs '("Ada")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (pop completions)))
              ((symbol-function 'read-from-minibuffer)
               (lambda (&rest _)
                 (pop inputs))))
      (should
       (equal
        (codex-ide-mcp-elicitation-handle-request
         '((request
            . ((message . "Need settings")
               (requestedSchema
                . ((type . "object")
                   (properties
                    . ((name . ((type . "string")
                                (title . "Name")))))
                   (required . ("name"))))))))
        '((action . "accept")
          (content . ((name . "Ada")))))))))

(ert-deftest codex-ide-mcp-elicitation-url-request-can-open-browser ()
  (let ((opened-url nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "open and continue"))
              ((symbol-function 'browse-url)
               (lambda (url &rest _)
                 (setq opened-url url))))
      (should
       (equal
        (codex-ide-mcp-elicitation-handle-request
         '((mode . "url")
           (message . "Authenticate")
           (url . "https://example.com/auth")))
        '((action . "accept"))))
      (should (equal opened-url "https://example.com/auth")))))

(ert-deftest codex-ide-initialize-session-advertises-elicitation-capability ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (captured-params nil))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((session (codex-ide--create-process-session)))
          (cl-letf (((symbol-function 'codex-ide--request-sync)
                     (lambda (_session method params)
                       (should (equal method "initialize"))
                       (setq captured-params params)
                       '((ok . t)))))
            (codex-ide--initialize-session session)))
        (should (equal (alist-get 'experimentalApi
                                  (alist-get 'capabilities captured-params))
                       t))
        (should (equal (alist-get 'form
                                  (alist-get 'elicitation
                                             (alist-get 'capabilities captured-params)))
                       '()))
        (should (equal (alist-get 'url
                                  (alist-get 'elicitation
                                             (alist-get 'capabilities captured-params)))
                       '()))))))

(ert-deftest codex-ide-handle-server-request-renders-inline-elicitation-form ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let* ((session (codex-ide--create-process-session))
               (process (codex-ide-session-process session))
               (displayed-buffer nil)
               (message-text nil))
          (setf (codex-ide-session-current-turn-id session) "turn-elicitation-1"
                (codex-ide-session-status session) "running")
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_time _repeat fn &rest args)
                       (apply fn args)
                       nil))
                    ((symbol-function 'codex-ide-display-buffer)
                     (lambda (buffer &optional _action)
                       (setq displayed-buffer buffer)
                       (selected-window)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-text (apply #'format format-string args))))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (ert-fail "elicitation should not use completing-read")))
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _)
                       (ert-fail "elicitation should not use read-from-minibuffer"))))
            (codex-ide--handle-server-request
             session
             '((id . 17)
               (method . "elicitation/create")
               (params
                . ((message . "Need settings")
                   (requestedSchema
                    . ((type . "object")
                       (properties
                        . ((name . ((type . "string")
                                    (title . "Name")))
                           (count . ((type . "integer")
                                     (title . "Count")))
                           (enabled . ((type . "boolean")
                                       (title . "Enabled")))
                           (choice . ((type . "string")
                                      (title . "Choice")
                                      (enum . ("a" "b"))
                                      (enumNames . ("Option A" "Option B"))))))
                       (required . ("name" "count" "enabled" "choice")))))))))
          (should (eq displayed-buffer (codex-ide-session-buffer session)))
          (should (equal message-text
                         (format "Codex input required in %s"
                                 (buffer-name (codex-ide-session-buffer session)))))
          (should (= (hash-table-count (codex-ide--pending-approvals session)) 1))
          (with-current-buffer (codex-ide-session-buffer session)
            (let ((text (buffer-string)))
              (should (string-match-p (regexp-quote "[Input required]") text))
              (should (string-match-p "Request: MCP elicitation (form): Need settings" text))
              (should (string-match-p "\\[submit\\]" text))
              (should (string-match-p "\\[decline\\]" text))
              (should (string-match-p "\\[cancel\\]" text))))
          (let* ((approval (gethash 17 (codex-ide--pending-approvals session)))
                 (fields (plist-get approval :fields))
                 (name-field (seq-find (lambda (field)
                                         (equal (plist-get field :name) 'name))
                                       fields))
                 (count-field (seq-find (lambda (field)
                                          (equal (plist-get field :name) 'count))
                                        fields))
                 (enabled-field (seq-find (lambda (field)
                                            (equal (plist-get field :name) 'enabled))
                                          fields))
                 (choice-field (seq-find (lambda (field)
                                           (equal (plist-get field :name) 'choice))
                                         fields)))
            (with-current-buffer (codex-ide-session-buffer session)
              (let ((inhibit-read-only t))
                (goto-char (marker-position (plist-get name-field :start-marker)))
                (insert "Ada")
                (goto-char (marker-position (plist-get count-field :start-marker)))
                (insert "7"))
              (codex-ide--set-elicitation-choice-value
               session 17 enabled-field "false" :json-false)
              (codex-ide--set-elicitation-choice-value
               session 17 choice-field "Option B" "b"))
            (codex-ide--submit-buffer-elicitation session 17))
          (should
           (equal
            (seq-filter
             (lambda (payload)
               (equal (alist-get 'id payload) 17))
             (mapcar (lambda (payload)
                       (json-parse-string payload
                                          :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object :json-false))
                     (codex-ide-test-process-sent-strings process)))
            '(((id . 17)
               (result . ((action . "accept")
                          (content . ((name . "Ada")
                                      (count . 7)
                                      (enabled . :json-false)
                                      (choice . "b"))))))))))))))

(ert-deftest codex-ide-handle-server-request-renders-inline-url-elicitation ()
  (let ((project-dir (codex-ide-test--make-temp-project)))
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let* ((session (codex-ide--create-process-session))
               (process (codex-ide-session-process session))
               (opened-url nil))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_time _repeat fn &rest args)
                       (apply fn args)
                       nil))
                    ((symbol-function 'browse-url)
                     (lambda (url &rest _)
                       (setq opened-url url)))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (ert-fail "elicitation should not use completing-read")))
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _)
                       (ert-fail "elicitation should not use read-from-minibuffer"))))
            (codex-ide--handle-server-request
             session
             '((id . 18)
               (method . "mcpServer/elicitation/request")
               (params
                . ((request
                    . ((mode . "url")
                       (message . "Authenticate")
                       (url . "https://example.com/auth")))))))
            (with-current-buffer (codex-ide-session-buffer session)
              (should (string-match-p "MCP elicitation (url): Authenticate"
                                      (buffer-string)))
              (should (string-match-p "\\[open and continue\\]" (buffer-string)))
              (goto-char (point-min))
              (search-forward "[open and continue]")
              (backward-char 1)
              (push-button))
            (should (equal opened-url "https://example.com/auth"))
            (should
             (equal
              (seq-filter
               (lambda (payload)
                 (equal (alist-get 'id payload) 18))
               (mapcar (lambda (payload)
                         (json-parse-string payload
                                            :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object :json-false))
                       (codex-ide-test-process-sent-strings process)))
              '(((id . 18)
                 (result . ((action . "accept")))))))))))))

(provide 'codex-ide-mcp-elicitation-tests)

;;; codex-ide-mcp-elicitation-tests.el ends here
