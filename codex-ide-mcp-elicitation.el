;;; codex-ide-mcp-elicitation.el --- MCP elicitation support for codex-ide -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:

;; Helpers for handling MCP elicitation requests from app-server sessions.

;;; Code:

(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)

(defun codex-ide-mcp-elicitation-capabilities ()
  "Return the MCP client capabilities supported by codex-ide."
  '((elicitation . ((form . ())
                    (url . ())))))

(defun codex-ide-mcp-elicitation-normalize-request (params)
  "Normalize elicitation PARAMS across protocol variants."
  (or (alist-get 'request params)
      params))

(defun codex-ide-mcp-elicitation-normalize-completion (params)
  "Normalize elicitation completion PARAMS across protocol variants."
  (or (alist-get 'completion params)
      params))

(defun codex-ide-mcp-elicitation-handle-request (params)
  "Handle an MCP elicitation request with PARAMS.
Return a result alist suitable for a JSON-RPC success response."
  (setq params (codex-ide-mcp-elicitation-normalize-request params))
  (condition-case nil
      (pcase (or (alist-get 'mode params) "form")
        ("form" (codex-ide-mcp-elicitation--handle-form params))
        ("url" (codex-ide-mcp-elicitation--handle-url params))
        (mode
         (error "Unsupported elicitation mode: %s" mode)))
    (quit '((action . "cancel")))))

(defun codex-ide-mcp-elicitation-format-request (params)
  "Return a short human-readable summary for elicitation PARAMS."
  (setq params (codex-ide-mcp-elicitation-normalize-request params))
  (let ((mode (or (alist-get 'mode params) "form"))
        (message (string-trim (or (alist-get 'message params) ""))))
    (string-join
     (delq nil
           (list
            (format "MCP elicitation (%s)" mode)
            (unless (string-empty-p message)
              message)))
     ": ")))

(defun codex-ide-mcp-elicitation-format-completion (params)
  "Return a short human-readable completion summary for elicitation PARAMS."
  (setq params (codex-ide-mcp-elicitation-normalize-completion params))
  (if-let ((elicitation-id (alist-get 'elicitationId params)))
      (format "MCP elicitation completed: %s" elicitation-id)
    "MCP elicitation completed"))

(defun codex-ide-mcp-elicitation-field-specs (schema)
  "Return normalized field specs for elicitation SCHEMA."
  (let ((properties (alist-get 'properties schema))
        (required (mapcar #'codex-ide-mcp-elicitation--field-name
                          (alist-get 'required schema)))
        fields)
    (unless (equal (alist-get 'type schema) "object")
      (error "Unsupported elicitation schema type: %S"
             (alist-get 'type schema)))
    (dolist (entry properties)
      (pcase-let* ((`(,name . ,spec) entry)
                   (field-name (codex-ide-mcp-elicitation--field-name name)))
        (push
         (list :name name
               :field-name field-name
               :title (or (alist-get 'title spec) field-name)
               :description (alist-get 'description spec)
               :type (or (alist-get 'type spec) "string")
               :default (alist-get 'default spec)
               :enum (append (alist-get 'enum spec) nil)
               :enum-names (append (alist-get 'enumNames spec) nil)
               :requiredp (member field-name required))
         fields)))
    (nreverse fields)))

(defun codex-ide-mcp-elicitation-parse-field-value (field raw-value)
  "Parse RAW-VALUE for FIELD and return a typed elicitation value.
FIELD should be one of `codex-ide-mcp-elicitation-field-specs' results."
  (let ((type (plist-get field :type))
        (requiredp (plist-get field :requiredp)))
    (cond
     ((plist-get field :enum)
      (codex-ide-mcp-elicitation--parse-enum field raw-value requiredp))
     ((equal type "boolean")
      (codex-ide-mcp-elicitation--parse-boolean raw-value requiredp))
     ((equal type "integer")
      (codex-ide-mcp-elicitation--parse-integer raw-value requiredp))
     ((equal type "number")
      (codex-ide-mcp-elicitation--parse-number raw-value requiredp))
     ((equal type "string")
      (codex-ide-mcp-elicitation--parse-string raw-value requiredp))
     (t
      (error "Unsupported elicitation field type: %s" type)))))

(defun codex-ide-mcp-elicitation-format-field-prompt (field)
  "Return a human-readable prompt for FIELD."
  (concat
   (plist-get field :title)
   (if (plist-get field :requiredp) " (required)" " (optional)")
   (if-let ((description (plist-get field :description)))
       (format " - %s" description)
     "")
   (if-let ((default (plist-get field :default)))
       (format " [default: %s]" default)
     "")))

(defun codex-ide-mcp-elicitation--handle-form (params)
  "Collect and return a form elicitation result for PARAMS."
  (let ((action
         (codex-ide-mcp-elicitation--choose
          (format "%s "
                  (codex-ide-mcp-elicitation-format-request params))
          '(("submit" . accept)
            ("decline" . decline)
            ("cancel" . cancel)))))
    (pcase action
      ('accept
       (let ((content
              (codex-ide-mcp-elicitation--collect-form-content
               (alist-get 'requestedSchema params))))
         `((action . "accept")
           (content . ,content))))
      ('decline '((action . "decline")))
      (_ '((action . "cancel"))))))

(defun codex-ide-mcp-elicitation--handle-url (params)
  "Collect and return a URL elicitation result for PARAMS."
  (let* ((url (or (alist-get 'url params)
                  (error "URL elicitation requires a url field")))
         (host (url-host (url-generic-parse-url url)))
         (choices
          '(("open and continue" . open)
            ("continue" . accept)
            ("decline" . decline)
            ("cancel" . cancel)))
         (prompt
          (concat
           (format "%s" (codex-ide-mcp-elicitation-format-request params))
           (if host
               (format " [%s]" host)
             "")
           " "))
         (action (codex-ide-mcp-elicitation--choose prompt choices)))
    (pcase action
      ('open
       (browse-url url)
       '((action . "accept")))
      ('accept '((action . "accept")))
      ('decline '((action . "decline")))
      (_ '((action . "cancel"))))))

(defun codex-ide-mcp-elicitation--collect-form-content (schema)
  "Prompt for values matching elicitation SCHEMA."
  (let ((content nil))
    (dolist (field (codex-ide-mcp-elicitation-field-specs schema))
      (let ((value (codex-ide-mcp-elicitation--read-field field)))
        (unless (eq value :codex-ide-mcp-elicitation-omit)
          (push (cons (plist-get field :name) value) content))))
    (nreverse content)))

(defun codex-ide-mcp-elicitation--field-name (name)
  "Normalize elicitation field NAME for comparisons."
  (cond
   ((symbolp name) (symbol-name name))
   ((stringp name) name)
   (t (format "%s" name))))

(defun codex-ide-mcp-elicitation--read-field (field)
  "Read and parse a value for FIELD."
  (let* ((type (plist-get field :type))
         (requiredp (plist-get field :requiredp))
         (default (plist-get field :default))
         (prompt (concat (codex-ide-mcp-elicitation-format-field-prompt field)
                         ": ")))
    (cond
     ((plist-get field :enum)
      (codex-ide-mcp-elicitation--read-enum prompt field requiredp))
     ((equal type "boolean")
      (codex-ide-mcp-elicitation--read-boolean prompt requiredp default))
     ((equal type "integer")
      (codex-ide-mcp-elicitation--read-integer prompt requiredp default))
     ((equal type "number")
      (codex-ide-mcp-elicitation--read-number prompt requiredp default))
     ((equal type "string")
      (codex-ide-mcp-elicitation--read-string prompt requiredp default))
     (t
      (error "Unsupported elicitation field type: %s" type)))))

(defun codex-ide-mcp-elicitation--read-string (prompt requiredp default)
  "Read a string with PROMPT.
REQUIREDP controls empty values and DEFAULT is the default answer."
  (codex-ide-mcp-elicitation--parse-string
   (read-from-minibuffer prompt (when default (format "%s" default)))
   requiredp))

(defun codex-ide-mcp-elicitation--read-integer (prompt requiredp default)
  "Read an integer with PROMPT.
REQUIREDP controls empty values and DEFAULT is the default answer."
  (codex-ide-mcp-elicitation--parse-integer
   (read-from-minibuffer prompt (when default (format "%s" default)))
   requiredp))

(defun codex-ide-mcp-elicitation--read-number (prompt requiredp default)
  "Read a number with PROMPT.
REQUIREDP controls empty values and DEFAULT is the default answer."
  (codex-ide-mcp-elicitation--parse-number
   (read-from-minibuffer prompt (when default (format "%s" default)))
   requiredp))

(defun codex-ide-mcp-elicitation--read-boolean (prompt requiredp default)
  "Read a boolean with PROMPT.
REQUIREDP controls empty values and DEFAULT is the default answer."
  (let* ((choices
          (append
           '(("true" . t)
             ("false" . :json-false))
           (unless requiredp
             '(("skip" . :codex-ide-mcp-elicitation-omit)))))
         (initial
          (cond
           ((eq default t) "true")
           ((eq default :json-false) "false")
           (t nil))))
    (codex-ide-mcp-elicitation--choose prompt choices initial)))

(defun codex-ide-mcp-elicitation--read-enum (prompt field requiredp)
  "Read an enum value with PROMPT using FIELD.
REQUIREDP controls whether the field may be omitted."
  (let* ((values (plist-get field :enum))
         (names (plist-get field :enum-names))
         (choices nil))
    (cl-mapc
     (lambda (value label)
       (push (cons (or label (format "%s" value)) value) choices))
     values
     (append names (make-list (max 0 (- (length values) (length names))) nil)))
    (setq choices (nreverse choices))
    (unless requiredp
      (setq choices (append choices '(("skip" . :codex-ide-mcp-elicitation-omit)))))
    (codex-ide-mcp-elicitation--choose prompt choices)))

(defun codex-ide-mcp-elicitation--parse-string (value requiredp)
  "Parse string VALUE respecting REQUIREDP."
  (cond
   ((and (string-empty-p value) requiredp)
    (user-error "A value is required"))
   ((string-empty-p value)
    :codex-ide-mcp-elicitation-omit)
   (t value)))

(defun codex-ide-mcp-elicitation--parse-integer (value requiredp)
  "Parse integer VALUE respecting REQUIREDP."
  (cond
   ((string-empty-p value)
    (if requiredp
        (user-error "An integer is required")
      :codex-ide-mcp-elicitation-omit))
   ((string-match-p (rx string-start (? "-") (+ digit) string-end) value)
    (string-to-number value))
   (t
    (user-error "Expected an integer"))))

(defun codex-ide-mcp-elicitation--parse-number (value requiredp)
  "Parse number VALUE respecting REQUIREDP."
  (cond
   ((string-empty-p value)
    (if requiredp
        (user-error "A number is required")
      :codex-ide-mcp-elicitation-omit))
   ((string-match-p
     (rx string-start
         (? "-")
         (or
          (+ digit)
          (seq (+ digit) "." (* digit))
          (seq (* digit) "." (+ digit)))
         (? (any "eE") (? (any "+-")) (+ digit))
         string-end)
     value)
    (string-to-number value))
   (t
    (user-error "Expected a number"))))

(defun codex-ide-mcp-elicitation--parse-boolean (value requiredp)
  "Parse boolean VALUE respecting REQUIREDP."
  (cond
   ((or (eq value t) (equal value "true")) t)
   ((or (eq value :json-false) (equal value "false")) :json-false)
   ((or (null value)
        (eq value :codex-ide-mcp-elicitation-omit)
        (equal value ""))
    (if requiredp
        (user-error "A boolean choice is required")
      :codex-ide-mcp-elicitation-omit))
   (t
    (user-error "Expected true or false"))))

(defun codex-ide-mcp-elicitation--parse-enum (field value requiredp)
  "Parse enum VALUE for FIELD respecting REQUIREDP."
  (let ((values (plist-get field :enum)))
    (cond
     ((member value values) value)
     ((or (null value)
          (eq value :codex-ide-mcp-elicitation-omit)
          (equal value ""))
      (if requiredp
          (user-error "A choice is required")
        :codex-ide-mcp-elicitation-omit))
     (t
      (user-error "Expected one of: %s"
                  (mapconcat (lambda (item) (format "%s" item))
                             values ", ")))))) 

(defun codex-ide-mcp-elicitation--choose (prompt choices &optional initial-input)
  "Prompt with PROMPT and return the selected value from CHOICES.
CHOICES is an alist mapping completion labels to results."
  (let ((selection (completing-read prompt choices nil t initial-input)))
    (cdr (assoc selection choices))))

(provide 'codex-ide-mcp-elicitation)

;;; codex-ide-mcp-elicitation.el ends here
