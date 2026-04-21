;;; codex-ide.el --- Codex app-server integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Duncan Gillis
;; URL: https://github.com/dgillis/codex-ide
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (transient "0.9.0"))
;; Keywords: ai, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Top-level package entrypoint for codex-ide.
;;
;; This file intentionally stays thin.  It owns package metadata, user-facing
;; customization variables, and the module wiring that assembles the codex-ide
;; feature set.  Functional behavior lives in narrower modules:
;;
;; - `codex-ide-core.el` for shared session state primitives.
;; - `codex-ide-session.el` for process/session lifecycle and command entry
;;   points like starting, stopping, and resetting sessions.
;; - `codex-ide-transcript.el` for prompt lifecycle and transcript-oriented
;;   session orchestration.
;; - `codex-ide-protocol.el` for JSON-RPC and app-server request helpers.
;; - Other focused `codex-ide-*.el` modules for context, logging, threads,
;;   display policy, bridge helpers, and supporting UI surfaces.
;;
;; Keeping this entrypoint small prevents the package root from becoming the
;; accidental home for unrelated logic and makes module boundaries much easier
;; to maintain.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;###autoload
(defgroup codex-ide nil
  "Codex app-server integration for Emacs."
  :group 'tools
  :prefix "codex-ide-")

;;;###autoload
(defcustom codex-ide-cli-path "codex"
  "Path to the Codex CLI executable."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-buffer-name-function #'codex-ide--default-buffer-name
  "Function used to derive the Codex session buffer name."
  :type 'function
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-cli-extra-flags ""
  "Additional flags appended to the `codex app-server` command."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-model nil
  "Optional model name for new or resumed threads."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Model"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-reasoning-effort nil
  "Optional reasoning effort for new Codex turns."
  :type '(choice (const :tag "Default" nil)
                 (const "none")
                 (const "minimal")
                 (const "low")
                 (const "medium")
                 (const "high")
                 (const "xhigh"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-running-submit-action 'steer
  "Action used by `codex-ide-submit' while a Codex turn is running."
  :type '(choice (const :tag "Steer active turn" steer)
                 (const :tag "Queue next turn" queue))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-session-baseline-prompt "
- You are a Codex server running inside Emacs.
- You can use MCP tools to inspect and interact with the running Emacs session.
- Interpret Emacs terminology as relevant context to the user's request: buffers, regions, windows, point, mark, current file, etc.
- Responses are rendered as Markdown in an Emacs buffer.
- Markdown pipe tables are rendered as visible tables.
- In table cells, wrap code-like identifiers, filenames, paths, symbols, and expressions in backticks.
- Use markdown links for code references, for example [`foo.el`](/tmp/foo.el#L3C2).
- Avoid bare underscores or asterisks for code-like text inside tables; use backticks instead.
- Do not needlessly use Emacs commands to accomplish agent tasks."
  "Optional baseline prompt injected into the first real prompt of a new thread."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Prompt"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-buffer-name-prefix "codex"
  "Prefix used when creating Codex session buffer names."
  :type 'string
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-focus-on-open t
  "Whether to focus the Codex window after showing it."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-new-session-split nil
  "Window split direction to use when showing newly created Codex sessions."
  :type '(choice (const :tag "Default display behavior" nil)
                 (const :tag "Vertical split" vertical)
                 (const :tag "Horizontal split" horizontal))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-buffer-display-when-approval-required t
  "Whether to display a Codex buffer when it requires approval."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defconst codex-ide-display-buffer-options nil
  "Ordered display policy keys for `codex-ide-display-buffer'.")

;;;###autoload
(defcustom codex-ide-session-enable-visual-line-mode t
  "Whether Codex session buffers should enable `visual-line-mode' by default."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-approval-policy "on-request"
  "Approval policy for new or resumed Codex threads."
  :type '(choice (const "untrusted")
                 (const "on-failure")
                 (const "on-request")
                 (const "never"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-sandbox-mode "workspace-write"
  "Sandbox mode for new or resumed Codex threads."
  :type '(choice (const "read-only")
                 (const "workspace-write")
                 (const "danger-full-access"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-personality "pragmatic"
  "Personality for new or resumed Codex threads."
  :type '(choice (const "none")
                 (const "friendly")
                 (const "pragmatic"))
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-request-timeout 10
  "Seconds to wait for synchronous app-server responses."
  :type 'number
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-log-max-lines 10000
  "Maximum number of lines to keep in each Codex log buffer."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-logging-enabled nil
  "Whether codex-ide should create and write diagnostic log buffers."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-log-stream-deltas nil
  "Whether to log every streamed output delta."
  :type 'boolean
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-resume-summary-turn-limit 100
  "How many recent turns to summarize when resuming a stored thread."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defun codex-ide-toggle-logging-enabled ()
  "Toggle `codex-ide-logging-enabled' interactively."
  (interactive)
  (setq codex-ide-logging-enabled
        (not codex-ide-logging-enabled))
  (message "codex-ide logging %s"
           (if codex-ide-logging-enabled "enabled" "disabled")))

(require 'codex-ide-core)
(require 'codex-ide-errors)
(require 'codex-ide-window)
(require 'codex-ide-log)
(require 'codex-ide-context)
(require 'codex-ide-nav)
(require 'codex-ide-renderer)
(require 'codex-ide-session-mode)
(require 'codex-ide-protocol)
(require 'codex-ide-threads)
(require 'codex-ide-transcript)
(require 'codex-ide-transient)
(require 'codex-ide-mcp-bridge)
(require 'codex-ide-session)
(require 'codex-ide-delete-session-thread)

(provide 'codex-ide)

;;; codex-ide.el ends here
