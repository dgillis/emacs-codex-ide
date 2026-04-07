# AGENTS.md

This file gives project-specific instructions for agents working on `codex-ide`.

## Purpose

`codex-ide` is a Codex agent UI that runs inside Emacs. It is not a terminal wrapper. The primary UX is native Emacs buffers, windows, commands, and editor context, with Codex sessions backed by `codex app-server`.

## Architecture

Keep the module boundaries clear:

- `codex-ide.el`: main package entry point and core session UI. Owns session lifecycle, process management, JSON-RPC transport, transcript rendering, prompt submission, session/log buffers, modeline/header state, and project-aware buffer context.
- `codex-ide-bridge.el`: Emacs-side bridge helpers. Owns optional bridge configuration, server readiness checks, tool dispatch, and context reporting for the external bridge process.
- `bin/codex-ide-mcp-server.py`: standalone MCP proxy that talks to a running Emacs via `emacsclient` and forwards JSON tool calls into `codex-ide-bridge--json-tool-call`.
- `codex-ide-transient.el`: transient-based command menus and configuration UI. Treat this as command-surface glue, not the home for core business logic.
- `tests/codex-ide-tests.el`: ERT coverage for session setup, command assembly, process handling, bridge config, context composition, and transcript behavior.
- `bin/run-tests.sh`: canonical test runner.

Design expectations:

- Keep core behavior in `codex-ide.el` and `codex-ide-bridge.el`; UI wrappers should stay thin.
- Prefer built-in Emacs facilities and stock-Emacs-compatible code paths.
- Do not add new external package dependencies.
- When touching architecture, preserve the model that Codex runs as an Emacs-native agent UI with optional bridge access back into the live editor.

## Development Rules

- This project should be compatible with stock Emacs without requiring external packages.
- Favor built-in libraries and simple data structures over framework-style abstractions.
- Keep bridge functionality optional and conservative by default.
- After making changes, note that files can be reloaded into the running Emacs session through the Emacs bridge, but only do so if the user explicitly requests it.
- Run existing tests after changes and add or update tests when behavior changes.
- Never commit code unless the user explicitly asks for a commit.

## Commands

Run tests:

```bash
bin/run-tests.sh
```

Run tests directly in batch mode:

```bash
emacs -Q --batch \
  --eval "(setq load-prefer-newer t)" \
  --eval "(setq load-suffixes '(\".el\"))" \
  -L . \
  -L tests \
  -l tests/codex-ide-tests.el \
  -f ert-run-tests-batch-and-exit
```

Byte-compile the main files in batch mode:

```bash
emacs -Q --batch \
  --eval "(setq load-prefer-newer t)" \
  -L . \
  -f batch-byte-compile \
  codex-ide.el codex-ide-bridge.el codex-ide-transient.el
```

Notes:

- If batch compilation fails because `transient` is unavailable, treat that as a compatibility issue to fix rather than papering over it.
- If you batch-compile for testing or validation, remove any generated `.elc` files afterward.
- For quick structural validation when compilation is blocked, use a paren check:

```bash
emacs -Q --batch --eval "(with-temp-buffer (insert-file-contents \"codex-ide.el\") (emacs-lisp-mode) (check-parens) (princ \"ok\"))"
```

Reload changed files into the running Emacs session over the bridge:

```bash
emacsclient --eval '(progn (load-file "/absolute/path/to/codex-ide-bridge.el") (load-file "/absolute/path/to/codex-ide-transient.el") (load-file "/absolute/path/to/codex-ide.el") "ok")'
```

Reload a single file into the running Emacs session:

```bash
emacsclient --eval '(progn (load-file "/absolute/path/to/codex-ide.el") "ok")'
```

Check bridge status from Emacs:

```bash
emacsclient --eval '(codex-ide-bridge-status)'
```

Run an Emacs command over the bridge:

```bash
emacsclient --eval '(codex-ide-check-status)'
```

Open the Codex menu over the bridge:

```bash
emacsclient --eval '(call-interactively #'codex-ide-menu)'
```

## Change Checklist

Before finishing work:

- Mention when reloading changed Elisp files into the live Emacs session with `emacsclient` is available, but only perform it if the user explicitly requests it.
- Run `bin/run-tests.sh`.
- Add or update ERT tests for behavioral changes.
- If relevant, verify batch compilation or explain clearly why it failed.
- Remove any `.elc` files generated during testing or validation.
- Do not commit unless the user explicitly requested it.
