# AGENTS.md

This file gives project-specific instructions for agents working on `codex-ide`.

## Purpose

`codex-ide` is a Codex agent UI that runs inside Emacs. It is not a terminal wrapper. The primary UX is native Emacs buffers, windows, commands, and editor context, with Codex sessions backed by `codex app-server`.

## Architecture

Keep the module boundaries clear:

- `codex-ide.el`: main package entry point and core session UI. Owns session lifecycle, process management, JSON-RPC transport, transcript rendering, prompt submission, session/log buffers, modeline/header state, and project-aware buffer context.
- `codex-ide-mcp-bridge.el`: Emacs-side bridge helpers. Owns optional bridge configuration, server readiness checks, tool dispatch, and context reporting for the external bridge process.
- `bin/codex-ide-mcp-server.py`: standalone MCP proxy that talks to a running Emacs via `emacsclient` and forwards JSON tool calls into `codex-ide-mcp-bridge--json-tool-call`.
- `codex-ide-transient.el`: transient-based command menus and configuration UI. Treat this as command-surface glue, not the home for core business logic.
- `tests/*-tests.el`: ERT coverage for session setup, command assembly, process handling, bridge config, context composition, and transcript behavior.
- `tests/integration/*`: optional integration tests, if present, that run in an isolated Emacs process and exercise `codex-ide` against a real `codex app-server` instance. Keep these separate from the normal ERT suite because they are slower, environment-dependent, and may use real Codex authentication or network access.
- `bin/run-tests.sh`: canonical test runner.

Design expectations:

- Keep core behavior in `codex-ide.el` and `codex-ide-mcp-bridge.el`; UI wrappers should stay thin.
- Prefer built-in Emacs facilities and stock-Emacs-compatible code paths.
- Do not add new external package dependencies.
- When touching architecture, preserve the model that Codex runs as an Emacs-native agent UI with optional bridge access back into the live editor.

Coding conventions:

- When defining key maps, place the `define-key` calls at the top-level of the package so they will take effect when reloading files.
- Tests should be organized in files resembling the source code. Ex: tests for "codex-ide-foo.el" should go in "tests/codex-ide-foo-tests.el".

## Development Rules

- This project should be compatible with stock Emacs without requiring external packages.
- Favor built-in libraries and simple data structures over framework-style abstractions.
- Keep bridge functionality optional and conservative by default.
- After making changes, note that files can be reloaded into the running Emacs session through the Emacs bridge, but only do so if the user explicitly requests it. When doing this, do not bother reloading changes in test files.
- After all non-trivial file changes run the `emacs/lisp_check_parens` to ensure no mismatched parens.
- Run existing tests after changes and add or update tests when behavior changes.
- Do not run integration tests that require a real Emacs process or real Codex instance unless the user explicitly requests them.
- Never commit code unless the user explicitly asks for a commit.

## Commands

Run tests:

```bash
bin/run-tests.sh
```

Run optional real-Codex integration tests only when explicitly requested:

```bash
bin/run-integration-tests.sh
```

By default this opens a normal Emacs session so failures can be observed and
debugged interactively, uses the shell's current working directory as the Codex
project under test, and runs a timer-driven async integration runner so Emacs
stays responsive while Codex streams. For automation, run it with
`CODEX_IDE_INTEGRATION_BATCH=1` to use the ERT-backed batch mode and exit with
the ERT status.

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
  codex-ide.el codex-ide-mcp-bridge.el codex-ide-transient.el
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
emacsclient --eval '(progn (load-file "/absolute/path/to/codex-ide-mcp-bridge.el") (load-file "/absolute/path/to/codex-ide-transient.el") (load-file "/absolute/path/to/codex-ide.el") "ok")'
```

Reload a single file into the running Emacs session:

```bash
emacsclient --eval '(progn (load-file "/absolute/path/to/codex-ide.el") "ok")'
```

Check bridge status from Emacs:

```bash
emacsclient --eval '(codex-ide-mcp-bridge-status)'
```

Run an Emacs command over the bridge:

```bash
emacsclient --eval '(codex-ide-show-cli-info)'
```

Open the Codex menu over the bridge:

```bash
emacsclient --eval '(call-interactively #'codex-ide-menu)'
```

## Change Checklist

Before finishing work:

- Mention when reloading changed Elisp files into the live Emacs session with `emacsclient` is available, but only perform it if the user explicitly requests it.
- Run `bin/run-tests.sh`.
- Do not run optional real-Codex integration tests unless explicitly requested.
- Add or update ERT tests for behavioral changes.
- If relevant, verify batch compilation or explain clearly why it failed.
- Remove any `.elc` files generated during testing or validation.
- Do not commit unless the user explicitly requested it.
