# AGENTS.md

This file gives project-specific instructions for agents working on `codex-ide`.

## Purpose

`codex-ide` is a Codex agent UI that runs inside Emacs. It is not a terminal wrapper. The primary UX is native Emacs buffers, windows, commands, and editor context, with Codex sessions backed by `codex app-server`.

## Architecture

### Principles

The architecture of this project is evolving. Some things to follow when making changes:
- Organize code into single purpose elisp files.
- Aim for clean unidirectional dependencies.
- Within reason, write code in a DRY way.
- Look for code reuse opportunities to factor out duplicative functionality.

### Directory structure

Following summarizes the current (not neccessarily ideal) directory structure:

- `codex-ide.el`: an undesirably monolithic module. Avoid adding new functionaltiy to it (where possible) and look for opportunities to extract funcitonality out.
- `codex-ide-core.el`: baseline dependencies needed (almost) everywhere. This should not depend on anything else.
- `codex-ide-renderer.el`: responsible for displaying agent generated text in Emacs buffers.
- `codex-ide-transient.el`: transient-based command menus and configuration UI. Treat this as command-surface glue, not the home for core business logic.
- `codex-ide-mcp-bridge.el`: Emacs-side bridge helpers. Owns optional bridge configuration, server readiness checks, tool dispatch, and context reporting for the external bridge process.
- `codex-ide-*.el`: Other elisp source code whose name/documentation describes its primary purpose.
- `bin/codex-ide-mcp-server.py`: standalone MCP proxy that talks to a running Emacs via `emacsclient` and forwards JSON tool calls into `codex-ide-mcp-bridge--json-tool-call`.
- `tests/*-tests.el`: ERT coverage for session setup, command assembly, process handling, bridge config, context composition, and transcript behavior.

## Development Practices

Unless instructed otherwise, adhere to the practices below.

### Testing & Validation

- Add/update ERT tests to ensure coverage of all changes.
- Avoid overly brittle tests (e.g., things like asserting exact values of defvar defaults).
- Validate all non-trivial changes via the `run-tests` skill.
- After successful test runs, reload modified elisp files (excluding tests) via the `elisp-reload` skill.
- Do NOT run integration tests (these are for human driven use).

### Tooling & Libraries

- This project should be compatible with stock Emacs without requiring external packages.
- Prefer built-in Emacs facilities and stock-Emacs-compatible code paths.
- Favor built-in libraries and simple data structures over framework-style abstractions.
- Keep MCP bridge functionality optional and conservative by default.
- Do not add new external dependencies.

### Coding Conventions

- When defining key maps, place the `define-key` calls at the top-level of the package so they will take effect when reloading files.
- Tests should be organized in files resembling the source code. Ex: tests for "codex-ide-foo.el" should go in "tests/codex-ide-foo-tests.el".

### Change management

- Remove any `.elc` files generated during testing/validation.
- Never commit code unless the user explicitly asks for a commit.
