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

Following summarizes the intended module structure:

- `codex-ide.el`: top-level entrypoint only. This should mainly hold package metadata, customization variables, and module wiring. Avoid placing substantive business logic here.
- `codex-ide-core.el`: core session/state primitives shared across the codebase. This is the bottom dependency layer and should not depend on other `codex-ide-*.el` files.
- `codex-ide-errors.el`: error normalization, classification, guidance text, and recovery decisions shared by session, protocol, and transcript code.
- `codex-ide-log.el`: log buffer creation, trimming, and stderr capture. This owns implementation-facing diagnostics, not user-facing transcript rendering.
- `codex-ide-window.el`: buffer/window display policy. This owns reuse, split, focus, and presentation decisions when showing Codex buffers.
- `codex-ide-context.el`: prompt context extraction and payload composition. This owns Emacs/session context formatting and prompt payload assembly.
- `codex-ide-protocol.el`: JSON-RPC transport helpers plus thread/config/model request helpers. This owns app-server request/response formatting and protocol-level bookkeeping, not transcript mutations.
- `codex-ide-threads.el`: thread selection and thread list candidate formatting helpers.
- `codex-ide-session-mode.el`: major/minor modes, prompt-local navigation, and mode-specific buffer behavior for session buffers.
- `codex-ide-session.el`: session/process lifecycle. This owns CLI startup, process filters/sentinels, session creation/teardown, and primary interactive session commands.
- `codex-ide-renderer.el`: view-oriented rendering helpers. This should focus on buffer presentation, markdown rendering, text properties, overlays, and other logic that acts on explicit inputs plus buffer state. It should not depend on other `codex-ide-*.el` files.
- `codex-ide-transcript.el`: transcript controller logic. This owns the orchestration that maps Codex session state and protocol events into transcript updates, prompt submission flow, approvals, elicitation rendering, and transcript-specific state transitions. It should use `codex-ide-renderer.el` for rendering primitives rather than duplicating renderer policy.
- `codex-ide-transient.el`: transient-based command menus and configuration UI. Treat this as command-surface glue, not the home for core business logic.
- `codex-ide-mcp-bridge.el`: Emacs-side bridge helpers. Owns optional bridge configuration, server readiness checks, tool dispatch, and context reporting for the external bridge process.
- `codex-ide-mcp-elicitation.el`: MCP elicitation payload normalization and formatting helpers shared by bridge/transcript code.
- `codex-ide-nav.el`, `codex-ide-section.el`, `codex-ide-status-mode.el`, `codex-ide-session-list.el`, `codex-ide-session-buffer-list.el`, `codex-ide-session-thread-list.el`, `codex-ide-debug-info.el`, `codex-ide-delete-session-thread.el`, `codex-ide-utils.el`: focused support modules whose names should continue to match their single responsibility.
- `codex-ide-*.el`: if a new concern does not fit an existing file cleanly, prefer creating a new focused module over growing an unrelated one.
- `bin/codex-ide-mcp-server.py`: standalone MCP proxy that talks to a running Emacs via `emacsclient` and forwards JSON tool calls into `codex-ide-mcp-bridge--json-tool-call`.
- `tests/*-tests.el`: ERT coverage organized alongside the source modules they exercise.

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
- Use `cl` keywords when functions have many (more than 3) parameters.
- Tests should be organized in files resembling the source code. Ex: tests for "codex-ide-foo.el" should go in "tests/codex-ide-foo-tests.el".

### Change management

- Remove any `.elc` files generated during testing/validation.
- Never commit code unless the user explicitly asks for a commit.
