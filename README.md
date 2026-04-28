# Codex IDE for Emacs

[![GNU Emacs 28.1+](https://img.shields.io/badge/GNU%20Emacs-28.1%2B-blue.svg)](https://www.gnu.org/software/emacs/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Codex IDE for Emacs is a pure Emacs Codex client, inspired by [claude-code-ide.el](https://github.com/manzaltu/claude-code-ide.el).

This package provides native integration with `codex app-server` which, unlike terminal-based wrappers, renders Codex sessions as normal Emacs buffers and keeps the interaction surface fully inside Emacs.

## Overview

### Features

- Runs Codex as an Emacs major mode with no terminal wrapper.
- Renders code blocks with full Emacs major-mode syntax highlighting instead of terminal-style formatting.
- Displays diffs using Emacs diff rendering, so patches look and read like they belong in Emacs.
- Turns Codex file and code references into clickable Emacs widgets that jump straight to real buffers.
- Keeps approvals in-buffer with an interactive review flow for confirming commands and changes without leaving the session.
- Lets you expand or collapse transcript detail, so you can skim the headline progress or inspect the full turn-by-turn output.
- Uses MCP integration to give Codex awareness of your live Emacs window and buffer state when that extra context is available.
- Provides an interactive configuration menu for model choice, sandboxing, personality, and other session controls.
- Shows live header-line status for quota and token usage while a session is running.
- Provides a session management mode to preview, search, and restore previous Codex sessions from inside Emacs.

### Screenshots

#### Codex mode inside Emacs

![Emacs state aware](https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/codex-mode-inside-emacs.png)
_Codex knows what file and region inside Emacs is active._

#### Run multiple Codex sessions

![Multiple codex sessions](https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/run-multiple-codex-sessions.jpg)
_Run and manage multiple agents at once._

#### Expandable Codex output

![Toggle agent output](https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/expandable-codex-output.jpg)
_Expand or collapse detail within Codex output_

#### View and resume prior Codex sessions

![Manage past sessions](https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/view-and-resume-prior-sessions.jpg)
_Mode for viewing and restoring past Codex sessions._

<!--
#### Emacs mode-based code rendering

![Major-mode syntax coloring](https://github.com/user-attachments/assets/2dc363f4-ab76-44c2-b45b-51729c908465)
_Code blocks rendered according to Emacs major mode._

#### Interactive approvals

![Interactive approvals](https://github.com/user-attachments/assets/0ab2989b-4cc1-47f9-adbf-cd273ae9fe1f)
_Emacs-widget based interactive approvals_
-->

## Installation

### Prerequisites

- Emacs 28.1 or higher
- Codex CLI installed and available on `PATH`
- `transient` installed
- `python3` and `emacsclient` available if you want the optional Emacs MCP bridge

### Installing Codex CLI

See the official app-server documentation: [OpenAI Codex app-server docs](https://developers.openai.com/codex/app-server#api-overview).

### Installing the Emacs Package

To install using `use-package` with `:vc` on Emacs 30+:

```emacs-lisp
(use-package codex-ide
  :vc (:url "https://github.com/dgillis/emacs-codex-ide" :rev :newest)
  :bind ("C-c C-;" . codex-ide-menu))
```

To install using `use-package` and [straight.el](https://github.com/radian-software/straight.el):

```emacs-lisp
(use-package codex-ide
  :straight (:type git :host github :repo "dgillis/emacs-codex-ide")
  :bind ("C-c C-;" . codex-ide-menu))
```

After installation, run `M-x codex-ide-menu` or `M-x codex-ide` to start a session for the current project.

## Getting Started

### `codex-ide-menu`

Use `M-x codex-ide-menu` as the main entry point. It opens a transient menu for starting a new session, continuing the most recent session, sending a prompt from the minibuffer, switching to existing buffers, opening buffer lists, and adjusting configuration.

![The main menu is the recommended starting point for everyday Codex IDE commands.](https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/codex-ide-menu.png)

### `codex-ide-session-mode`

`codex-ide-session-mode` is the buffer interface to Codex. It renders the conversation transcript, keeps the active prompt editable in-place, streams assistant output, turns file references into links, and handles interruption or approval flows from inside Emacs.

Key bindings:

- `C-c RET` submits the active prompt.
- `C-c C-c` or `C-c C-k` interrupts the current response.
- `C-M-p` and `C-M-n` move between prompt lines.
- `M-p` and `M-n` cycle prompt history while point is in the active prompt.
- `TAB` and `S-TAB` move between clickable buttons and file links.

## Examples

#### Codex session buffer mode

https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/context-aware-prompt.mov

https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/buffer-code-links.mov

#### Manage sessions mode

https://github.com/dgillis/emacs-codex-ide/blob/1c0bb00a35c8fcb20e8a30e28cdc56d774267049/screenshots/manage-sessions-mode.mov

## License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT).

## Disclaimer

Codex(R) is a trademark of OpenAI. Codex(R) is an application developed by OpenAI.

This project is not affiliated with, endorsed by, or sponsored by OpenAI.
