---
name: elisp-reload
description: Reload one or more Emacs Lisp files from this repository into the running Emacs session with `emacsclient --eval`. Use this skill when an agent needs live-session validation after editing `.el` files and the user has asked for reloads.
---

# Elisp Reload

Use this skill to load changed Elisp into the running Emacs session.

## Workflow

1. Pass one or more file paths to `./.agents/skills/elisp-reload/scripts/elisp-reload.sh`.
2. Prefer absolute or repo-relative `.el` paths.
3. Only use this when live reloading is appropriate; editing files does not imply reload permission.

## Notes

- Use the explicit repo-root path above; do not reinterpret it relative to `SKILL.md`.
- The script resolves repo-relative paths against the repository root.
- It loads files in the order they are provided.
