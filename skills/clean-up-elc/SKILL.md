---
name: clean-up-elc
description: Delete byte-compiled `.elc` files produced during validation in this repository. Use this skill after batch compilation or other checks that leave generated `.elc` artifacts behind.
---

# Clean Up Elc

Use this skill to remove generated `.elc` files without touching source files.

## Workflow

1. Pass one or more `.el` or `.elc` paths to `./skills/clean-up-elc/scripts/clean-up-elc.sh`.
2. For `.el` inputs, the script deletes the sibling `.elc` file if present.
3. For `.elc` inputs, the script deletes that file directly.

## Notes

- Use the explicit repo-root path above; do not reinterpret it relative to `SKILL.md`.
- Missing files are ignored so the cleanup step is idempotent.
- Keep this limited to generated `.elc` artifacts.
