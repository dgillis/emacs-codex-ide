---
name: elisp-compile
description: Validate byte compilation for a specified Emacs Lisp file in this repository. Use this skill when an agent needs a focused compile check for one `.el` file; on success it removes the generated `.elc` artifact.
---

# Elisp Compile

Use this skill to run a single-file batch byte-compile check and leave the worktree clean afterward.

## Workflow

1. Pass exactly one `.el` file path to `./skills/elisp-compile/scripts/elisp-compile.sh`.
2. The script runs `emacs -Q --batch --eval "(setq load-prefer-newer t)" -L . -f batch-byte-compile FILE`.
3. If compilation succeeds, it deletes the generated `.elc` file before exiting.
4. If compilation fails, it leaves the `.elc` artifact state unchanged and returns the compiler error.

## Notes

- Use the explicit repo-root path above; do not reinterpret it relative to `SKILL.md`.
- This is for targeted validation, not full-suite testing.
- Use the cleanup skill directly when `.elc` artifacts already exist from other commands.
