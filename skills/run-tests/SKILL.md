---
name: run-tests
description: Run the codex-ide test suite from this repository. Use this skill when an agent needs to validate changes by running `bin/run-tests.sh`, either for the full suite or for one or more specific ERT files via `--test-file FILE`.
---

# Run Tests

Use this skill to execute the repository's canonical test runner.

## Workflow

1. Run `scripts/run-tests.sh` with no arguments to execute the full ERT suite.
2. Pass `--test-file FILE` one or more times to scope the run to specific files.
3. Report the failing command, failing test file, and the first useful error when tests fail.

## Notes

- Prefer this skill over ad hoc `emacs --batch` invocations when the goal is normal project validation.
- The script runs from the repo root and forwards arguments to `bin/run-tests.sh`.
