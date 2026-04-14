#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INTEGRATION_DIR="$ROOT_DIR/tests/integration"

export CODEX_IDE_INTEGRATION_TESTS=1
export CODEX_IDE_INTEGRATION_PROJECT_DIR="${CODEX_IDE_INTEGRATION_PROJECT_DIR:-$PWD}"

ARGS=(
  --no-desktop
  --no-init-file
  --eval "(setq mac-command-modifier 'meta ns-command-modifier 'meta)"
  --eval "(setq load-prefer-newer t)"
  --eval "(setq load-suffixes '(\".el\"))"
  -L "$ROOT_DIR"
  -L "$ROOT_DIR/tests"
  -L "$INTEGRATION_DIR"
)

if [[ "${CODEX_IDE_INTEGRATION_BATCH:-}" == "1" ]]; then
  while IFS= read -r test_file; do
    ARGS+=(-l "$test_file")
  done < <(find "$INTEGRATION_DIR" -maxdepth 1 -type f -name '*-tests.el' | sort)

  exec emacs "${ARGS[@]}" \
    --batch \
    --eval '(progn
              (message "codex-ide-it: runner batch=1 project=%s"
                       (getenv "CODEX_IDE_INTEGRATION_PROJECT_DIR"))
              (message "codex-ide-it: starting ERT selector ^codex-ide-integration-")
              (ert-run-tests-batch-and-exit "^codex-ide-integration-"))'
fi

exec emacs "${ARGS[@]}" \
  --eval '(when (display-graphic-p)
            (toggle-frame-maximized))' \
  -l "$INTEGRATION_DIR/codex-ide-integration-runner.el" \
  --eval '(progn
            (delete-other-windows)
            (switch-to-buffer (get-buffer-create "*Messages*"))
            (split-window-right)
            (other-window 1)
            (message "codex-ide-it: runner batch=nil project=%s"
                     (getenv "CODEX_IDE_INTEGRATION_PROJECT_DIR"))
            (message "codex-ide-it: starting async interactive integration runner")
            (codex-ide-integration-run))'
