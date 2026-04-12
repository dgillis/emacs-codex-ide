#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_DIR="$ROOT_DIR/tests"

ARGS=(
  -Q
  --batch
  --eval "(setq load-prefer-newer t)"
  -L "$ROOT_DIR"
  -L "$TEST_DIR"
)

while IFS= read -r test_file; do
  ARGS+=(-l "$test_file")
done < <(find "$TEST_DIR" -maxdepth 1 -type f -name '*-tests.el' | sort)

exec emacs "${ARGS[@]}" -f ert-run-tests-batch-and-exit
