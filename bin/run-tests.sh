#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_DIR="$ROOT_DIR/tests"
mapfile -t TEST_FILES < <(find "$TEST_DIR" -maxdepth 1 -type f -name '*-tests.el' | sort)

ARGS=(
  -Q
  --batch
  --eval "(setq load-prefer-newer t)"
  --eval "(setq load-suffixes '(\".el\"))"
  -L "$ROOT_DIR"
  -L "$TEST_DIR"
)

for test_file in "${TEST_FILES[@]}"; do
  ARGS+=(-l "$test_file")
done

exec emacs "${ARGS[@]}" -f ert-run-tests-batch-and-exit
