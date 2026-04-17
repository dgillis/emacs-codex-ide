#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_DIR="$ROOT_DIR/tests"
SELECTED_TEST_FILES=()

ARGS=(
  -Q
  --batch
  --eval "(setq load-prefer-newer t)"
  -L "$ROOT_DIR"
  -L "$TEST_DIR"
)

usage() {
  cat <<EOF
Usage: $(basename "$0") [--test-file FILE]...

Run codex-ide ERT tests.

Options:
  --test-file FILE  Load only the named test file. May be repeated.
                    Accepts a basename like codex-ide-tests.el, a path
                    relative to the repo root, or an absolute path.
  -h, --help        Show this help text.
EOF
}

resolve_test_file() {
  local candidate="$1"

  if [[ "$candidate" = /* ]]; then
    printf '%s\n' "$candidate"
    return
  fi

  if [[ "$candidate" == tests/* ]]; then
    printf '%s\n' "$ROOT_DIR/$candidate"
    return
  fi

  if [[ "$candidate" == */* ]]; then
    printf '%s\n' "$ROOT_DIR/$candidate"
    return
  fi

  printf '%s\n' "$TEST_DIR/$candidate"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --test-file)
      if [[ $# -lt 2 ]]; then
        echo "error: --test-file requires a file argument" >&2
        usage >&2
        exit 1
      fi
      SELECTED_TEST_FILES+=("$(resolve_test_file "$2")")
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

if [[ ${#SELECTED_TEST_FILES[@]} -eq 0 ]]; then
  while IFS= read -r test_file; do
    ARGS+=(-l "$test_file")
  done < <(find "$TEST_DIR" -maxdepth 1 -type f -name '*-tests.el' | sort)
else
  for test_file in "${SELECTED_TEST_FILES[@]}"; do
    if [[ ! -f "$test_file" ]]; then
      echo "error: test file not found: $test_file" >&2
      exit 1
    fi
    ARGS+=(-l "$test_file")
  done
fi

exec emacs "${ARGS[@]}" -f ert-run-tests-batch-and-exit
