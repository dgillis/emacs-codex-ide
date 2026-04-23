#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

if [[ $# -ne 1 ]]; then
  echo "usage: $(basename "$0") FILE" >&2
  exit 1
fi

if [[ "$1" = /* ]]; then
  file_path="$1"
else
  file_path="$REPO_ROOT/$1"
fi

if [[ ! -f "$file_path" ]]; then
  echo "error: file not found: $file_path" >&2
  exit 1
fi

cd "$REPO_ROOT"
emacs -Q --batch --eval "(setq load-prefer-newer t)" -L . -f batch-byte-compile "$file_path"
"$REPO_ROOT/.agents/skills/clean-up-elc/scripts/clean-up-elc.sh" "$file_path"
