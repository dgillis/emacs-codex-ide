#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

if [[ $# -lt 1 ]]; then
  echo "usage: $(basename "$0") FILE..." >&2
  exit 1
fi

for input_path in "$@"; do
  if [[ "$input_path" = /* ]]; then
    resolved_path="$input_path"
  else
    resolved_path="$REPO_ROOT/$input_path"
  fi

  case "$resolved_path" in
    *.el)
      target_path="${resolved_path}c"
      ;;
    *.elc)
      target_path="$resolved_path"
      ;;
    *)
      echo "error: unsupported path: $input_path" >&2
      exit 1
      ;;
  esac

  rm -f "$target_path"
done
