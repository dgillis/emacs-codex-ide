#!/usr/bin/env bash
set -euo pipefail

BIN_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$BIN_DIR/.." && pwd)"

elisp_string_literal() {
  local value="$1"
  value="${value//\\/\\\\}"
  value="${value//\"/\\\"}"
  printf '"%s"' "$value"
}

if [[ $# -eq 0 ]]; then
  set -- "$REPO_ROOT"/*.el
  if [[ "$1" = "$REPO_ROOT/*.el" ]]; then
    echo "error: no .el files found in $REPO_ROOT" >&2
    exit 1
  fi
fi

load_forms=""
for input_path in "$@"; do
  if [[ "$input_path" = /* ]]; then
    file_path="$input_path"
  else
    file_path="$REPO_ROOT/$input_path"
  fi

  if [[ ! -f "$file_path" ]]; then
    echo "error: file not found: $file_path" >&2
    exit 1
  fi

  load_forms="$load_forms (load-file $(elisp_string_literal "$file_path"))"
done

eval_form="(progn$load_forms \"ok\")"
exec emacsclient --eval "$eval_form"
