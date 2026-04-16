#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

elisp_string_literal() {
  local value="$1"
  value="${value//\\/\\\\}"
  value="${value//\"/\\\"}"
  printf '"%s"' "$value"
}

if [[ $# -lt 1 ]]; then
  echo "usage: $(basename "$0") FILE..." >&2
  exit 1
fi

LOAD_FORMS=()
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

  LOAD_FORMS+=("(load-file $(elisp_string_literal "$file_path"))")
done

eval_form="(progn ${LOAD_FORMS[*]} \"ok\")"
exec emacsclient --eval "$eval_form"
