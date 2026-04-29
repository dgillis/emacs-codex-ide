#!/usr/bin/env bash
set -euo pipefail

SOURCE_PATH="${BASH_SOURCE[0]}"
while [ -L "$SOURCE_PATH" ]; do
  SOURCE_DIR="$(cd -P "$(dirname "$SOURCE_PATH")" && pwd)"
  LINK_TARGET="$(readlink "$SOURCE_PATH")"
  if [[ "$LINK_TARGET" = /* ]]; then
    SOURCE_PATH="$LINK_TARGET"
  else
    SOURCE_PATH="$SOURCE_DIR/$LINK_TARGET"
  fi
done

ROOT_DIR="$(cd -P "$(dirname "$SOURCE_PATH")/.." && pwd)"

FORMAT_TARGETS=()
while IFS= read -r path; do
  FORMAT_TARGETS+=("$path")
done < <(git -C "$ROOT_DIR" diff --cached --name-only --diff-filter=ACMR -- '*.el' 'bin/elisp-format.sh')

if [[ ${#FORMAT_TARGETS[@]} -gt 0 ]]; then
  "$ROOT_DIR/bin/elisp-format.sh" "${FORMAT_TARGETS[@]}"
  git -C "$ROOT_DIR" add -- "${FORMAT_TARGETS[@]}"
fi

"$ROOT_DIR/bin/generate-autoloads.sh"
git -C "$ROOT_DIR" add codex-ide-autoloads.el

"$ROOT_DIR/bin/run-tests.sh"

exec emacs -Q --batch \
  --eval "(setq load-prefer-newer t)" \
  -L "$ROOT_DIR" \
  --eval "(require 'codex-ide-autoloads)" \
  --eval "(require 'codex-ide)" \
  --eval "(princ \"codex-ide loaded successfully\n\")"
