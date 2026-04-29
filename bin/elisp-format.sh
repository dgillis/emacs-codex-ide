#!/usr/bin/env bash
# elisp-format.sh --- Batch-indent Emacs Lisp files  -*- lexical-binding: t; -*-

# Commentary:

# Usage:
#   bin/elisp-format.sh FILE [FILE...]
#
# Each FILE may be absolute or relative to the repository root. The script
# loads the file into `emacs-lisp-mode', runs `indent-region' across the whole
# buffer, and writes the formatted result back to disk.

# Code:

set -euo pipefail

if [ "$#" -eq 0 ]; then
echo "Usage: $0 file1.el [file2.el ...]"
exit 1
fi

# Requires are so custom indent on imported packages take effect.
emacs --batch \
--eval "(progn
          (require 'cl-lib)
          (require 'eieio)
          (require 'ert)
          (require 'subr-x)
          (dolist (file command-line-args-left)
            (with-current-buffer (find-file-noselect file)
              (emacs-lisp-mode)
              (indent-region (point-min) (point-max))
              (save-buffer))))" \
"$@"
