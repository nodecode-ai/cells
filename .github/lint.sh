#!/usr/bin/env bash
# lint.sh --- Nodecode's structural lint over every Lisp file of this repository.
#
# usage: .github/lint.sh [LINTER]
#   LINTER  a lint-structure.lisp to run (default: Nodecode main's, fetched)
#
# The rules are the ones Nodecode holds its own sources to: every form reads,
# siblings that each begin a line start at one column, and an IS description
# rides its form's line. They are read from Nodecode main rather than copied
# here, so the two never drift apart. Needs sbcl. Exit 0 when every file
# passes.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
if [ -n "${1:-}" ]; then
  linter="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
else
  linter="$(mktemp)"
  trap 'rm -f "$linter"' EXIT
  curl -fsSL -o "$linter" \
    https://raw.githubusercontent.com/nodecode-ai/nodecode/main/src/tools/check/lint-structure.lisp
fi

cd "$here"
git ls-files -z '*.lisp' '*.asd' | xargs -0 sbcl --script "$linter"
