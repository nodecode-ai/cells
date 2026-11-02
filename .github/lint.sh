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

repo=nodecode-ai/nodecode

# fetch_linter FILE --- write Nodecode main's lint-structure.lisp to FILE.
# The linter has moved with Nodecode's layout (src/tools/, src/tools/check/,
# tools/check/), so its path is read from main's tree; when the GitHub API
# does not answer (rate limit), the homes it has had are tried newest first.
fetch_linter() {
  local auth=() path found
  if [ -n "${GITHUB_TOKEN:-}" ]; then
    auth=(-H "Authorization: Bearer $GITHUB_TOKEN")
  fi
  found="$(curl -fsSL ${auth[@]+"${auth[@]}"} \
             "https://api.github.com/repos/$repo/git/trees/main?recursive=1" \
           | grep -o '"path": *"[^"]*lint-structure\.lisp"' \
           | sed 's/.*"\([^"]*\)"$/\1/' | head -1)" || found=""
  for path in $found tools/check/lint-structure.lisp \
              src/tools/check/lint-structure.lisp src/tools/lint-structure.lisp; do
    if curl -fsSL -o "$1" "https://raw.githubusercontent.com/$repo/main/$path" 2>/dev/null; then
      echo "linter: $repo main $path" >&2
      return 0
    fi
  done
  echo "lint.sh: no lint-structure.lisp found in $repo main" >&2
  return 1
}

here="$(cd "$(dirname "$0")/.." && pwd)"
if [ -n "${1:-}" ]; then
  linter="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
else
  linter="$(mktemp)"
  trap 'rm -f "$linter"' EXIT
  fetch_linter "$linter"
fi

cd "$here"
git ls-files -z '*.lisp' '*.asd' | xargs -0 sbcl --script "$linter"
