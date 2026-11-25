#!/bin/sh
# selffix spec, Lisp: the checks are spec.lisp, run by the nodecode build's own
# `trial' verb; this only gives it a root, then runs the cell's tests. Run from a
# nodecode-cells copy's root. NC_BUILD picks the build (default: the installed
# one); NODECODE_SRC the core checkout the cell's tests run against. Last line N/14.
# SPDX-License-Identifier: MIT
set -u
COPY=$(pwd) SPEC=$(cd "$(dirname "$0")" && pwd)
NC=$(readlink -f "${NC_BUILD:-$(command -v nodecode)}")
[ -x "$NC" ] || { echo "no nodecode build"; echo 0/14; exit 1; }
R=$(mktemp -d "${TMPDIR:-/tmp}/selffix-lspec.XXXXXX")
trap '[ -n "${KEEP:-}" ] || rm -rf "$R"' EXIT
mkdir -p "$R/outer" && echo '{"update": {"mode": "off"}}' > "$R/outer/config.jsonc"
env SELFFIX_SPEC_COPY="$COPY" SELFFIX_SPEC_ROOT="$R" SELFFIX_SPEC_NC="$NC" \
    SELFFIX_SPEC_KEY=sk-planted-spec OPENAI_API_KEY=sk-planted-spec \
    NODECODE_HOME="$R/outer/" HOME="$R/outer" XDG_CACHE_HOME="$R/outer/cache" \
    timeout 2400 "$NC" trial < "$SPEC/spec.lisp" > "$R/trial.out" 2>&1
P=$(sed -n 's/^nodecode-trial-score \([0-9][0-9]*\).*/\1/p' "$R/trial.out" | tail -1)
cat "$R/report" 2>/dev/null
[ -n "$P" ] || { echo "FAIL spec.lisp gave no score:"; tail -5 "$R/trial.out"; P=0; }
SRC=${NODECODE_SRC:-}
for d in "$HOME/nodecode/nodecode" "$HOME/Lab/nodecode/nodecode-public"; do [ -z "$SRC" ] && [ -d "$d" ] && SRC=$d; done
tests=$(NODECODE_SRC=$SRC bash .github/test-cell.sh tools/nodecode-selffix 2>&1 | tail -1)
case "$tests" in *" 0 failed"*) P=$((P + 1)); echo "pass the cell's tests: $tests";; *) echo "FAIL the cell's tests: $tests";; esac
echo "$P/14"
