#!/bin/bash
# selffix spec: idle self-fixes end to end on a real organism. SPDX-License-Identifier: MIT
#
# Run from a nodecode-cells copy's root. NC_BUILD picks the nodecode build (default: the
# installed one). A scratch home carries this copy's cron and selffix cells and a planted
# defect (plant/: every turn whose session's input says SELFFIX-PLANT fails on an undefined
# function). A real turn fails there and lands in the store, beside one an outside provider
# fails. Then a turn of that organism, scripted by a loopback provider, runs the cell's own
# backlog, cycle and ledger, and the test the cycle locked is run on its own: it fails while
# the defect is there, unchanged it passes once the defect is gone, and it runs sealed (no
# keys, no network, the real home hidden, the home's store untouched). Last line: N/TOTAL.
set -u
T=13 P=0
COPY=$(pwd) SPEC=$(cd "$(dirname "$0")" && pwd) CELL=$(pwd)/tools/nodecode-selffix
pass() { P=$((P + 1)); echo "pass $*"; }
fail() { echo "FAIL $*"; }
[ -f "$CELL/nodecode-selffix.asd" ] && [ -f "$CELL/repro.lisp" ] || { echo "no selffix cell in $COPY"; echo 0/$T; exit 1; }
NC=$(readlink -f "${NC_BUILD:-$(command -v nodecode)}")
[ -x "$NC" ] || { echo "no nodecode build at $NC"; echo 0/$T; exit 1; }
R=$(mktemp -d "${TMPDIR:-/tmp}/selffix-spec.XXXXXX"); PIDS=()
cleanup() { for p in "${PIDS[@]}"; do kill "$p" 2>/dev/null; done; [ -n "${KEEP:-}" ] || rm -rf "$R"; }
trap cleanup EXIT
S=$R/home
mkdir -p "$S/cells" "$R/bin" "$R/cache" "$R/work"
ln -s "$NC" "$R/bin/nodecode"
ln -s "$COPY/tools/nodecode-cron" "$S/cells/nodecode-cron"
ln -s "$CELL" "$S/cells/nodecode-selffix"
cp -r "$SPEC/plant" "$S/cells/selffix-plant"
SP=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1])')
cat > "$S/config.jsonc" <<EOF
{ "providers": { "stub": { "sdk": "openai-completions", "base_url": "http://127.0.0.1:$SP/v1", "models": ["stub"] } },
  "general": { "default_model": { "provider": "stub", "model": "stub" } },
  "selffix": { "idle_minutes": 30 } }
EOF
echo '[]' > "$R/script.json"
python3 "$SPEC/stub.py" "$SP" "$R/script.json" 2>"$R/stub.log" & PIDS+=($!)
export PATH="$R/bin:$PATH" NODECODE_HOME="$S" XDG_CACHE_HOME="$R/cache"
cd "$R/work"
drive() { timeout 400 python3 "$SPEC/drive.py" nodecode "$1" 300; }
jget() { python3 -c 'import json,sys; o=json.loads(sys.argv[1]); v=o
for k in sys.argv[2:]: v=v.get(k) if isinstance(v,dict) else None
print("" if v is None else v)' "$@"; }

# 1. a turn an outside provider fails (HTTP 402) lands too
echo '[{"role":"assistant","content":"","stub_status":402}]' > "$R/script.json"
two=$(drive "SELFFIX-OUTSIDE: summarize the inbox")
echo '[]' > "$R/script.json"
if [ "$(jget "$two" end)" = turn.failed ]; then pass "a turn the provider fails: $(jget "$two" condition_type)"
else fail "the outside turn did not fail: $two"; fi

# 2. a real turn fails on the planted defect and lands in the store, newest, so it ranks first among equals
one=$(drive "SELFFIX-PLANT: tidy the release notes")
if [ "$(jget "$one" condition_type)" = UNDEFINED-FUNCTION ] && jget "$one" detail | grep -q RULES-SECTION-GONE; then
  pass "a real turn fails on the planted defect: $(jget "$one" detail)"
else fail "the planted turn did not fail as planted: $one"; fi

# 3-7. a scripted turn runs the cell's backlog, cycle and ledger in the organism
FORM='(let ((nodecode-selffix::*busy* (constantly nil)) (items (nodecode-selffix:backlog)))
  (multiple-value-bind (status detail) (nodecode-selffix::cycle :idle-seconds 0)
    (multiple-value-bind (again again-detail) (nodecode-selffix::cycle :idle-seconds 0)
      (format nil "SPEC top=~a | status=~a | detail=~a | again=~a ~a | findings=~a | sha=~a | items=~d"
              (getf (first items) :key) status detail again again-detail
              (namestring (nodecode-selffix::finding-dir "")) 
              (mapcar (lambda (r) (nodecode-selffix::row r "sha256")) (nodecode-selffix::findings (nodecode-selffix::ledger-rows)))
              (length items)))))'
python3 - "$R/script.json" "$FORM" <<'PY'
import json, sys
json.dump([{'role': 'assistant', 'content': '', 'tool_calls': [{'id': 'call_spec_1', 'type': 'function',
           'function': {'name': 'eval', 'arguments': json.dumps({'form': sys.argv[2]})}}]}], open(sys.argv[1], 'w'))
PY
three=$(drive "run the selffix spec form")
echo '[]' > "$R/script.json"
spec=$(jget "$three" tool_results | tr '\n' ' ')
echo "organism: ${spec:0:900}"
case "$spec" in *"SPEC top=turn-failed UNDEFINED-FUNCTION"*) pass "the backlog ranks the planted failure first";;
  *) fail "the backlog did not rank the planted failure first";; esac
case "$spec" in *"status=FINDING"*) pass "a cycle turns it into a finding";; *) fail "no finding from the cycle";; esac
case "$spec" in *"again=NOT-REPRODUCED"*|*"again=NOTHING"*) pass "the next cycle does not try the found item again (${spec#*again=})";;
  *) fail "the next cycle did not move past the finding";; esac
TEST=$(ls "$S"/selffix/findings/*/repro.sh 2>/dev/null | head -1)
if [ -n "$TEST" ] && [ -f "$(dirname "$TEST")/finding.md" ]; then pass "the finding is written with its test: ${TEST#$S/}"
else fail "no findings/<fp>/repro.sh with finding.md under $S/selffix"; TEST=; fi
SHA=$( [ -n "$TEST" ] && sha256sum "$TEST" | cut -c1-64)
if [ -n "$SHA" ] && grep -q "\"sha256\":\"$SHA\"" "$S/selffix/ledger.jsonl" 2>/dev/null; then pass "the ledger locks the test by its sha256 ${SHA:0:12}"
else fail "the ledger does not hold the test's sha256 ${SHA:-none}"; fi

[ -n "$TEST" ] || { echo "$P/$T"; exit 0; }
# 8-10. the locked test on its own, with a key planted in the environment
store_before=$(sha256sum "$S"/history.db* 2>/dev/null | sha256sum)
out=$(OPENAI_API_KEY=sk-planted-spec ANTHROPIC_API_KEY=sk-planted-spec timeout 900 sh "$TEST" 2>&1); code=$?
echo "test: $(echo "$out" | tr '\n' ' ' | cut -c1-600)"
if [ $code = 1 ] && echo "$out" | grep -q '^REPRODUCED'; then pass "the locked test fails while the defect is there (exit 1)"
else fail "the locked test did not fail as recorded (exit $code)"; fi
if echo "$out" | grep -q 'keys among them: none; network: unreachable; the real home: hidden'; then pass "it ran sealed: no keys (two were planted), no network, the real home hidden"
else fail "the test did not report a sealed run"; fi
store_after=$(sha256sum "$S"/history.db* 2>/dev/null | sha256sum)
left=$(find "${TMPDIR:-/tmp}" -maxdepth 1 -type d -name 'selffix-test.*' -newer "$R/script.json" 2>/dev/null | wc -l)
if [ "$store_before" = "$store_after" ] && [ "$left" = 0 ]; then pass "the home's store is untouched and no throwaway home is left behind"
else fail "the test touched the home's store or left $left throwaway home(s)"; fi

# 11. the fix: the defect gone, the same file passes
rm -rf "$S/cells/selffix-plant"
out=$(timeout 900 sh "$TEST" 2>&1); code=$?
if [ $code = 0 ] && [ "$(sha256sum "$TEST" | cut -c1-64)" = "$SHA" ] && echo "$out" | grep -q 'NOT REPRODUCED'; then
  pass "with the defect removed the same file, unchanged, passes (exit 0)"
else fail "the unchanged test did not pass once the defect was gone (exit $code): $(echo "$out" | tail -2 | tr '\n' ' ')"; fi

# 12. a test that cannot seal refuses rather than running open
out=$(PATH=/nonexistent NODECODE_BIN=/nonexistent/nodecode /bin/sh "$TEST" 2>&1); code=$?
if [ $code = 2 ]; then pass "a test with no build says it could not run (exit 2), never a pass"
else fail "a test with no build answered exit $code"; fi

# 13. the cell's own tests
SRC=${NODECODE_SRC:-}
for d in "$HOME/nodecode/nodecode" "$HOME/Lab/nodecode/nodecode-public"; do [ -z "$SRC" ] && [ -d "$d" ] && SRC=$d; done
cd "$COPY"
tests=$(NODECODE_SRC=$SRC bash .github/test-cell.sh tools/nodecode-selffix 2>&1 | tail -1)
case "$tests" in *" 0 failed"*) pass "the cell's tests: $tests";; *) fail "the cell's tests: $tests";; esac
echo "$P/$T"
