#!/usr/bin/env bash
# score.sh --- the wake PR's shared scorer.
#
# usage, from the root of any nodecode-ai/cells working copy:
#   git fetch -q origin wake-scorer && git show REV:.github/wake-spec/score.sh | bash -s REV
# REV is the scorer commit; the spec is read from it, so the branch under
# test needs no copy. It runs .github/test-cell.sh's pristine-tree sandbox
# for tools/nodecode-cron with the last step swapped: load the cron tests,
# load wake-spec.lisp, run its WAKE-SPEC- slice. The last line is PASSED/TOTAL;
# a branch where the spec cannot load scores 0/TOTAL.
set -uo pipefail
rev=${1:?usage: score.sh SCORER-REV}
here=$PWD
work=$(mktemp -d /tmp/wake-score.XXXXXX)
trap 'rm -rf "$work"' EXIT
git show "$rev:.github/wake-spec/wake-spec.lisp" > "$work/wake-spec.lisp"
total=$(grep -c '^(deftest wake-spec-' "$work/wake-spec.lisp")
if [ -z "${NODECODE_SRC:-}" ]; then
  for candidate in "$HOME/nodecode/nodecode" "$HOME/Lab/nodecode/nodecode-public"; do
    if [ -d "$candidate/.git" ]; then NODECODE_SRC=$candidate; break; fi
  done
fi
export NODECODE_SRC
slot=wake-$(printf %s "$here" | md5sum | cut -c1-8)
{ sed "s|^here=.*|here=\"$here\"|" .github/test-cell.sh | grep -v 'asdf:test-system'
  cat <<EOF
    --eval '(asdf:load-system "nodecode-cron/test")' \\
    --eval '(dolist (s (list "WAKE" "WAKE-NEXT")) (export (intern s "CRON") "CRON"))' \\
    --eval '(load "$work/wake-spec.lisp")' \\
    --eval '(nodecode.test::run-wakespec-tests)'
EOF
} > "$work/run.sh"
bash "$work/run.sh" tools/nodecode-cron "$slot" > "$work/log" 2>&1
grep -E '^(FAIL|fail|  )' "$work/log" | head -60
line=$(grep -E '^[0-9]+ wakespec tests, [0-9]+ failed' "$work/log" | tail -1)
if [ -n "$line" ]; then
  ran=${line%% *}
  failed=$(printf %s "$line" | sed -E 's/.*, ([0-9]+) failed.*/\1/')
  echo "$((ran - failed))/$total"
else
  tail -15 "$work/log"
  echo "0/$total"
fi
