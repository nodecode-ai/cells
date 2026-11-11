#!/usr/bin/env bash
# check-test.sh --- check.sh on a host that holds a release's build tree
# judges what a user's machine gets.
#
# usage: .github/check-test.sh
#
# A release counts a system it registered and did not load as there while
# that system's file is, and the file exists only on the host that built the
# release. The stand-in release here does the same with one file under
# NODECODE_BUILD_HOME: it passes when it can see the file, as the real one
# passes on the build host, and refuses when it cannot, as every user's
# machine does. check.sh must refuse with that file present, run a release
# that needs nothing from the tree as before (same user, same home, same
# index), and leave the tree outside as it was. Needs unprivileged user
# namespaces, as check.sh on such a host does.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/build" "$work/bin"
touch "$work/build/ironclad.asd"
OUTER_UID="$(id -u)"
export NODECODE_BUILD_HOME="$work/build" OUTER_UID

# The stand-in: `trial' reads check.lisp on stdin and prints its score.
cat > "$work/bin/needs-the-tree" <<'EOF'
#!/usr/bin/env bash
cat > /dev/null
if [ -e "$NODECODE_BUILD_HOME/ironclad.asd" ]; then
  echo "ok nodecode-stand-in at 0" > "$HUB_CHECK_REPORT"; echo "nodecode-trial-score 1"
else
  echo "FAIL nodecode-stand-in: nodecode-stand-in needs ironclad, which this image cannot find" > "$HUB_CHECK_REPORT"
  echo "nodecode-trial-score 0"
fi
EOF
cat > "$work/bin/needs-nothing" <<'EOF'
#!/usr/bin/env bash
cat > "$HOME/check.lisp"
test -s "$HOME/check.lisp" && test "$(id -u)" = "$OUTER_UID" && test -r "$HUB_CHECK_INDEX" \
  && test -d "$HUB_CHECK_CELLS" && [ "$NODECODE_HOME" = "$HOME/.nodecode" ] \
  || { echo "nodecode-trial-score 0"; exit 0; }
echo "ok nodecode-stand-in at 0" > "$HUB_CHECK_REPORT"; echo "nodecode-trial-score 1"
EOF
chmod +x "$work/bin/"*

failed=0
expect() {  # expect WHAT RC PATTERN -- COMMAND...
  local what="$1" want="$2" pattern="$3" out rc=0; shift 4
  out="$("$@" 2>&1)" || rc=$?
  if [ "$rc" = "$want" ] && grep -q -- "$pattern" <<<"$out"; then
    echo "ok   $what"
  else
    echo "FAIL $what (exit $rc, wanted $want and /$pattern/):"; failed=1
    while IFS= read -r line; do echo "     $line"; done <<<"$out"
  fi
}

expect "an entry only the build tree satisfies is refused" 1 "FAIL nodecode-stand-in: .* cannot find" \
  -- "$here/.github/check.sh" "$work/bin/needs-the-tree"
expect "an entry that needs nothing of it installs, as the same user" 0 "every entry installs and loads" \
  -- "$here/.github/check.sh" "$work/bin/needs-nothing"
if [ -e "$work/build/ironclad.asd" ]; then echo "ok   the tree outside is as it was"; else echo "FAIL the tree outside lost its file"; failed=1; fi
exit "$failed"
