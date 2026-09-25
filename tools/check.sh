#!/usr/bin/env bash
# check.sh --- install and load every entry of index.json against a Nodecode
# release, the way an operator's /setup would, in a scratch home.
#
# usage: tools/check.sh [NODECODE]    (default: nodecode on PATH)
#
# The release's `trial' verb evaluates tools/check.lisp in a fresh process of
# a scratch home: nothing here reads or writes the operator's own home, and
# the update poller is off. Exit 0 when every entry installs and loads.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
binary="${1:-$(command -v nodecode || true)}"
test -x "$binary" || { echo "check: no nodecode binary (pass its path)" >&2; exit 2; }

home="$(mktemp -d)"
trap 'rm -rf "$home"' EXIT
mkdir -p "$home/.nodecode/addons"
printf '{ "update": { "mode": "off" } }\n' > "$home/.nodecode/config.jsonc"

score="$(HOME="$home" NODECODE_HOME="$home/.nodecode" \
  XDG_CACHE_HOME="$home/.cache" XDG_DATA_HOME="$home/.local/share" \
  XDG_CONFIG_HOME="$home/.config" \
  HUB_CHECK_INDEX="$here/index.json" \
  HUB_CHECK_ADDONS="$home/.nodecode/addons/" \
  HUB_CHECK_REPORT="$home/report.txt" \
  "$binary" trial < "$here/tools/check.lisp" | sed -n 's/^nodecode-trial-score //p' || true)"

if [ -s "$home/report.txt" ]; then
  sed 's/^/check: /' "$home/report.txt"
fi
if [ "$score" != 1 ]; then
  echo "check: refused (score '${score:-none}')" >&2
  exit 1
fi
echo "check: every entry installs and loads"
