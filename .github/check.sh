#!/usr/bin/env bash
# check.sh --- install and load every entry of index.json against a Nodecode
# release, the way an operator's /setup would, in a scratch home.
#
# usage: .github/check.sh [NODECODE]    (default: nodecode on PATH)
#
# The release's `trial' verb evaluates .github/check.lisp in a fresh process of
# a scratch home: nothing here reads or writes the operator's own home, and
# the update poller is off. Exit 0 when every entry installs and loads.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
binary="${1:-$(command -v nodecode || true)}"
test -x "$binary" || { echo "check: no nodecode binary (pass its path)" >&2; exit 2; }

home="$(mktemp -d)"
trap 'rm -rf "$home"' EXIT
mkdir -p "$home/.nodecode/cells"
printf '{ "update": { "mode": "off" } }\n' > "$home/.nodecode/config.jsonc"

# A release keeps the source path of every system its build registered and
# did not load, and counts such a system as there while that file is: on the
# host that built the release, and on no other machine. There this check
# would pass an entry that names one, which every user's machine refuses. So
# on a host that holds a build tree -- NODECODE_BUILD_HOME, CI's
# /var/tmp/nodecode-ci-build, a local build's /var/tmp/nodecode-build -- the
# trial runs with each under an empty tmpfs, in a user and mount namespace of
# its own: same user, nothing outside it touched, and the check judges what a
# user's machine gets. A host that holds one and cannot hide it is refused.
trial=("$binary" trial)
mounts=""
for tree in ${NODECODE_BUILD_HOME:+"$NODECODE_BUILD_HOME"} /var/tmp/nodecode-ci-build /var/tmp/nodecode-build; do
  if [ -d "$tree" ]; then
    mounts+="mount -t tmpfs nodecode-build-hidden $(printf %q "$tree") || exit 1; "
  fi
done
if [ -n "$mounts" ]; then
  hide=(unshare --user --map-current-user --mount --keep-caps bash -c)
  if ! "${hide[@]}" "$mounts" >/dev/null 2>&1; then
    echo "check: this host holds a release's build tree, where a release finds systems no user's machine has, and cannot hide it (unshare: user and mount namespaces); nothing is judged" >&2
    exit 2
  fi
  trial=("${hide[@]}" "${mounts}exec \"\$@\"" hidden "${trial[@]}")
fi

score="$(HOME="$home" NODECODE_HOME="$home/.nodecode" \
  XDG_CACHE_HOME="$home/.cache" XDG_DATA_HOME="$home/.local/share" \
  XDG_CONFIG_HOME="$home/.config" \
  HUB_CHECK_INDEX="$here/index.json" \
  HUB_CHECK_CELLS="$home/.nodecode/cells/" \
  HUB_CHECK_REPORT="$home/report.txt" \
  "${trial[@]}" < "$here/.github/check.lisp" | sed -n 's/^nodecode-trial-score //p' || true)"

if [ -s "$home/report.txt" ]; then
  sed 's/^/check: /' "$home/report.txt"
fi
if [ "$score" != 1 ]; then
  echo "check: refused (score '${score:-none}')" >&2
  exit 1
fi
echo "check: every entry installs and loads"
