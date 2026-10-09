#!/usr/bin/env bash
# test-cell.sh --- one cell's test slice against a pristine Nodecode tree.
#
# usage: .github/test-cell.sh FOLDER [SLOT]
#   FOLDER  a cell folder of this repository, e.g. tools/nodecode-guard
#   SLOT    a name for the scratch home this run uses (default: the cell's
#           name); two runs at once must use two slots
#
# The tree is `git archive' of a Nodecode checkout (~/nodecode/nodecode, or
# NODECODE_SRC) at the commit .github/nodecode-rev pins, unpacked once under
# ~/.cache/nodecode-cells/, so neither a peer's edits in that checkout nor a
# new commit there moves the tree under runs already going. Every cell folder
# here is on the ASDF registry, so a cell's test that needs another cell (the
# rooms need nodecode-channel-kit) finds it in this repository, and the slice
# runs through the cell's own test-op. The run's HOME, XDG_CACHE_HOME and
# NODECODE_HOME are a scratch home of its own, with quicklisp, .sbclrc and
# the shared fasl cache linked in; the environment is emptied of provider
# keys. Nothing here reads or writes ~/.nodecode.
#
# The run has no network: it executes in a user and network namespace of its
# own (unshare -rn) whose only interface is loopback, so a test may bind a
# localhost callback, and a test that forgot a stub fails instead of dialling
# a provider.
set -euo pipefail
here=$(cd "$(dirname "$0")/.." && pwd)
cell=$(cd "$here/${1:?usage: .github/test-cell.sh FOLDER [SLOT]}" && pwd)
name=$(basename "$cell")
slot=${2:-$name}
src=${NODECODE_SRC:-$HOME/nodecode/nodecode}
rev=$(git -C "$src" rev-parse --short=12 "$(cat "$here/.github/nodecode-rev")")
root=$HOME/.cache/nodecode-cells
tree=$root/tree-$rev
if [ ! -d "$tree/src" ]; then
  mkdir -p "$root"
  tmp=$(mktemp -d "$root/tree-$rev.XXXX")
  git -C "$src" archive "$rev" src resources | tar -x -C "$tmp"
  mv -T "$tmp" "$tree" 2>/dev/null || rm -rf "$tmp"
fi
home=$root/homes/$slot
mkdir -p "$home/.cache" "$HOME/.cache/common-lisp"
ln -sfn "$HOME/quicklisp" "$home/quicklisp"
ln -sfn "$HOME/.sbclrc" "$home/.sbclrc"
ln -sfn "$HOME/.cache/common-lisp" "$home/.cache/common-lisp"
registry=""
for folder in "$here"/*/nodecode-*/ "$cell/"; do registry="$registry #p\"$folder\""; done
cd "$tree/src"
exec unshare -rn sh -c 'ip link set lo up && exec "$@"' sh \
  env -i PATH="$PATH" TERM="${TERM:-dumb}" LANG=C.UTF-8 \
  HOME="$home" XDG_CACHE_HOME="$home/.cache" NODECODE_HOME="$home/.nodecode" \
  sbcl --dynamic-space-size 4096 --control-stack-size 8MB --noinform --non-interactive \
    --eval '(require :asdf)' \
    --eval '(push (truename ".") asdf:*central-registry*)' \
    --eval "(dolist (folder (list$registry)) (push folder asdf:*central-registry*))" \
    --eval "(asdf:test-system \"$name\")"
