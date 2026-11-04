#!/usr/bin/env bash
# usage: test/e2e.sh   (needs google-chrome, node 22+, and the network for nc://web)
#   NODECODE  the release to run (default: nodecode on PATH)
#   SHOTS     a folder to keep the two screenshots in (optional)
#
# e2e.sh --- the terminal cell the way an operator meets it: a real nodecode
# release on a scratch home, the cell installed as a folder, /terminal/ opened
# in a real (headless) Chrome that types into it. One line per check, then
# "score N/M". Nothing reads or writes the operator's own home; the link's
# token is never printed.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cell=${CELL:-$(cd "$here/.." && pwd)}
binary=$(readlink -f "${NODECODE:-$(command -v nodecode)}")
work=$(mktemp -d /data/scratch/terminal-e2e.XXXX)
shots=${SHOTS:-$work/shots}
mkdir -p "$shots"
pass=0; total=0; spid=
check() { total=$((total+1)); if [ "$2" = ok ]; then pass=$((pass+1)); echo "ok   $1"; else echo "FAIL $1${3:+ ($3)}"; fi; }
cleanup() { [ -n "$spid" ] && kill "$spid" 2>/dev/null && wait "$spid" 2>/dev/null; rm -rf "$work"; }
trap cleanup EXIT
export HOME=$work/home NODECODE_HOME=$work/home/.nodecode SHELL=/bin/bash
export XDG_CACHE_HOME=$work/home/.cache XDG_DATA_HOME=$work/home/.local/share XDG_CONFIG_HOME=$work/home/.config
mkdir -p "$NODECODE_HOME/cells" "$HOME/project"
printf '{ "update": { "mode": "off" } }\n' > "$NODECODE_HOME/config.jsonc"
printf 'PS1="\\u@e2e:\\w$ "\n' > "$HOME/.bash_profile"
if "$binary" add nc://web --yes >"$work/add.log" 2>&1; then check "the web cell installs" ok; else check "the web cell installs" no "$(tail -1 "$work/add.log")"; fi
cp -r "$cell" "$NODECODE_HOME/cells/nodecode-terminal"
rm -rf "$NODECODE_HOME/cells/nodecode-terminal/test"
"$binary" serve >"$work/serve.log" 2>&1 & spid=$!
link=
for _ in $(seq 1 120); do
  link=$("$binary" web --print 2>/dev/null | grep -o 'http://127.0.0.1:[0-9]*/web/#t=[^ ]*' | head -1)
  [ -n "$link" ] && break; sleep 0.5
done
[ -n "$link" ] && check "the organism serves its page" ok || { check "the organism serves its page" no; tail -5 "$work/serve.log"; echo "score $pass/$total"; exit 0; }
port=${link#http://127.0.0.1:}; port=${port%%/*}; token=${link#*#t=}
code=$(curl -s -o /dev/null -w '%{http_code}' "http://127.0.0.1:$port/terminal/")
[ "$code" = 200 ] && check "/terminal/ is served by the installed cell" ok || check "/terminal/ is served by the installed cell" no "HTTP $code"
PROFILE=$work/chrome PORT=$port TOKEN=$token CWD=$HOME/project SHOTS=$shots node "$here/drive.mjs" 2>&1 | while IFS= read -r line; do
  echo "$line"
done > "$work/drive.out"
grep -E '^(ok|FAIL) ' "$work/drive.out" | while IFS= read -r line; do echo "$line"; done
n=$(grep -c '^ok ' "$work/drive.out"); f=$(grep -c '^FAIL ' "$work/drive.out")
pass=$((pass+n)); total=$((total+n+f))
grep -vE '^(ok|FAIL) ' "$work/drive.out" | tail -5
opid=$(ss -ltnpH "sport = :$port" | grep -o 'pid=[0-9]*' | head -1 | cut -d= -f2)
shells=$(pgrep -P "$opid" -c bash 2>/dev/null || echo 0)
[ "$shells" -ge 1 ] && check "the shell is a child of the organism itself (the page's reload did not end it)" ok || check "the shell is a child of the organism itself" no "$shells"
echo "score $pass/$total"
