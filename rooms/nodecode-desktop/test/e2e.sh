#!/usr/bin/env bash
# e2e.sh --- the desktop cell end to end, the way an operator meets it.
#
# usage: test/e2e.sh
#   NODECODE      the release to run (default: nodecode on PATH)
#   LISPTRON_DIR  lisptron's checkout (default: ~/Lab/nodecode/lisptron)
#   DESKTOP_E2E_SHOT  where to keep a copy of what /desktop look saw (optional)
#
# A real nodecode release serves a scratch home of its own on a hidden X
# screen: the web cell installed with `nodecode add', this cell and lisptron
# copied into its cells folder as an operator installs a folder, and
# /desktop run the way a shell in another process runs a cell's command --
# through the gateway's slash route, into the organism where the cell runs.
# Nothing reads or writes the operator's own home. The page's link carries
# the operator token; nothing here prints it. One line per check, then
# "score N/M" last.
set -uo pipefail
cell=$(cd "$(dirname "$0")/.." && pwd)
binary=$(readlink -f "${NODECODE:-$(command -v nodecode)}")
lisptron=${LISPTRON_DIR:-$HOME/Lab/nodecode/lisptron}
work=$(mktemp -d /tmp/nodecode-desktop-e2e.XXXX)
pass=0; total=0
xpid=; spid=

check() {  # NAME VERDICT [DETAIL]
  total=$((total+1))
  if [ "$2" = ok ]; then pass=$((pass+1)); echo "ok   $1"; else echo "FAIL $1${3:+ ($3)}"; fi
}
finish() { echo "score $pass/$total"; exit 0; }
cleanup() {
  [ -n "$spid" ] && kill "$spid" 2>/dev/null && wait "$spid" 2>/dev/null
  [ -n "$xpid" ] && kill "$xpid" 2>/dev/null
  rm -rf "$work"
}
trap cleanup EXIT

# --- a hidden screen, and a home of its own ------------------------------------
for n in $(seq 200 249); do [ -e /tmp/.X$n-lock ] || break; done
Xvfb :$n -screen 0 1600x1000x24 -nolisten tcp >/dev/null 2>&1 & xpid=$!
sleep 1
export HOME=$work/home NODECODE_HOME=$work/home/.nodecode
export XDG_CACHE_HOME=$work/home/.cache XDG_DATA_HOME=$work/home/.local/share XDG_CONFIG_HOME=$work/home/.config
# Xvfb has no DRI3, so WebKit's GPU path cannot start on it; a desktop session needs neither line.
export DISPLAY=:$n GDK_BACKEND=x11 WEBKIT_DISABLE_DMABUF_RENDERER=1
mkdir -p "$NODECODE_HOME/cells"
printf '{ "update": { "mode": "off" } }\n' > "$NODECODE_HOME/config.jsonc"

if "$binary" add nc://web --yes >"$work/add.log" 2>&1 && [ -d "$NODECODE_HOME/cells/nodecode-web" ]; then
  check "the web cell installs with nodecode add" ok
else
  check "the web cell installs with nodecode add" no "$(tail -1 "$work/add.log")"
fi
cp -r "$cell" "$NODECODE_HOME/cells/nodecode-desktop"
rm -rf "$NODECODE_HOME/cells/nodecode-desktop/test"
mkdir -p "$NODECODE_HOME/cells/lisptron"
(cd "$lisptron" && git ls-files -z lisptron.asd src LICENSE* | xargs -0 cp --parents -t "$NODECODE_HOME/cells/lisptron")

# --- the organism, headless, as a desktop session would launch it -------------
"$binary" serve >"$work/serve.log" 2>&1 & spid=$!
link=
for _ in $(seq 1 120); do
  link=$("$binary" web --print 2>/dev/null | grep -o 'http://127.0.0.1:[0-9]*/web/#t=[^ ]*' | head -1)
  [ -n "$link" ] && break
  sleep 0.5
done
if [ -z "$link" ]; then
  check "the organism serves this home's page" no "$("$binary" web --print 2>&1 | tail -1)"
  finish
fi
check "the organism serves this home's page" ok
port=${link#http://127.0.0.1:}; port=${port%%/*}
token=${link#*#t=}
opid=$(ss -ltnpH "sport = :$port" | grep -o 'pid=[0-9]*' | head -1 | cut -d= -f2)

slash() {  # LINE -> the command's answer text
  curl -s -m 60 -X POST "http://127.0.0.1:$port/api/gateway/slash" \
    -H "authorization: Bearer $token" -H 'content-type: application/json' \
    -d "$(jq -nc --arg l "$1" '{line:$l}')" | jq -r '.text // .error // .'
}
# Mapped ones only: GTK keeps an unmapped leader window of the same name.
windows() { xdotool search --onlyvisible --name '^Nodecode$' 2>/dev/null; }
webkit_loaded() { grep -q libwebkit2gtk "/proc/$opid/maps" 2>/dev/null; }

commands=$(curl -s -m 30 "http://127.0.0.1:$port/api/gateway/slash" -H "authorization: Bearer $token" | jq -r '.commands[].name' 2>/dev/null)
if grep -qx desktop <<<"$commands"; then
  check "the organism loaded the desktop cell: /desktop is one of its commands" ok
else
  check "the organism loaded the desktop cell: /desktop is one of its commands" no "$(grep -i 'desktop\|lisptron' "$work/serve.log" | tail -2)"
  finish
fi
if [ -n "$opid" ] && ! webkit_loaded; then
  check "no web engine in the organism before the first window" ok
else
  check "no web engine in the organism before the first window" no "pid ${opid:-unknown}"
fi

# --- /desktop ---------------------------------------------------------------------
answer=$(slash /desktop)
case $answer in *opened*) check "/desktop opens a window" ok ;; *) check "/desktop opens a window" no "$answer" ;; esac
status=
for _ in $(seq 1 60); do
  status=$(slash '/desktop status')
  case $status in *"gateway connected"*) break ;; esac
  sleep 0.5
done
case $status in
  *"page open"*"gateway connected"*) check "the page's own state reaches open over its /gateway socket" ok ;;
  *) check "the page's own state reaches open over its /gateway socket" no "$status" ;;
esac
wid=$(windows | head -1)
wpid=$( [ -n "$wid" ] && xprop -id "$wid" _NET_WM_PID | grep -o '[0-9]*$')
if [ -n "$wid" ] && [ "$wpid" = "$opid" ] && webkit_loaded; then
  check "the window is the organism's own: same process as its gateway" ok
else
  check "the window is the organism's own: same process as its gateway" no "window ${wid:-none}, its pid ${wpid:-?}, gateway pid ${opid:-?}"
fi
sleep 1
answer=$(slash "/desktop look $work/look.png")
colours=$( [ -s "$work/look.png" ] && identify -format '%k' "$work/look.png" 2>/dev/null)
[ -n "${DESKTOP_E2E_SHOT:-}" ] && [ -s "$work/look.png" ] && cp "$work/look.png" "$DESKTOP_E2E_SHOT"
if [ "${colours:-0}" -gt 50 ]; then
  check "/desktop look shows the agent the real UI" ok
else
  check "/desktop look shows the agent the real UI" no "${answer}; ${colours:-0} colours"
fi
answer=$(slash /desktop)
count=$(windows | wc -l)
case $answer in
  *"brought it forward"*) [ "$count" = 1 ] && check "a second /desktop brings the window forward, never a second one" ok \
                            || check "a second /desktop brings the window forward, never a second one" no "$count windows" ;;
  *) check "a second /desktop brings the window forward, never a second one" no "$answer" ;;
esac
answer=$(slash '/desktop close')
sleep 1
status=$(slash '/desktop status')
if [[ $answer == *closed* && $status == *"no window"* && -z $(windows) ]]; then
  check "/desktop close closes it" ok
else
  check "/desktop close closes it" no "$answer; $status"
fi
if kill -0 "$spid" 2>/dev/null && [ "$(curl -s -o /dev/null -w '%{http_code}' -m 10 -I "http://127.0.0.1:$port/web/")" = 200 ]; then
  check "the organism keeps serving after its window closes" ok
else
  check "the organism keeps serving after its window closes" no "$(tail -2 "$work/serve.log")"
fi
finish
