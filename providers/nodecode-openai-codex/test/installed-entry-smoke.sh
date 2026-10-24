#!/usr/bin/env bash
# Linux installed-binary smoke. No real auth/config, no network/provider calls.
# The caller may place this low-footprint probe in its normal resource controller.
set -euo pipefail
cell="$(cd "$(dirname "$0")/.." && pwd)"
bin="${NODECODE_BIN:-$(command -v nodecode)}"
bin="$(realpath "$bin")"
: "${NODECODE_PI_PACKAGE_DIR:?set the installed Pi coding-agent package directory}"
parent="${TMPDIR:-$HOME/.cache}"
mkdir -p "$parent"
home="$(mktemp -d "$parent/nodecode-codex-entry.XXXXXX")"
trap 'rm -rf -- "$home"' EXIT
mkdir -p "$home/nodecode/cells" "$home/pi"
ln -s "$cell" "$home/nodecode/cells/nodecode-openai-codex"
SMOKE_HOME="$home" node --input-type=module - <<'JS'
import fs from 'node:fs';
const home=process.env.SMOKE_HOME;
const auth=`${home}/pi/auth.json`;
const entry=(access,accountId)=>({type:'oauth',access,refresh:'synthetic-refresh',expires:4102444800000,accountId});
fs.writeFileSync(auth,JSON.stringify({'openai-codex':entry('synthetic-access-one','synthetic-one'),'openai-codex-2':entry('synthetic-access-two','synthetic-two')}),{mode:0o600});
// No provider entries or hand-written account aliases: native discovery owns them.
fs.writeFileSync(`${home}/nodecode/config.jsonc`,JSON.stringify({update:{mode:'off'},failover:{on:['rate_limit'],chain:[{provider:'openai-codex-2',model:'gpt-6.1-sol'}]}}));
JS
unshare -rn sh -c 'ip link set lo up && exec "$@"' sh \
  env -i PATH="$PATH" TERM=dumb LANG=C.UTF-8 HOME="$home" \
  NODECODE_HOME="$home/nodecode" XDG_CACHE_HOME="$home/cache" \
  XDG_CONFIG_HOME="$home/config" XDG_DATA_HOME="$home/data" NODECODE_EPHEMERAL=1 \
  PI_CODING_AGENT_DIR="$home/pi" NODECODE_PI_PACKAGE_DIR="$NODECODE_PI_PACKAGE_DIR" \
  "$bin" --pristine trial < "$cell/test/installed-entry-smoke.lisp" > "$home/result.log" 2>&1
cat "$home/result.log"
if ! grep -qx 'nodecode-trial-score 42' "$home/result.log"; then
  # Diagnostics contain only synthetic test material from this owned home.
  test ! -f "$home/accounts-error.txt" || cat "$home/accounts-error.txt"
  exit 1
fi
printf 'Installed-entry smoke passed: private pipe, two accounts, Responses, isolated state, 429 failover.\n'
