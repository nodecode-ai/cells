# nodecode-aside

Aside Browser: drives the user's signed-in Aside browser through the local `aside` CLI. A Nodecode rewrite of [leonphull/hermes-aside-browser](https://github.com/leonphull/hermes-aside-browser) by [leonphull](https://github.com/leonphull).

[Aside](https://aside.com) is a browser that keeps the user's real signed-in
sessions and talks to the machine through one command-line program. This cell
runs that program: the model asks it for a task in the browser, the browser
does it in the profile the user is already logged in to, and the answer comes
back as text. Nothing of the user's cookies passes through Nodecode, and this
cell reads no cookie jar, no password store and no Aside model file.

The wrapper adds what a raw command line does not have. The operator's pins —
host, account, model, effort, speed and permission — ride every call, and the
model cannot set a host, an account or a permission even if it sends one. A
session is never continued from here, because the cell cannot verify the
permission or the host it would run under -- a `session_id` on a call is
refused rather than quietly ignored; `archive` and `delete` need an explicit
confirmation. A long screenshot is named rather than pasted into the
turn. And with `browser_policy` set to `always`, the built-in browser's tools
are refused while Aside answers, so a logged-in site is not opened twice.

## Get it

Tell the agent "add the aside cell", or run `nodecode add nc://nodecode-aside`.

## Set it up

Aside must already be installed and running; this cell never downloads or
installs it. Install or update the CLI from Aside's own **Settings →
Developers**, then name it once. In `/settings` (or the page's Settings tab),
under `aside`:

- `bin` — required. The `aside` executable, for example `~/.local/bin/aside`,
  which is where the app usually puts it. Without it the folder is `REFUSED`
  with the reason on the `/cells` line and in the model's notices.
- `permission` — `ask` (the default), `guard` or `full-access`: what the CLI
  may do without asking. It is the operator's setting, never a tool argument,
  and it covers `aside_exec` only: `aside_repl` runs a script the model wrote,
  and the vendor's own MCP connector is outside this wrapper entirely.
- `browser_policy` — `prefer` (the default) lets the model choose; `always`
  refuses the built-in browser's tools while Aside answers. `blocked_tools`
  names what counts as the built-in browser; empty means `browser_*` and
  `web_fetch`.
- `host`, `account`, `model`, `effort`, `speed` — pins. Empty means Aside's
  own default, and `host: "local"` pins the local host, which is not
  guaranteed otherwise.
- `timeout_seconds` — one `aside_exec` task's ceiling; 300 by default.

If the executable is not where `bin` says, the cell also looks at `$ASIDE_BIN`,
then `PATH`, then `~/.local/bin`, `/usr/local/bin` and `/opt/homebrew/bin`.

## Use it

- `/aside` shows what the probe said and the pins behind it, with a row for
  each thing it does; `/aside status`, `/aside sessions`, `/aside exec …`,
  `/aside repl …` and `/aside stop ses_…` do them directly.
- `aside_status` — is the CLI there and does the app answer (READY,
  ASIDE_NOT_RUNNING, NEEDS_ASIDE), and which pins are in force.
- `aside_exec` — one task in the signed-in browser. The route for logged-in
  sites and anything that takes several steps.
- `aside_repl` — one one-shot Playwright-style JavaScript script, for a
  snapshot, a click or a screenshot. Bindings do not survive the call.
- `aside_session` — list, stop, archive, delete. Never resume: start a new
  `aside_exec` and restate the context.
- `aside_memory` — one to three read-only searches of Aside's memory.
- `(help :aside)` is the manual a model reads.

## What runs

One executable, an argument vector and no shell. The probe is one
`aside repl` that prints a marker; `aside_exec` is
`aside exec [--host …] [--account …] [--model …] [--effort …] [--speed …] --permission … <prompt>`;
`aside_memory` is `aside memory search [--account …] --json -n N <query>`. The
prompt is one argument, so a task that reads like a sentence stays one. A
question that starts with `-` is refused rather than read as a flag, and a
session id must start with `ses_`. Output is the CLI's own, with a long
base64 line named rather than carried and the whole cut at 48k.

## Layout

- `cell.lisp` — the section, the five tools, the gate, the command and the manual.
- `test/` — the slice: the lifecycle, each tool through a scripted CLI, the command, the gate.

`.github/test-cell.sh tools/nodecode-aside` runs it offline: no test needs
Aside installed, and none starts a process.
