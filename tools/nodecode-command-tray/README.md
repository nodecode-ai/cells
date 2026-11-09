# nodecode-command-tray

Command Tray: the shell commands this agent ran, with their exit codes, as two tools and `/commands`. A Nodecode rewrite of [aydnOktay/hermes-command-tray](https://github.com/aydnOktay/hermes-command-tray) by [aydnOktay](https://github.com/aydnOktay).

Every `(sh ...)` the agent runs through eval lands in the tray as it finishes:
the command, `ok` or `fail` with the exit code, the directory it ran in, the
last non-empty line of its output, when it last ran and how many times. A
command that runs twice is one row with a run count, the list keeps the newest
40, and token-shaped strings are redacted before anything is written.

It runs nothing itself, calls no service and needs no key. The rows are a JSON
file under the home (`command-tray/state.json`), so a restart does not lose
them.

## Get it

Tell the agent "add the command-tray cell", or run `nodecode add nc://command-tray`.

## Use it

Ask the agent what it ran, and it calls `command_tray_list`:

```
1. pytest -q — ok (0) ×2
   3 passed
   in /work/project
2. make deploy — fail (2)
   ERROR: no such target
```

- `command_tray_list` — the rows; `markdown: false` answers the same rows as
  JSON instead.
- `command_tray_clear` — empty the tray, for a fresh list on a new topic.
- `/commands` in any shell — the same list; `/commands clear` empties it.
- `(help :command-tray)` — the manual the model reads.
- `(command-tray:runs)` and `(command-tray:clear)` — the same two verbs
  through eval, for a layer or a snippet.

## Settings

Nothing is required; the tray fills itself. In `/settings`, the page's
Settings tab, or the `command-tray` section of `config.jsonc`:

```jsonc
"command-tray": { "max_items": 40, "preview_chars": 160, "redact": true }
```

`max_items` bounds the list, `preview_chars` how much of an output line a row
keeps (0 for none), and `redact` masks token-shaped strings — GitHub and
OpenAI-shaped keys, AWS key ids, `key = value` assignments and bearer headers
— in both the command and the kept output line.

## How it is wired

One piece of advice around `NODECODE.EVOLVED:SH`: the command runs through the
chain first, its own answer comes back untouched, and the row is recorded from
what the advice saw. A tray that cannot write its file never fails the command
it was watching. The rows are written atomically — a temporary file and one
rename — and read once at start; a file that is missing, or is not the shape
this cell writes, is an empty tray and never a refused start.

Every row assertion in the tests runs against a stubbed shell, so the suite
spawns nothing and dials nothing: run
`.github/test-cell.sh tools/nodecode-command-tray` from the cells repository.
