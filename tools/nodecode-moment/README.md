# nodecode-moment

Moment: reports when a file the session stands on changed under it, and hands the model the new text. A Nodecode rewrite of [noprijbi99-dotcom/moment](https://github.com/noprijbi99-dotcom/moment) by [noprijbi99-dotcom](https://github.com/noprijbi99-dotcom).

A conversation pins the prompt it opened with — `SYSTEM.md`, the rules in
`AGENTS.md`, its standing sections — and the model quotes back whatever it read
earlier in the session. When a file behind that changes mid-session — you
editing a note, a memory written from another surface, rules rewritten in
another shell — the copy in the conversation is stale, and nothing names the
file or the lines that moved. Moment fingerprints the files a session stands on
at its first round, re-checks them every round after, and when one moved it
puts a receipt in that round's request: which file, when the baseline was
taken, when it was detected, the lines that changed, and the text as it stands
now, with one line telling the model to answer from the new state and say
plainly that the file drifted. Whatever `moment.watch` adds is checked the same
way — including a folder of memory definitions, `cells/<name>-memory/`, which
is one file per memory and which the harness reads through its index rather
than as files.

## Get it

Tell the agent "add the moment cell", or run `nodecode add nc://moment`.

## Set it up

Set `moment.state_file`, the JSON file moment keeps its session baselines in,
in `/settings` (or the `moment` section of `~/.nodecode/config.jsonc`):
`moment/state.json` under the home is the usual answer. Until it is set, the
tool and `/moment` refuse and say so.

Three optional knobs sit beside it: `watch` adds the files, or folders of
`.lisp` definitions, whose drift is reported on top of the home's three house
files (`cells/<name>-memory/` is the usual one, since a memory there is one
file); `max_chars` (2000) caps how much of a changed file one receipt
carries; `keep_sessions` (50) and `keep_days` (30) bound the state file.

## Use it

Nothing to type. Edit `SOUL.md` in another window while a session is running,
and its next turn opens with:

```
[moment] DRIFT RECEIPT — SOUL.md changed mid-session.
Path: ~/.nodecode/SOUL.md
Session baseline: 2026-05-04T09:12:03Z · Detected: 2026-05-04T09:40:51Z
Changed lines (session start -> now):
  +You prefer plain words over jargon.
Current content of SOUL.md:
...
Instruction: prefer the state above over the copy you read earlier in this session ...
```

Each change is reported once: the baseline then advances to the new bytes, so a
second change is a second receipt, and a session where nothing changed hears
nothing at all. Ask `moment_check` (or type `/moment`) at any time for the
report: per-file status against the session's start — unchanged, changed,
created, deleted, or no baseline — the lines that changed, each file's current
fingerprint, and the receipts recorded this session. `/moment reset` takes the
baseline again from the files as they are now. `(help :moment)` has the same
instructions for the model.

It does not rewrite the pinned prompt — the harness composes that at each
request, and a prompt cache is worth more than a rewrite; moment tells the model
alongside it, in the conversation, with the file named. It judges files, not
what the harness composes from them.

The report is per session, and a baseline is taken by the session's own first
round, so a session started before the cell was on has none (it says so, and
`/moment reset` gives it one). Baselines older than `keep_days` are dropped and
at most `keep_sessions` are kept, so the state file stays bounded on a
long-lived gateway.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: fingerprints, the receipt, the `:round` hook, the `moment_check` tool, `/moment`, the moment section. |
| `test/cell-test.lisp` | The tests: lifecycle, the tool and the command, a change reported once through a stubbed turn, a file that appears and one that goes, and the refusal without `state_file`. |
| `config.example.jsonc` | The section, commented. |
| `LICENSE` | The original's MIT licence. |

## Credits

noprijbi99-dotcom's [moment](https://github.com/noprijbi99-dotcom/moment) at
`1af58dd`, MIT, copyright (c) 2026 noprijbi99-dotcom. See [NOTICE](NOTICE) for
what the rewrite keeps and what it leaves out.
