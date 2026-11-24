# nodecode-trove

Trove: lossless recall over every session's log - search, read and cite every message. A Nodecode rewrite of [misterdas/hermes-trove](https://github.com/misterdas/hermes-trove) by [misterdas](https://github.com/misterdas).

Nodecode keeps every input, answer, tool call and result in a durable event
log, and eviction only moves a visibility floor: the window forgets, the log
does not. Trove is the door onto that log. Each row it answers cites
`[session seq]`, and the seq is a log position, so a row read now names the
same entry after the window has moved on.

## Get it

Tell the agent "add the trove cell", or run `nodecode add nc://trove`.

## Use it

Nothing has to be set up: the cell reads the log the image already writes.

| Ask for | What happens |
| --- | --- |
| "where did we deploy the cells?" | `trove_search` greps every session at once, newest first, each row cited. |
| "read that session" | `trove_read` walks one session in order, oldest first, and pages older with `before=`. |
| "read the entry at 812" | `trove_entry` returns that entry's body whole. |
| "which sessions are there?" | `trove_sessions` lists id, message count, last activity, title and folder. |
| "put what we know about X together" | `trove_evidence` compiles the matching rows into one cited digest under a character budget. |
| "what does trove see?" | `trove_status` says how many sessions and events the log holds, and where it is. |

From the operator's side, `/trove` typed alone opens a panel (a search row and
one row per session), `/trove QUERY` searches, `/trove read SESSION` reads
one, and `/trove status` answers the status line.

The section `trove` in `~/.nodecode/config.jsonc` bounds how much one answer
shows; every member has a default:

```json
{ "trove": { "limit": 20, "clip": 200, "budget": 4000, "sessions": 12 } }
```

A row's body is clipped to `clip` characters and a search answers at most
`limit` rows, because an answer prints back into the very window it was
recalled from; `trove_entry` is the read that returns a body whole.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The six tools, `/trove`, the section and the manual topic. |
| `test/cell-test.lisp` | The slice `just trove-test` runs: lifecycle, every tool over a private log, the panel. |

## License

MIT, Copyright (c) 2026 Stephen Schoettler. See `LICENSE` and `NOTICE`.
