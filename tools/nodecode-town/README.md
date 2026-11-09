# nodecode-town

A pixel town of the running sessions: one resident per session, a building per tool, and a page to watch. A Nodecode rewrite of [sxuff/hermes-town](https://github.com/sxuff/hermes-town) by [sxuff](https://github.com/sxuff).

## Get it

Tell the agent "add the town cell", or run `nodecode add nc://town`.

## Set it up

It needs one thing: a salt, any word of your own. A resident is named by a digest of that word and its session id, so with the salt set the page never shows an id and another home's town cannot match its names against yours. Set it in `/settings` (the town section, `salt`), or ask the agent to set it.

Two more settings have defaults: `residents` (24, how many sessions the town holds; past that the quietest departs) and `journal` (128, how many events it keeps).

## Use it

Run a turn, then open `/town page`'s URL in a browser on the same machine, or click through from the gateway's own page. The town draws a building per place and a resident per session, walking that resident to the building the tool it just called is worked in — the library for a read or a search, the forge for `eval` and `sh`, the post office for a message, the observatory for the web, the town hall for a plan. A finished turn sits the resident down in the tavern.

- `/town` prints the town in text: each resident's pseudonym, role, place and calls, and the page's URL.
- `/town page` names the URL alone; `/town clear` empties it.
- `(town:snapshot)` is the JSON the page polls; `(town:journal)` and `(town:residents)` are the same facts in Lisp.

The cell keeps a pseudonym, a role, a place, a tool name and whether the call failed — never a prompt, a tool argument, a tool result or a session id — so the page can be left open on a screen someone else sees.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the two observers, the town in memory, `/town` and the snapshot |
| `page.lisp` | The served page, and its two routes |
| `test/` | The slice: lifecycle, the walks, the snapshot, the command, the routes |

## License

MIT. The original is Copyright (c) 2026 Hermes Town contributors: see `LICENSE` and `NOTICE`.
