# nodecode-lightkeeper

Lightkeeper: a map of how your codebase works, kept in a vault of markdown notes outside the repo and handed to the agent as it works. A Nodecode rewrite of [Light5566/claude-lightkeeper](https://github.com/Light5566/claude-lightkeeper) by [Light5566](https://github.com/Light5566).

The vault is an ordinary folder of Obsidian-compatible notes: one note per module, flow, integration, platform trap, decision or release. A note says what the area is, `Where it lives` (the files and folders it owns), `How it works`, `Gotchas`, and `Connects to` — every link there carries its reason, and that list is the blast radius of a change. The notes live outside the repo, so reading them costs nothing in the checkout and a code review never sees them.

## Get it

Tell the agent "add the lightkeeper cell", or run `nodecode add nc://lightkeeper`.

## Set it up

The cell reads one folder, so it needs one setting. Point it at the vault:

```
"lightkeeper": { "vault": "/home/you/notes/my-project", "repo": "/home/you/code/my-project" }
```

- `vault` is the folder of notes, and it is required: the cell refuses to start without it.
- `repo` is the checkout the vault covers. Set it and the primer stays silent in a session working somewhere else; leave it out and every session hears it. With it set, `/lightkeeper` also names the notes that claim a path the checkout no longer has.
- `max_notes` (default 5) bounds one find.

No key, no network, no service: the notes are files.

## Use it

Three tools, and two hooks that need no asking.

| Call | What it answers |
| --- | --- |
| `lightkeeper_find` `{"query": "books chapters"}` | The notes a topic names, best first: name and tags first, then the note's own lines. Each hit prints its one line, where the note is, and its linked neighbours with the reason for each link. |
| `lightkeeper_find` `{"paths": ["app/Models/Book.php"]}` | The notes that own those files: an exact path first, then the longest folder a note claims, then a note that claims the same file name. A path nothing claims says so — that is the check for a gap in the map. |
| `lightkeeper_note` `{"note": "Books"}` | One note whole, or `{"section": "Gotchas"}` for one heading. |
| `lightkeeper_prime` | What the vault covers, how many notes, and how to query it. |

The hooks:

- **The primer** rides a session's first round: one line saying the vault is there, where, how many notes, and how to query it. It is in the notices the model reads, not in the transcript.
- **The pointer** lands on the result of a call that wrote files: the note that owns each written file, once per session per note, with what to do about it. It can never block a write — a hook that fails answers with the result it was given.

`/lightkeeper` answers the vault's state: how many notes, which are still stubs, and (with `repo` set) which claimed paths are gone. `/lightkeeper find WORDS` and `/lightkeeper note NAME [SECTION]` are the same two queries with no JSON.

## What it does not do

The original also runs `vault check`, `tidy`, `sync` and the git-hook/release machinery that keeps a vault honest and catches it up against a production branch. This cell reads and points; it writes no note and runs no git. See `NOTICE`.

## License

MIT. The original is Copyright (c) 2026 Light5566: see `LICENSE` and `NOTICE`.
