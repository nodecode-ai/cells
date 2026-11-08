# nodecode-deja

Deja Memory: recalls past coding sessions from every agent on this machine, so the model remembers earlier work. A Nodecode rewrite of [vshulcz/deja-vu](https://github.com/vshulcz/deja-vu) by [vshulcz](https://github.com/vshulcz).

The memory is the coding sessions this machine already wrote to disk — Claude Code, Codex, Cursor, Nodecode and the other agents deja reads, including the months before anything was installed. It is indexed locally by [deja](https://github.com/vshulcz/deja-vu), a single Go binary: no model, no API key, no server.

## Get it

Tell the agent "add the deja cell", or run `nodecode add nc://deja`.

## Set it up

The cell drives the `deja` program, so that has to be there first:

```sh
brew install deja-vu        # or: go install github.com/vshulcz/deja-vu/cmd/deja@latest
```

It is found on `PATH`. If it lives somewhere else, give its path in the cell's `deja` setting (`/settings`); a cell that cannot find it refuses to start and says so. The first search may build the index over the transcripts already on disk, which takes seconds; after that a search is milliseconds.

## Use it

The model gets the same three tools a Hermes user got:

| Tool | What it answers |
| --- | --- |
| `deja_recall` | What past sessions on this machine did with a term — 1-3 exact terms (a path, an error string, a flag), paged with `offset`. |
| `deja_fix` | For an error message, what was run after the same error last time, and whether it worked. |
| `deja_blame` | For a file, the sessions that edited it and what each concluded. |

And it arrives on its own, as it did in Hermes: at a session's first request, deja's digest of where the work in this project stood; at each turn, what the turn's own prompt matches. Most turns it adds nothing, which is the point — a memory that talks every turn is wallpaper. A cron run recalls nothing on its own; its job can still call the tools. What deja says is framed as reference data, not instructions: a line marked `User said (this project)` is shown and confirmed before it changes a command or a file.

You can look yourself:

```
/deja parser fix          the operator's own search
(deja:recall "make check")
(deja:fix "error: unknown flag -x")
(deja:blame "src/parser.lisp")
```

`(help :deja)` has the same guidance the model is given.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `deja` | `deja` | The program's name on `PATH`, or its path. |
| `auto` | on | The digest and the prompt match at each turn. Off leaves the tools and `/deja`. |
| `max_chars` | 6000 | The longest recall one turn carries, in characters. |

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The three verbs, the round hook that records the digest, the standing section, the tools and `/deja`. |
| `package.lisp` | The `deja` package: three verbs, the condition every refusal signals. |
| `test/` | Offline tests: the argv each tool builds, the round's reminder over a real store, the refusals, and one real run of a stand-in program. |

What the Hermes plugin got from its host and Nodecode has no seam for — a compression recovery packet, the session-end stamp, mirroring `MEMORY.md` entries into the index — is not here; deja rebuilds its index on its next refresh either way.

## License

MIT. The original is Copyright (c) 2026 Vladislav Shulcz: see `LICENSE` and `NOTICE`.
