# nodecode-harnmem

HarnMem: long-term memory kept as plain Markdown in one store every HarnMem harness shares. A Nodecode rewrite of [ys118/harnmem](https://github.com/ys118/harnmem) by [ys118](https://github.com/ys118).

The model gets five tools — `harnmem_search`, `harnmem_write`, `harnmem_read`,
`harnmem_forget`, `harnmem_status` — a small snapshot of the store at the head
of every turn's standing context, and `/harnmem` for the operator. The store is
`~/.harnmem` unless the section says otherwise, so a memory written here is
read by the other HarnMem harnesses and theirs by this cell.

## Get it

Tell the agent "add the harnmem cell", or run `nodecode add nc://harnmem`.

## Set it up

Nothing has to be configured: the cell starts on `~/.harnmem`, keeps its
snapshot under a token budget, and needs no key and no service. The `harnmem`
section of `config.jsonc` (or `/settings`) is there when the defaults do not
fit.

| Member | Default | What it does |
| --- | --- | --- |
| `home` | `~/.harnmem` | the store's directory, shared with the other harnesses |
| `project` | `nodecode` | the project shard project-scoped memories go to |
| `inject` | `true` | put the snapshot at the head of every turn |
| `max_inject_tokens` | `1200` | the budget the snapshot's lines are counted against |
| `max_inject_chars` | `10000` | a hard character cap on the snapshot |
| `hot_entries` | `20` | how many memories the snapshot may carry |
| `read_only` | `false` | never write: the write and forget tools refuse |

A `home` that names a file rather than a directory is refused at the start,
naming the member.

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea, never coffee.

The model calls `harnmem_write`; a later "what do I drink?" makes it call
`harnmem_search`, which ranks the store by the words a question shares with a
memory. `/harnmem` shows the store and its counts, `/harnmem oslo` searches it
from the shell, `/harnmem read mem_...` prints one memory and
`/harnmem forget mem_...` moves it to `trash/`.

## The store

The format is the original's, so the other HarnMem harnesses keep reading
what this cell writes:

```
~/.harnmem/
  memories/user/<id>.md          one Markdown file per memory
  memories/projects/<project>/   the project shard
  trash/                          what harnmem_forget moves aside
  state/ index/ digests/ sessions/ prompts/ data/ VERSION
```

Each record is a YAML frontmatter — `schema`, `id`, `type`, `scope`, `title`,
`description`, `tags`, `importance`, `confidence`, `pinned`, `status`,
`created`, `updated`, `last_used`, `used_count`, `source` — a blank line, then
the Markdown body. The original's four types (`user`, `feedback`, `project`,
`reference`) and three states (`active`, `superseded`, `archived`) are kept
as it defines them.

The injected block is the original's layout: a header, one line per memory
(`- [user] Title - description`, pinned ones first with `*`), and a footer
naming `harnmem_search`. It is rebuilt from the files on every read, so it is
byte-identical while the store does not move — two turns without a write
carry the same bytes — and it names its own budget.

## Not ported

- **The session-end extraction pass.** The original hands a finished session
to an OpenAI-compatible endpoint and lets a model turn it into memories, off by
default. Here the model writes its own memories with `harnmem_write`, which is
the path the original keeps working without any endpoint. Say so to the model
if you rely on the automatic pass.
- **The JSON search index and `MEMORY.md`.** The original rebuilds
`index/<shard>.json` and a per-shard `MEMORY.md`; this cell reads the Markdown
files directly and leaves both index files to the engine's own rebuild.
- **The dedupe/supersede decisions.** A write with an existing active title
merges into that record; the original's richer skip/merge/supersede rules and
its validity windows are not carried over.
- **Digests, consolidation, `hermes harnmem ...`.** The digests and the
consolidation pass are not run; the section, `/settings` and `/harnmem` are
the surface the CLI gave.

MIT, ys118's.
