# nodecode-memtomem

Memtomem: markdown notes the model saves, searches and recalls, shared with your other coding agents. A Nodecode rewrite of [memtomem/memtomem](https://github.com/memtomem/memtomem) by [memtomem](https://github.com/memtomem).

The model gets five tools over one local store: `mem_search` finds what the
notes say about a topic, `mem_recall` reads a date range, `mem_add` saves an
entry the user asked you to keep, `mem_index` checks a directory of notes, and
`mem_status` says where the store is and how much it holds. `/memtomem` prints
the store, and `/memtomem QUERY` searches it from the shell.

The store is memtomem's own, so it is not a copy of it: notes saved here are
notes the same store gives to the other agents that read it, and theirs are
found here. Nothing leaves the machine — a search reads the markdown files and
scores them with BM25, the ranker memtomem uses when it has no embeddings.

## Get it

Tell the agent "add the memtomem cell", or run `nodecode add nc://memtomem`.

## Set it up

There is nothing to set up: a fresh store needs no bootstrap. The defaults are
memtomem's — the store at `~/.memtomem`, notes under its `memories/`
directory — and the first `mem_add` creates the notes directory. If memtomem is
already set up for your other agents, this cell reads the same notes without
reading memtomem's `config.json`: point `notes_dir` at the directory that
configuration fills.

The `memtomem` section of `config.jsonc` (or `/settings`) changes any of it:

```jsonc
{
  "memtomem": {
    "root": "~/.memtomem",   // the store: notes, config.json and memtomem's index
    "notes_dir": "~/.memtomem/memories",
    "max_results": 8,         // hits one search answers with unless the call asks otherwise
    "max_bytes": 4000000      // how much of the notes one read takes in
  }
}
```

| Member | Default | What it does |
| --- | --- | --- |
| `root` | `~/.memtomem` | the store the status line and the notes directory are built from |
| `notes_dir` | `<root>/memories` | the markdown notes read and written |
| `max_results` | `8` | hits one search or recall answers with unless the call says otherwise |
| `max_bytes` | `4000000` | how much of the store one read takes in before it stops |

A relative `root` or `notes_dir` is refused at the start, naming the member.

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that deploys go out on Thursdays, never on Fridays.

The model calls `mem_add`, which appends one entry to the day's file in the
store — a `## ` heading, a `> created:` stamp, the tags, and the text — in the
same shape memtomem writes. Later, "when do deploys go out?" makes it call
`mem_search`, and the answer names the file, the heading and the score of the
section it found. "What did we decide last week?" is `mem_recall`, which reads
the entries stamped in that window, newest first, and says what search is.

`mem_status` is the one to call when a search comes back empty: it names the
store and the notes directory, counts the files and entries, and says whether
a `config.json` is in force there. A store with no notes directory yet is a
fresh store, not a broken one.

## What it reads

- The notes, recursively, under `notes_dir`: `.md` and `.markdown` files.
- Each file's `## ` sections, with the `> created:` and `> tags:` lines that
  follow a heading read as that entry's date and tags; a file's text before
  its first heading is an entry of its own.
- Nothing else: no `config.json`, no index, no `.env`, and no other agent's
  files.

An entry without a `> created:` line takes its date from the file's write
time, so an older store still reads by date.

## Not ported

- **The SQLite index and the embeddings.** The original's server indexes the
  notes and can add dense retrieval and reranking. A cell bundles no Python,
  so a search reads the notes themselves and ranks them with BM25, the
  original's default. `mem_index` therefore checks and counts rather than
  builds an index.
- **The rest of the MCP server's actions** — `mem_do`'s context-gateway,
  `ingest`, export and import, the project tiers, the file watcher. The six
  skills the original ships call none of them, and their workflows are what
  the five tools here carry.
- **The redaction guard** the original applies to an added entry, its
  idempotency key, and its consolidation of similar entries; `mem_add` here
  appends the entry and names a similar one when it finds one.
- **The Hermes plugin manifest and the uvx launch**: this is a native cell, so
  there is no MCP server and nothing to download.

Apache-2.0, memtomem's.
