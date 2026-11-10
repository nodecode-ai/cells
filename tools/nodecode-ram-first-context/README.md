# nodecode-ram-first-context

RAM-first context: read-only keyword recall over a live Mnemosyne SQLite memory database. A Nodecode rewrite of [ether-btc/ram-first-context](https://github.com/ether-btc/ram-first-context) by [ether-btc](https://github.com/ether-btc).

Mnemosyne is the memory provider a Hermes Agent runs, and it keeps what it
remembers in one SQLite file with an FTS5 index over it. This cell reads that
file: keyword recall with no embeddings, no copy in memory and no network. The
original registered one tool against it; the rewrite keeps the tool and adds a
command, so the same search can be run by hand while the memory is still being
written by the other harness.

Read-only is a property of the statements, not a promise: the cell opens a
connection of its own with `PRAGMA query_only` on and issues only `SELECT`s,
so a search cannot change a byte of the database.

## Get it

Tell the agent "add the ram-first-context cell", or run `nodecode add nc://ram-first-context`.

## Set it up

One setting is needed, because the database belongs to the other harness:

```jsonc
{
  "ram-first-context": {
    "db_path": "~/.hermes/mnemosyne/data/mnemosyne.db", // the Mnemosyne database to search, read-only
    "default_limit": 5,      // how many results /ram-first-context answers
    "max_limit": 50,         // the ceiling a call's limit is held to
    "busy_timeout_ms": 5000, // how long to wait on a database another process is writing
    "usage_log": false       // one local line per search; never the query's words
  }
}
```

`db_path` is where the `mnemosyne` memory provider keeps its database — the
same file its `working_memory` table and `fts_working` index live in. Ask the
agent to set it, or put it in `~/.nodecode/config.jsonc` yourself. Until it is
set the cell runs and every search refuses, naming the setting.

## Use it

Ask for it in words, or search from a shell:

```
/ram-first-context terminal flags
```

or from eval, which answers the same rows the original tool did:

```lisp
(ram-first-context:recall "terminal*")  ; => a JSON array of rows
```

| What | It answers |
| --- | --- |
| `mnemosyne_fts_recall` (tool) | `query`, `limit`: a JSON array of `id`, `content`, `source`, `importance`, `timestamp`, `memory_type`, `metadata_json`, `rank` |
| `/ram-first-context TERMS` | the same search as lines: the count, then one line a match |
| `/ram-first-context` bare | the database it reads, the limits, whether the usage log is on |

Every whitespace-separated term must match, and each is a literal word: a
memory that says `AND`, or carries a `:` in it, is found by searching for it
rather than being parsed as FTS5 syntax. A trailing `*` searches a prefix
(`terminal*` finds `terminal` and `terminals`). Results come back
bm25-ranked; when the `fts_working` index is missing, or the file cannot run
the FTS statement, the search falls back to `LIKE` over `working_memory`,
ordered by importance then timestamp, with `rank` 0 and `%`/`_` in the query
still literal. An empty query answers an empty array.

An `{"error": ...}` record — still a JSON array, never a raised exception —
is the answer when the section names no database or the file is not there,
which is how the original fail-closed envelope reads too.

## What is in it

| File | What it is |
| --- | --- |
| `recall.lisp` | The two statements, the term quoting, the LIKE fallback, the JSON answer. |
| `cell.lisp` | The tool, `/ram-first-context`, the manual, and the config section. |
| `test/` | Offline tests: a Mnemosyne-shaped database, both paths, the refusals, the read-only proof. |

## License

MIT. The plugin is Copyright (c) 2026 ether-btc: see `LICENSE` and `NOTICE`.
