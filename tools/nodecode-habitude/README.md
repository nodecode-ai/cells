# nodecode-habitude

Habitude: read-only search over your own journal or Obsidian notes, answered with file and line citations. A Nodecode rewrite of [legott-ai/habitude-agent-integrations](https://github.com/legott-ai/habitude-agent-integrations) by [legott-ai](https://github.com/legott-ai).

## Get it

Tell the agent "add the habitude cell", or run `nodecode add nc://habitude`.

## Set it up

No key and no network. Name the folder of Markdown notes to read, in `/settings` or on the Settings tab:

```jsonc
{
  "habitude": {
    "vault": "~/Documents/Journal",
    "max_results": 8,
    "refresh_seconds": 60
  }
}
```

`vault` is the whole scope: that folder, its subfolders, its `.md` files, and nothing else. Dot-directories are skipped, files over 2 MB are skipped, and nothing is ever written to it.

## Use it

Ask about your own writing and past entries, or type `/habitude` with the words you are looking for. The cell gives the model four tools:

| Tool | Use |
| --- | --- |
| `search_journal` | `{query: "writing routine, this week"}`: lexical matches, each with its path, date, line range and a `reference`. |
| `read_excerpt` | `{reference: "..."}`: the passage one citation names; references expire on a new search or a changed note. |
| `index_status` | how many notes and excerpts are indexed, and when the index was built. No note bodies. |
| `refresh_index` | rebuild the index after a note was written. The notes are untouched. |

The same four are `(habitude:search-journal "writing routine")`, `(habitude:read-excerpt "...")`, `(habitude:index-status)` and `(habitude:refresh-index)` from eval, and `(help :habitude)` answers the whole manual.

Matching is lexical: it ranks notes by the words they share with the question, so ask in the words a note would carry. A miss is a miss, not proof a note is absent. Excerpts enter the conversation and ride to the provider the session is on, so they are framed as data and never as instructions.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The walk, the in-memory index, the search and the four tools, declared once with `nle:define-cell`. |
| `package.lisp` | The `nodecode-habitude` package, nicknamed `habitude`. |
| `config.example.jsonc` | The section, illustrated. |
| `test/cell-test.lisp` | The slice: a search that cites, the excerpt it names, a miss, a refresh, a stale reference. |

## License

MIT. The original plugin is Copyright (c) 2026 Habitude: see `LICENSE` and `NOTICE`.
