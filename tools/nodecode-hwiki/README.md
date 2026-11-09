# nodecode-hwiki

A folder of markdown pages as long-term memory, searched locally: the model recalls from it before a request and files new facts into a zone you review, and you edit the same files in any editor. A Nodecode rewrite of [ottosulin/hwiki](https://github.com/ottosulin/hwiki) by [ottosulin](https://github.com/ottosulin).

The model gets the same four tools a Hermes user got, the same store rules, and the same recall before a model request:

| Tool | What it answers |
| --- | --- |
| `wiki_memory_search` | Ranked excerpts across the wiki, each with its page, heading and outbound `[[wikilinks]]`; an optional frontmatter `spine` filter. |
| `wiki_memory_read` | One page in full by path, slug or `[[wikilink]]`, with its frontmatter, links and backlinks. |
| `wiki_memory_write` | Appends a dated bullet to the memory zone; a duplicate is detected and a secret-shaped note is refused. |
| `wiki_memory_status` | What is indexed, where writes land, and — with `lint` — dangling wikilinks and orphan pages. |

Before each model request the excerpts this turn's prompt matches arrive as a `## Wiki Memory (recalled)` block, capped by `prefetch_max_chars`, so an answer is grounded in your notes and can cite the page it came from. A prompt shorter than `min_query_chars`, or `auto_recall: false`, leaves it to the tools.

Nothing leaves the machine: the index is built in memory over the markdown files, and there is no network call.

## Get it

Tell the agent "add the hwiki cell", or run `nodecode add nc://hwiki`.

## Set it up

Point the cell at a folder of markdown — an existing Obsidian vault works:

```jsonc
{
  "hwiki": {
    "wiki_path": "~/wiki"
  }
}
```

`wiki_path` is the only required member. The rest have defaults:

| Member | Default | What it does |
| --- | --- | --- |
| `wiki_path` | — | the wiki root: the folder of markdown pages |
| `memory_dir` | `wiki/memory` | the one wiki-relative folder the agent may write to |
| `untrusted_dirs` | `raw` | folders never indexed, recalled or readable: third-party documents |
| `demote_paths` | `log.md`, `wiki/memory/builtin-mirror.md` | pages ranked after the curated ones |
| `auto_recall` | `true` | hand the model what a turn's prompt recalls |
| `prefetch_limit` | `4` | excerpts one turn recalls |
| `prefetch_max_chars` | `1400` | the longest recall one turn carries |
| `min_query_chars` | `8` | a shorter prompt recalls nothing |
| `max_hits_per_page` | `2` | excerpts one page may contribute to one search |
| `reindex_seconds` | `60` | how long an index is reused before the wiki is walked again |

A path that is not a directory is refused at the start, naming the member.

## Use it

The layout the original recommends, and the one this cell assumes:

```
my-wiki/
├── wiki/              indexed and recalled, human-curated
│   ├── concepts/ ...
│   └── memory/        the only place the agent writes (dated bullets)
└── raw/               never indexed, recalled or readable
```

Ask the model to remember something, then ask about it in a later session:

> Remember that deploys go out on Tuesdays.

The model calls `wiki_memory_write`, and `wiki/memory/decisions.md` gains a dated bullet. Later, "when do we deploy?" makes it call `wiki_memory_search` first. You review the memory zone in your editor, promote what matters into proper pages, and `git diff` shows everything the agent added.

`/hwiki` prints the index, `/hwiki QUERY` searches it, `/hwiki lint` names dangling wikilinks and orphan pages, and `/hwiki index` rebuilds it.

Third-party material — clipped articles, vendor advisories, anything that might carry an injected instruction — belongs in `raw/`: it is never indexed, never recalled and not readable through the tools, so it cannot reach the model as memory. Summarise it into `wiki/` yourself.

## Not ported

The SQLite FTS5 index is replaced by one this cell builds in memory (BM25 over the same heading-scoped chunks), rebuilt on the first search after `reindex_seconds` and after every write; qmd's hybrid retrieval, the `include`/`exclude` globs, the `hermes hwiki` subcommands beyond what `/hwiki` answers, and the mirror of the harness's built-in memory writes are left out. Details in `NOTICE`.

## License

MIT. hwiki is Copyright (c) 2026 Otto Sulin: see `LICENSE` and `NOTICE`.
