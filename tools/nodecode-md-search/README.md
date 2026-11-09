# nodecode-md-search

Markdown memory: full-text search over your own markdown folders, ranked by bm25. A Nodecode rewrite of [kuehnberger/hermes-qdrant-memory](https://github.com/kuehnberger/hermes-qdrant-memory) by [kuehnberger](https://github.com/kuehnberger).

The model gets one tool, `md_search`, that answers the passages of your
notes, vault or documentation that match a query: the file, the heading chain
it sits under (`Guide > Install > Linux`) and one excerpt around the match.
`/md-search` runs the same search by hand and shows what the index holds.

Nothing is embedded and nothing leaves the machine. The index is built in the
Nodecode process from the folders you name, chunked by heading, and ranked
with bm25 — no SQLite, no model download, no Qdrant, no network.

## Get it

Tell the agent "add the md-search cell", or run `nodecode add nc://md-search`.

## Set it up

Name the folders to search. Each entry is a path, or `label=path` when the
label an answer quotes should be something other than the folder's own name:

```jsonc
// ~/.nodecode/config.jsonc
{
  "md-search": {
    "roots": ["~/Documents/notes", "vault=~/Documents/vault", "~/src/project/docs"],
    "refresh_seconds": 300
  }
}
```

| Member | Default | What it does |
| --- | --- | --- |
| `roots` | none | the folders searched, each a path or `label=path`; a directory starting with a dot is skipped |
| `skip` | `.git` `.obsidian` `.trash` `node_modules` `__pycache__` `.venv` `venv` | directory names never walked |
| `max_results` | `5` | how many passages one search answers when the caller names no limit |
| `snippet_chars` | `170` | how many characters of a passage one result quotes |
| `max_chunk_chars` | `1400` | the longest passage before a section is split on its paragraphs |
| `max_file_kb` | `5120` | a markdown file bigger than this is not indexed |
| `max_files` | `5000` | the most markdown files one index holds |
| `refresh_seconds` | `300` | how long an index is reused before the folders are walked again; `0` walks them on every search |

With no `roots` the tool still registers and names the setting it needs, so
"set up md-search" finishes it in the conversation, in `/settings`, or in the
page's Settings tab.

## Use it

Ask for what you know you wrote — "what did I write about the backup
rotation?" — and the model calls `md_search`. Search by the words you wrote,
not by a question phrased for a model: the ranking is bm25 over the passage's
terms, with a section heading counted twice so a title outranks a mention in
a body.

```
/md-search                 the corpus and what the index holds
/md-search backup rotation  the same search, by hand
/md-search reindex          rebuild the index now
```

A note edited since the index was built is found again after
`refresh_seconds`, or at once with `/md-search reindex`. A finished turn does
not write anything: this cell reads, it never edits a note.

## Test

```sh
.github/test-cell.sh tools/nodecode-md-search
```
