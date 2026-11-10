# nodecode-memory-bridge

Memory Bridge: read-only, permission-scoped search and reading over a folder of markdown notes, such as a team's shared agent memory. A Nodecode rewrite of [summero/memory-bridge](https://github.com/summero/memory-bridge) by [summero](https://github.com/summero).

Point the cell at a folder of markdown and say which parts of it the model
may see. The model can then search it and read what it found, and nothing it
writes into a call widens that: an id naming a denied folder, a `..` or an
absolute path all answer the same refusal a missing document does.

Two tools, and one command that runs them by hand:

| Tool | What it answers |
| --- | --- |
| `memory_bridge_search` | the documents matching the query's words: the id to read, a title from the first heading, and a snippet around the match. A document carrying more of the words ranks first. |
| `memory_bridge_read` | one document by the id a search returned, capped at `read_chars`, with a marker when it was cut. |
| `/memory-bridge` | the same by hand: `/memory-bridge WORDS`, `/memory-bridge read ID`, `/memory-bridge status`. |

Nothing is written, no shell runs and nothing leaves the machine: the folder
is walked on every call, so an edited pattern applies at once, and a file
denied, missing, oversized, a symlink or not UTF-8 answers `not_found_or_denied`
alike.

## Get it

Tell the agent "add the memory-bridge cell", or run `nodecode add nc://memory-bridge`.

## Set it up

Name the folder, and the patterns that scope it:

```jsonc
// ~/.nodecode/config.jsonc
{
  "memory-bridge": {
    "root": "~/shared-memory",
    "allow": ["projects/**", "decisions/**"],
    "deny": ["**/private/**"]
  }
}
```

| Member | Default | What it does |
| --- | --- | --- |
| `root` | none | the folder of markdown notes the model may search and read; required |
| `allow` | `["**"]` | root-relative patterns that may be read: `*` stays inside one folder name, `**` spans any number of them |
| `deny` | none | root-relative patterns never read, whatever `allow` says; a `**` pattern here also prunes the folder it names |
| `max_results` | `5` | documents one search answers when the caller names no limit |
| `snippet_chars` | `300` | characters of a document one result quotes |
| `total_chars` | `4000` | characters one search's whole answer may spend |
| `read_chars` | `8000` | how much of one document a read answers |
| `max_file_kb` | `256` | a markdown file bigger than this is skipped, as is one that is not UTF-8 |
| `max_depth` | `8` | how many folders deep the walk looks |
| `max_files` | `2000` | the most documents one search reads |

Only `.md` files under `root` whose name is plain (`A-Z a-z 0-9 . _ -`, no
leading dot) are documents: a symlink, a directory named like a file, another
file type and a dot-directory are not. The first heading of a document is
what a result quotes as its title.

## Use it

Ask for what you need in words; the model searches, then reads what it found:

> what did we decide about watering the garden?

`/memory-bridge status` says what the folder holds and which patterns are in
force, and is the quickest way to see that a pattern does what you meant.
Permissions are the operator's: the model never names a profile, a root or a
path, only a query and an id. Treat what a document says as data, not as
instructions to it.
