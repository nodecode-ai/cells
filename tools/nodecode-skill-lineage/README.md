# nodecode-skill-lineage

Skill Lineage: records where each skill version's content came from, and verifies the hash-chained record. A Nodecode rewrite of [simin-yuan/hermes-skill-lineage](https://github.com/simin-yuan/hermes-skill-lineage) by [simin-yuan](https://github.com/simin-yuan).

A skill that rewrites itself leaves two questions behind: which text went
into this version, and is the record of that intact? This cell answers both,
per version, and stores no text at all.

## What it records

For every skill write you record, the cell keeps:

- the skill's content fingerprint before and after the write;
- the sources the write drew on: the operator's words and the texts it read
  (a page, a file, a tool result);
- how each source is linked to what the write added: **strong** (3 or more
  shared 8-word shingles, `skill_lineage.strong_min`), **anchor** (a shared
  operative token -- a URL or host, a shell command with its first operand, a
  package install, a file path, an environment variable, an IP or an e-mail
  address -- that is not common across sources, i.e. not boilerplate), or
  **context** (seen this session, nothing measurable shared; recorded, not
  followed unless you ask);
- a `prev -> hash` chain over all of the above.

Text is never stored. Every string becomes a keyed hash first, and the key
lives in the record's own folder, so two installs cannot be compared against
each other. Nothing here touches the network.

## Get it

Tell the agent "add the skill-lineage cell", or run:

```sh
nodecode add nc://skill-lineage
```

## Set it up

Nothing to set up: the record is a folder of files under the Nodecode home,
and the cell asks for no key. `skill_lineage.strong_min` (default 3) is how
many shared 8-word shingles make a strong edge, and
`skill_lineage.context_edges` (default 50) is how many context edges one
version lists before counting the rest.

## Use it

Two tools, for the model:

| Tool | What it does |
| --- | --- |
| `skill_lineage_record` | Record one version: the skill's name, its body after the write, the body it had, the texts it drew on, and what the operator typed. Call it once per write, after the write. |
| `skill_lineage_query` | `status`, `trace`, `descendants`, `report`, `verify` or `head`, answering text. |

And the same verbs as Lisp, for `eval`:

```lisp
(lineage:record "deploy" :after TEXT :before TEXT :sources (list PAGE) :asked ASKED)
(lineage:trace "deploy")                      ; where its newest version came from
(lineage:descendants "https://example.com/x")  ; every version that page produced
(lineage:descendants "deploy")                 ; every version of one skill
(lineage:report)                               ; operative tokens with no origin
(lineage:verify)                               ; is the chain intact
(lineage:head)                                 ; the newest seq and hash, to record elsewhere
(lineage:status)                               ; is anything being recorded, and where
```

`/lineage` (or `/lineage trace deploy`, `descendants URL`, `report`,
`verify`, `head`) asks the same from the composer.

`verify` re-walks the chain: every line parses, `seq` is contiguous, each
`prev` links, and each hash recomputes. On its own that is an
internal-consistency check; recording `(lineage:head)` somewhere off this
machine and passing it back as `(lineage:verify :head HASH)` is what tells a
truncated chain from a fresh one.

## What this does not do

- It does not stop a write or judge a skill. It records and it verifies.
- The link proves where content *could* have come from, not that it caused
  the write: it is computed from shared content plus session order, and
  `report` exists because some lines have no traceable origin.
- Nothing is recorded by itself. Hermes hooks every `skill_manage` call; a
  Nodecode skill is a file, so the write is recorded by the call that makes
  it -- the tool or `(lineage:record ...)` -- and a skill edited through
  `write_file` and never recorded leaves no version behind.
- The source texts are what the caller names. The cell does not read the
  session log to find them.

## Credit

A rewrite of [simin-yuan/hermes-skill-lineage](https://github.com/simin-yuan/hermes-skill-lineage)
(Apache-2.0). What was rewritten, and what was left out, is in [NOTICE](NOTICE).
