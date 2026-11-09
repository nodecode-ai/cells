# nodecode-fabric

Fabric: a shared markdown memory across sessions and agents — write what a turn settled, recall it by the words of the next task. A Nodecode rewrite of [esaradev/icarus-plugin](https://github.com/esaradev/icarus-plugin) by [esaradev](https://github.com/esaradev).

The memory is a folder of markdown files, one entry each, with a small
frontmatter header: who wrote it, when, what kind of entry it is, one summary
line, tags, and the body. Any agent or session that points at the same folder
reads and writes the same memory, so what one session learned the next one
finds — the original's idea, kept whole, without a server, a key or a database.

## Get it

Tell the agent "add the fabric cell", or run `nodecode add nc://fabric`.

## Set it up

One setting is required: `fabric.agent`, the name this home's entries carry.
Set it in `/settings` (or in the `fabric` section of
`~/.nodecode/config.jsonc`), for example `"agent": "nodecode"`. Until it is
set the cell refuses to start and says so.

Everything else has a working default: the folder is `~/fabric` (or
`$FABRIC_DIR`, or `fabric.dir`), it is created on start, and no key and no
network are involved. Point `dir` at the folder another agent already writes
to and the two share one memory.

## Use it

The model gets six tools:

| Tool | What it does |
| --- | --- |
| `fabric_write` | Write one entry: type, content, one summary line, tags, and for a handoff `status: open` with `assigned_to`. |
| `fabric_recall` | The entries that best match the words of the task, ranked, deduplicated, on a character budget. |
| `fabric_search` | Grep every entry for an exact word, name or path, with the lines that matched. |
| `fabric_pending` | Tasks assigned to this agent, reviews of its work, open tickets that name a customer. |
| `fabric_brief` | What waits, what this agent last did, what other agents changed, and one suggested next action. |
| `fabric_curate` | Mark an entry's training value (`high`, `normal`, `low`) in its own file. |

Entries an agent hands over name each other by `agent:id`, and every recall
prints that reference, so a review can point at the exact entry it reviews.

Before each turn the cell recalls what the turn's own words match and records
it onto the turn, framed as reference data — silence is the normal answer.
`/fabric QUERY` is the operator's own search; `/fabric` alone says where the
folder is, how many entries it holds and what waits.

From eval, the same verbs:

```lisp
(fabric:remember "decision" "the queue deduplicates by event id" "retry storm fixed")
(fabric:recall "retry storm")
(fabric:pending)
(fabric:brief)
```

`(help :fabric)` has the same guidance the model is given.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `agent` | — (required) | The name this session's entries carry, and the name a handoff is addressed to. |
| `dir` | `~/fabric` | The folder entries are read from and written to; `$FABRIC_DIR` is read when this is unset. |
| `project` | the working directory's name | The project id stamped on each entry. |
| `max_results` | 5 | How many entries one recall returns. |
| `max_tokens` | 2000 | The character budget of one recall, four characters to a token, as the original counted. |
| `auto` | on | Recall what the turn's prompt matches before the request. Off leaves the tools and `/fabric`. |

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The entry format, the ranked retrieval, the six verbs, the round hook and the declaration. |
| `package.lisp` | The `fabric` package: six verbs, the words, the condition. |
| `test/` | Offline tests over a scratch folder: each tool through its contract, ranking, search, handoffs, curate, the round's recall, the refusals. |

What the original did through its Hermes host — fine-tuning a replacement
model on the fabric with Together AI, switching the host's model config to it,
the Obsidian formatting and the weekly report — is not here; see `NOTICE`.

## License

MIT, the original's. Copyright (c) 2026 esaradev: see `LICENSE` and `NOTICE`.
