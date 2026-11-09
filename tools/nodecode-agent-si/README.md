# nodecode-agent-si

Agent SI: sourced memory that keeps the user's own words as evidence, and searches the agent sessions already on this machine. A Nodecode rewrite of [joelbrilliant/hermes-agent-si](https://github.com/joelbrilliant/hermes-agent-si) by [joelbrilliant](https://github.com/joelbrilliant).

Two stores, one face. The brain holds what is true now as claims that cite a
source and a quote, behind an evidence gate: a quote must be at least five
consecutive words the user actually typed in this conversation, and a second
source with another value makes a fact **disputed** instead of overwriting it.
The session index holds what happened before, read from the Claude Code, Codex
and Grok session files already on this machine. Nothing here runs a model,
needs a key or dials the network: the search is keyword ranking over both
stores, merged into one list.

## Get it

Tell the agent "add the agent-si cell", or run `nodecode add nc://agent-si`.

## Use it

The model gets three tools:

| Tool | What it does |
| --- | --- |
| `brain_search {query, limit?, side?}` | One ranked list over the brain and the past sessions. Each hit says which side found it; `side` narrows to `truth` or `history`. |
| `brain_get {id, offset?}` | A fact key with every source and whether they disagree, a note with the source message, or a session's own text, paged. |
| `brain_remember {fact, quote, key?, value?, title?, kind?}` | Keep what the user said. The quote must be five consecutive words from one of their messages, and that message is kept as the source. With `key` and `value` it is a tracked fact; without them a sourced note. |

`/agent-si` shows what the brain holds, `/agent-si QUERY` searches it, and
`/agent-si index` re-reads the session folders. `(help :agent-si)` is the
manual page.

Before each turn the cell puts a compact recall block in front of the message
being answered — up to `prefetch_hits` hits, ids and snippets, framed as data
rather than instructions. Facts come from the tools, never from the model's
memory: if the brain has nothing, it says so, and a disputed fact is a
question for the person rather than a value to pick.

Where it keeps things, all of it local:

- `brain/facts.jsonl`, `brain/records.jsonl`, `brain/sources.jsonl`: the
  claims, the notes and the user's messages they cite.
- `brain/index/<harness>-<digest>.txt`: one excerpt file per session, with its
  origin on the first line. The session files themselves are only read, and
  never edited.

The `agent-si` section of `config.jsonc` takes `brain_dir` (empty keeps the
brain under the Nodecode home), `index_sessions` to turn the session index
off, `session_roots` for other folders of session files, `recall` to turn the
per-turn block off, and `prefetch_hits`. Nothing is required.

## What it does not do

The original vendored an engine of change packets, pending advice and its own
SQLite index over Hermes's `state.db`; this rewrite keeps the facts, the
evidence gate and the session search, and reads session files instead of
opening another app's database. The demo brain, the backup and restore hooks
and the background refresh thread are gone: the index is built at start and by
`/agent-si index`, which is one keyword scan and takes a moment.

Run `.github/test-cell.sh tools/nodecode-agent-si` from the cells repository.
The tests cover the lifecycle, a planted session file found and paged, the gate
refusing a quote the user never typed, a fact disputed by a second value, a
note that outlives a restart, and the recall block.
