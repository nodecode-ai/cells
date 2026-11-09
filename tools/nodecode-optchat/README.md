# nodecode-optchat

OptChat Memory: endless local memory for a session. A Nodecode rewrite of [aska-digital/optchat-memory](https://github.com/aska-digital/optchat-memory) by [aska-digital](https://github.com/aska-digital).

Every message of every turn -- the operator's words, the agent's replies, its
tool calls and their results -- is appended to a local log under the home, one
JSON line a message, and never edited or deleted. From that log grows a binary
tree of one-line summaries: each message becomes a line, adjacent lines merge
in pairs, and the older the messages the more a line covers. Every request
carries the resulting *view*, a fixed-budget tiling of the whole log, oldest
first, so a session opened a year later still reads what happened. The agent
opens any line back down to the verbatim message with two tools.

Based on Victor Taelin's OptChat spec, as the original plugin is.

## Get it

Tell the agent "add the optchat cell", or run `nodecode add nc://optchat`.

## Set it up

Nothing is required and no key is needed: the cell writes under the home and
summarizes with the auxiliary model Nodecode already has. Its section in
`config.jsonc` (or `/settings`) is optional:

| Setting | Default | What it is |
| --- | --- | --- |
| `store_dir` | `optchat` | the folder under the home the log and the tree live in; one memory per folder |
| `view_budget_chars` | `16000` | characters of each request the view may take (at least 2000) |
| `node_bytes` | `512` | characters one summary line may take (at least 128) |
| `compact_enabled` | `true` | summarize in the background, one side call per long message or merge |

A section with `"enabled": false` turns the cell off, and the log stops growing.

## Use it

- Every request carries an `optchat` block: a header line with the count of
  messages remembered, then `<chat>` with one line a stretch of the log,
  `id+n|text`. A short message is its own line, word for word; a long one is
  summarized in the background, in order; a line not summarized yet reads
  `(not summarized yet: zoom it)`.
- `optchat_zoom` (id, n) opens a line into the two lines of n/2 messages it
  was made from; n = 1 gives the message in full.
- `optchat_date` (id) says when a message was written.
- `/optchat` prints how much is remembered, how much is summarized and how the
  view fits; `/optchat zoom ID N` and `/optchat date ID` open a line from the
  command line.
- In eval: `(optchat:zoom 2184 8)`, `(optchat:message-date 2184)`,
  `(optchat:status)`.

The log is the truth; the tree is only how the past is read. A summary that
could not be written (no model reachable) leaves its line saying so and is
built on a later pass -- nothing in the log is lost meanwhile.

## What it does not do

- It does not replace the session's context: recent turns stay in the
  conversation, and the view is the deep past beside them.
- It does not delete, edit or age anything out; the log grows for as long as
  the folder is used. Remove the folder to forget.
- One writer per home: the original locks the log with `flock` so two processes
  cannot share it; here one Nodecode process owns the home, and a second
  gateway on the same folder appends to the same files without a lock.

## Gaps against the original

- The summary call is one `nle:complete` per node on the auxiliary model, as
  the original's `call_llm(task="optchat_compact")` is one call; the original
  appends a size-correction turn and retries up to five times when the reply
  overruns the budget, and this rewrite cuts the reply at the budget instead.
- Budgets are characters here, bytes there; a UTF-8 character can cost more
  than one of them.
- The original keys its state per Hermes profile through `hermes_home_key`;
  Nodecode has one home per process, so this cell has one log per `store_dir`.
- Messages are dated to the second, without the original's UTC offset suffix.

## Test it

```sh
.github/test-cell.sh tools/nodecode-optchat
```

Ten tests, offline: the cell installs and stops, a turn's frames land in the
log and render in the block, a restart reads the same folder, a short message
is its own line with no model call, a long one goes through a stubbed
`nle:complete`, zoom opens a built line, `/optchat` answers, and a bad setting
refuses.
