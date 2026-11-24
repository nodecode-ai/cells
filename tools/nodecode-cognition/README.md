# nodecode-cognition

Cognition memory: durable facts from your own messages, kept locally with provenance and recalled under a budget. A Nodecode rewrite of [aska-digital/cognition-memory](https://github.com/aska-digital/cognition-memory) by [aska-digital](https://github.com/aska-digital).

The operator states a preference, a decision or a project fact, and it stays:
no save call, no service, no account. Each incoming prompt is read once for a
lasting statement — `I prefer …`, `we decided …`, `remember that …`, `the
project uses …` — and what matches goes onto a durable queue, then through a
fetch-first merge into one commit log under the home's `cognition/`
directory. Every committed entry carries provenance: the session it came
from, the author, when it was kept, the label it is filed under and a trace
id. A push whose base changed commits both versions under a conflict ref
instead of overwriting the other writer. Committed entries ride each request
under a token budget, with a marker when something was dropped.

## Get it

Tell the agent "add the cognition cell", or run `nodecode add nc://cognition`.

## Set it up

Nothing is required: the store is `<home>/cognition/`, capture and recall are
both on, and the budget is 8000 tokens. The section takes:

- `dir` — where the store lives, when it should not be under the home.
- `repos` — labels a memory may be filed under; the first is used for new ones.
- `capture` — read each incoming prompt for a lasting statement (default on).
- `recall` — put committed memories in every request (default on).
- `budget_tokens` — the prompt budget recall fits into (default 8000, at least 64).
- `queue-max-entries` — pending entries kept before the oldest is dropped with
  a tombstone (default 1000).
- `author` — who memories are stamped with; empty reads the home's
  `.gitconfig` `user.name`, and nothing else is read outside the store.

## Use it

The model gets three tools:

- `cognition_search` — find a committed memory by its words, before answering
  anything that may depend on an earlier session.
- `cognition_status` — the queue's backlog: how much is pending (extracted and
  not yet committed), the oldest age, the next retry, and the tombstones of
  entries the bound or the 24h cap dropped.
- `cognition_dream` — a read-only digest of what is known and the keep-both
  conflicts a person still has to choose between. Dream never writes.

And `/cognition` for the operator: bare, the store's line and a panel;
`/cognition search QUERY`, `/cognition status`, `/cognition dream`, and
`/cognition remember TEXT` to keep one by hand.

The store is two files, both readable and diffable: `committed.json` is the
sole truth and `queue.json` the pending pushes and their tombstones. Nothing
leaves the machine and no runtime beyond Nodecode is needed.

## Test it

Offline, from the cells checkout, with every HTTP call stubbed:

`NODECODE_SRC=<nodecode> .github/test-cell.sh tools/nodecode-cognition`

13 tests, 0 failed — install and stop, idempotent start, a disabled cell
installing nothing, a refused section, the verbs refusing while stopped, a
lasting statement committed with provenance, chatter left out, a stale push
keeping both, budgeted recall with its marker, committed memory riding every
prompt, `status` and `dream` answering, a conflict in the digest, and
`/cognition` answering.
