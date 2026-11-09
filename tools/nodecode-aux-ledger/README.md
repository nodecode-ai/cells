# nodecode-aux-ledger

Aux Ledger: what the model calls behind a turn cost, one line each. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

A session does not only make the requests you asked for. A title, a note, a
classification -- the side calls Nodecode runs on its auxiliary target -- each
costs tokens, each retries on its own, and none of them shows up in the
conversation. This cell writes one line per side call to a local JSONL ledger
and answers `/aux` with what they cost.

A row holds identifiers, timings and token counts: the session, the task name,
the provider, the model, the retry count, the wall time, the kind of failure
when there was one, and the input, cache, output and reasoning tokens the
provider reported. It never holds the instruction, the message or the answer.

## Get it

Tell the agent "add the aux-ledger cell", or run `nodecode add nc://aux-ledger`.

## Set it up

The `aux-ledger` section of `config.jsonc` names one file:

```jsonc
"aux-ledger": {
  "file": "~/.nodecode/aux-ledger/calls.jsonl",
  "record": true,
  "max_rows": 5000
}
```

- `file` -- the ledger. Relative paths read against the working directory.
- `record` -- write each side call as it happens; `false` keeps reading and
  stops writing, without deleting what is already there.
- `max_rows` -- rows kept when the file grows past its weight; the newest win.

Without `file` the cell refuses to start, and `/cells` says so.

## Use it

```
/aux             last 24 hours, by task
/aux session     the most recent session that made a side call
/aux all         everything in the ledger
/aux 7d          the last 7 days (any number of hours or days: 6h, 30d)
/aux models      add to any of the above to group by model instead of task
/aux providers   add to any of the above to group by provider instead of task
/aux days        add to any of the above to group by day, newest first
/aux recent      add to any of the above to list the newest 10 calls instead
/aux recent 25   the newest 25 instead (1 to 100)
/aux clear       delete the ledger
```

```
Auxiliary LLM calls, all recorded:

task       calls  failed  input  output  time
side           4       1  1,321      62  1.3s
title          1       0    321      12  1.1s
total          5       1  1,642      74  2.4s

1 call(s) reported no token usage (unstreamed or failed).
```

- **calls** counts attempts, so a retried call shows up once per attempt.
- **input** is everything the provider read: fresh input plus cache reads and
  cache writes.
- **time** is the wall time of the attempts.

The model can ask the same question through the `aux_ledger` tool: a `window`,
a `group_by` of `task`, `model`, `provider` or `day`, and a `recent` count.

Side calls the cell did not name read as `side`. A layer or another cell that
knows what its side call is for names it:

```lisp
(aux-ledger:with-task "recap"
  (nle:complete "Summarise the last turn in one line." (turn-text)))
```

and `/aux` groups those calls under `recap`.

From eval the same answers are `(aux-ledger:report "7d providers")`,
`(aux-ledger:report "recent 25")` and `(aux-ledger:clear)`.

## What it watches

`nle:complete` is Nodecode's one side call: it runs on the auxiliary target
when one is configured and on the session's own target otherwise. The cell
advises it, and advises the provider entry under it, so one row carries the
attempts the lane made and the usage the winning attempt reported.

A ledger that cannot be written is said once, as a warning, and never fails
the side call it was recording.
