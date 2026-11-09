# nodecode-magi

Magi: memory writes wait for a person's yes. A Nodecode rewrite of [MarcosAlves90/magi](https://github.com/MarcosAlves90/magi) by [MarcosAlves90](https://github.com/MarcosAlves90).

Hermes' memory plugin keeps two things side by side: a queue of memory
writes waiting for approval, and the durable entries those writes land in.
This cell keeps the same two, in the same files. A pending proposal is one
JSON record under `<store>/pending/memory/`, the stores are `MEMORY.md` and
`USER.md`, one entry a block separated by a line holding a single section
sign. Point `store` at a Hermes profile and the queue a Hermes install
already has is the queue this cell reviews.

## Get it

Tell the agent "add the magi cell", or run `nodecode add nc://magi`.

## Set it up

Name the store folder in `/settings` (the `magi` section of
`config.jsonc`):

```jsonc
{
  "magi": {
    "store": "~/.nodecode/memory",  // the folder holding MEMORY.md, USER.md and pending/memory
    "char_limit": 4000              // the size MEMORY.md is allowed before it wants compacting
  }
}
```

`store` is required: a cell with none answers every verb with the one
sentence naming it. Nothing is written until a write is approved.

## Use it

A write is proposed, read, then approved or rejected by name. The model has
five tools:

| Tool | What it does |
| --- | --- |
| `magi_pending` | the proposals waiting, one line each |
| `magi_show` | one proposal in full, with the diff it would make |
| `magi_stage` | propose an add, a replace or a remove |
| `magi_decide` | approve it — apply and drop it — or reject it |
| `magi_memory` | the durable entries, numbered, with the budget |

and you have the same by hand:

| Command | What it does |
| --- | --- |
| `/magi` | the store, the queue and the budget; and where to go next |
| `/magi pending` | the proposals waiting |
| `/magi show ID` | one proposal and its diff (a prefix of the id is enough) |
| `/magi approve ID` | apply it to the store and take it out of the queue |
| `/magi reject ID` | take it out of the queue, changing nothing |
| `/magi memory [QUERY]` | the stored entries, or the ones carrying QUERY |
| `/magi user [QUERY]` | the same over USER.md |
| `/magi add memory TEXT` | store an entry directly, with no proposal |
| `/magi stats` | the line above, on its own |

From eval the same verbs are Lisp: `(magi:list-pending)`,
`(magi:show-record "id")`, `(magi:stage "memory" "add" "text")`,
`(magi:decide "id" "approve")`, `(magi:entries "memory")`,
`(magi:add-entry "memory" "text")`. A refusal is `ERROR: MAGI-ERROR` and
says which setting or which entry it needs.

A replace or a remove quotes the entry it names; the cell finds it, and
refuses at staging time — not at approval — when the store holds no such
entry or more than one carries the quote.

## What it writes

Only inside `store`: the two stores, the pending folder, and nothing else.
No network, no subprocess, no telemetry. Compaction is a proposal like any
other: stage a `replace` that consolidates several entries and approve it.

## What it does not do

The original's Desktop page, its AI compaction previews and its local
history log are not carried over: see `NOTICE`.

## License

MIT: see `LICENSE` and `NOTICE`.
