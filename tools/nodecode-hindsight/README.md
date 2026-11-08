# nodecode-hindsight

Hindsight: long-term memory across sessions — recall, retain and reflect over a knowledge-graph memory bank, cloud or local. A Nodecode rewrite of [vectorize-io/hindsight](https://github.com/vectorize-io/hindsight) by [vectorize-io](https://github.com/vectorize-io).

The cell speaks the same HTTP API as the original Hermes plugin, so it talks
to the same service and the same bank: [Hindsight](https://hindsight.vectorize.io)
by Vectorize, either Hindsight Cloud or a `hindsight-api` server you run
yourself.

## Get it

Tell the agent "add the hindsight cell", or run `nodecode add nc://hindsight`.

## Set it up

1. Make a bank key at [hindsight.vectorize.io](https://hindsight.vectorize.io) — or run `hindsight-api` yourself and set `api_url` to it.
2. In `/settings`, put the key in `hindsight.api_key`, or export `HINDSIGHT_API_KEY`.
3. `api_url` defaults to Hindsight Cloud; `bank_id` defaults to `nodecode`, the partition every session of this home shares.
4. `/hindsight` prints the bank it is on, the endpoint and the switches.

Without a key the cell still installs, and every verb answers one sentence
saying what to set — it never dials.

## Use it

Talk, and the memory comes to you: at each model request, the turn's own
input is recalled and the best memories ride that request under
`# Hindsight memory`; the operator's words are written to the bank once per
turn. Both halves have a switch in the section (`auto_recall`,
`auto_retain`).

The model also has the three tools the original plugin gives it:

| Tool | What it does |
| --- | --- |
| `hindsight_recall` | the memories a query recalls, ranked and numbered |
| `hindsight_reflect` | a reasoned answer synthesized across the whole bank |
| `hindsight_retain` | store one memory, with a context label, tags and an optional event time |

and you have the same three by hand from `/hindsight`:

| Command | What it does |
| --- | --- |
| `/hindsight` | the bank, the endpoint, the switches, the memories injected this session |
| `/hindsight recall QUERY` | recall by hand |
| `/hindsight reflect QUESTION` | synthesize across the bank |
| `/hindsight retain TEXT` | store something now |

From eval, the same verbs are Lisp: `(hindsight:recall "…")`,
`(hindsight:reflect "…")`, `(hindsight:remember "…" :context "…")`.

Recall answers `No relevant memories found.` for a bank nothing was retained
to yet (Hindsight's 404), which is an empty memory rather than an error.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, its one condition, `(hindsight:…)` |
| `wire.lisp` | the three API calls, the key, the bank |
| `cell.lisp` | the three tools, the contracts, the `:round` hook, the manual |
| `status.lisp` | `/hindsight` and the one `nle:define-cell` declaration |

## What it does not do

The original's local-embedded mode ran a Python daemon (`hindsight-embed`,
`uvx hindsight-api`) itself; this cell reaches a server at `api_url` instead,
so a local setup is one you start. Multi-bank scoping, repository-selected
banks, the per-process retain queue with resend, and the fine-grained
retain and recall tuning knobs are not carried over: see `NOTICE`.

## License

MIT, Vectorize AI, Inc.'s: see `LICENSE` and `NOTICE`.
