# nodecode-factledger

Factledger: a durable ledger of typed facts where a new value closes the old row instead of deleting it, so one truth holds per key and the history stays. A Nodecode rewrite of [explorelifehus-eng/factledger](https://github.com/explorelifehus-eng/factledger) by [explorelifehus-eng](https://github.com/explorelifehus-eng).

A memory keeps what fits in the prompt. A ledger keeps what outlives it: record a
price, a status, a limit or a decision once, with the source it came from, and
look it up whenever it matters instead of paying for it in every round. The file
is TSV — one row a fact, the original's ten columns — so it reads, diffs and
greps like any other note.

## Get it

Tell the agent "add the factledger cell", or run `nodecode add nc://factledger`.

## Use it

The model calls the `factledger` tool, one action a verb:

| Action | What it does |
| --- | --- |
| `add` | Record a fact: `scope`, `subject`, `predicate`, `value` and `source`. A different value closes the row it supersedes (`valid_to`, `superseded_by`); the same value changes nothing. |
| `show` | The truth holding today, or `as_of` an ISO date, for a `scope` and/or `subject`. |
| `search` | Rows whose subject, value, predicate or source carries a term. |
| `history` | Every row for a subject, oldest first: what changed and when. |
| `compile` | The truth holding today as compact lines inside `budget` characters, for a prompt. |
| `stale` | Rows whose `valid_from` is `days` old or older: what wants re-checking. |
| `export` | The rows as JSON; `include_history` adds the closed ones. |
| `check` | The ledger's integrity: a duplicate id, an empty column, a conflicting truth, an overlap, a successor that is not there. An empty list is healthy. |
| `stats` | Rows, active, closed and the scopes. |

Conventions the original states, and this keeps: `scope` is the domain namespace
(`market`, `legal`), `subject` the tracked entity (`competitor.x`, `case.y`),
`predicate` the property (`price`, `status`); `source` is mandatory and should be
verbatim; `confidence` is `stated` (first-party), `derived` (computed) or
`assumed` (a guess). A value is superseded, never deleted, so a mistake stays
visible and reversible. Never quote a stored value without its `valid_from`.

From a shell, `/factledger` shows the totals and a panel;
`/factledger show`, `search TERM`, `history SUBJECT`, `stale`, `check`, `stats`,
`compile` and `add scope | subject | predicate | value | source` answer the rest.
From eval the same verbs are `(factledger:add-fact "market" "competitor.x" "price" "10" "price list")`,
`active-facts`, `search-facts`, `fact-history`, `compile-view`, `stale-facts`,
`export-facts`, `check-ledger` and `ledger-stats`; `(help :factledger)` is the
page the model reads.

Nothing leaves the machine and there is no key. The ledger lives under the
Nodecode home at `factledger/facts.tsv` unless the section names another file:

```jsonc
{
  "factledger": {
    "path": "~/notes/facts.tsv",     // where the ledger is
    "aliases": "~/notes/aliases.tsv",// other predicate spellings, one canonical
    "stale_days": 30,                // how old a fact may be before stale names it
    "budget": 800,                   // characters in a compiled view
    "order": "confidence"            // or "recency": which facts survive the budget
  }
}
```

The aliases file is the original's: one line a predicate,
`canonical<spelling>` then a tab then the other spellings, comma-separated; a line
beginning `#` is a comment.

## Tests

Run `.github/test-cell.sh tools/nodecode-factledger` from the cells repository,
or `just factledger-test` from Nodecode. The tests cover the lifecycle and a
setting out of shape, every action through the tool seam, a value superseded with
its history readable as of a date, a fact without a source, a call that names no
action, the TSV file and its header, a compiled view inside its budget, and
`/factledger` reading and recording.
