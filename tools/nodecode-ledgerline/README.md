# nodecode-ledgerline

Ledgerline: what every session costs, by day, model and session, with budgets and a receipt. A Nodecode rewrite of [Adolanium/hermes-ledgerline](https://github.com/Adolanium/hermes-ledgerline) by [Adolanium](https://github.com/Adolanium).

## Get it

Tell the agent "add the ledgerline cell", or run `nodecode add nc://ledgerline`.

## Set it up

Nothing is required: the ledger reads what this machine's store already
recorded. To have it watch a budget, set whole dollars in the `ledgerline`
section:

```jsonc
"ledgerline": { "monthly_budget_usd": 50, "session_budget_usd": 5, "warn_percent": 80 }
```

`0` is no budget. Nothing is sent anywhere and no key is needed.

## Use it

The model gets three tools:

- `ledger_spend` (`range`: today, 7d, 30d, month, all): the total, the
  cache share, a line per day and per model, the costliest sessions, and the
  few things worth changing.
- `ledger_session` (`session_id`): one session's receipt — cache reads,
  fresh input, cache writes, output, its models, and its share of the
  per-session budget.
- `ledger_budget` (`session_id` optional): this month against the monthly
  budget with the pace to month end and the two warning lines.

Type `/ledger` for today's numbers, `/ledger 30d`, `/ledger month`,
`/ledger all`, `/ledger session s-XXXX` or `/ledger budget`.

Every figure is what the store recorded, at the price it recorded: a round
the gateway could not price is named unknown and counted, never guessed.

## What it leaves to Nodecode

Nodecode's own `/usage` and Dashboard draw the same store with their own
reading; this cell adds the per-model and per-session split, a receipt a
model can ask for mid-turn, and budgets with a pace. It does not send
alerts to a channel, run scheduled reports, or change the desktop.

## Tests

```sh
.github/test-cell.sh tools/nodecode-ledgerline
```
