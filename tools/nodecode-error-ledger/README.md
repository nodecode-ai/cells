# nodecode-error-ledger

Error Ledger: the provider calls that failed, by model, provider, cause and day. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

## Get it

Tell the agent "add the error-ledger cell", or run `nodecode add nc://error-ledger`.

## Set it up

Nothing is required: the cell reads what the log already holds and starts
on its defaults. Two settings change the report.

```jsonc
{
  "error-ledger": {
    "window_hours": 24,  // how far back the bare report looks
    "recent_rows": 10    // how many of the newest failures `recent' lists
  }
}
```

## Use it

```
/error-ledger             last 24 hours, by model
/error-ledger session     the most recent session that had a failed call
/error-ledger all         everything in the log
/error-ledger 7d          the last 7 days (any number of hours or days: 6h, 30d)
/error-ledger providers   add to any of the above to group by provider
/error-ledger reasons     add to any of the above to group by cause
/error-ledger days        add to any of the above to group by day, newest first
/error-ledger recent      add to any of the above to list the newest 10 failures
/error-ledger recent 25   the newest 25 instead (1 to 100)
/error-ledger clear       hide what the log holds so far from the report
```

`/error-ledger all` answers a table:

```
Provider errors, all recorded:

model       errors  moved      top reason  top status
the-model        2      1  rate_limit (1)     429 (1)

One row per failed attempt the log recorded; the retry after it may have succeeded.
```

- **errors** counts failed attempts, not failed turns: an attempt Nodecode
  retried twice before the round moved to another model is two rows.
- **moved** counts the failures that cost the turn its model — the ones
  Nodecode moved off.
- **top reason** is the failure's own classification (`rate_limit`,
  `overloaded`, `timeout`, ...) when the round moved, or the budget the
  failure spent (`provider_request`, `provider_stream`, `provider_contract`)
  when it was retried in place; grouped by `reasons`, this column names the
  model it happened on most instead.
- **top status** is the most common HTTP status; a failure with none (a
  timeout, an in-stream cut) leaves it empty.

`/error-ledger recent` says when rather than how often: the newest failures
of the chosen window, newest first, so you can tell whether a provider is
still failing or stopped an hour ago.

The model can ask the same question through the `error_ledger` tool
(`window`, `by`, `recent`), and eval reaches everything:
`(error-ledger:report :window :day :by :provider)`,
`(error-ledger:failures :window :all)` for the rows as data, and
`(error-ledger:clear)`.

One honest difference from the original: nothing is recorded twice. The
original kept its own file and its own hook; Nodecode's log already holds a
`turn.provider_retry` fact for every failed attempt it will retry and a
`turn.provider_fallback` fact when the round moves to another model, so this
cell folds those and writes nothing — `clear` only hides what is recorded so
far, since the log is the organism's own record and this cell does not
destroy it.
