# nodecode-payops

Paystack: read-only transaction listing and reconciliation reports. A Nodecode rewrite of [BigWave45/payops-hermes-](https://github.com/BigWave45/payops-hermes-) by [BigWave45](https://github.com/BigWave45).

Two tools, the same names and arguments as the original:

| Tool | What it does |
| --- | --- |
| `payops_list_transactions` | A date range of Paystack transactions, one object each: reference, amount in minor units, currency, status, paid_at. |
| `payops_reconcile` | The same transactions plus the account's settlements, summarized per currency, with duplicate references and the report's own limits. |

Nothing here moves money. The two endpoints it reaches are Paystack's
transaction and settlement listers, both GET. There is no charge, refund,
transfer or payout call anywhere in the cell.

Ask "list my Paystack transactions from 1 to 31 October 2026", "reconcile
last month", or "are there duplicate references in October?" — and the model
picks the tool.

## Get it

Tell the agent "add the payops cell", or run `nodecode add nc://payops`.

## Set it up

A Paystack secret key is required — a test key is enough, and the safe one
to start with (Paystack's dashboard, Settings, API Keys and Webhooks).

Name the key, never paste it into the config: export it in the shell that
starts Nodecode, then, in `/settings` or in the `payops` section of
`config.jsonc`:

```jsonc
"payops": {
  "secret_key_env": "PAYSTACK_SECRET_KEY",   // the variable that holds it
  "max_pages": 5,                             // pages of 100 records per call, 1 to 20
  "max_rows": 100                             // transactions listed in one answer
}
```

| Key | Meaning |
| --- | --- |
| `secret_key_env` | The environment variable holding the key (default `PAYSTACK_SECRET_KEY`). |
| `secret_key` | A key written in the section instead; the environment variable wins. |
| `base_url` | The API base, `https://api.paystack.co` unless you proxy it. https only. |
| `max_pages` | Pages of 100 records one call reads when the call asks for none. The ceiling is 20. |
| `max_rows` | How many transactions one answer lists; the rest is counted and said. |
| `timeout` | How long one Paystack request may take, in seconds. |

With no key the cell still starts and says so once; a call then answers what
is missing and where to put it.

## Use it

Dates are `YYYY-MM-DD`, both ends inclusive.

- `payops_list_transactions {from_date, to_date, max_pages, providers}`
- `payops_reconcile {from_date, to_date, max_pages, providers}`

`max_pages` is 1 to 20; left out, the section's `max_pages` applies. A
range longer than the cap is read up to the cap, and `provider_warnings`
says how many pages were left. `providers` takes `paystack` only — the one
provider this release reads.

The operator drives the same thing from the shell:

- `/payops 2026-10-01..2026-10-31` — one line: how many transactions, how
  many succeeded, the successful total per currency, the settlements, and
  how many exceptions.
- `/payops` alone — the last seven days.
- `/payops 2026-10-01 2026-10-31` — a space instead of the `..`.

And from eval, in the `payops` package:

```lisp
(payops:transactions "2026-10-01" "2026-10-31")
(payops:reconcile "2026-10-01" "2026-10-31" :max-pages 10)
(payops:format-amount 123456 "NGN")   ; => "NGN 1,234.56"
```

`(help :payops)` is the short version of this section, in the manual the
model reads.

### What the report claims, and what it does not

- Transaction totals and settlement totals are reported separately;
  settlement timing lags transaction activity, so they are never treated as
  equal.
- A duplicate reference is one reference seen twice in the range, within one
  provider.
- It is not bank-statement or merchant-order-ledger reconciliation.

Amounts stay in minor currency units, exactly as Paystack returns them, so
no rounding is introduced anywhere in the read. `/payops` formats them for a
person; `payops:format-amount` uses the currency tables the original
carried, including the zero- and three-decimal currencies.

The key is sent as `Authorization: Bearer <key>` to `base_url` and to
nothing else, over https. Reads only: the cell never writes a file.

## License

MIT. Copyright (c) 2026 PayOps contributors: see `LICENSE` and `NOTICE`.
