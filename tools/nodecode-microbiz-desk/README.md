# nodecode-microbiz-desk

MicroBiz Desk: customers, sales, debts and a weekly report for a small business. A Nodecode rewrite of [Danzilla0/microbiz-desk](https://github.com/Danzilla0/microbiz-desk) by [Danzilla0](https://github.com/Danzilla0).

## Get it

Tell the agent "add the microbiz-desk cell", or run `nodecode add nc://microbiz-desk`.

## Set it up

It needs one thing: a folder for its data file. The agent asks for it on the
first start, or set it in `/settings`:

```jsonc
{
  "microbiz-desk": {
    "data_dir": "~/business/desk",   // where desk.json is kept
    "currency": "NGN"                 // the label amounts are shown in
  }
}
```

Everything stays in that one file, and nothing else on this computer is read
or written. There is no key, no account and no network call.

## Use it

Talk normally — "Mama Ngozi bought 2 bags of rice for 45000 and paid 20000",
"who owes me?", "how was my week?" — and the model records and answers:

| Tool | What it does |
| --- | --- |
| `microbiz_add_sale` | One sale; the customer is created when new; what they paid now is recorded with it. |
| `microbiz_record_payment` | Money paid toward a debt. More than the debt is refused, and so is a payment from someone who owes nothing. |
| `microbiz_who_owes_me` | Every debtor, biggest first, with the total and the days since their last sale. |
| `microbiz_customer_statement` | One customer's sales, payments and balance. |
| `microbiz_report` | Sales, cash collected, what is still owed, best customers and top items for the last N days (default 7). |

The same things work without a model, from `/microbiz-desk`:

```
/microbiz-desk sale "Mama Ngozi" "2 bags rice" 45000 --paid 20000
/microbiz-desk pay "Mama Ngozi" 10000
/microbiz-desk owing
/microbiz-desk report --days 7
/microbiz-desk statement "Mama Ngozi"
```

A customer name may be partial when exactly one customer matches it; two
matching names are refused rather than guessed. Amounts are kept in whole
minor units of the currency label, so a column of totals never drifts, and
"2,500.5" reads as 2,500.5. `(help :microbiz-desk)` lists all of it.

## License

MIT. The original is Copyright (c) 2026 Daniel Wisdom: see `LICENSE` and `NOTICE`.
