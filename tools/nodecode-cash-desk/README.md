# nodecode-cash-desk

Cash Desk: chases overdue invoices from CSV exports. A Nodecode rewrite of [FutureInvoked/cash-desk](https://github.com/FutureInvoked/cash-desk) by [FutureInvoked](https://github.com/FutureInvoked).

## Get it

Tell the agent "add the cash-desk cell", or run `nodecode add nc://cash-desk`.

## Set it up

The cell runs with no settings, but drafting refuses until `sender_name` names the business that signs the letters. The rest is optional:

| Setting | What it is |
| --- | --- |
| `sender_name` | the business that signs every reminder |
| `from_address` | the `From:` header of the `.eml` files |
| `default_language` | `en` or `cs`, for customers whose export carries none |
| `currency` | for files with no currency column |
| `payment_iban` | printed when an invoice's export has no IBAN of its own |
| `date_order` | `auto`, `dmy` or `mdy`; auto refuses a file whose slashed dates read both ways |
| `ignore_balances_below` | a small open balance that is not chased automatically (default 10.00) |
| `import_dirs` | extra folders `cashdesk_import` may read CSV exports from |
| `outbox` | where approved `.eml` files are written |

## Use it

1. Export your customers, your open invoices and a bank statement as CSV, and import them:
   `cashdesk_import` with `kind` `customers`, `invoices` or `bank`. A generic, Xero, QuickBooks or Stripe invoice export is read; a bank statement with English or Czech headers is read; the same file is never imported twice.
2. `cashdesk_match` applies the payments that are certain — the reference names exactly one invoice, in its currency, for exactly its open balance — and puts everything else in review, where a receipt pauses the reminders of the invoices it may pay.
3. `cashdesk_draft` writes one letter per customer at the highest stage reached: 7, 14, 21 or 28 days overdue, in their language, with the ledger's own invoice lines, totals, IBAN and reference.
4. `cashdesk_revise` rewords a draft under Figure Lock: every amount, invoice number, date, reference and IBAN must stay exactly as the ledger has it, and a financial line may not change.
5. A person approves the exact text (`/cashdesk approve D1`), then `cashdesk_send` writes it to the outbox as one `.eml` file per recipient, ready for their mail client. Nothing is sent before approval, and a revision needs a fresh one.

`cashdesk_status` and `cashdesk_brief` read the ledger: open balance by age bucket and currency, drafts waiting, customers on hold. `cashdesk_queue` shows each draft's exact text and the hash a person approves. `cashdesk_reply` records a customer's reply (a dispute holds them); `cashdesk_hold` and `/cashdesk release` do the same by hand.

`/cashdesk` with no argument shows the ledger and a panel: approve a draft, send an approved one, release a held customer. `(help :cash-desk)` carries the same verbs for `eval`.

The ledger is one JSON file under the home's `cash-desk/` folder, mode 0600. Nothing here dials a network: the reminders leave as files.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the ten `cashdesk_*` tools, `/cashdesk`, the manual topic and the `cash-desk` section |
| `ledger.lisp` | the ledger file, the CSV importers and the matching rule |
| `chase.lisp` | the reminder ladder, the rendered letters and their figures |
| `verbs.lisp` | reading, Figure Lock, approval, the outbox, replies and holds |
| `text.lisp` | money as whole cents, dates as day numbers, and the CSV reader |

## License

MIT. Cash Desk is Copyright (c) 2026 FutureInvoked: see `LICENSE` and `NOTICE`.
