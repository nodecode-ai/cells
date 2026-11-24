# nodecode-obol

Obol: invoices and Stripe payment links, with a local ledger and risk checks. A Nodecode rewrite of [UEddy/Obol](https://github.com/UEddy/Obol) by [UEddy](https://github.com/UEddy).

Ask for an invoice in the conversation and the cell drafts it in the user's own Stripe account: their client's exact email, the lines, the terms. A draft emails nobody; when the user asks to send it, Stripe finalizes it and sends the client the payment link. Every invoice is also kept in a ledger under the Nodecode home, so "who owes me?" and "has Acme paid?" answer from this machine without a call.

Before a write, the same deterministic checks the original made run here: an invoice with the same client and amount inside thirty days is called a possible duplicate, an amount more than three times that client's average is called unusual, and a known client writing from a new address is named. They are warnings in the answer, not refusals.

The original was a Hermes plugin with an approval hook on every write. Nodecode has no per-call approval seam, so this cell keeps the fact that matters to a user — a draft emails nobody, and sending is a separate call — and states it in every tool description.

## Get it

Tell the agent "add the obol cell", or run `nodecode add nc://obol`.

## Set it up

A Stripe secret or restricted key goes in the `obol` section's `api_key`. A test key (`sk_test_...`) is the safe default; a live key (`sk_live_...`) is refused until `allow_live` is on, so a live key alone can never bill a real client by accident.

## Use it

The model gets seven tools:

| Tool | What it does |
| --- | --- |
| `obol_create_invoice` | Draft an invoice for a client and their lines; `send` finalizes and emails the link. |
| `obol_send_invoice` | Finalize a draft and send it. |
| `obol_get_invoice` | One invoice's status and amount, by id. |
| `obol_list_invoices` | What this home has billed, optionally by status. |
| `obol_send_reminder` | Send the payment email again for an open invoice. |
| `obol_void_invoice` | Void an open invoice, or delete a draft. |
| `obol_mark_paid` | Record that a client paid, with a note. |

And the operator's own view:

```
/obol              # what is outstanding, and who owes it
/obol overdue      # only what is late
/obol sync         # rewrite the ledger from the newest Stripe invoices
```

```lisp
(obol:list-invoices)                 ; the ledger as text
(obol:create-invoice call)           ; the verb under the tool
```

Amounts are in currency units: `1200` is 1,200.00. A call with no key, a live key with `allow_live` off, or a refused Stripe status answers `ERROR: OBOL-ERROR` and names why, without claiming anything was sent.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `api_key` | | Stripe secret or restricted key; test keys are safest. |
| `allow_live` | `false` | Let a live key send real invoices. |
| `default_currency` | `usd` | The ISO code an invoice uses when it names none. |
| `days_until_due` | `14` | Payment terms when a call names none. |
| `timeout_seconds` | `20` | How long one Stripe exchange may take. |

`(help :obol)` has the same guidance the model is given.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The `obol` package and the condition every refusal signals. |
| `cell.lisp` | The Stripe seam, the ledger, the risk checks, the seven tools, `/obol` and the section. |
| `test/` | Offline tests: lifecycle, a draft, a sent invoice, a recorded payment, a duplicate warning, the command, and every refusal. |

## What the rewrite does not do

- **No `ctx.llm` draft-from-message tool.** The original asked the configured model to turn a forwarded message into proposed invoice arguments; a cell has no such call, so the model itself extracts the fields and calls `obol_create_invoice`.
- **No approval hook.** Hermes gated every write through its approval prompt; Nodecode has no per-call approval seam, so the draft/send split carries the same safety and every tool description says a send emails the client.
- **No dashboard tab.** The original shipped a web view; here the ledger reads through `/obol` and `obol_list_invoices`.
- **Two risk checks, not four.** Duplicate and unusual amount are here; lookalike domains and Stripe's radar/dispute reads are not.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-obol
```

Every exchange is stubbed, so nothing needs a network.

MIT licensed. The original is Copyright (c) 2026 UEddy: see `LICENSE` and `NOTICE`.
