# nodecode-recall-vault

Recall Vault: a money policy vault and memory for the agent. A Nodecode rewrite of [tydavid5/recall-vault](https://github.com/tydavid5/recall-vault) by [tydavid5](https://github.com/tydavid5).

You write your money rules once — a per-transaction cap, a daily cap, a
confirm threshold and an allowlist — and the agent is held to them before any
payment-like tool call runs. Over a cap, the call is blocked with the rule
quoted back. Above the threshold, it waits for you: **Allow once** or
**Deny**. An address nobody allowed is refused. Every decision is written to
a local audit ledger, and the rules ride every request, so a long
conversation cannot talk the agent out of what it was told.

Dry run: this cell moves no money. A check logs a decision and books it
toward today; the payment itself is the paying tool's business.

## Get it

Tell the agent "add the recall-vault cell", or run `nodecode add nc://recall-vault`.

## Set it up

Nothing is required. A fresh vault starts with the original's defaults:

| Rule | Default |
| --- | --- |
| Per-transaction cap | 1 NEAR |
| Daily cap | 2 NEAR |
| Confirm above | 0.5 NEAR |
| Allowlist | `recallwallet-freelancer.testnet` (the original's demo address) |
| Currency | NEAR |

Change them with `/recall-vault set`, which is yours alone, or from the
`recall-vault` section of `~/.nodecode/config.jsonc`:

| Setting | Default | What it does |
| --- | --- | --- |
| `store_file` | `recall-vault/policy.json` under the home | where the rules, today's spending and the remembered rules are kept; the ledger (`ledger.json`) sits beside it |
| `max_ledger` | 200 | how many decisions the ledger keeps |
| `confirm_by_ask` | true | ask **Allow once** / **Deny** before a payment above the threshold; off blocks it instead |
| `ask_seconds` | 120 | how long that question waits |
| `inject` | true | put the rules in front of the model at every request |

Nothing here needs a key, and nothing leaves the machine: no network call, no
subprocess, no other app's file.

## Use it

```
/recall-vault                                   the rules in force, and today
/recall-vault policy
/recall-vault ledger
/recall-vault check 0.4 recallwallet-freelancer.testnet
/recall-vault remember Never pay a brand-new address without asking first.
/recall-vault set per_tx_cap=1 daily_cap=2 confirm_above=0.5 allowlist=a,b reset_spent=true
```

An empty allowlist (`allowlist=`) means nobody is allowed, not allow all.

The agent has five tools of its own:

| Tool | What it does |
| --- | --- |
| `recall_vault_check` | check a payment; an allowed one is booked toward today |
| `recall_vault_policy` | the caps, the threshold, the allowlist, today's spending and your rules |
| `recall_vault_set_policy` | change a cap, the threshold or the allowlist — **a change the model asks for is put to you as a question first**, showing the exact change |
| `recall_vault_ledger` | every decision, newest first |
| `recall_vault_remember` | write down a plain-language rule — also put to you first |

From Lisp, or from an eval form, the same decisions are
`(recall-vault:check 0.4 "someone.testnet")`, `(recall-vault:policy)`,
`(recall-vault:ledger)`, `(recall-vault:remember "...")` and
`(recall-vault:set-policy :per-tx-cap "2")`.

### What a payment is

A tool call is held only when its name carries `pay`, `payment`, `transfer`,
`withdraw`, `payout` or `settle` (or `send` with money, payment or near in
it) **and** its arguments carry both an amount and a recipient. `send_message`,
`send_email` and `read_file` are never touched, because they carry no amount.

### The decision order

1. an amount that is not a positive finite number — blocked;
2. an empty allowlist — blocked, nobody is allowed;
3. over the per-transaction cap — blocked, `perTxCap`;
4. over the day's cap once this is added — blocked, `dailyCap`;
5. a recipient off the allowlist — blocked, `allowlist`;
6. above the confirm threshold — the operator is asked;
7. otherwise allowed, and booked.

### It is a speed bump, not a vault door

`eval` has the machine's full authority, so a determined turn can call
`(recall-vault:set-policy ...)` from Lisp and step around the question. It
stops the common slip: a payment the rules forbid, a new address, a spend
past the day.

## Testing

From this repository, `.github/test-cell.sh tools/nodecode-recall-vault`,
offline against a pinned Nodecode tree. The tests cover each decision and its
rule, the caps, the empty allowlist, the ledger, the remembered rules riding
every request, a payment-like call blocked before it runs and one that runs
and is booked, an ordinary tool left alone, a model's change put to the
operator both ways, and the lifecycle.
