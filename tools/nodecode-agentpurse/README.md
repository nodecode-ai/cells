# nodecode-agentpurse

AgentPurse: search, quote and buy inside an AgentPurse sandbox allowance, and only after a person approves. A Nodecode rewrite of [maaz-bin-hassan/agentpurse](https://github.com/maaz-bin-hassan/agentpurse) by [maaz-bin-hassan](https://github.com/maaz-bin-hassan).

AgentPurse is a hosted purse with a clerk that allows, holds or denies every payment in its own code, on the PayPal sandbox. This cell is its twelve tools as twelve Lisp verbs, called through `eval`; its `/purse` command; and the human approval the Hermes plugin got from Hermes' prompt.

## Get it

Tell the agent "add the agentpurse cell", or run `nodecode add nc://agentpurse`.

## Set it up

The key goes in the `agentpurse` section of the config: `api_key_env` names the variable that holds it, or `api_key` holds it inline.

```jsonc
{
  "agentpurse": {
    "api_key_env": "AGENTPURSE_API_KEY",
    "base_url": "https://agentpurse.vercel.app",
    "spend_limit_cents": 5000,
    "approval_timeout_seconds": 120
  }
}
```

`spend_limit_cents` caps what one session may capture (integer cents; `0` refuses every purchase). `approval_timeout_seconds` is how long a purchase waits for a person's answer; `0` asks and does not wait. `base_url` is https, or http on localhost for a server of your own.

## Use it

Ask for something to buy. The agent quotes it, then pays only after you say yes:

1. `(purse:get-budget)`, `(purse:search-products "oat milk")`, `(purse:get-quote ...)`. If the guard blocked the quote, the agent stops there.
2. `(purse:request-purchase :quote-id "q-1")` puts the merchant, the amount, the allowance left and this session's spend in front of you as a question. Nothing is sent until you choose **Approve once**.
3. In a shell with no page that answers questions (the TUI alone), the agent gets back a waiting purchase instead: **`/purse approve`** answers it, and the same call then sends it. `/purse deny` drops it.

`/purse` alone opens a panel of what it can do next; `/purse budget`, `/purse receipts`, `/purse pending`, `/purse kill` and `/purse resume` are the rest. The kill switch refuses the next purchase and cancels open price watches.

Prices are integer cents everywhere. A quote pays once; its approval expires in ten minutes. Sandbox money only: a captured receipt is a PayPal sandbox payment, not a retailer order, so never say the item is on its way.

## What is in it

| File | What it is |
| --- | --- |
| `client.lisp` | The AgentPurse MCP over HTTP: one JSON-RPC session per call, a read's retry, a purchase's single attempt, redirects refused. |
| `state.lisp` | `purse/state.json` under the home: the kill switch, the quote cache, each session's cents, and the one-time approval grants. |
| `gate.lisp` | The preview, the hard blocks, and the question put to a person. |
| `verbs.lisp` | The twelve verbs, and the two that reach the server only with an approval. |
| `cell.lisp` | The primer `(help :agentpurse)` answers, `/purse`, and the section declaration. |

## License

MIT. AgentPurse is Copyright (c) 2026 MaAz Khan Hassan: see `LICENSE` and `NOTICE`.
