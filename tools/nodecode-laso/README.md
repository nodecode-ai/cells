# nodecode-laso

Laso Finance: spend USDC on prepaid cards, gift cards and payouts from chat. A Nodecode rewrite of [LasoFinance/hermes-plugin](https://github.com/LasoFinance/hermes-plugin) by [LasoFinance](https://github.com/LasoFinance).

The original gave a Hermes agent 66 tools over Laso's MCP server, one per endpoint
of the Laso API: prepaid cards (US and international), reloadable cards, gift
cards, push-to-card, Venmo and PayPal payouts, ACH bank payments, the banking
application, a managed agent wallet, and x402 payments. This cell reaches the same
66 through two tools, because a model reading 66 schemas spends its window on them:
`laso_tools` searches the catalog, `laso_call` runs one entry.

## Get it

Tell the agent "add the laso cell", or run `nodecode add nc://laso`.

## Set it up

One member is required: `agent_name`, the name your Laso dashboard shows for this
agent.

```jsonc
"laso": { "agent_name": "Nodecode" }
```

Then the agent signs itself in from the conversation, the way the plugin did: its
first Laso call answers a link and a short code, you open the link on any device
(phone, laptop, wherever you are signed in to Laso), check that the page shows the
code, and approve. The next call finishes connecting and saves the key; after that
the tool runs. Nothing needs a browser on the machine Nodecode runs on, so it works
over SSH and from a chat channel.

The connection is an ordinary Laso API key labelled "Hermes" — Laso's own name for
this integration's client document, which the sign-in names itself by. You can see
and revoke it on the Laso agent dashboard; the cell drops a key Laso rejects and
signs in again.

To skip the sign-in, put a `lasoak_` key from the dashboard in the section:

```jsonc
"laso": {
  "agent_name": "Nodecode",
  "api_key_env": "LASO_API_KEY",       // or "api_key": "lasoak_..." written here
  "base_url": "https://laso.finance",  // only for another Laso host
  "timeout_seconds": 120
}
```

The approved key is kept in this home's `laso-api-key` file, readable by you alone.
Nothing else on the machine is read.

## Use it

Ask for what you want in money's terms: "what is my Laso balance", "send $20 to the
coffee vendor on Venmo", "order a $50 Amazon gift card", "put $100 on my reloadable
card". The agent searches the catalog, then calls.

Every tool that moves money, withdraws, deletes a saved recipient or mints a key
refuses until the call carries `confirm: true`: the model has to tell you what will
happen — amount, recipient, product — and pass the flag after you agree. That is the
original's Hermes approval hook, in a cell's own means: 19 of the 66 are gated, the
same 19.

Two things the original disclosed and this keeps. `buy_with_saved_card` charges the
card you saved yourself without a `confirm` flag, because Laso asks for a passkey
approval on your own device before any order. And a paid route (the tools whose
description says PAID ROUTE) settles its USDC price from the agent wallet, so fund
that wallet first: ask for the wallet, its address, and send USDC on Solana.

`/laso` reports the connection; `/laso signin` starts or polls a sign-in; `/laso
signout` drops the key. `(help :laso)` carries the guidance the plugin gave the
model.
