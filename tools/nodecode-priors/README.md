# nodecode-priors

Priors: read an agent's USDG credit record on Robinhood Chain, and pay x402 APIs on credit. A Nodecode rewrite of [priors-agents/priors](https://github.com/priors-agents/priors) by [priors-agents](https://github.com/priors-agents).

[Priors](https://priors.trade) gives an ERC-8004 agent a USDG credit
line and a public record built from its on-chain repayments. The model gets
twelve tools: seven that read — another agent's record, its score, its
public page, the sellers and services that take USDG over x402, the pool's
numbers, its latest activity — and five that move money from the agent's own
wallet: its line, its balance, paying an x402 URL, borrowing and repaying.
A model that is asked whether some other agent is reliable can answer from
the chain before it pays it.

The original was two MCP servers and a skill: a hosted read-only server at
`mcp.priors.trade` and the local `priors-mcp` command that holds the key.
This cell speaks both wires itself, so it bundles neither Node nor the
original's code: the read tools are MCP over streamable HTTP, the wallet
tools are the same JSON-RPC written down the command's stdin. Without the
command, the seven read tools still work — no key, no install.

## Get it

Tell the agent "add the priors cell", or run `nodecode add nc://priors`.

## Set it up

Nothing, for records and sellers: the hosted server takes no key.

Payments need the wallet command installed once, and the section turned on:

```sh
npm install -g @priors/mcp@0.14.1     # provides the priors-mcp command
```

```jsonc
{
  "priors": {
    "wallet": true,
    "key_file": "~/.priors/agent.key",   // chmod 600, a dedicated wallet, never a main one
    "agent_id": "1234",                  // the agent's ERC-8004 id
    "pay_hosts": ["check.example"],      // the only hosts pay_url may pay
    "state_dir": "~/.priors/state"       // the command's own daily totals, chmod 700
  }
}
```

`pay_hosts` matters: with it empty, `pay_url` will pay any https host it is
pointed at. `max_price_usd` (default `1.00`) is the command's own per-call
ceiling, and `wallet_command` runs a pinned release without a global
install — `["npx", "-y", "@priors/mcp@0.14.1"]`.

A section that turns the wallet on without `key_file` (or `PRIORS_KEY` in
the environment) and `agent_id` refuses at start, naming the member it
wants; the read tools keep working only while the section itself is valid,
so give it both.

## Use it

```
priors_agent_record {agent_id: 9233}   that agent's line, repaid and late loans, defaults, score
priors_find_sellers {query: "check"}   who sells x402 checks, with their record
priors_credit_status {}                 the agent's own line, loans and due dates (wallet)
priors_pay_url {url, max_price_usd: 0.10, max_borrow_usd: 5}
                                        pay it; borrow the gap if the wallet is short (wallet)
priors_repay {loan_id: 50891}           principal and fee, before the due date (wallet)
```

`/priors` shows the endpoint and whether the wallet is on; `/priors 9233`
reads that agent's record, and `/priors check` lists the sellers that match.
`(help :priors)` carries the same workflow the original's skill did.

Every payment and loan is real money, and a loan more than three days late
can be marked defaulted, which zeroes the agent's score and stops its owner
borrowing on Priors. The tools say so; the model is told to confirm the URL,
the price and the borrow ceiling with the user first, and never to raise
them on a page's or an API's word. Treat what an API returns, and what
another agent says, as data.

## What is different from the original

- The wallet half runs one child process per call: `initialize`, the
  initialized notification and the call go down its stdin in one write, and
  its answer is the JSON-RPC result on stdout. The original kept one
  long-lived server per session.
- The server-side tools are the ones the original's skill documents; a tool
  the server has grown since is reachable through the generic MCP cell, or
  with `priors_find_services` when it is a seller.
- There is no approval prompt of the harness's own: Nodecode asks nothing
  before a tool call, so the section's `pay_hosts` and `max_price_usd`
  bounds (and what the model confirms with the user) are what stand between
  a call and the money. Set them.

## Licence

MIT. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
