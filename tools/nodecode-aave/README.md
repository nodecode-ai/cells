# nodecode-aave

Aave MCP: live Aave V3 and V4 markets, wallet positions and health factor, DAO governance, and unsigned transactions for the user's own wallet to sign. A Nodecode rewrite of [aave/skills](https://github.com/aave/skills) by [aave](https://github.com/aave).

## Get it

Tell the agent "add the aave cell", or run `nodecode add nc://aave`.

## Set it up

Nothing is required. The cell talks to `https://mcp.aave.com`, Aave's own MCP
server, which asks for no key and holds none: a wallet address in a call is
public blockchain data, and every answer is either public market data, a
simulation, or a transaction the user has not signed yet.

Two settings if you want them, in the `aave` section of the config:

```jsonc
{ "aave": { "endpoint": "https://mcp.aave.com", "timeout_seconds": 30 } }
```

`endpoint` moves the cell to another server speaking the same protocol (a
value that is not an http(s) URL refuses, naming itself); `timeout_seconds`
bounds one call. `(restart-cells "nodecode-aave")` picks up a change.

What leaves the machine: every tool call goes to `mcp.aave.com`, which records
tool usage for reliability and analytics - the tool called and its outcome, the
MCP session id, the client name and user agent. A wallet address is recorded
only as a keyed hash. No private key, seed phrase or other secret is needed
here, and none should be put in a prompt.

## Use it

The model gets the tools Aave's own guide names, and the manual topic that
teaches the order to call them in: discover (`get_markets`), inspect
(`get_user_summary`, `get_user_positions`, `get_reserve_details`), simulate
(`preview_action`), build (`prepare_action`), then hand over **unsigned**.
`(help :aave)` answers the whole primer.

- `get_started` — the server's own map of what it offers and the prompts it
  suggests, for a cold turn.
- `get_markets`, `get_reserve_details`, `get_apy_history` — rates, risk
  parameters and rate history on every chain Aave serves.
- `get_user_summary`, `get_user_positions`, `get_wallet_balances`,
  `get_user_activity`, `get_user_summary_history` — what a wallet holds, what
  it owes, its health factor and what it did.
- `get_user_rewards`, `prepare_claim_rewards` — the rewards claimable now, and
  the unsigned claim.
- `preview_action`, `prepare_action`, `get_repay_with_supply_quote` — a
  simulation with the resulting health factor, and the unsigned transaction
  (with its approval step) for the user's wallet to sign.
- `search_governance_proposals`, `get_governance_proposal` — Aave DAO
  proposals, which take no version: governance is the DAO's, not a market's.
- `get_sgho_vault`, `get_transaction_processed` — the savings vault, and
  whether a sent transaction was processed.
- `aave_call` — any other tool the server serves, by name, including one
  added after this cell was written.

`/aave` reports the endpoint and whether a session is open. `/aave tools`
asks the server what it serves now and names the tools this cell does not
carry (call those with `aave_call`). `/aave forget` closes the session.

From eval, the same server through one function:

```lisp
(nodecode-aave:call-tool "get_markets" (nlk:json-object "symbols" #("USDC")))
(nodecode-aave:tool-names)
```

The tool set is served by Aave's server at run time, so this cell advertises
the everyday surface and reaches the rest through `aave_call` rather than
pinning a wire that would drift.
