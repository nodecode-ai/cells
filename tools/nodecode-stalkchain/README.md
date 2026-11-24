# nodecode-stalkchain

StalkChain: crypto research on coins, traders and wallets, in plain English. A Nodecode rewrite of [StalkC/stalkchain-plugin](https://github.com/StalkC/stalkchain-plugin) by [StalkC](https://github.com/StalkC).

Ask it whether a coin looks safe before buying it, who a trader really is before copying them, what several top traders bought in the last hour, what a wallet holds, and tell it to email you when a token crosses a price. It reads public on-chain data across Solana, Ethereum, Base, BNB Chain, Monad, Hyperliquid and Robinhood Chain, plus the StonkFun launchpad. It never trades, moves funds or signs anything; the only thing it changes is your own StalkChain alerts.

## Get it

Tell the agent "add the stalkchain cell", or run `nodecode add nc://stalkchain`.

## Set it up

You need a StalkChain account with credits. Copy the API key from [data.stalkchain.com/dashboard](https://data.stalkchain.com/dashboard) and either export it:

```sh
export STALKCHAIN_API_KEY=...            # read from the environment by default
```

or put it in the cell's section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "stalkchain": {
    "api_key_env": "STALKCHAIN_API_KEY"   // or "api_key": "..." written inline
  }
}
```

`/stalkchain` with no argument then asks the server how its data is -- a free call that spends no credits -- and answers with the endpoint and the health line. Every other call spends the account's credits; the tool descriptions say when a call is a cheap one.

## Use it

- "Is this coin safe to buy?" with a contract or mint address goes to `stalkchain_token_check`: buy and sell flow, holder concentration, the dev's and insiders' positions, and which tracked traders hold it.
- "Should I copy this trader?" goes to `stalkchain_trader_report`: the wallets that belong to them, profit by window, win rate with the sample size behind it, current positions.
- "What did the top traders buy in the last hour?" goes to `stalkchain_smart_money`.
- "What does this wallet hold, and what in it looks risky?" goes to `stalkchain_wallet`.
- "Email me when BONK goes above $0.0001, then show my alerts, then delete that one" goes to `stalkchain_alerts`.
- Anything else in the catalogue goes to `stalkchain_tool` by name: `stalkchain_token_onchain` for a Solana safety check, `stalkchain_token_quality` for organic versus wash volume, `stalkchain_fomo_search` for the cheapest lookup by handle or ticker, `stalkchain_fomo_leaderboard`, `stalkchain_token_exit_check`, and the `stonk_*` family for StonkFun.

`/stalkchain <handle or ticker>` searches traders and tokens without spending a model turn.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The MCP client (streamable HTTP, one session, one request at a time), the six tools, `/stalkchain`, and the `(help :stalkchain)` primer that carries the original's research doctrine. |
| `nodecode-stalkchain.asd` | The system and its test system. |
| `test/cell-test.lisp` | Offline tests: the section, the session lifecycle, each tool's frame and answer, the command. |
| `config.example.jsonc` | The section, commented. |

The primer is where the original's eighteen skills went: cheapest data first, a missing value is null and never zero, tracked traders are a curated set and not every holder, facts rather than recommendations, and alerts only after you confirm the token, the target and where it goes.

## Gaps

- **Sign-in is by API key.** The original's browser OAuth flow needs a browser; the key comes from the same account and spends the same credits.
- **The skills are prose, not files.** A cell contributes tools and a manual topic, so the eighteen skills and three agents are folded into the primer and the tool descriptions rather than loaded as skill files.
- **Answers are text.** The original's interactive cards (switchable charts, a leaderboard you can page in place) are a Claude and ChatGPT surface with no terminal counterpart.
- **The server's tool list is served at run time,** so a tool StalkChain adds appears to `stalkchain_tool` without a new release of this cell, and one it retires stops answering.

## Test

```sh
.github/test-cell.sh tools/nodecode-stalkchain
```

No network: every request is a stubbed `nlk:http`, so no key is spent.

## License

MIT. The original is Copyright (c) 2026 StalkChain: see `LICENSE` and `NOTICE`.
