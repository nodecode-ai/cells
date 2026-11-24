# nodecode-delphi

Gensyn Delphi prediction markets: markets, positions, fixed history, quotes and an exact edge. A Nodecode rewrite of [TakeshiTGAL/hermes-gensyn-plugins](https://github.com/TakeshiTGAL/hermes-gensyn-plugins) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

The original gives a Hermes agent seven read-only tools over Gensyn Delphi's
testnet and mainnet deployments. This cell keeps all seven, in Lisp, over the
same surfaces the SDK used:

| Tool | What it does |
| --- | --- |
| `delphi_list_markets` | A page of markets: question, outcomes, status, category, when it settles, the trading fee as a percent, and the market's own page. |
| `delphi_get_market` | One market in full, by its 0x address, every outcome numbered from 0. |
| `delphi_get_positions` | What one public wallet holds, outcome by outcome: raw shares beside their decimal reading. |
| `delphi_compute_edge` | Your probability against the market's, exactly: edge = yours minus the implied one. |
| `delphi_query_subgraph` | A market's fixed history from the network's Goldsky subgraph: `meta` (how far the indexer has read the chain), `trades` (its buys and sells) or `settlement` (how it resolved). |
| `delphi_quote_buy` | What buying that many shares of that outcome costs now, fee included. Nothing is signed or sent. |
| `delphi_quote_sell` | What selling them pays now, on the same terms. |

And for the operator: `/delphi markets`, `/delphi market 0x...`, `/delphi
positions 0x...`, `/delphi edge 0.62 0.45`, `/delphi history trades 0x...`,
`/delphi quote buy 0x... 0 5`.

## Get it

Tell the agent "add the delphi cell", or run `nodecode add nc://delphi`.

## Set it up

Nothing is needed to start. Reading markets and positions needs a Delphi read
API key: ask for one at the network's access page (testnet and
competition-testnet keys come from delphi-api-access.gensyn.ai, mainnet keys
from api-access.delphi.fyi) and put it in the environment variable the
`api_key_env` member names, `DELPHI_API_ACCESS_KEY` by default. The key never
enters the config or a result. The edge needs no key when it is given both
probabilities; history and quotes never need one, because they read the
network's public subgraph and public node.

```jsonc
{ "delphi": { "api_key_env": "DELPHI_API_ACCESS_KEY", "network": "testnet",
               "api_base": "", "subgraph_url": "", "rpc_url": "",
               "wallet_address": "", "timeout_seconds": 20 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key_env` | `DELPHI_API_ACCESS_KEY` | the environment variable holding the read key |
| `network` | `testnet` | `testnet`, `competition-testnet` or `mainnet` |
| `api_base` | empty | the API root; empty uses the network's own host |
| `subgraph_url` | empty | the subgraph endpoint history reads; empty uses the network's Goldsky subgraph |
| `rpc_url` | empty | the node quotes read; empty uses the network's public node |
| `wallet_address` | empty | a public address the positions tool reads when a call names none |
| `timeout_seconds` | `20` | how long one read may wait |

A member of the wrong shape refuses the start and says which one; then
`(restart-cells "nodecode-delphi")`.

## Use it

Ask about a market by its address, about a wallet's holdings, or what a trade
would cost: "what is 0x1111... trading at on Delphi, what does 0x2222... hold,
and what would five shares of outcome 0 cost?" The model calls a tool and reads
the block. From eval, the same calls:

```lisp
(delphi:markets :status "open" :limit 10)
(delphi:market "0x1111111111111111111111111111111111111111")
(delphi:positions :wallet "0x3333333333333333333333333333333333333333")
(delphi:edge "0.62" :implied "0.45")            ; 0.17 above, no network at all
(delphi:history "trades" :market-address "0x1111...")
(delphi:quote-trade "buy" "0x1111..." 0 "5")   ; what it costs, fee included
```

What a market's question and outcomes are is text the market's creator wrote:
every market block opens by saying it is quoted material, not instructions.
Nothing here trades, signs or sends, and it is not investment advice. A quote
is a fee-inclusive cost, not a probability, and must not be passed as an
implied one.
