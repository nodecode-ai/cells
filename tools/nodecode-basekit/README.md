# nodecode-basekit

Basekit: read-only Base (chain 8453) chain reads - network and gas in dollars, wallet portfolios, transaction receipts, token facts, transfers and approvals - as six tools the model calls. A Nodecode rewrite of [D0xedDevi0/hermes-basekit](https://github.com/D0xedDevi0/hermes-basekit) by [D0xedDevi0](https://github.com/D0xedDevi0).

## Get it

Tell the agent "add the basekit cell", or run `nodecode add nc://basekit`.

## Set it up

Nothing is required. Out of the box the cell reads Base through a keyless
public endpoint (`https://base-rpc.publicnode.com`), prices with DefiLlama and
falls back to Coinbase's spot ETH price. The `basekit` section is optional:

| Member | What it is |
| --- | --- |
| `rpc_url` | your own node or provider (Alchemy, QuickNode, Coinbase); the public default caps `eth_getLogs` ranges |
| `watch_address` | the wallet `/basekit wallet` reads when no address is typed |
| `timeout_seconds` | one request's ceiling, 20 by default |
| `log_window` | the default block window for transfers and approvals, 20000 by default (at most 50000) |
| `basescan_api_key` | only adds `base_token`'s source-code check; without it that field says `not_configured` |

`/basekit` with no argument shows the chain state it is reading.
`(help :basekit)` is the model's guide to the tools.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `base_network` | head block, gas and base fee, ETH/USD, and the dollar cost of a transfer, an ERC-20 transfer and a swap |
| `base_portfolio` | ETH and ERC-20 balances with USD values for one address |
| `base_tx` | one transaction decoded: status, block and time, from/to, fee in ETH and USD, every token movement and approval |
| `base_token` | name, symbol, decimals, supply, owner(), real proxy detection (EIP-1967, EIP-1167, `implementation()`) and selector hints |
| `base_activity` | transfers in and out of one address for one token, inside a stated block window |
| `base_allowances` | approvals still live on one token, each re-read on-chain, with the USD at risk |

From the keyboard: `/basekit status`, `/basekit full` (adds the endpoint),
`/basekit wallet <address>`, `/basekit token <address>`,
`/basekit activity <address> <token>`.

**Nothing here signs.** There is no private key, no transaction path and no
key needed: basekit reads, and the user decides. Every answer names the block
it was read at, and the two log-based tools state the block window they
actually scanned - a transfer outside it is not in the answer.

### It is the plugin's six tools

The original registered six agent tools, a `/basekit` command, a CLI command,
a skill and a prompt hook. The six tools keep their names and their arguments,
and the answers keep the plugin's honesty rule: the block is quoted, an
unpriced token says so rather than guessing, and bytecode hints are labelled
as hints.

## Not ported

- **The `pre_llm_call` hook.** It injected one cached line of chain state,
  gated on Base-related words in the user's message. A cell's round hook does
  not see that message here, so the same guidance lives in `(help :basekit)`
  and in the tools' own descriptions ("call base_network before quoting a
  fee"). A model that follows them pays the same one call and nothing on
  unrelated turns.
- **The ledger, the CLI command and the desktop pane/status chip.** The ledger
  was a JSONL file of past reads under the plugin's data folder; the CLI and
  the pane were Hermes surfaces. `/basekit` and the tools answer the same
  questions without a file to keep.
- **The automatic window narrowing.** When a busy token blew the endpoint's
  result cap, the original halved the log window and retried. Here a rejected
  window is one error line telling the model to narrow `blocks` or set its own
  `rpc_url`, which is one call instead of up to seven.
- **Multicall3.** The original batched contract reads through the canonical
  Multicall3 deployment; this cell sends them as one JSON-RPC batch, which is
  the same one round trip without an ABI encoder for `aggregate3`.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-basekit
```

Every request is a stubbed `nlk:http`: the RPC endpoints answer a small fixed
chain and DefiLlama answers two prices, so nothing reaches the network and no
key is needed.

MIT licensed, as the original is.
