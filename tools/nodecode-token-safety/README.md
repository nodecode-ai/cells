# nodecode-token-safety

Grey Ridge token safety: a pre-trade check on a Base ERC-20 — a honeypot, a measured transfer tax, the owner and mint flags, the liquidity, and one verdict. A Nodecode rewrite of [rezearcher/greyridge-token-safety-skill](https://github.com/rezearcher/greyridge-token-safety-skill) by [rezearcher](https://github.com/rezearcher).

## Get it

Tell the agent "add the token safety cell", or run `nodecode add nc://token-safety`.

## Set it up

One setting, no key, no account: `api_base` is the API root the report is asked from. Paste

```
https://x402-data-api.sigrunner.workers.dev
```

in `/settings`, or set it in one line:

```lisp
(config-set '("token-safety") '(:api_base "https://x402-data-api.sigrunner.workers.dev"))
```

Then `(restart-cells "nodecode-token-safety")`. The free preview needs no key and no signup; it is metered at three real-token checks a day per address, and every answer says so.

## Use it

Ask the agent "is this Base token safe?" with the address, or call the tools yourself:

- `token_safety {token}` — the full report: whether a sell goes through (a honeypot reverts it), the transfer tax measured on a plain transfer, `ownerRenounced`, `canMint`, `canPause`, `hasBlacklist`, `upgradeableProxy`, the Uniswap v3 WETH-pool liquidity in dollars, and a `verdict` (SAFE / CAUTION / AVOID) with a `risk_score` and its `reasons`.
- `swap_gate {token, side, notional_usd}` — the same checks sized into a go / hold / block decision for one trade.

A person types `/token-safety 0x…` for the verdict and its reasons in chat, and eval reads the same answer:

```lisp
(token-safety:report "0x4200000000000000000000000000000000000006")
(token-safety:gate "0x4200000000000000000000000000000000000006" :side "buy" :notional-usd 500)
```

Read the verdict plainly: `AVOID` means do not trade and tell the user why; `CAUTION` means tradeable with risk, so show the reasons and let them decide; `SAFE` means no trap these checks find, which is not a guarantee. Base (chain 8453) only, the DEX buy/sell tax is not measured (it reads `null`), liquidity is the Uniswap v3 WETH pool only, and the report is a heuristic from a third-party service, not financial advice.

## What it talks to

The Grey Ridge x402 API at the `api_base` the section names: `GET /chain/token-report/preview?token=0x…` and `GET /gate/swap/preview?token=0x…&side=buy&notional_usd=500`. The token address, the side and the size are the only things sent, and the only things the cell knows. Every answer carries the API's paid route in `paid_route` (0.01 USDC a call through an x402 client on Base); this cell does not pay it.

## Files

| File | What it is |
| --- | --- |
| `cell.lisp` | The two contracts, the two tools, `/token-safety`, the manual topic and the section. |
| `wire.lisp` | The one HTTP seam, the address guard, and the shape an answer takes. |
| `test/cell-test.lisp` | The lifecycle and both tools against a stubbed API, offline. |

MIT. The original is Copyright (c) 2026 Grey Ridge Signals Group LLC: see `LICENSE` and `NOTICE`.
