# nodecode-polymarket

Prediction Market Scout: live Polymarket odds, 24h movers, attention signals, a price-alert watchlist and a one-call briefing. A Nodecode rewrite of [HATWHY4041/prediction-market-scout](https://github.com/HATWHY4041/prediction-market-scout) by [HATWHY4041](https://github.com/HATWHY4041).

Read-only and keyless: the cell reads Polymarket's two public endpoints (Gamma for markets, CLOB for price history) and nothing else, never places an order and never touches a wallet.

## Get it

Tell the agent "add the polymarket cell", or run `nodecode add nc://polymarket`.

## Set it up

Nothing is required: no key, no account, no wallet. Every setting has a default, and the cell runs as it is.

In `/settings` (or the Settings tab), under `polymarket`:

| Setting | Default | What it does |
| --- | --- | --- |
| `min_volume_24h` | `5000` | ignore markets quieter than this over the last 24 hours (USD) |
| `move_pts` | `8` | a 24h change of at least this many points is a big move |
| `closing_hours` | `72` | how far ahead "closing soon" looks |
| `default_limit` | `5` | how many markets each list shows, when the model does not say |
| `wide_spread` | `5` | a bid/ask gap of at least this many points is wide |
| `spike_ratio` | `3` | 24h volume this many times its daily average is a spike |
| `band_low`, `band_high` | `25`, `75` | the price band that counts as still undecided |
| `timeout_seconds` | `10` | one request's ceiling |
| `watchlist` | `polymarket/watchlist.sexp` | where the alert file lives, under the home |

## Use it

Ask in words — "what is hot on Polymarket?", "what moved today?", "the odds on a fed rate cut", "watch that market and tell me above 70" — and the model calls one of six tools:

| Tool | Use it for |
| --- | --- |
| `polymarket_scan` | `trending`, `movers` or `closing_soon` |
| `polymarket_search` | find markets by keyword, before the deep dive |
| `polymarket_market` | one market: every outcome, spread, liquidity, a 7-day price summary, its flags |
| `polymarket_signals` | markets worth a closer look |
| `polymarket_watch` | `add` / `remove` / `list` / `check` alert rules |
| `polymarket_brief` | one finished markdown briefing |

Or type `/polymarket`: alone it answers the briefing and opens a panel of the lists, and `/polymarket trending`, `movers`, `closing`, `signals`, `search <words>`, `market <slug>`, `watch add <slug> above 70` go straight to one.

The same verbs are Lisp, for eval: `(polymarket:scan "movers")`, `(polymarket:market "slug")`, `(polymarket:watch "add" :market "slug" :above 70)`, `(polymarket:brief)`. `(help :polymarket)` is the manual.

A watchlist check reports only the alerts that are new since the last check, so it is safe to put on a schedule — a cron cell turn whose prompt calls `polymarket_watch` with `action=check`, or `polymarket_brief` for a daily message.

The file under the home is rewritten after every change, so a restart loses nothing, and it can be read or edited by hand.

What the numbers mean: a yes price of `0.62` is about a 62% implied chance — a price, not a fact. Points are percentage points of that price. Flags are prompts to look closer; thin markets show prices nobody would trade at. Informational only, not financial advice.
