# nodecode-oddsradar

Odds Radar: Polymarket odds, a persistent watchlist and its alerts. A Nodecode rewrite of [WV629/hermes-oddsradar](https://github.com/WV629/hermes-oddsradar) by [WV629](https://github.com/WV629).

## Get it

Tell the agent "add the oddsradar cell", or run `nodecode add nc://oddsradar`.

## Set it up

Nothing to set up. Both Polymarket hosts are public and read-only: no key, no
account, no wallet, no trading. The settings exist if you want them, in the
`oddsradar` section of `config.jsonc`:

| Setting | Default | What it is |
| --- | --- | --- |
| `max_watches` | `50` | the most watches the list may hold |
| `timeout_seconds` | `10` | how long one Polymarket call may wait |
| `movers_min_volume` | `50000` | the 24h volume a market needs before `odds_movers` ranks it |
| `sparkline` | `true` | draw the 7-day sparkline in `odds_market` and `/oddsradar show` |
| `data_file` | `""` | the watchlist file; empty keeps it under this home at `oddsradar/watchlist.json` |

A setting out of range (a `max_watches` of 0) refuses the start, and the
refusal names the setting.

## Use it

Ask for the odds and the model reaches for a tool:

- `odds_search("fed cut")` — active markets with their odds. Prices are
  probabilities, 0-1.
- `odds_market("fed-cut-december")` — one market in full: every outcome, the
  live CLOB midpoint, 24h and 7d moves in points, volume, liquidity, the end
  date, the resolution, and a 7-day sparkline. A polymarket.com link works as
  well as a slug; an event with several markets answers with the slugs to pick.
- `odds_watch("add", market="fed-cut-december", above=60)` — the persistent
  watchlist. `move_pts` alerts when the odds move that many points from the
  last alert (5 when no other rule is given); `above` and `below` alert once
  when the odds cross that percent, and re-arm after a one-point pullback.
- `odds_check()` — every watch evaluated now, answering the alerts that fired:
  moves, crossings, resolutions. Firing resets that watch's baseline, so one
  move alerts once; `peek=true` looks without consuming anything.
- `odds_movers()` — the biggest 24h moves among the most-traded markets.

The operator has the same engine as one command: `/oddsradar show <url|slug>`,
`/oddsradar <query>`, `/oddsradar watch <url|slug> [move=5] [above=60] [below=40]`,
`/oddsradar list`, `/oddsradar unwatch <id>`, `/oddsradar check`,
`/oddsradar movers`. Bare, `/oddsradar` offers a row to type a market in.

The watchlist is one JSON file, written atomically and under one lock, so two
turns — or a turn and a check — never clobber each other, and it survives
restarts. A scheduled `/oddsradar check` (or `odds_check` from a cron prompt) is
the recurring alert: the rules decide, so the check costs no reasoning at all.

A fall prints with its sign (`▼ -18.0 pts`), and an alert names both prices
(`37.5% → 19.5% in 69s`) and the market's page.

Every request is a read-only HTTPS GET to `gamma-api.polymarket.com` (search,
markets, events) or `clob.polymarket.com` (midpoints, price history), with a
10-second timeout; the client refuses any other host. Nothing else leaves the
machine: no key, no telemetry, no subprocess, and the watchlist stays in this
home.

## What is in it

| File | What it is |
| --- | --- |
| `api.lisp` | The two hosts, the one HTTP seam, and the lenient JSON readers. |
| `markets.lisp` | One market, a URL or slug resolved to it, the live price, search, movers, the sparkline and the blocks. |
| `watch.lisp` | The watchlist file, the rules and the alert engine. |
| `cell.lisp` | The five tools, the `/oddsradar` command, the settings and the one declaration. |
| `test/` | Offline tests over a scripted Polymarket. |

## License

MIT, the original's: see `LICENSE` and `NOTICE`.
