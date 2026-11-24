# nodecode-fatetell

FateTell: Chinese BaZi (Four Pillars) charts, the almanac and saved charts, computed by FateTell's own engine. A Nodecode rewrite of [RussGuo/fatetell-mcp](https://github.com/RussGuo/fatetell-mcp) by [RussGuo](https://github.com/RussGuo).

## Get it

Tell the agent "add the fatetell cell", or run `nodecode add nc://fatetell`.

## Set it up

Casting a chart and reading the almanac need nothing. The tools that read a FateTell account — the saved charts, the Day Master judgment, the daily fortune, the chart calendar — and the one that saves a new chart need a sign-in:

1. Sign in at FateTell and connect an MCP client once, or take the OAuth access token FateTell issues to a connected app.
2. Put the token in the cell's section, `access_token`, or name the environment variable that holds it in `access_token_env`.

Either way it stays out of this folder. `mcp_url` moves every call to another endpoint; `timeout_seconds` bounds one call.

## Use it

Ask for a chart ("a chart for a birth on 1990-05-17 at 08:30 in Taipei"), for the almanac, or for a saved chart's fortune. The model calls:

| Tool | What it answers |
| --- | --- |
| `fatetell_chart` | A birth's chart: pillars with Ten Gods, hidden stems and Na Yin, the Day Master, the luck pillars, the palaces, today's pillars. |
| `fatetell_almanac` | Stems, branches, lunar dates and solar terms for 1 to 31 days. |
| `fatetell_saved` | The account's charts, or one of them in full. |
| `fatetell_judgment` | FateTell's Day Master verdict and the favorable and unfavorable elements. |
| `fatetell_fortune` | One day for a saved chart: energy, relations with the natal pillars, do and avoid. |
| `fatetell_calendar` | The same for the days or months ahead. |
| `fatetell_create` | Save a chart to the account — the only call that writes. |

`/fatetell` takes the operator's own almanac lookup — `/fatetell 2026-01-31 3` — and with no argument says where the cell points. The model also reads `(help :fatetell)`, the original's reading skill condensed: which tool for which question, how to read the pillars, and how to present a chart without predicting events.

Everything is computed on FateTell's server; the cell only carries the call and reads the answer. The same input always gives the same chart.

## Credits

A rewrite of [RussGuo/fatetell-mcp](https://github.com/RussGuo/fatetell-mcp) by RussGuo, at commit `5d81776071ddf897abaf3a65ed9f203ca87372a5`. MIT; see `LICENSE` and `NOTICE`.
