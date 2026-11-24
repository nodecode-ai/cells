# nodecode-fxmacrodata

FXMacroData: macro and FX research data as 100 tools -- indicator history with its release
calendars and source links, policy rates, FX spot and reference rates, positioning,
commodities and seasonality. A Nodecode rewrite of
[fxmacrodata/hermes-fxmacrodata](https://github.com/fxmacrodata/hermes-fxmacrodata) by
[fxmacrodata](https://github.com/fxmacrodata).

## Get it

Tell the agent "add the fxmacrodata cell", or run `nodecode add nc://fxmacrodata`.

## Set it up

Nothing is required. FXMacroData's USD catalogue, indicator history and release calendar are
public, so the tools answer on a fresh install:

```
fxmd_data_catalogue      {"currency":"USD"}          -- the real indicator slugs
fxmd_indicator_history    {"currency":"USD","indicator":"policy_rate","limit":10}
fxmd_release_calendar     {"currency":"USD","start_date":"2026-01-01","end_date":"2026-01-08","timezone":"UTC"}
```

A key lifts the ceiling on the protected datasets. Paste it in `/settings` under the
`fxmacrodata` section's `api_key`, or export the variable `api_key_env` names
(`FXMACRODATA_API_KEY` by default). It rides an `X-API-Key` header on REST and a Bearer
header on MCP, never a URL and never a tool argument. `base_url` and `mcp_url` name the two
hosts the cell dials; `timeout_seconds` (1-120) bounds one exchange.

## Use it

Ask in words: "use FXMacroData to find USD indicators, show policy-rate history and the
next week's releases with sources". Or type `/fxmacrodata policy_rate` for the USD macro
research brief, and `/fxmacrodata` alone for what the cell registered and whether a key
answers.

The whole inventory is 100 tools, every name starting `fxmd_`:

- **the original's 41 REST operations**, `fxmd_health` … `fxmd_stream_events`: catalogue,
  calendar (with its snapshot, coverage and per-event version history), announcements and
  their changes, predictions and their coverage, FX spot, reference, intraday and session
  rates, curves, rate differentials, factors, market sessions, COT and positioning, risk
  sentiment, commodities, settlement contracts and series, press releases, native histories,
  financial prices, and the read-only research-panel POST.
- **the 57 hosted MCP tools**, `fxmd_mcp_*`: macro news, seasonality, source metadata,
  visual artifacts, and the briefing, intel, heatmap, scenario, backtest and regime tasks.
- **two composed tools**: `fxmd_usd_macro_brief` (catalogue, ten history rows and a
  seven-day UTC calendar window in one answer) and `fxmd_indicator_history_snapshot` (every
  page of a requested window, pinned to the first `dataset_version` it sees, refusing a
  changed version, a repeated page or a broken page chain).

Every tool answers one JSON object: `operation`, `data` (the payload exactly as the service
sent it), `records` (an additive row projection), `source_url`, `citations`, `provider_url`,
and `notices` when the free tier delays or narrows a window. From eval the same call is:

```lisp
(fxmacrodata:call-tool "fxmd_data_catalogue" (list "currency" "USD"))
(help :fxmacrodata)
```

A `date` is an observation period, not proof the figure was public then: keep
`announcement_date`, `time_announced`, `publication_time_status` and the capture clocks with
the value, keep a null null, and relay the service's own free-tier notices rather than
presenting a delayed figure as current.

## What it talks to

Only two hosts: `api.fxmacrodata.com` over REST and `mcp.fxmacrodata.com` over MCP, both
named in the section and both replaceable there. Nothing is written to disk, no background
process starts, and there is no telemetry.
