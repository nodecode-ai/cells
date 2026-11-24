# nodecode-telemetry

Telemetry: what every session spent — tokens, cost, latency and tool calls — in a local ledger, with daily and monthly caps that refuse a tool call once they are gone. A Nodecode rewrite of [nujovich/hermes-telemetry](https://github.com/nujovich/hermes-telemetry) by [nujovich](https://github.com/nujovich).

## Get it

Tell the agent "add the telemetry cell", or run `nodecode add nc://nodecode-telemetry`.

## Set it up

Nothing is required: with no section at all the cell records in `observe` mode and enforces nothing. To choose, the section goes in `~/.nodecode/config.jsonc` (or `/settings`):

```jsonc
{
  "telemetry": {
    "mode": "enforce",
    "daily_usd": "5",
    "monthly_usd": "100",
    "session_usd": "0"
  }
}
```

| setting | default | what it does |
| --- | --- | --- |
| `mode` | `observe` | `observe` records and warns the model; `enforce` refuses a tool call once a cap is spent |
| `daily_usd` | `5` | the US-dollar ceiling for one UTC day; `0` turns it off |
| `monthly_usd` | `100` | the ceiling for one calendar month; `0` turns it off |
| `session_usd` | `0` | one session's own daily ceiling — a cron fire or a busy chat lane — `0` for none |

The caps are written as text so a fraction is a fraction (`"0.50"` for a test run), and a value that is not a number is refused at start, naming the member. `/budget set daily 10` writes the same member into the shared config and moves the running cap at once; `enforce` is off by default because recording is the half every user wants and refusing is the half that should be asked for.

## Use it

There is nothing to call. Once the cell is on:

- every provider round appends one row: the provider and model, input, output, cache-read, cache-write and reasoning tokens, the cost the core priced (`cost_known` says whether it could), the round's time to first token and stream time;
- every tool call appends one row: the tool's name, its wall time, and whether it failed (an `ERROR:` result or a signal);
- every refusal appends one row too, so `/stats` can say what a cap cost you.

`/stats [today|week|month|sessions|tools|models|raw]` reads it — one line for a window, or the heaviest sessions, tools or models, newest rows for `raw`:

```
today · $1.2345 over 12 rounds · in 45,600 · out 3,200 · cache 120,000 · 34 tool calls (2 failed) · 12.3s of tool time
```

`/budget` says where the windows stand and takes the caps:

```
budget (enforce): today $1.23 of $5.00 · this month $12.30 of $100.00 · BLOCKED on daily · days reset 00:00 UTC
```

`/budget set daily 10`, `/budget set monthly 100`, `/budget set session 2`, and `/budget mode observe|enforce`.

In `enforce` mode a call past a cap answers the model with one sentence instead of running:

```
budget: every tool call is refused while the daily cap of $5.00 is spent (today $5.12) — /budget shows the windows, /budget set daily N raises the cap, and mode observe records without enforcing.
```

In `observe` mode every call runs and the model is told, once a turn, how much of the cap is gone. The rounds already billed still land: the gate stops work, it does not undo spend.

The model can ask for the same line itself, through the `telemetry` tool (`window`: `today`, `week`, `month`, `sessions`, `tools`, `models`). `(help :telemetry)` in the manual says the same as this section.

Nothing leaves the machine: the ledger is `telemetry/ledger.jsonl` under the home, one JSON object a line, appended and never rewritten, read and folded when the cell starts. Deleting it is the whole reset. The core's own per-turn budget (`nle:turn-budget`) caps a turn's calls, tokens and seconds; this cell caps money across sessions and days, and the two compose.

Run `.github/test-cell.sh tools/nodecode-telemetry` from the cells repository. The tests cover the lifecycle, one round becoming one row with the core's price, a failed and a signalling tool call still rowed, enforce refusing a call past a cap while observe lets it through, a session cap that blocks one session alone, `/budget set` and `/budget mode`, every `/stats` window, and a ledger that outlives the cell that wrote it.
