# nodecode-athletedata

Athletedata: the athlete's own training and health data from 39 apps, through the athletedata MCP server. A Nodecode rewrite of [athletedata-health/mcp](https://github.com/athletedata-health/mcp) by [athletedata-health](https://github.com/athletedata-health).

athletedata is a hosted MCP server for athletes. Connect your training and
health apps once at athletedata.health -- Garmin, Oura, WHOOP, Hevy, Apple
Health, Intervals.icu, Cronometer, Withings, Zwift and more -- then ask the
agent about your own data instead of pasting numbers: activities, training
load (CTL, ATL, TSB), HRV and resting heart rate, readiness, sleep trends,
power and pace curves, race predictions, nutrition and body composition.

The cell speaks the server's own protocol, MCP over Streamable HTTP, with the
core's HTTP seam. It registers two tools:

| Tool | What it does |
| --- | --- |
| `athletedata_tools` | The server's tools, one line each, a `*` marking one that writes. |
| `athletedata_call` | Calls one of them: `tool` is its name, `arguments` its own JSON object. |

The server serves its tool set at run time, so the two verbs outlive any
version of it; the list is asked once and kept for the process.

## Get it

Tell the agent "add the athletedata cell", or run:

```sh
nodecode add nc://athletedata
```

## Set it up

You need an athletedata account (there is a 7-day free trial) and the key on
its MCP page, `sk_soma_...`. Put it in the environment variable the section
names, `ATHLETEDATA_API_KEY` by default:

```sh
export ATHLETEDATA_API_KEY=sk_soma_...
```

or set `athletedata.api_key_env` to another variable's name. The section:

```jsonc
"athletedata": {
  "api_key_env": "ATHLETEDATA_API_KEY",     // the variable holding the key
  "url": "https://mcp.athletedata.health/mcp",
  "timeout_seconds": 60,
  "allow_writes": false                       // true: a writing tool runs without confirm
}
```

A key that is set but refused, or a section with no key at all, is said in the
answer and in the cell's own line; `/athletedata` shows which variable it is
looking at and whether one is set. Every call, with its arguments, goes to
mcp.athletedata.health: that is the whole point of the cell, and nothing else
is sent anywhere.

## Use it

Ask about your training, sleep or recovery; the agent lists the server's
tools and picks one. From eval:

```lisp
(help :athletedata)                         ; the primer the model reads
(nle::find-tool "athletedata_tools")        ; what the model calls
```

or type `/athletedata` for its state and `/athletedata tools` for the list.

Writing tools -- push a workout to the watch, log a meal, create a Hevy
routine -- change the athlete's own accounts. The server marks the tools that
only read; any other one refuses to run until the call carries `confirm: true`
(or `allow_writes` is on), so the agent asks first.

## What is in it

| File | What it is |
| --- | --- |
| `wire.lisp` | MCP over Streamable HTTP: one POST per message, the event stream read when the answer comes as one, the session id echoed. |
| `cell.lisp` | The section, the manual topic, the two tools and `/athletedata`. |
| `test/` | The lifecycle, the missing key, the list, the confirm gate and the session, all offline. |

## License

MIT. The original package is Copyright (c) 2026 HealthspanOS AG: see `LICENSE`
and `NOTICE`.
