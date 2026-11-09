# nodecode-hass-statistics

Home Assistant statistics: two read-only tools that list the long-term statistics your Home Assistant instance records and fetch their aggregated history. A Nodecode rewrite of [goncami/hermes-homeassistant-statistics](https://github.com/goncami/hermes-homeassistant-statistics) by [goncami](https://github.com/goncami).

| Tool | What it does |
| --- | --- |
| `ha_list_statistics` | The statistic ids the Recorder holds, each with its unit and source, narrowed by a case-insensitive `search`. |
| `ha_get_statistics` | Aggregated history for one or more ids over a time range: `mean`, `min`, `max`, `state`, `sum` and `change`, by day, hour, week, month or year. |

Ask "how warm was the office last week?", "how much energy did the house use yesterday?", "show me the boiler's history for October" — the model finds the id with `ha_list_statistics` and reads the values with `ha_get_statistics`.

## Get it

Tell the agent "add the hass-statistics cell", or run `nodecode add nc://hass-statistics`.

## Set it up

The cell talks to the instance you already run, over its WebSocket API — statistics are not on the REST API, so this is the endpoint Home Assistant's own dashboard reads.

1. **A long-lived access token.** In Home Assistant: your **Profile** → **Security** → **Long-Lived Access Tokens** → **Create Token**, and copy it.
2. **The instance's base URL**, scheme included, e.g. `http://homeassistant.local:8123` or `https://ha.example.com`.

Then, in `/settings` (or in the `hass-statistics` section of `config.jsonc`):

```jsonc
"hass-statistics": {
  "url": "http://homeassistant.local:8123",
  "token_env": "HASS_TOKEN",   // or a file: "token_file": "~/.config/hass-token"
  "timeout": 30
}
```

A token is named, never written into the config: `token_env` holds the name of the environment variable that carries it (export `HASS_TOKEN` in the shell that starts Nodecode), and `token_file` a file only you can read. Set exactly one.

Optional, and off by default:

| Setting | What it does |
| --- | --- |
| `entity_allowlist` | Statistic ids or patterns (`sensor.office_*`, `energy:*`) the tools are limited to. |
| `entity_denylist` | Patterns the tools always exclude; it wins over the allowlist, and a denied id is refused before anything is sent. |
| `max_ids` | Distinct ids one query may name (100). |
| `max_rows` | The row budget of one answer (10000), estimated before the query and checked again on what came back. |
| `max_bytes` | The output budget of one answer, in bytes (1048576). |

The same `HASS_URL`-style settings can serve `nodecode-home-assistant` beside this cell: both read a `token_env`, and their sections are separate keys.

## Use it

You do not call the tools; the model does, when a question is about history. A person can call the same functions from eval, in the `hass-statistics` package:

```lisp
(hass-statistics:list-statistics (nlk:make-json-object "search" "energy"))
```

`(help :hass-statistics)` is the model's manual for the two verbs, kept while the cell runs.

### What the guards are

- **The token goes to your instance only.** One authenticated WebSocket exchange per call — greet, authenticate, one Recorder command, close — to the URL in the section. Nothing else is dialled, and no error text quotes the token or the URL.
- **The operator's lists are a final veto.** With `entity_denylist` or `entity_allowlist` set, a discovery answer is filtered locally and a query naming an excluded id is refused before the socket opens. Patterns are exact ids, globs (`*`, `?`) and family prefixes, where `.` and `_` are interchangeable at the boundary; a denylist wins over an allowlist.
- **Budgets, not truncation.** A query estimated past `max_rows`, or an answer that actually holds more rows or bytes, is refused with what to narrow — never silently cut.
- **Timestamps need a timezone.** `start_time` and `end_time` are ISO-8601 with an offset, and the end must be later than the start.
- **Read-only.** The two commands, `recorder/list_statistic_ids` and `recorder/statistics_during_period`, read; the cell calls no service and subscribes to no event.

### What the rewrite leaves out

The original reads its settings from the harness's environment (`HASS_URL`, `HASS_TOKEN`, the two lists, the three budgets); here they are the `hass-statistics` section, so a token is named by `token_env` or `token_file` and never written inline. The plugin's own test suite, its `VALIDATION.json` and its manifest checks are the Hermes plugin's, and are not carried over; the cell's tests are its own.

## License

MIT. The tools, their schemas and their guards are the original plugin's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
