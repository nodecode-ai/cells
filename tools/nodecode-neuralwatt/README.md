# nodecode-neuralwatt

NeuralWatt: your account's health, model prices, balance, spend, energy, usage, requests and sessions, as nine tools the model can call. A Nodecode rewrite of [0xGingi/neuralwatt-hermes](https://github.com/0xGingi/neuralwatt-hermes) by [0xGingi](https://github.com/0xGingi).

Every view is one or more plain GETs to the NeuralWatt API with the account's
own key, and nothing is written anywhere. The API is content-free: it stores no
prompt or completion text, so what comes back is counts, costs, timings, model
names and session ids.

## Get it

Tell the agent "add the neuralwatt cell", or run `nodecode add nc://neuralwatt`.

## Set it up

One key. Paste a NeuralWatt API key into the section's `api_key`, or export the
variable `api_key_env` names (`NEURALWATT_API_KEY` by default) and leave
`api_key` empty. With neither, the cell refuses at start and says so.

| Member | Default | What it is |
| --- | --- | --- |
| `api_key` | empty | the key, written in the section |
| `api_key_env` | `NEURALWATT_API_KEY` | the variable holding the key, read when `api_key` is empty |
| `base_url` | `https://api.neuralwatt.com` | where the API answers; an `https://` URL, since the key goes wherever it points |
| `timeout_seconds` | `30` | the ceiling on one request |

A gateway of your own in front of the API is what `base_url` is for: that host
then receives the key.

## Use it

Ask, and the model calls the tool:

| Tool | What it answers |
| --- | --- |
| `neuralwatt_health` | the gateway's own status, and the detailed component checks |
| `neuralwatt_models` | the catalog: public models, and the account's own |
| `neuralwatt_prices` | input, output and cached input, USD per million tokens |
| `neuralwatt_quota` | balance, allowance, usage totals, rate-limit tier |
| `neuralwatt_dashboard` | balance, allowance, today's spend, recent energy (`days`) |
| `neuralwatt_energy` | kWh, joules and carbon, with a daily breakdown |
| `neuralwatt_usage` | `summary`, `by_model`, `by_key`, or a cost and latency `compare` |
| `neuralwatt_requests` | a page of request rows, or `top_expensive` |
| `neuralwatt_sessions` | `list`, one session's `detail`, or its `families` |

The same views in the shell:

```
/neuralwatt              the dashboard
/neuralwatt prices       the price table
/neuralwatt requests     the newest request rows
```

And through eval: `(neuralwatt:dashboard)`, `(neuralwatt:prices)`,
`(neuralwatt:usage-view "compare")`, `(neuralwatt:energy :start-date "2026-01-01")`,
`(neuralwatt:sessions-view "detail" :session-id "...")`.

A comparison and a ranking scan the per-request rows a page at a time (200 rows
a page, `max_pages` of them) and say when the window is only the pages they
read, so a partial answer never reads as a whole one.

Run `.github/test-cell.sh tools/nodecode-neuralwatt` from the cells repository.
The tests are offline: every exchange is stubbed.
