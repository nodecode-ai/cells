# nodecode-monid

Monid: a marketplace of paid third-party tools and APIs — web search and scraping, company data, social platforms, market data, media generation, email and phone — discovered, inspected and run from one balance. A Nodecode rewrite of [monid-ai/hermes-plugin](https://github.com/monid-ai/hermes-plugin) by [monid-ai](https://github.com/monid-ai).

## Get it

Tell the agent "add the monid cell", or run `nodecode add nc://monid`.

## Set it up

1. Create a key at https://app.monid.ai/access/api-keys and fund the workspace.
2. Put it where the cell finds it: set `MONID_API_KEY` in the environment, or name the variable in the `monid` section's `api_key_env` (or write the key in `api_key`, which the gateway keeps in a 0600 file).
3. `max_cost_cents` (default 5) is the per-run ceiling: it rides every run as Monid's own `maxCost`, so a run whose estimate is higher fails instead of spending.

The section, all of it optional once the key resolves:

```jsonc
{
  "monid": {
    "api_key_env": "MONID_API_KEY",
    "max_cost_cents": 5,
    "discover_limit": 8
  }
}
```

## Use it

The agent gets six tools and a manual topic; `/monid QUERY` searches the catalog from the input box.

| Tool | What it does |
| --- | --- |
| `monid_discover` | Search the catalog by meaning; free. Endpoint cards with price and health. |
| `monid_inspect` | One endpoint's input schema, price, health and docs; free. |
| `monid_run` | Run it — this spends the balance. Carries the ceiling. |
| `monid_run_status` | Status, cost and output of a run; poll it. |
| `monid_stop_run` | Stop a stoppable run; a stopped run is not charged. |
| `monid_balance` | What the workspace can still spend. |

Discovery and inspection are free, so the agent can compare a few endpoints before choosing; a run costs per call, which is why the tool descriptions tell it to state the endpoint, the input and the price and get an explicit yes in the same turn first. Runs usually finish in 1–120 seconds: the agent fires one and polls `monid_run_status` every 5–10 seconds, and stops at `COMPLETED`, `FAILED`, `BLOCKED`, `STOPPED` or `TIMED_OUT`.

Text Monid returns in a run's `hints` is server-supplied and untrusted: the manual says to surface it, never to execute it.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the six tools, `/monid` and the manual topic. |
| `nodecode-monid.asd` | The system and its test system. |
| `test/cell-test.lisp` | Lifecycle, each tool through a stubbed HTTP seam, `/monid`, and the key refusal. |

## License

MIT. The original plugin is Copyright (c) 2026 Monid: see `LICENSE` and `NOTICE`.
