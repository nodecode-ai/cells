# nodecode-reme

ReMe Memory: automatic long-term memory across sessions. A Nodecode rewrite of [agentscope-ai/ReMe](https://github.com/agentscope-ai/ReMe) by [agentscope-ai](https://github.com/agentscope-ai).

Before each turn the cell asks a ReMe action service what it holds about the
operator's newest ask, and what comes back rides the request as one `reme`
harness block. When a turn completes, the cell sends the exchange back, so
ReMe can file it and extract from it. Nothing is written twice: the memory
lives in the service, on this machine or wherever it runs.

## Get it

Tell the agent "add the reme cell", or run `nodecode add nc://reme`.

## Set it up

The cell talks to a ReMe action service, so one has to be listening. ReMe is
an open-source, file-based memory framework; run its service the way its own
documentation says (`pip install "reme-ai[core]"`, then `reme serve`) and it
listens on `http://127.0.0.1:2333` by default.

Then name it in the `reme` section of `~/.nodecode/config.jsonc` (or in
`/settings`):

```jsonc
"reme": {
  "endpoint": "http://127.0.0.1:2333"
}
```

`endpoint` is the only required setting:

| Setting | Default | What it does |
| --- | --- | --- |
| `endpoint` | — | the base URL of the ReMe action service |
| `recall` | `true` | ask before each turn, and put what it finds in the request |
| `record` | `true` | send each completed turn back for extraction |
| `recall_limit` | `5` | the most notes one recall may carry |
| `recall_timeout` | `5` | seconds one search may take before the turn goes on without it |
| `request_timeout` | `60` | seconds one recording or start may take |
| `health_timeout` | `2` | seconds one health check may take |
| `retry_seconds` | `30` | how long a service that just failed is left alone |

`/reme health` says whether the service answers at all; `/reme` shows the
endpoint in use and what last happened.

## Use it

- **Automatically.** Nothing to type. Recalled notes appear in the request
  under `reme`; each completed turn is recorded once, off the publishing
  thread, by one writer thread the cell starts and stops.
- **By asking the model to recall.** The `reme_recall` tool takes a query and
  a limit and answers what the service holds, which the model calls when a
  question turns on something an earlier session may have learned.
- **From eval.** `(reme:recall "deploy key")` searches now,
  `(reme:health)` checks the service, `(reme:status)` reports the settings.
- **`/reme`** shows the status and a panel; `/reme recall QUERY` searches
  now; `/reme health` checks the service.

A service that is down is not a failed turn: the request goes out without a
`reme` block, and the cell says so in `/reme` and leaves the service alone for
`retry_seconds` before trying again.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the :FRAME observer, the harness-section advice, the verbs, `reme_recall` and `/reme`. |
| `client.lisp` | The three ReMe actions (`health_check`, `search`, `auto_memory`) over `nlk:http`, and the scoped session id. |
| `package.lisp` | The package, the condition and the live tables. |
| `test/cell-test.lisp` | Twelve offline tests: every action is a stubbed HTTP seam. |

## License

Apache-2.0, the original's. ReMe is Copyright 2025 Alibaba Group: see
`LICENSE` and `NOTICE`.
