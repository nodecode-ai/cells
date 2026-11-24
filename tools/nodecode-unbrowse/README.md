# nodecode-unbrowse

Unbrowse: read pages, run website tasks and drive a cloud browser through the
hosted Unbrowse MCP. A Nodecode rewrite of
[unbrowse-ai/unbrowse](https://github.com/unbrowse-ai/unbrowse) by
[unbrowse-ai](https://github.com/unbrowse-ai).

Unbrowse is a hosted service that turns a website into callable routes. This
cell gives the model the service's own tools — read a page as markdown, find a
reusable capability for a site, run it, answer what it asks for, and fall back
to a cloud browser when nothing fits — as verbs it calls through `eval`, plus
`/unbrowse` for the account's balance. Everything executes in Unbrowse's
cloud: the learned routes, the sessions, the saved logins.

## Get it

Tell the agent "add the unbrowse cell", or run `nodecode add nc://unbrowse`.

## Set it up

Create an API key at <https://unbrowse.ai/app>, then put it in the cell's
section — in `/settings`, on the settings page, or by asking the agent to set
`unbrowse.api_key`:

```jsonc
{
  "unbrowse": {
    // Unbrowse API key (ub_live_...) or OAuth access token.
    "api_key": "ub_live_...",
    // The hosted MCP endpoint every call goes to.
    "mcp_url": "https://unbrowse.ai/mcp",
    // Org keys only: the user each call is made for.
    "end_user": "",
    // The ceiling on one Unbrowse call, in seconds.
    "timeout_seconds": 180
  }
}
```

`UNBROWSE_API_KEY` (and `UNBROWSE_END_USER`) are read when the section is
empty, which is how the original plugin's environment worked. Without a key
the verbs refuse and say so; nothing else is needed. Calls are billed to the
Unbrowse account, so `(unbrowse:credits)` is the balance.

## Use it

The model learns the vocabulary from `(help :unbrowse)`; every verb is a
Lisp function in the `unbrowse` package, called through `eval`, and every one
answers a string — a refusal is `ERROR: UNBROWSE-ERROR` carrying the
service's own code.

- `(unbrowse:scrape "url")` — one page as clean markdown, main content only.
- `(unbrowse:discover "what you need")` — the capabilities this workspace
  already has; then `(unbrowse:run :capability "id" :input (nlk:json-object ...))`,
  `(unbrowse:inspect "runId")`, and
  `(unbrowse:resume "runId" :answers (nlk:json-object "field" "value"))` for
  what the run asks for. `(unbrowse:sites)`, `(unbrowse:map "url")`.
- No capability? `(unbrowse:browse-open "url")` then
  `(unbrowse:browse-snapshot "sessionId")`, `(unbrowse:browse-act "sessionId" "click" :ref "@e3")`,
  `(unbrowse:browse-finish "sessionId")` to compile what it did into a route.
- Logins: `(unbrowse:credentials-list)`, `(unbrowse:credentials-request "https://site")`
  hands the user a one-time link — no password ever rides a tool argument.
- `(unbrowse:index "url")` learns a whole site in the background;
  `(unbrowse:index-status :job-id "...")` follows it.
- Anything else the workspace has: `(unbrowse:tools)` lists it,
  `(unbrowse:call-tool "unbrowse.some.tool" (nlk:json-object ...))` calls it.
- `/unbrowse` reads the balance and the month's usage; `/unbrowse tools`
  lists the tools this key can call.

A run's status and its verified result are the only proof that something
happened — not a transport success, a screenshot or a plan. `browser_capacity`
means the cloud browsers are busy: wait and retry.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The `unbrowse` package, its condition and its settings. |
| `mcp.lisp` | The hosted MCP: one JSON-RPC POST per call, JSON or SSE answered, the key as a bearer header. |
| `verbs.lisp` | The 24 core tools as verbs, keyword arguments being their input schema. |
| `cell.lisp` | The section, the manual topic, `/unbrowse`, `START-CELL`. |

## Differences from the plugin

- Hermes registered 24 model-facing tools; here the same tools are verbs
  taught by the manual, and `(unbrowse:call-tool ...)` reaches the ones a
  workspace learns later.
- The plugin's `pre_tool_call` guard and its `web_extract` backend redirect
  Hermes' built-in browser and extractor to Unbrowse. Nodecode ships neither,
  so there is nothing to redirect: the manual says to read public pages with
  `unbrowse:scrape`.
- The plugin's settings file, `.env` lookup and per-profile secret scopes
  became one section, with the same two environment variables honoured.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-unbrowse
```

Every Unbrowse endpoint is a stubbed `nlk:http` and the key a stubbed
credential lookup, so nothing reaches the network.

MIT licensed. The service, its tools and their schemas are Unbrowse AI's: see
`NOTICE` and `LICENSE`.
