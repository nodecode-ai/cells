# nodecode-botsee

BotSee: AI-visibility research through BotSee's hosted MCP server, as two model tools. A Nodecode rewrite of [sidekickjanet/hermes-product-plugins](https://github.com/sidekickjanet/hermes-product-plugins) by [sidekickjanet](https://github.com/sidekickjanet).

## Get it

Tell the agent "add the botsee cell", or run `nodecode add nc://botsee`.

## Set it up

Make an API key in your BotSee account, then paste it into `api_key`, or leave that empty and let the cell read the variable `api_key_env` names (`MCP_BOTSEE_API_KEY` by default).

```jsonc
{ "botsee": { "api_key": "your-botsee-api-key", "url": "https://botsee.io/mcp" } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the BotSee API key; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `MCP_BOTSEE_API_KEY` | the variable read when `api_key` is empty |
| `url` | `https://botsee.io/mcp` | the hosted MCP endpoint this cell speaks to |
| `protocol_version` | `2025-06-18` | the MCP version asked for at initialize; the server's answer wins |
| `timeout_seconds` | `90` | seconds one exchange with the server may take |

A section that names no key refuses the start and says so, and neither tool is
registered until one is set. Then `(restart-cells "nodecode-botsee")`, or set it
in `/settings`. Say `/botsee tools` to try the key.

## Use it

The model gets two tools. `botsee_tools` asks the server what it advertises and
answers one line a tool with the arguments that tool takes; one request, free.
`botsee_call` calls one of them with that tool's own arguments and answers the
server's text.

The server's catalogue is its own and changes without this cell changing, so the
model is told to discover before it calls and never to invent a tool name. Keep the
approval line in view: read-only tools (account, sites, completed runs, results)
are free, while a tool that runs an analysis, generates content, writes a record
or tops the balance up spends the account's credits, so ask for the scope and the
budget first, and use `dry_run: true` for a priced preview where the tool offers
one.

From eval, the same two calls:

```lisp
(botsee:tools)                                  ; what the server advertises
(botsee:tools :refresh t)                        ; ask it again
(botsee:call "run_analysis" "{\"domain\": \"example.com\", \"dry_run\": true}")
```

`(help :botsee)` answers the same guidance the model reads; `*primer*` is the
text, so a layer can reword it. The cell is the MCP client: it discovers and
calls, it does not decide what a BotSee run costs.
