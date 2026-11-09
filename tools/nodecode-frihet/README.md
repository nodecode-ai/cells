# nodecode-frihet

Frihet: invoices, expenses and tax filings through Frihet's MCP, with a gate over irreversible operations. A Nodecode rewrite of [Frihet-io/hermes-frihet](https://github.com/Frihet-io/hermes-frihet) by [Frihet-io](https://github.com/Frihet-io).

## Get it

Tell the agent "add the frihet cell", or run `nodecode add nc://frihet`.

## Set it up

Make an API key in your Frihet workspace, then paste it into `api_key`, or leave that empty and let the cell read the variable `api_key_env` names (`FRIHET_API_KEY` by default).

```jsonc
{ "frihet": { "api_key": "your-key", "mcp_url": "https://mcp.frihet.io/mcp", "ask": true } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the Frihet API key; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `FRIHET_API_KEY` | the variable read when `api_key` is empty |
| `mcp_url` | `https://mcp.frihet.io/mcp` | the endpoint; leave it unless you run a private mirror |
| `timeout_seconds` | `15` | seconds one exchange may take |
| `ask` | `true` | whether an irreversible operation asks you before it runs |

A section naming no key refuses the start and says so, and the tool is not registered until one is set. Then `(restart-cells "nodecode-frihet")`, or set it in `/settings`, and say `/frihet doctor`.

## Use it

Ask the agent to read or draft something in Frihet -- "what is unpaid this month", "draft an invoice for three days of consulting", "send invoice 42" -- and it calls the one tool, `frihet_mcp`, with the Frihet MCP operation and its arguments.

The cell holds no Frihet operation of its own; the endpoint does. What it adds is the operating layer the original plugin added:

- **`/frihet status`** reads locally and dials nothing: the endpoint, whether it is the canonical one, where the key comes from, the timeout.
- **`/frihet doctor`** makes an unauthenticated `initialize` (no credential, no claim) and then one authenticated `tools/list`, and reports `endpoint_reachable`, `authenticated` and `tools_available` apart -- reachable is not authenticated, and neither is ready.
- **`/frihet operations`** lists every operation the endpoint advertises.
- **`/frihet setup`** says where the key goes and what to do after it is there. It never takes a key as an argument, so a key cannot land in a transcript.
- **The gate**: an operation whose name reaches a tax authority, moves money or deletes state (`send`, `markPaid`, `issue`, `delete`, `revoke`, `registerVerifactu`, `applyCreditNote`) is irreversible, and one that matches nothing the cell knows is unclassified. Either way you are asked, Allow once or Deny, and the call runs only on Allow once -- or when the call carries `"confirm": true` after you agreed in words. Reads and draft-writes pass without a question.

From eval, the same calls:

```lisp
(frihet:status)
(frihet:doctor)
(frihet:operations)
(frihet:call-operation "list_invoices")
(frihet:call-operation "get_invoice" :arguments (nlk:json-object "invoice_id" "INV-1"))
```

`(help :frihet)` is the operating contract the original shipped as a skill: read before write, draft first, honour `Idempotency-Key`, reconcile before retry, and Frihet is the authority for what exists. `*primer*` is the text, so a layer can reword it.

The tool's answer is the operation's own text content, cut at 20,000 characters; a key that appears in a failed answer's body is masked before the model sees it. Points to note: the endpoint must speak MCP over streamable HTTP (a plain JSON answer or a `text/event-stream` one both read), and a cell registered this way talks to the endpoint directly, so it uses the section's key where Hermes used its host's MCP client. `nodecode-mcp` stays the cell that serves an MCP server's tools as tools of their own; this one is the Frihet-specific door.

## License

MIT. The original plugin is Copyright (c) 2026 Frihet-io: see `LICENSE` and `NOTICE`.
