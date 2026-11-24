# nodecode-foresee

Foresee: home and auto insurance quotes across carriers, with optional live premiums read back from the carriers' own quote forms. A Nodecode rewrite of [Foresee-Tech/foresee](https://github.com/Foresee-Tech/foresee) by [Foresee-Tech](https://github.com/Foresee-Tech).

## Get it

Tell the agent "add the foresee cell", or run `nodecode add nc://foresee`.

## Set it up

Nothing is required. The cell talks to Foresee's own hosted server,
`https://agents.go-foresee.com/mcp`, and Foresee asks for no key and no sign-in.

Set a member in `/settings` (or the page's Settings tab) under `foresee` only
if you want to change something:

| Member | What it does |
| --- | --- |
| `endpoint` | another MCP address, for a proxy of your own; `https` anywhere, plain `http` only to this machine or a private network |
| `timeout_seconds` | one call's ceiling, 120 by default |
| `max_chars` | how much of one answer the model reads, 6000 by default |
| `api_key`, `api_key_env` | a bearer key, only for a server that asks for one |

`/foresee` shows the endpoint, the session and the tools the server
advertises, and `(help :foresee)` is the model's guide to the two tools.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `quote_insurance` | instant estimates from every carrier Foresee prices: a monthly point estimate and confidence interval each, the cost of each coverage, a price ladder per limit or deductible, and the carrier's own quote url |
| `live_carrier_quotes` | Foresee's agents complete the carriers' own quote forms and read back the premium the page printed — with the user's explicit, verbatim go-ahead |

From the keyboard, `/foresee` (or `/foresee live` to narrow it) shows the
connection and what the server offers there.

A quote needs a ZIP, an age (or date of birth) and, for auto, each vehicle's
year, make and model; the coverage asked for goes with it as numbers
(`{"auto": {"bi": "100/300", "pd": 100, "coll_deductible": 500}}`), and
anything left out comes back as a declared assumption. Foresee prices
estimates; it does not bind or take payment, and the purchase finishes on the
carrier's own site.

A live walk sends the person's details on to the named carriers, which may make
a soft credit-based insurance score check and may email or phone. The cell
sends nothing until the user has said yes in the conversation, and the tool
passes their go-ahead verbatim. Never an SSN.

### The two tools are the plugin's two tools

The original was a portable manifest plus one `mcp.json` entry pointing at
Foresee's hosted server, and three skills describing how to shop with it. This
cell speaks MCP's streamable HTTP itself — one JSON-RPC POST per exchange, the
session id the initialize issued echoed on every later request, the answer read
as a JSON body or from the SSE frame it arrives in — so it needs no MCP
transport library, no local server and no Node. The skills are the manual
topic, `(help :foresee)`.

## Not ported

- **The remote schema.** The plugin let the server describe its own tools; the
  two argument lists here are declared in the cell, from the skills' own
  description of them, so the model is shown the same shape without a network
  round trip before its first call.
- **The three separate skills.** They are one manual topic, in the order a
  quote is run: what to gather, how to read the answer, how to present it.
- **The bundle's `plugin.json` marketplace fields** (brand colour, composer
  icon, default prompts) — a chat surface's business, not a tool's.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-foresee
```

Every exchange is a stubbed `nlk:http` behaving like a small MCP server, so
nothing reaches Foresee and the run needs no network and no key.

MIT licensed, as the original is.
