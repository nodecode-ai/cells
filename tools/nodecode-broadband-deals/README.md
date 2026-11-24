# nodecode-broadband-deals

FindBroadband Deals: compare the UK broadband deals at an exact address, priced on their true whole-contract cost, and check whether a bill or renewal quote is too high. A Nodecode rewrite of [FindBroadband/mcp](https://github.com/FindBroadband/mcp) by [FindBroadband](https://github.com/FindBroadband).

## Get it

Tell the agent "add the broadband-deals cell", or run `nodecode add nc://broadband-deals`.

## Set it up

Paste the MCP endpoint into the section:

```jsonc
{
  "broadband-deals": {
    "endpoint": "https://www.findbroadbanddeals.com/api/mcp/standard",
    "timeout_seconds": 45
  }
}
```

`endpoint` is the only required setting, and it is the whole of the setup: FindBroadband's server takes no key, no account and no sign-in. It is the same endpoint the plugin's `mcp.json` named. `timeout_seconds` bounds one call, 45 by default.

## Use it

The model gets four tools, the four the server documents:

| Tool | What it answers |
| --- | --- |
| `findbroadband_addresses` | The addresses at a UK postcode, each with its uprn and display address. |
| `findbroadband_deals` | The deals at that property, or across the postcode without one: a cheapest, best value and fastest pick and a provider-varied list, each with its monthly price, contract length, speed and whole-contract cost. Filters: budget, minimum speed, provider, longest contract, no mid-contract rises, mobile and satellite. |
| `findbroadband_overpaying` | A verdict on a bill or a renewal quote against comparable deals at the address, with the saving over 24 months. |
| `findbroadband_deal` | The full terms of one deal from an earlier answer, and whether it is available at the address. |

From eval, or a line the agent runs for you:

```lisp
(broadband-deals:addresses "SS13 1HJ")
(broadband-deals:deals "SS13 1HJ" :max-monthly-price 30 :min-speed-mbps 150)
(broadband-deals:overpaying "SS13 1HJ" 44 100 "Sky" :quote-type "renewal_offer")
(broadband-deals:deal-terms "65571" :postcode "SS13 1HJ")
```

Each answers FindBroadband's own text, unchanged. `/broadband-deals SS13 1HJ` looks the deals up for the operator, `/broadband-deals addresses SS13 1HJ` the address list, and `/broadband-deals` alone says where the cell points.

The numbers are FindBroadband's, computed by its server and passed through as given: `total_contract_cost` and `effective_monthly_cost` already include intro pricing, later rises and setup fees. Switching completes on findbroadbanddeals.com; nothing is bought in the chat.

## What leaves the machine

The postcode and the address you ask about, and the filters, go to the endpoint the section names — findbroadbanddeals.com, which computes every price. Nothing else does: no name, no email, no conversation, no key, and no other host is dialled. The server's terms allow personal, non-commercial use and disallow bulk querying or commercial reuse of its data; use it for your own broadband.

## Test

```sh
.github/test-cell.sh tools/nodecode-broadband-deals
```

14 tests, 0 failed. Every exchange is a stubbed HTTP call over a string stream, so the run is offline: the handshake is sent once a start, each tool's JSON-RPC name and arguments go on the wire, the server's text is the answer, an error answer and a refused status become a repair, an event-stream answer is read to its reply, a session id is echoed, and `/broadband-deals` answers both ways.

## Gaps

- The server also publishes an outage checker at another endpoint. The plugin this cell rewrites named only the deals endpoint, so the cell carries only the deals tools; the section's `endpoint` can be pointed at another MCP server of the same shape, but the tools are the deals four.
- The answer is passed through as the server writes it, JSON text included, because the figures are FindBroadband's and re-rendering them here could drift from them.
