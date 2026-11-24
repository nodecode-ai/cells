# nodecode-broadband-outage

Internet Outage Checker: whether a UK broadband provider is down right now, where, what the provider has said and how the last 30 days went. A Nodecode rewrite of [FindBroadband/mcp](https://github.com/FindBroadband/mcp) by [FindBroadband](https://github.com/FindBroadband).

## Get it

Tell the agent "add the broadband-outage cell", or run `nodecode add nc://broadband-outage`.

## Set it up

Paste the MCP endpoint into the section:

```jsonc
{
  "broadband-outage": {
    "endpoint": "https://www.findbroadbanddeals.com/api/mcp/outages",
    "timeout_seconds": 45
  }
}
```

`endpoint` is the only required setting, and it is the whole of the setup: FindBroadband's server takes no key, no account and no sign-in. It is the same endpoint the plugin's `mcp.json` named. `timeout_seconds` bounds one call, 45 by default.

## Use it

The model gets three tools, the three the server documents:

| Tool | What it answers |
| --- | --- |
| `findbroadband_outage_overview` | Every UK provider FindBroadband tracks: Down, Issues earlier or Working fine, the place where the provider named one, the headline, days with incidents in the last 30, and a link to each provider's page. |
| `findbroadband_outage_status` | One provider: the status and headline, current and recent past incidents in the provider's own words with the places they named, a day by day picture of the last 30 days, and links. |
| `findbroadband_outage_history` | The incidents of the last 30 days, of a shorter window (`days`, 1 to 30) or of one day (`date`, YYYY-MM-DD), with a count per provider when none is named. |

From eval, or a line the agent runs for you:

```lisp
(broadband-outage:overview)
(broadband-outage:provider-status "BT")
(broadband-outage:history :provider "Virgin Media" :days 7)
```

Each answers FindBroadband's own text, unchanged. `/broadband-outage` says where the cell points, `/broadband-outage BT` asks for one provider, `/broadband-outage overview` for every one, and `/broadband-outage history BT 7` for a provider's last week.

Two things the answer says and the tools' descriptions repeat: `status_based_on` tells whether the status rests on the provider's own status page or on public reports the provider never confirmed, and there is no outage data for individual addresses — only what has been reported and where. The history reaches back 30 days.

## What leaves the machine

A provider name, a day count or a date — never a postcode, an address, a name or the conversation — goes to the endpoint the section names, findbroadbanddeals.com, which holds the data. No key travels, and no other host is dialled. The server's terms allow personal, non-commercial use and disallow bulk querying or commercial reuse of its data; use it for your own broadband.

## Test

```sh
.github/test-cell.sh tools/nodecode-broadband-outage
```

15 tests, 0 failed. Every exchange is a stubbed HTTP call over a string stream, so the run is offline: the handshake is sent once a start, each tool's JSON-RPC name and arguments go on the wire, the server's text is the answer, a day outside 1 to 30 and a date of another shape are refused before any request, an error answer and a refused status become a repair, an event-stream answer is read to its reply, a session id is echoed, and `/broadband-outage` answers both ways.

## Gaps

- The answer is passed through as the server writes it, JSON text included, because the statuses and the headlines are FindBroadband's and re-rendering them here could drift from them.
- Only the outage endpoint is here. FindBroadband's deals server is a cell of its own (`broadband-deals`).
