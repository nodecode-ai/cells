# nodecode-lastscanned

LastScanned: track parcels and ocean containers across 549 carriers, and compare USPS, UPS and FedEx rate quotes. A Nodecode rewrite of [dragonhuntr/lastscanned-plugin](https://github.com/dragonhuntr/lastscanned-plugin) by [dragonhuntr](https://github.com/dragonhuntr).

The model gets four tools that speak to the service's own MCP endpoint,
`https://mcp.lastscanned.com/mcp`:

| Tool | What it does |
| --- | --- |
| `track_package` | Status, estimated delivery and scan history for one tracking or container number |
| `track_packages` | The same for up to 100 numbers at once, at most 35 per carrier |
| `get_rates` | USPS, UPS and FedEx quotes between two US postal codes; quotes only |
| `list_carriers` | The carrier ids the tracking tools accept; free |

`/lastscanned NUMBER` tracks one, `/lastscanned carriers` lists the ids and
`/lastscanned rates FROM TO WEIGHT` quotes a parcel; with no argument it says
where it points. `(help :lastscanned)` is the model's page.

## Get it

Tell the agent "add the lastscanned cell", or run `nodecode add nc://lastscanned`.

## Set it up

Nothing is required. Without a key the service answers within its free daily
allowance: one tracking lookup and five quotes a day, and a request from a
cloud or hosting network gets no free tracking lookup — a hosted agent wants a
key.

A key from [app.lastscanned.com/keys](https://app.lastscanned.com/keys) uses
your plan. Put it in the environment:

```sh
echo 'LASTSCANNED_API_KEY=…' >> ~/.nodecode/.env
```

or in the cell's section, which `/settings` writes:

```jsonc
{
  "lastscanned": {
    "api_key_env": "LASTSCANNED_API_KEY",   // or "api_key" written inline
    "base_url": "https://mcp.lastscanned.com/mcp",
    "timeout_seconds": 30
  }
}
```

Each tracked number and each quote counts toward the plan or the credits,
which is why the manual tells the model to look up only the numbers it was
asked about and not to poll one in a loop.

## Use it

Ask where a shipment is, or name the number:

- "where is 1Z999AA10123456784?"
- "track MSKU1234567 — it is a Maersk box"
- "what does a 2 lb 10x8x6 parcel cost from 10001 to 90210?"
- "which carriers can you look up?"

Container numbers work in the tracking tools for Maersk (including Hamburg
Süd), MSC, COSCO, Evergreen, ONE and Yang Ming. A leased container — the
`TGHU` and `TCNU` prefixes — sails with any line, so the model asks which
line rather than guessing. On a container, `delivered` means the empty box
came back, not that the cargo arrived. Watches and webhooks belong to the
service's REST API, not to this cell.
