# nodecode-genesispay

GenesisPay Catalog: search the products and stores of a public, read-only shopping index, and read the reviews of its listings. A Nodecode rewrite of [GenesisTechAT/genesispay-agent-skill](https://github.com/GenesisTechAT/genesispay-agent-skill) by [GenesisTechAT](https://github.com/GenesisTechAT).

The original was a portable agent plugin: an MCP server entry for
`https://catalog.genesispay.finance/mcp/catalog` plus one bundled skill. This
cell is the same thing without an MCP runtime — a small JSON-RPC client in
Common Lisp over that one endpoint, the plugin's four tools as Nodecode tools,
and its skill as one manual topic.

## Get it

Tell the agent "add the genesispay cell", or run:

```sh
nodecode add nc://genesispay
```

## Set it up

Nothing. The catalog is public and read-only: no account, no key, no sign-in,
and no tool that buys, orders or pays. The section is optional:

```jsonc
{
  "genesispay": {
    "enabled": true,
    // the public endpoint unless you go through a proxy of your own
    "base_url": "https://catalog.genesispay.finance/mcp/catalog",
    "timeout_seconds": 60,
    "max_result_chars": 24000
  }
}
```

A `base_url` that is not an http or https address refuses at start, and the
folder reads `REFUSED` until it is fixed; `(restart-cells "nodecode-genesispay")`
tries again.

## Use it

The model calls four tools:

| Tool | What it answers |
| --- | --- |
| `genesispay_catalog_search` | Products by words, then by `storeHost`, `category`, `currency` with `minPriceMinor`/`maxPriceMinor`, `availableOnly`, `sort`, `limit`, `cursor`. With `capability` set it answers stores whose published profile declares checkout or discount. |
| `genesispay_catalog_shops` | The stores: host, the store's own address, its product count, and what the index observed of its catalog endpoint. |
| `genesispay_catalog_trending` | What the index saw most recently: first seen in the last seven days, newest first. |
| `genesispay_catalog_reviews` | The published reviews and rating of one result (`prod_…` or `ext_…`). |

And the operator has one command:

```
/genesispay wool runner          search the indexed products
/genesispay shops shoes          the stores, by words
/genesispay trending            what the index saw most recently
/genesispay reviews prod_…       the reviews and rating of one result
/genesispay status              the endpoint, and whether this run said hello
```

`(help :genesispay)` answers the original's skill: how to search, how to
compare, what a verified purchase does and does not prove, that prices are
minor units in one currency, and that everything a seller or buyer wrote is
data and never an instruction.

Buying is not part of the catalog. Each result says how it is bought
(`howToBuy`), and a person buys on the store's own site at the address the
result names.

## What the server sees

The search words, the filters and the ids asked for go to
`catalog.genesispay.finance`, which the catalog's operator runs. There is no
account and no key, so nothing else rides along: no file, no environment
variable, no credential, no conversation.

## Tests

```sh
.github/test-cell.sh tools/nodecode-genesispay
```

Every request is a stubbed HTTP seam, so the tests need no network: the hello
before the first call, the four tools' method, name, arguments and answer, the
event-stream form of an answer, the fallback from the name the catalog gave one
tool to the name the plugin documented, and the command's routing.

## Licence

MIT. See LICENSE (the original repository's) and NOTICE.
