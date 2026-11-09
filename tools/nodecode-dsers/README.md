# nodecode-dsers

DSers: dropshipping work from chat — find suppliers, publish listings, set prices, fulfill orders and track parcels. A Nodecode rewrite of [dsers/dsers-agent-plugin](https://github.com/dsers/dsers-agent-plugin) by [dsers](https://github.com/dsers).

## Get it

Tell the agent "add the dsers cell", or run `nodecode add nc://dsers`.

## Set it up

DSers serves its own tools over a remote MCP endpoint, so the cell needs the
access token that endpoint issues for your account. An Advanced, Pro or
Enterprise DSers plan includes it; the Basic plan does not.

1. Authorize the DSers MCP connection once (the service documents its OAuth
   sign-in at <https://www.dsers.com/en/mcp-server>) and keep the access token.
2. Put it where the cell reads it, in the `dsers` section of
   `~/.nodecode/config.jsonc`: `"token"` written inline, or `"token_env"`
   naming an environment variable that holds it.
3. `(restart-cells "nodecode-dsers")`, then ask `/dsers tools`.

`config.example.jsonc` shows the section. The endpoint defaults to the
service's own server and can be pointed elsewhere for a regional account.

## Use it

- **`dsers_tools`** lists the tools this account may call, each with the
  first line of what it does and its arguments. The list is the server's, not
  a fixed one: a plan, a store or a channel decides what appears.
- **`dsers_call`** calls one by name with its own arguments, e.g.
  `(dsers_call "list_stores")`, and answers the tool's text.
- **`/dsers`** shows the endpoint in use and whether a token is there;
  **`/dsers tools`** lists the tools.
- **`(help :dsers)`** is the workflow: what DSers connects, which reads are
  free of consequences, which writes spend money or publish, and what each
  error code means.

The tools cover the store overview, supplier and product search, the Import
List, price and profit checks, supplier mapping, order diagnosis and
placement, and parcel tracking. Writes — publishing a listing, changing a
price, placing a supplier order, cancelling one, sending tracking — are the
account's own operations; the cell asks the model to show what will change
and wait for your yes first.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The two verbs, the `/dsers` command, the manual topic, the standing note, and the JSON-RPC exchange with the server |
| `test/cell-test.lisp` | Offline tests; every exchange is stubbed |
| `config.example.jsonc` | The section, documented |
| `NOTICE`, `LICENSE` | Credit to the original plugin, and its licence |

It speaks MCP's streamable HTTP transport: `initialize`, `tools/list`,
`tools/call`, one session at a time, re-initialized when the server drops it.
