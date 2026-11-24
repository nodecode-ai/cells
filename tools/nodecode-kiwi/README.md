# nodecode-kiwi

Kiwi Buyer: find suppliers on the Kiwi network, send RFQs, compare quotes and
negotiate a non-binding agreement. A Nodecode rewrite of
[harrylabsj/hermes-plugin-kiwi](https://github.com/harrylabsj/hermes-plugin-kiwi) by
[harrylabsj](https://github.com/harrylabsj).

## Get it

Tell the agent "add the kiwi cell", or run `nodecode add nc://kiwi`.

## Set it up

Nothing but Node.js and npm on `PATH`: the cell starts the buyer runtime the
original registered, at the same pinned version.

```jsonc
{
  "kiwi": {
    "command": "npx",
    "package": "@harrylabsj/kiwi@0.12.24",
    "timeout_seconds": 120,
    "db_path": ""
  }
}
```

| Key | Default | Purpose |
| --- | --- | --- |
| `command` | `npx` | the launcher that starts the buyer runtime, or a wrapper taking `-y PACKAGE mcp serve` |
| `package` | `@harrylabsj/kiwi@0.12.24` | the pinned runtime package: the tool set follows it, not this page |
| `timeout_seconds` | `120` | the longest one call may take, the first download included |
| `db_path` | empty | the runtime's SQLite store, when it should not keep its own default under the home |

A blank `command` or `package` refuses the start and says which key to fill.
The first call downloads the pinned package; after that it is local.

## Use it

Ask in ordinary words - "find me suppliers for an insulated bottle, then get
quotes for two", "what did the merchants answer", "counter at 12 yuan and
show me the agreement" - and the model calls the runtime's tools:

| Tool | What it does |
| --- | --- |
| `kiwi_search` | suppliers on the Kiwi Network, by short product words |
| `kiwi_request_quotes` | start an RFQ from a purchase intent, with an idempotency key |
| `kiwi_get_task` | the quotes, partial failures and pending approvals of a task |
| `kiwi_negotiate` | counter-offer or clarify with one merchant |
| `kiwi_accept_agreement` | accept the non-binding agreement (asks you first) |
| `kiwi_get_agreement` | the agreement and its audit digest |
| `kiwi_handoff` | the checkout / PO / contact entry point (asks you first) |
| `kiwi_approve`, `kiwi_reject` | answer a pending approval after you agree |
| `kiwi_follow_merchant`, `kiwi_unfollow_merchant`, `kiwi_list_follows`, `kiwi_get_follow_updates` | the merchants followed, and what they changed |
| `kiwi_tools` | what the installed runtime answers, in its own words |

`/kiwi tools` starts the runtime and lists the tools it answers. `(help :kiwi)`
in eval explains the same workflow to the model: search, quote, read,
negotiate, accept, hand off.

Kiwi never pays, orders or locks stock: a handoff is an entry point you finish
yourself. Internet listings, when the session has web tools, are a second,
separately labelled section with the platform and the original link - they are
never mixed into the Network results and never sent as merchants.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the 14 tools, `/kiwi`, the manual topic and the primer |
| `client.lisp` | the runtime as a child process: one JSON-RPC handshake and one call |
| `test/` | the cell's own tests, over a stubbed stdio seam; no network |

Each call starts the pinned runtime, shakes hands over MCP's stdio transport,
makes its one `tools/call` and closes it; the buyer's state lives in the
runtime's own SQLite store, so a session that dies costs nothing.

## License

Apache-2.0. The runtime it starts is the original author's; see `NOTICE` for
what was rewritten and what was left out. `LICENSE` is the original's,
unchanged.
