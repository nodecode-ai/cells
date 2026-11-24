# nodecode-hyatlas

HyAtlas memory: an eight-layer, self-hosted memory server, recalled and captured through its HTTP API. A Nodecode rewrite of [tuancookiez-hub/HyAtlas-Memory](https://github.com/tuancookiez-hub/HyAtlas-Memory) by [tuancookiez-hub](https://github.com/tuancookiez-hub).

HyAtlas is a separate program: `hyatlas-go`, a single Go binary that runs on
your machine, keeps memories in eight layers in an embedded vector store, and
builds a bitemporal knowledge graph over them. In `lite` mode it makes no AI
call at all; in `pro` and `ultra` it sends the memory text you capture to the
AI endpoint the server is configured with, and extracts facts, intentions,
summaries and entity pages from it.

The cell is the client side, as the plugin was:

- four tools the model can call — `hyatlas_search`, `hyatlas_recent`,
  `hyatlas_add`, `hyatlas_status`;
- `/hyatlas status|search <query>|add <text>|recent`, and alone, the server's
  health;
- **recall before a turn**: at the first round of every turn the server is
  searched with that turn's own message, and the nearest memories — the Now
  page, entity pages, profile, open intentions, task experiences and facts,
  each with its layer, its dates and its score, and the versions it replaced —
  arrive in the round about to be asked;
- **capture after a turn**: the turn's user and assistant text is sent to the
  server for extraction, best effort, from a worker of its own;
- `(help :hyatlas)`: the manual topic the model reads.

## Get it

Tell the agent "add the hyatlas cell", or run `nodecode add nc://hyatlas`.

## Set it up

The HyAtlas server has to be installed and running first (its own repository's
`scripts/install.sh`, or a release binary; it listens on `127.0.0.1:19528`). The
cell needs its address, and the `hyatlas` section of `config.jsonc` (or
`/settings`) names it:

```jsonc
{
  "hyatlas": {
    "base_url": "http://127.0.0.1:19528",  // required: where the server listens
    "user_id": "default",                  // whose memories these are
    "agent_id": "default"                  // the agent id every write is stamped with
  }
}
```

The rest is optional: `recall` and `capture` (both on) turn the two halves of
the turn off and on, `search_limit` (10), `recall_limit` (5) and
`recent_limit` (20) bound the queries, and `timeout_seconds` (15) bounds one
request. The server needs no key; it answers only its own machine.

## Use it

Ask the model something that depends on what it knows about you: the recall
block already carries the memories nearest your message. When it needs more it
calls `hyatlas_search` (a query; optionally one layer and a limit), or
`hyatlas_recent` to see what was just saved. `hyatlas_add` stores a context
dump; `hyatlas_status` says whether the server is healthy, what mode it runs
in, and how many rows each layer holds.

`/hyatlas` alone shows the server's health; `/hyatlas search what do I prefer`,
`/hyatlas add ...` and `/hyatlas recent` do what the tools do.

Memory is whatever the server keeps: nothing is stored in the cell, and
stopping the cell stops recall and capture, not the server.
