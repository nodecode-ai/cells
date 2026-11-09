# nodecode-severian

Severian: cited long-term memory on your own machine, through its HTTP API. A Nodecode rewrite of [Sahil-SS9/Severian](https://github.com/Sahil-SS9/Severian) by [Sahil-SS9](https://github.com/Sahil-SS9).

The model gets five tools — `severian_search`, `severian_ingest`,
`severian_get_observation`, `severian_get_memory` and
`severian_search_shared` — that keep a user's remembered facts in a Severian
server's store and give every hit the observations it came from, `/severian`
to show the scope or search it, and a worker that writes each finished turn
in as one observation.

Severian is the engine; this cell is a client of it. Nothing is bundled: the
cell speaks to a server you run.

## Get it

Tell the agent "add the severian cell", or run `nodecode add nc://severian`.

## Set it up

Run a Severian server of your own (it installs from PyPI as `severian`), then
point the cell at it:

```sh
severian serve --port 8000 --database ~/severian/db.sqlite \
  --fts ~/severian/fts.sqlite --vectors ~/severian/vectors.sqlite
```

The `severian` section of `config.jsonc` (or `/settings`) names that address
and the five scope dimensions every request carries:

```jsonc
{
  "severian": {
    "base_url": "http://127.0.0.1:8000",  // required: your server
    "tenant_id": "nodecode",
    "profile_id": "default",
    "collection_id": "profile",
    "agent_id": "nodecode",
    "session_id": "shared"
  }
}
```

A blank or non-http `base_url` is refused at the start, naming the member, as
are an integer under its floor or a value of the wrong type.

| Member | Default | What it does |
| --- | --- | --- |
| `base_url` | none, required | the address of your running Severian server |
| `api_key` | unset | the Bearer key, when the server runs with auth |
| `api_key_env` | unset | the environment variable holding that key |
| `tenant_id` | `nodecode` | the tenant: one store owner, the first half of the boundary |
| `profile_id` | `default` | the profile: one person or line of work inside the tenant |
| `collection_id` | `profile` | the collection: the original keeps one profile-wide memory and calls it profile |
| `agent_id` | `nodecode` | this agent's identity; writes are stamped with it |
| `session_id` | `shared` | `shared` keeps one memory across chats; a session id keeps one chat apart |
| `search_limit` | `10` | how many memories one search asks for unless the call says otherwise |
| `derive` | `false` | run the server's derivation pass before a search, so a turn just written is searchable at once |
| `capture` | `true` | write each finished turn into the store as one observation |
| `max_chars` | `4000` | how much of each message a captured turn sends, cut at a sentence end |
| `timeout_seconds` | `20` | one request's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea and live in Oslo.

The model calls `severian_ingest`; the server keeps the observation exactly
as it was written and derives searchable memories from it. Later, "what do I
drink?" makes it call `severian_search`, and every hit comes back with its
memory id, its fused score and the observations it was derived from — which
`severian_get_memory` and `severian_get_observation` read back whole.

`/severian` prints the server, the five scope dimensions and the limits;
`/severian where do I live` searches the store from the shell.

With `capture` on (the default), every finished turn is written in the
background as one observation, `user: …` and `assistant: …`, cut at a
sentence end within `max_chars`, so a fact said in passing can be found
later without the model being asked to remember it. Five consecutive
failures pause that path for two minutes; a server that is down never fails
the turn that produced the text.

The scope is the boundary. Every request names a tenant, a profile, a
collection, an agent and a session; the server has no default scope and
never returns another scope's memories. `severian_search_shared` reads the
named agents' scopes as well as this one — each queried on its own, then
merged and re-ranked — with no wildcard and no other tenant.

## The wire

The cell speaks the engine's versioned HTTP API:

- `POST /api/v1/search` — a scope, a query, a limit and `derive`; answers
  the hits with their memory ids, scores and source observation ids.
- `POST /api/v1/observations` — a scope and the content; answers the
  observation id and the derive job it queued.
- `GET /api/v1/observations/<id>` and `GET /api/v1/memories/<id>` — the
  five scope dimensions as query parameters; answers the stored text, its
  hash, its kind and what it came from.
- `Authorization: Bearer <key>` when the server runs with auth.

`severian_search_shared` is `POST /api/v1/search` once per scope: this
agent's, then each listed agent's, asking for `max(limit, 50)` in each,
deduplicated by memory id and re-ranked by fused score, tie broken by
content then id, cut to the limit — the merge the original's
`search_shared` makes.

## Not ported

- **Correct and forget.** The engine's Python service revises an observation
  or replaces a memory in place; its HTTP API does not route either verb, so
  a correction here is a new `severian_ingest` observation, and the server's
  own derivation decides which fact now stands.
- **Task progress.** The original keeps a small task-state note per scope
  (`severian_task_progress`); the HTTP API carries no read or write for it.
- **The in-process context assembler.** The original builds a bounded,
  cited context block before a reply inside the Python process. Over HTTP the
  model calls `severian_search` and reads the hits.
- **Local embeddings and the PostgreSQL backend.** Both are server-side
  choices the cell never sees; it talks to whatever backend that server runs.
- **The Hermes provider lifecycle.** `prefetch`, `sync_turn`, session
  switching and delegation are Hermes's interfaces. Here a finished turn
  arrives on the `:frame` point and there is no per-message prefetch.

MIT, Sahil-SS9's.
