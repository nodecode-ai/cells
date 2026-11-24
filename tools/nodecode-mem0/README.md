# nodecode-mem0

Mem0 memory: semantic recall and durable facts, through Mem0's cloud or a self-hosted server. A Nodecode rewrite of [mem0ai/mem0](https://github.com/mem0ai/mem0) by [mem0ai](https://github.com/mem0ai).

The model gets four tools — `mem0_search`, `mem0_add`, `mem0_update`,
`mem0_delete` — that keep a user's facts on a Mem0 backend and find them
again by meaning, `/mem0` to show the backend or search it, and a worker
that sends each finished turn to Mem0 for extraction.

## Get it

Tell the agent "add the mem0 cell", or run `nodecode add nc://mem0`.

## Set it up

The cell needs a backend, and the `mem0` section of `config.jsonc` (or
`/settings`) names it. Platform mode, Mem0's cloud, needs a key:

```jsonc
{
  "mem0": {
    "mode": "platform",
    "api_key": "…",              // or api_key_env, or just MEM0_API_KEY in the environment
    "user_id": "nodecode-user"   // whose memories these are, the same across sessions
  }
}
```

Self-hosted mode talks to your own Mem0 server (its FastAPI `server/`), and
needs its address; the key only when the server asks for one:

```jsonc
{
  "mem0": {
    "mode": "self-hosted",
    "base_url": "http://127.0.0.1:8000",
    "api_key_env": "MEM0_SERVER_KEY",
    "user_id": "nodecode-user"
  }
}
```

A missing key in platform mode is refused at the start, naming the member;
a wrong `mode` or an integer under its floor is refused the same way.

| Member | Default | What it does |
| --- | --- | --- |
| `mode` | `platform` | `platform` for Mem0's cloud, `self-hosted` for your own server |
| `base_url` | `https://api.mem0.ai` in platform mode; required in self-hosted mode | the API address |
| `api_key` | unset | the Mem0 API key; `MEM0_API_KEY` and `api_key_env` are read too |
| `user_id` | `nodecode-user` | whose memories these are |
| `agent_id` | `nodecode` | the agent id every write is stamped with |
| `rerank` | `false` | rerank search results (platform mode only) |
| `search_limit` | `10` | how many memories one search asks for unless the call says otherwise |
| `capture` | `true` | send each finished turn to Mem0 for extraction |
| `max_chars` | `450` | how much of each message an extraction sends, cut at a sentence end |
| `timeout_seconds` | `20` | one request's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea and live in Oslo.

The model calls `mem0_add`. Later, "what do I drink?" makes it call
`mem0_search` first, and Mem0 answers with the fact and its id — the id
`mem0_update` and `mem0_delete` take. `/mem0` prints the backend, its
address, the user and the agent; `/mem0 where do I live` searches it from
the shell.

With `capture` on (the default), every finished turn goes to Mem0 in the
background, so facts are extracted without the model being asked; the turn's
two messages are cut at a sentence end within `max_chars`, as the original
does for a small embedder's window. A turn whose sync is still busy is
skipped rather than queued forever, and five consecutive backend failures
pause the capture path for two minutes.

## The two wires

- **platform**: `POST /v3/memories/add/`, `POST /v3/memories/search/`,
  `GET`/`PUT`/`DELETE /v1/memories/<id>/`, authenticated
  `Authorization: Token <key>`.
- **self-hosted**: `POST /memories`, `POST /search`,
  `GET`/`PUT`/`DELETE /memories/<id>`, authenticated `X-API-Key` when a key
  is set.

Both are the shapes the original's `PlatformBackend` and `SelfHostedBackend`
speak, and search is always scoped to `filters.user_id` so recall spans
agents and sessions for one principal, with `agent_id` and a
`metadata.channel` on every write.

## Not ported

- **OSS mode.** The in-process SDK — a Qdrant or PGVector store, an OpenAI
  or Ollama model and embedder — is a Python library, and a cell bundles no
  Python. Platform and self-hosted are the two wires carried.
- **Automatic recall before a reply.** The original searches on the turn's
  first message and injects what it finds before the model answers. Here the
  model calls `mem0_search` (the tools' descriptions and `(help :mem0)` say
  when), and only extraction is automatic. The recall seam Nodecode has
  reads standing sections once per turn and carries no per-message query.
- **`hermes memory setup` / `hermes memory status`.** The section, `/settings`
  and `/mem0` are the same surface.
- The PostHog usage events the Python client sends; a request here is the
  request.

Apache-2.0, mem0ai's.
