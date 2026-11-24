# nodecode-qdrant-memory

Qdrant memory: durable facts in a Qdrant store, recalled by meaning. A Nodecode rewrite of [cnkang/hermes-plugin-qdrant-memory](https://github.com/cnkang/hermes-plugin-qdrant-memory) by [cnkang](https://github.com/cnkang).

The model gets four tools — `qdrant_memory_search`, `qdrant_memory_add`,
`qdrant_memory_update`, `qdrant_memory_delete` — that keep a user's facts in a
Qdrant collection of their own and find them again by meaning. What the store
holds about the newest ask rides every request as one `qdrant-memory` block, so
recall needs no call; a finished turn is filed in the background, so a fact
said once is not lost; `/qdrant-memory` shows the store or searches it.

## Get it

Tell the agent "add the qdrant-memory cell", or run `nodecode add nc://qdrant-memory`.

## Set it up

Two endpoints, both your own:

```jsonc
{
  "qdrant_memory": {
    "url": "http://127.0.0.1:6333",                 // a Qdrant you run (its REST port)
    "embedding_url": "http://127.0.0.1:11434/api/embed",  // Ollama, or any OpenAI-compatible /v1/embeddings
    "embedding_model": "nomic-embed-text",
    "collection": "nodecode_memory"
  }
}
```

`collection` is created on the first add, at the vector size the embedding
endpoint answers, so the two ends must agree on the model. A Qdrant Cloud
store or one behind auth takes its key from `api_key` (or `api_key_env`, or
`QDRANT_API_KEY` in the environment). Nothing else is contacted: the memories,
the vectors and the ids stay in your store.

| Member | Default | What it does |
| --- | --- | --- |
| `url` | `http://127.0.0.1:6333` | the Qdrant REST base URL |
| `api_key` / `api_key_env` | unset | the Qdrant API key, when the store asks for one |
| `collection` | `nodecode_memory` | the collection the memories live in |
| `distance` | `Cosine` | the distance the collection is created with |
| `embedding_url` | required | the embeddings endpoint |
| `embedding_model` | `nomic-embed-text` | the model that endpoint serves, which sets the vector size |
| `namespace` | `default` | the memory space these tools read and write |
| `recall` | `true` | search for the newest ask before each turn, and put what it finds in the request |
| `recall_limit` | `5` | the most memories one recall or one search answers |
| `capture` | `true` | file each finished turn's ask in the background |
| `max_chars` | `400` | how much of a captured turn is stored |
| `retry_seconds` | `60` | how long a store that just failed is left alone |
| `timeout` | `20` | one request's ceiling, in seconds |

A missing `embedding_url`, or a URL without a scheme, is refused at the start,
naming the member; a Qdrant or an embedding endpoint that cannot be reached is
a refusal on the call, and a recall that fails is a turn that goes on without
it.

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea and live in Oslo.

The model calls `qdrant_memory_add` and gets the id. Later, "what do I drink?"
finds the fact in the prefetched block, or through `qdrant_memory_search`;
`qdrant_memory_update` corrects it by id (the new text is re-embedded, so it is
found by the new wording), `qdrant_memory_delete` forgets it. An identical
fact is not stored twice: the add answers the id that already holds it.

`/qdrant-memory` prints the store, the collection and how many memories it
holds; `/qdrant-memory where do I live` searches it from the shell.

From eval, the same four verbs are plain functions in the `qdrant-memory`
package: `(qdrant-memory:search "tea" :limit 3)`, `(qdrant-memory:add
"Prefers green tea")`, `(qdrant-memory:update ID :text "…")`,
`(qdrant-memory:delete ID)`, and `(qdrant-memory:status)`.

## The wire

Qdrant's REST API, as the original's Python client spoke it:
`GET`/`PUT /collections/<name>`, `PUT /collections/<name>/points?wait=true`,
`POST /collections/<name>/points/search` (and `/points/query` when a server
has dropped the older route), `POST /collections/<name>/points/scroll` for the
duplicate check, `POST /collections/<name>/points/payload` to stamp a memory,
`POST /collections/<name>/points/delete`. A point carries its text, its
canonical form, the namespace, its source and two timestamps; every search and
every scroll is filtered on the namespace.

## Not ported

- **The Python runtime.** An embedded Qdrant and a bundled ONNX embedder are
  replaced by the store and the embeddings endpoint you already run; the SQLite
  admission ledger and its crash replay are replaced by writing the point.
- **LLM extraction.** The original distils a turn into facts with the host's
  model. A cell takes no second model call, so a captured turn is filed as the
  operator's own sentence, and the tool descriptions ask the model for one
  self-contained fact.
- **The shared owner service**, its device grants, the standalone CLI and the
  stdio MCP server: other front ends for the same runtime, not the plugin's
  agent surface.

MIT, Kang Liu's.
