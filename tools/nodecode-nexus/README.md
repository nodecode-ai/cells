# nodecode-nexus

Nexus Memory: durable facts on a Qdrant collection, guardrails over them. A Nodecode rewrite of [Neboy72/nexus-memory](https://github.com/Neboy72/nexus-memory) by [Neboy72](https://github.com/Neboy72).

The model gets five tools - `nexus_recall`, `nexus_remember`, `nexus_forget`,
`nexus_guardrail_check`, `nexus_find_entities` - over the `nexus` collection
the original keeps, with the same payload fields, so a cell and the original
plugin (or any MCP-connected agent) share one memory. What the collection
holds about the newest ask rides every request as one `nexus` block, so recall
needs no call; a finished turn is filed in the background; `/nexus` shows the
store or searches it.

Two things are its own: a memory carries an access level (`public`, `trusted`,
`private`) and this cell reads only up to the level its section names, with a
memory that has no known level counting as private; and a memory stored under
category `rule` is a protection rule, which `nexus_guardrail_check` matches
against the targets of a destructive command before it runs.

## Get it

Tell the agent "add the nexus cell", or run `nodecode add nc://nexus`.

## Set it up

A Qdrant you run and an embeddings endpoint of the model the collection was
built with:

```jsonc
{
  "nexus": {
    "url": "http://127.0.0.1:6333",
    "collection": "nexus",
    "embedding_url": "http://127.0.0.1:11434/api/embed",
    "embedding_model": "qwen3-embedding:0.6b",
    "api_key_env": "QDRANT_API_KEY",
    "access_level": "private"
  }
}
```

`embedding_url` is the one required member: a vector has to come from
somewhere. Any OpenAI-compatible `/v1/embeddings` endpoint answers it as well
as Ollama's `/api/embed`, and its model must be the one the collection's
vectors were made with - the original's local model is 1024 dimensions. A
Qdrant Cloud store takes its key from `api_key` (or `api_key_env`, or
`QDRANT_API_KEY` in the environment). Nothing else is contacted: the memories,
the vectors and the ids stay in your store.

| Member | Default | What it does |
| --- | --- | --- |
| `url` | `http://127.0.0.1:6333` | the Qdrant REST base URL |
| `api_key` / `api_key_env` | unset | the Qdrant API key, when the store asks for one |
| `collection` | `nexus` | the collection the memories live in |
| `vector_name` | empty | the named vector, when the collection has one |
| `distance` | `Cosine` | what a collection this cell creates uses |
| `embedding_url` | - | required: the embeddings endpoint |
| `embedding_model` | `qwen3-embedding:0.6b` | the model that endpoint serves |
| `access_level` | `private` | the level this agent reads up to and writes at |
| `agent` | `nodecode` | the name stamped on what this cell writes |
| `scope` | `default` | the scope a memory is filed under |
| `recall` | `true` | recall for the newest ask before each turn |
| `recall_limit` | `5` | the most memories one recall or search answers |
| `capture` | `true` | file each finished turn in the background |
| `max_chars` | `400` | how much of a captured turn is stored |
| `retry_seconds` | `60` | how long a store that just failed is left alone |
| `timeout` | `20` | one request's ceiling, in seconds |

## Use it

```
nexus_remember {text: "Prefers green tea over coffee", category: "preference"}
nexus_recall   {query: "what does the user drink"}
nexus_remember {text: "Never delete /srv/protected - production data", category: "rule"}
nexus_guardrail_check {command: "rm -rf /srv/protected"}   -> verdict block, with the rule
nexus_forget   {memory_id: "..."}
```

From eval the same verbs are functions: `(nexus:recall "what does the user
drink")`, `(nexus:remember "..." :category "rule")`, `(nexus:status)`, and
`(help :nexus)` is the primer the model reads.
