# nodecode-xai-collections

xAI Collections: search the documents you indexed in xAI Collections. A Nodecode rewrite of [Julientalbot/hermes-xai](https://github.com/Julientalbot/hermes-xai) by [Julientalbot](https://github.com/Julientalbot).

Collections is xAI's own store for documents a team uploaded to
`console.x.ai`: they are chunked, indexed and searchable by meaning. The cell
gives the model one read-only tool, `xai_collections_search`, that returns
the best-matching passages of a query with their file title, page and score,
and a `/collections` command that lists or searches the same thing from the
shell. Nothing is uploaded, created or deleted here; upload documents in
xAI's console.

## Get it

Tell the agent "add the xai-collections cell", or run `nodecode add nc://xai-collections`.

## Set it up

The cell talks to `https://api.x.ai/v1` with an xAI API key — the same key
Hermes-style xAI users already have in `XAI_API_KEY`. The `xai-collections`
section of `config.jsonc` (or `/settings`) says where the key is and what a
search defaults to:

```jsonc
{
  "xai-collections": {
    "api_key_env": "XAI_API_KEY",   // the variable holding the key, or "api_key" inline
    "default_collection_ids": [],   // collection_<uuid> ids; empty searches every collection
    "default_limit": 5,              // passages per search unless the call says otherwise
    "timeout_seconds": 30
  }
}
```

A start with no key — none inline, none in `api_key_env`, none in
`XAI_API_KEY` — is refused, naming the member to set.

| Member | Default | What it does |
| --- | --- | --- |
| `api_key` | unset | the xAI API key, written inline |
| `api_key_env` | unset | the variable holding it; `XAI_API_KEY` is read either way |
| `default_collection_ids` | `[]` | `collection_<uuid>` ids searched when the model passes none; empty means every collection of the team |
| `default_limit` | `5` | passages one search returns unless the call says otherwise, at most 20 |
| `timeout_seconds` | `30` | one request's ceiling |

## Use it

Ask about something you uploaded:

> What does the lease say about renewal?

The model calls `xai_collections_search` with the question, and gets back
the passages, each with its file title, page and score — cite those titles.
`collection_ids` narrows a search to named collections (ids come from
`list_collections`), `limit` asks for 1–20 passages, and `filter` takes
xAI's AIP-160 metadata syntax (`year > 2024 AND type = "contract"`).

From the shell, `/collections` lists the collections with their ids and
document counts, and `/collections renewal` searches them and prints the
matches.

## The two wires

Each call is one HTTPS request with `Authorization: Bearer <key>`, both on
`https://api.x.ai/v1`:

- `GET /collections` — the team's collections (`collection_id`,
  `collection_name`, `documents_count`).
- `POST /documents/search` — `{query, source: {collection_ids}, limit, filter?}`
  answering `matches[]` of `file_id`, `chunk_content`, `score`,
  `page_number` and `fields.title`. Each passage is clipped at 2000
  characters in the answer.

With no ids in the call and none in the section, the cell lists the
collections first and searches all of them, as the original does; a team
with no collection gets one line saying so.

The key rides only to `api.x.ai`, and never appears in a tool result.

## Not ported

- **Grok Voice.** The plugin's other half is a full-duplex voice call: a
  button in Hermes Desktop's composer, a backend route minting a 300-second
  client secret, and a WebSocket of microphone audio to `wss://api.x.ai`.
  Nodecode has no desktop realtime composer to hang it on, so only the
  Collections module is carried.
- The plugin's `hermes plugins` install path and its settings page; the
  cell's own section and `/settings` are that surface here.

MIT, the original's.
