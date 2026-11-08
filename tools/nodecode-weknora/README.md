# nodecode-weknora

WeKnora Retrieval: hybrid search, whole-document reading and cited answers from your own WeKnora knowledge bases. A Nodecode rewrite of [Tencent/WeKnora](https://github.com/Tencent/WeKnora) by [Tencent](https://github.com/Tencent).

## Get it

Tell the agent "add the weknora cell", or run `nodecode add nc://weknora`.

## Set it up

One member is required: the API root of a WeKnora deployment you can reach. Everything else holds a default.

```jsonc
{
  "weknora": { "base_url": "http://localhost:8080" }
}
```

A bare origin gains `/api/v1`. Then `(restart-cells "nodecode-weknora")`, or set it in `/settings`. Ask `/weknora` to see which deployment the cell is talking to and which tools it registered.

| Member | Default | Notes |
| --- | --- | --- |
| `base_url` | none, required | the deployment's API root; a bare origin gains `/api/v1` |
| `api_key` | unset | sent as `X-API-Key`; unset is an unauthenticated deployment |
| `tenant_id` | unset | `X-Tenant-ID`, for a platform-scoped key that picks a workspace |
| `knowledge_base_ids` | `[]` | the default scope; empty means every knowledge base the credential can see |
| `agent_id` | unset | a custom agent id: `ask` then runs WeKnora's ReAct pipeline |
| `max_results` | `8` | passages one search returns, and the ceiling a call may ask for |
| `max_chunk_chars` | `1200` | per-passage character budget, before a passage is clipped |
| `request_timeout` | `30` | seconds one retrieval or document read may take |
| `chat_timeout` | `300` | seconds an answered question may take: it waits on WeKnora's own model |
| `resource_urls` | `public` | `public` returns directly loadable file URLs in citations, `handle` keeps internal `resource://` refs |
| `tool_prefix` | `weknora` | the prefix the four tools take, so two deployments can sit side by side |
| `tools.*` | all on | switch one tool off to spend fewer prompt tokens |

The key is only ever read from the section. It is sent as `X-API-Key` to the address you named and nowhere else, and a refusal never prints it back.

`config.example.jsonc` beside this file is the whole section, commented.

## Use it

Ask about your documents. The four tools are on the model from the first turn, so "what does our handbook say about expense claims" reaches them without being told to.

| Tool | WeKnora endpoint | What you get |
| --- | --- | --- |
| `weknora_list_knowledge_bases` | `GET /knowledge-bases` | the knowledge bases this credential can see, ids and all |
| `weknora_search` | `POST /knowledge-search` + `GET /knowledge/search` | ranked passages verbatim, each with its `knowledge_id`, score and chunk index, plus the documents a title-like query names |
| `weknora_read_document` | `GET /chunks/:knowledge_id` + `GET /knowledge/:id` | one document's passages reassembled in order, page 1 led by its title and WeKnora's summary |
| `weknora_ask` | `POST /sessions` then `POST /knowledge-chat/:id` or `POST /agent-chat/:id` | WeKnora's own answer, its citations, the server-side tools it used, and the `session_id` a follow-up continues in |

`search` is the workhorse: it hands the model the source text to reason over, which keeps the reasoning auditable, and it answers two questions a model cannot always tell apart - where is this discussed, and where is the document called X - in one call. `ask` delegates the whole question to WeKnora, which runs another model server-side: it is slow and returns a conclusion, so it is for broad synthesis over many documents.

Scope works without configuration. An unscoped call is refused by WeKnora's retrieval route and answered from nothing by its RAG one, so when `knowledge_base_ids` is empty the cell resolves every knowledge base the credential can see - once per start, shared by `search` and `ask`.

The same four answers are in the `weknora` package, for a prompt you would rather type in `eval` than put to a model:

```lisp
(weknora:knowledge-bases)                      ; what the credential can see
(weknora:search "expense claim deadline")       ; the passages, verbatim
(weknora:read-document "doc-1" :page 2)        ; a document, a page at a time
(weknora:ask "how do expense claims work?")     ; WeKnora's own answer
```

Each answers the text the tool answers, and a refusal is `WEKNORA-ERROR` with the status and WeKnora's own reason. `(help :weknora)` has the same in the manual.

Nothing here writes to the deployment: no ingestion, no chunk edits, no deletion. A WeKnora key scoped to `retrieve` is enough for three of the four tools; `ask` additionally needs `chat`, because it creates a session and streams an answer.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the `weknora` section, the four argument declarations, the four tools, `/weknora`, `(help :weknora)` |
| `client.lisp` | the REST and event-stream client: envelope, refusals, the public-to-handle fallback, the streamed answer |
| `tools.lisp` | the four verbs and what each answers, as text |
| `test/` | offline tests: the lifecycle, each tool through the dispatch seam against a stubbed wire, and every refusal |

## Licence

MIT. The original plugin is Copyright (C) 2026 Tencent: see `LICENSE` and `NOTICE`.
