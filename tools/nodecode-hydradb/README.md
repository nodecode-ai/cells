# nodecode-hydradb

HydraDB memory: recall before you answer, remember a note, forget one, and capture every turn. A Nodecode rewrite of [hydra-db/hydradb-hermes-plugin](https://github.com/hydra-db/hydradb-hermes-plugin) by [hydra-db](https://github.com/hydra-db).

The model gets three tools — `hydradb_recall`, `hydradb_remember`,
`hydradb_forget` — over HydraDB's long-term memory, `/hydradb` to show the
backend or recall from the shell, and a worker that stores each finished
turn in the background.

## Get it

Tell the agent "add the hydradb cell", or run `nodecode add nc://hydradb`.

## Set it up

The cell needs a HydraDB account: an API key and the database (tenant) it
reads and writes. Either export them, as the original does —

```sh
export HYDRADB_API_KEY="…"
export HYDRADB_DATABASE="my-database"
```

— or write the `hydradb` section of `config.jsonc` (or use `/settings`):

```jsonc
{
  "hydradb": {
    "api_key": "…",               // or api_key_env; HYDRADB_API_KEY is read either way
    "database": "my-database",    // or HYDRADB_DATABASE
    "collection": "my-collection" // optional sub-tenant, or HYDRADB_COLLECTION
  }
}
```

A start with no key and no database is refused, naming the member.

| Member | Default | What it does |
| --- | --- | --- |
| `api_key` | unset | the HydraDB API key; `api_key_env` and `HYDRADB_API_KEY` are read too |
| `database` | `HYDRADB_DATABASE` | the database (tenant) recall reads and capture writes to |
| `collection` | `HYDRADB_COLLECTION` | the sub-tenant inside the database |
| `base_url` | `https://api.hydradb.com` | the API address; `HYDRADB_BASE_URL` is read too |
| `kind` | `knowledge` | the family recall searches, as the original recalls |
| `search_limit` | `6` | how many passages one recall asks for unless the call says otherwise |
| `infer` | `true` | let HydraDB extract facts from what `hydradb_remember` stores |
| `infer_capture` | `false` | let it extract facts from a captured turn too, as the original does not |
| `capture` | `true` | store each finished turn in the background |
| `timeout_seconds` | `20` | one request's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea.

The model calls `hydradb_remember`. Later, "what do I drink?" makes it call
`hydradb_recall`, and HydraDB answers with the passages nearest the
question and their ids — the ids `hydradb_forget` takes. `/hydradb` prints
the database, the collection, the address and whether capture is on;
`/hydradb where do I live` recalls from the shell.

With `capture` on (the default), every finished turn goes to HydraDB in the
background as one `conversation` note, `User: …` then `Assistant: …`,
exactly as the original's `sync_turn` writes it — no extraction unless
`infer_capture` asks for one. Five consecutive backend failures pause the
capture path for two minutes, and the eight newest turns are the ones
queued. `(help :hydradb)` is the page the model reads.

## The three endpoints

HydraDB's public API, and nothing else:

- `POST /query` — hybrid recall, JSON body `{tenant_id, sub_tenant_id,
  query, kind, mode, max_results}`. The answer's `llm_prompt` is used as-is
  when the server renders one; else the `chunks` become the lines the model
  reads, each chunk's stored content envelope unwrapped to its prose and any
  trailing graph enrichment kept.
- `POST /context/ingest` — multipart, `app_knowledge` a JSON array of
  `{id, title, content: {text}, metadata, infer}`; ingestion is
  asynchronous.
- `DELETE /context` — JSON body `{tenant_id, sub_tenant_id, ids, kind}`.

All three authenticate with `Authorization: Bearer <key>`.

## Not ported

- **Recall before a reply.** The original's `prefetch` injects what HydraDB
  finds into the prompt of every turn. Nodecode's standing-section seam
  reads text once per turn and carries no per-message query, so the model
  calls `hydradb_recall` (the tool's description and `(help :hydradb)` say
  when); the capture after a turn is still automatic.
- **`hermes memory setup` / `hermes memory status`.** The section,
  `/settings` and `/hydradb` are the same surface.
- **The MCP server the repository mentions.** `hydradb_recall` is the
  pull-style recall it would have given, without another process.

Apache-2.0, HydraDB's.
