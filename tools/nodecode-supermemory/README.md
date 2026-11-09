# nodecode-supermemory

Supermemory: long-term memory that survives sessions, with the four tools the model calls to save, search, forget and profile it. A Nodecode rewrite of [supermemoryai/hermes-supermemory](https://github.com/supermemoryai/hermes-supermemory) by [supermemoryai](https://github.com/supermemoryai).

The model gets four tools — `supermemory_store`, `supermemory_search`,
`supermemory_forget`, `supermemory_profile` — that keep a user's memories in
one Supermemory container and find them again by meaning, `/supermemory` to
show the container or search it, and a worker that writes each finished turn
to the container as a conversation, so facts are extracted without the model
being asked.

## Get it

Tell the agent "add the supermemory cell", or run `nodecode add nc://supermemory`.

## Set it up

The cell needs a Supermemory API key — a key is made at
<https://console.supermemory.ai/keys?create=true> — and the `supermemory`
section of `config.jsonc` (or `/settings`) names it:

```jsonc
{
  "supermemory": {
    "api_key_env": "SUPERMEMORY_API_KEY",   // or "api_key" inline
    "container_tag": "nodecode"
  }
}
```

`SUPERMEMORY_API_KEY` in the environment is read either way. A missing key is
refused at the start, naming the member; a wrong `search_mode`, an integer
under its floor and a bad `base_url` are refused the same way.

| Member | Default | What it does |
| --- | --- | --- |
| `api_key` | unset | the Supermemory API key |
| `api_key_env` | unset | the environment variable holding it (`SUPERMEMORY_API_KEY` is read too) |
| `base_url` | `https://api.supermemory.ai` | the API address; a self-hosted server answering the same shapes goes here |
| `container_tag` | `nodecode` | the data partition: the same tag across sessions finds the same memories |
| `search_mode` | `hybrid` | `hybrid` (memories with their source chunks), `memories` or `documents` |
| `search_limit` | `10` | how many memories one search asks for unless the call says otherwise |
| `capture` | `true` | write each finished turn to the container |
| `entity_context` | the built-in | what a capture is worth extracting, at most 1500 characters |
| `timeout_seconds` | `20` | one request's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea and live in Oslo.

The model calls `supermemory_store`. Later, "what do I drink?" makes it call
`supermemory_search` first, and Supermemory answers with the memory and its id
— the id `supermemory_forget` takes.

`/supermemory` prints the container, the search mode and whether capture is
on; `/supermemory where do I live` searches memory from the shell.

With `capture` on (the default), every finished turn is queued in the
background and written as a document in the `[role: user]…[user:end]` layout
of the original, one document per session per 4-hour window, so the API
appends a session's turns to one document instead of starting another. The
entity context tells Supermemory what in a conversation is worth remembering.
A batch the service refused stays queued, and the queue keeps the newest 50
turns (256 KiB) so a service that is down cannot grow it forever.

## The wire

- `POST /v3/documents` with `content`, `containerTags`, `metadata` (stamped
  `sm_source: nodecode`), `customId` and `entityContext`.
- `POST /v3/search` with `q`, `containerTag`, `limit` and `searchMode`.
- `POST /v4/profile` with `containerTag` and, when the tool got one, `q`.
- `DELETE /v4/memories` with `id` and `containerTag`.

Every request carries `Authorization: Bearer <key>` and `x-sm-source: nodecode`.
The source stamp is the original's routing key: in the Supermemory app it
puts a write in that source agent's Space, so a user can filter and
bulk-manage what this cell wrote.

## Not ported

- **Recall before a reply.** The original searches on the turn's first message
  and injects a `<supermemory-context>` block before the model answers. Nodecode's
  recall seam reads standing sections once per turn and carries no per-message
  query, so here the model calls `supermemory_search` (the tools' descriptions
  and `(help :supermemory)` say when), and only capture is automatic.
- **The system-prompt block** the original prepends, and with it the
  `profile_frequency` throttle on the static/dynamic profile split; the
  `supermemory_profile` tool and the help line carry the same guidance.
- **The kebab-case tool aliases** (`supermemory-save` and friends). Nodecode's
  catalog has one name per tool, and the four snake_case names are the
  original's own.
- **Custom containers.** The original whitelists extra container tags that a
  tool call may name; here one section names the container the cell reads and
  writes, and `{identity}` templates are not expanded.
- **`hermes memory setup` / `hermes memory status`.** The section, `/settings`
  and `/supermemory` are the same surface.

MIT, supermemoryai's (the LICENSE is the repository's own, Copyright (c) 2025
Nous Research).
