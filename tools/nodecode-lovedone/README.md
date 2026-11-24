# nodecode-lovedone

LovedOne Family Inbox: a family's shared inbox for an aging parent, as model tools. A Nodecode rewrite of [Terahertz-Inc/lovedone-mcp](https://github.com/Terahertz-Inc/lovedone-mcp) by [Terahertz-Inc](https://github.com/Terahertz-Inc).

LovedOne gives each loved one a private address for doctors, pharmacies,
insurers and attorneys, and one inbox the whole family sees. The original
plugin pointed an agent at LovedOne's hosted MCP server; this cell speaks the
same JSON-RPC to the same endpoint with the image's own HTTP call, so the
model gets the same reads and the same writes without an MCP runtime.

## Get it

Tell the agent "add the lovedone cell", or run `nodecode add nc://lovedone`.

## Set it up

Sign in at LovedOne, open Settings -> MCP keys, and make a key. Then paste it
into `api_key`, or leave that empty and let the cell read the variable
`api_key_env` names (`LOVEDONE_API_KEY` by default).

```jsonc
{ "lovedone": { "api_key": "your-mcp-key", "timeout_seconds": 30 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the family MCP key; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `LOVEDONE_API_KEY` | the variable read when `api_key` is empty |
| `url` | `https://api.lovedone.app/mcp/v1` | the Family Inbox MCP endpoint |
| `timeout_seconds` | `30` | seconds one request may take |

The key can read a whole family workspace, and answers can carry health
information. A section that names no key refuses the start and says so. After
setting one, `(restart-cells "nodecode-lovedone")` or `/settings`. Say
`/lovedone` to try it.

## Use it

Ask about the family's mail: "summarize this week's messages in Mom's inbox
and who is handling each one", "what did the pharmacy send", "invite my
brother". The model calls the tool that answers, gets the server's own text,
and quotes it.

```lisp
(lovedone:call "list_loved_ones")                      ; who the workspace holds
(lovedone:call "list_inbox_messages" (nlk:json-object "loved_one_id" "…"))
(lovedone:call "ping")                                 ; does the key work
```

| Tool | What it does |
| --- | --- |
| `lovedone_ping` | the key works, and who it authenticated as |
| `lovedone_profile` | the signed-in person and their family workspace |
| `lovedone_list_loved_ones` | the loved ones: id, name, Family Inbox address |
| `lovedone_list_family_members` | who is on the workspace, their role and invite state |
| `lovedone_list_inbox_messages` | one loved one's recent messages, with senders and previews |
| `lovedone_get_inbox_message` | one message as plain text |
| `lovedone_list_care_events` | recent care events, one loved one or the workspace |
| `lovedone_invite_family_member` | **writes:** invite a family member by email |
| `lovedone_create_provider_code` | **writes:** mint a short-lived provider link code |
| `lovedone_reply_to_inbox_message` | **writes:** send a plaintext reply to a message |
| `lovedone_call` | any other tool the server offers, by the server's own name |

`/lovedone` shows the workspace and its loved ones; `/lovedone <loved-one-id>`
shows that loved one's recent inbox. `(help :lovedone)` answers the same
guidance the model reads; `*primer*` is the text, so a layer can reword it.

This is senior tech support and family coordination, not healthcare: no
diagnosis, no "patient". The write tools change a real family's shared
workspace, so the primer has the model confirm before one runs — a Hermes
install also had the harness ask, which a cell cannot enforce. The tools here
are the ten the original's README documents; one the server adds later is
reached with `lovedone_call`, so the cell never dials just to learn a schema.
