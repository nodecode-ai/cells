# nodecode-sgme

SGME memory: tagged long-term recall, skills, wiki and care signals from a self-hosted engine. A Nodecode rewrite of [freehul/sgme](https://github.com/freehul/sgme) by [freehul](https://github.com/freehul).

[SGME](https://github.com/freehul/sgme) is a memory engine you run yourself:
it distils each conversation into tagged, sourced memories, keeps a skills
library and a wiki, and hands back care signals and a profile block — all
behind one HTTP service. The cell is a thin bridge to it, as the original
plugin is: no model, no database, no embedding in here.

The model gets fourteen `sgme_*` tools — search the memory pool, search the
raw words of past sessions, ask an aggregate question, read the wiki, read a
skill, pull and acknowledge a care signal, check the server — plus `/sgme`
to show the server or search it, `(help :sgme)` for the discipline, a block
of the server's profile in each turn's request, and automatic capture: every
finished turn is appended to SGME, which refines it in the background.

## Get it

Tell the agent "add the sgme cell", or run `nodecode add nc://sgme`.

## Set it up

The cell needs a running SGME server and its keys. The `sgme` section of
`config.jsonc` (or `/settings`) names them:

```jsonc
{
  "sgme": {
    "base_url": "http://127.0.0.1:9910",   // the server, here on this machine
    "agent_key_env": "SGME_AGENT_KEY",     // or "agent_key" inline
    "admin_key_env": "SGME_ADMIN_KEY",     // optional: stats, refinement, roles, signals
    "agent_id": "nodecode"                 // this agent's tag on every append
  }
}
```

A server on this machine needs no key to start; one anywhere else needs an
agent key, and a start without one is refused, naming the member. Keys are
read from the environment by default and never written into the folder.

| Member | Default | What it does |
| --- | --- | --- |
| `base_url` | `http://127.0.0.1:9910` | the SGME server's address |
| `agent_key` / `agent_key_env` | unset / `SGME_AGENT_KEY` | the agent key every read and write carries |
| `admin_key` / `admin_key_env` | unset / `SGME_ADMIN_KEY` | stats, refinement, roles and care signals |
| `agent_id` | `nodecode` | the provenance tag on every append and signal pull |
| `inject` | `true` | put the server's profile block into each turn's request |
| `inject_mode` | `daily` | `daily`, `coding`, `work` or `full` |
| `inject_max_tokens` | `800` | the profile block's budget |
| `search_limit` | `5` | results one search asks for unless the call says otherwise |
| `capture` | `true` | append each finished turn to SGME |
| `refine_on_end` | `true` | ask SGME to refine what was appended |
| `timeout_seconds` | `30` | one request's ceiling |

## Use it

Ask about something from an earlier session and the model calls
`sgme_memory_search` before answering — results come back with their ids and
sources, and `(help :sgme)` tells it to say so plainly when the pool has
nothing rather than guess. `/sgme where did we leave the migration` searches
from the shell; `/sgme` alone prints the server, the agent tag and whether
capture and refinement are on.

With capture on (the default), each finished turn is queued to a worker and
appended as one `# <time> user` / `# <time> assistant` block, and a
refinement is triggered after it — both best effort, so an unreachable
server costs a warning and never a turn. The profile and role block comes
back from `POST /v1/inject` and `GET /v1/admin/care/active-role`, and is
recorded once per turn through Nodecode's `:round` point.

## The wire

| Tool | Endpoint |
| --- | --- |
| `sgme_memory_search`, `sgme_conversation_search`, `sgme_skill_search` | `POST /v1/search` (`scopes`: `memory`, `skills`, `wiki`, `wiki_pages`, `sessions`) |
| `sgme_answer` | `POST /v1/answer` |
| `sgme_inject` | `POST /v1/inject` |
| `sgme_wiki_search` / `sgme_wiki_pages` | `GET /v1/wiki/search` / `GET /v1/wiki/pages` |
| `sgme_skill_get` / `sgme_skill_list` | `GET /v1/skills/<name>` / `GET /v1/skills` |
| `sgme_signal_pull` | `GET /v1/events/pull?subscriber_id=<agent_id>` |
| `sgme_signal_claim` / `sgme_signal_ack` | `POST /v1/admin/care/signals/<id>/consume` / `.../ack` |
| `sgme_health` | `GET /v1/health` (no key) |
| `sgme_stats` | `GET /v1/admin/stats` (admin key) |

Every request carries `X-API-Key`; the service does the rest.

## Not ported

- **The rest of the 44 tools.** Roles, the credential vault, the three pools
  (ideas, demands, projects), skill and wiki writes, config and refinement
  administration are on SGME's own console; the fourteen here are what a
  session reaches for. Adding one is a `:tool` clause and a `call`.
- **Per-message recall before a reply.** The original searches on the turn's
  first message; here the model calls `sgme_memory_search` (the tool
  descriptions and `(help :sgme)` say when), and only the profile block and
  capture are automatic.
- **`hermes memory setup` / `status`, and the desktop panel's local
  `$HERMES_HOME/sgme.json` precedence.** The section, `/settings` and
  `/sgme` are the same surface; keys come from the environment or the
  section, never a second local file.
- **One thread per append.** Turns wait in a small queue that a worker
  drains, so a slow server cannot pile up threads.

MIT, freehul's.
