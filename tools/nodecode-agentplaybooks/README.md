# nodecode-agentplaybooks

AgentPlaybooks Memory: recall, explicit memory writes, archive and history in a private playbook. A Nodecode rewrite of [matebenyovszky/agentplaybooks](https://github.com/matebenyovszky/agentplaybooks) by [matebenyovszky](https://github.com/matebenyovszky).

The model gets six tools — `apb_memory_search`, `apb_memory_read`,
`apb_memory_write`, `apb_memory_archive`, `apb_memory_delete`,
`apb_memory_history` — that keep durable facts in one private playbook on
[AgentPlaybooks](https://agentplaybooks.ai) and find them again by keyword,
`/agentplaybooks` to show the playbook or search it, and a round hook that
searches what the turn's own words say and lets what matches ride that
request.

## Get it

Tell the agent "add the agentplaybooks cell", or run `nodecode add nc://agentplaybooks`.

## Set it up

Create a **private** playbook in AgentPlaybooks and a playbook-scoped key
with `memory:read` and `memory:write` in its Integrations tab, then name both
in the `agentplaybooks` section of `config.jsonc` (or `/settings`):

```jsonc
{
  "agentplaybooks": {
    "playbook_guid": "the GUID from the playbook's URL",
    "api_key_env": "AGENTPLAYBOOKS_MEMORY_API_KEY",   // or "api_key" inline
    "base_url": "https://agentplaybooks.ai"
  }
}
```

A missing key or a GUID that is not hex is refused at the start, naming the
member; so is a base URL that is not `https://` (plain `http://` only for
`localhost`).

| Member | Default | What it does |
| --- | --- | --- |
| `playbook_guid` | — | the private playbook's GUID; one playbook per profile |
| `api_key` | unset | the playbook-scoped memory key |
| `api_key_env` | unset | the variable holding that key; `AGENTPLAYBOOKS_MEMORY_API_KEY` is read either way |
| `base_url` | `https://agentplaybooks.ai` | the service address |
| `shared_playbooks` | none | read-only playbook GUIDs, each keyed by `AGENTPLAYBOOKS_SHARED_<GUID>_API_KEY` |
| `team_playbooks` | none | team playbooks this profile writes to, each keyed by `AGENTPLAYBOOKS_TEAM_<GUID>_API_KEY` |
| `recall` | `true` | search memory before each model request |
| `timeout_seconds` | `15` | one request's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that the deploy window is Friday afternoon.

The model calls `apb_memory_write`. Later, "when is the deploy window?"
makes it call `apb_memory_search` first — and, because `recall` is on, the
turn's own words are searched before the request goes out, so the fact is
often already in front of it. Take a key from a search result before
`apb_memory_read`, `apb_memory_archive` or `apb_memory_delete`; a
correction reuses the key, and the old content stays in `apb_memory_history`.
`/agentplaybooks` prints the playbook, its address and how many sources it
reaches; `/agentplaybooks deploy` searches it from the shell.

Search is literal, not semantic: short terms, and search again with other
words rather than one long question. Recall carries at most four keywords
over the personal and team playbooks, returns in about four seconds, and
injects at most eight entries and 8,000 characters — reference data, as the
prompt says, not instructions.

## The wire

`GET /api/playbooks/<guid>`, `GET /api/playbooks/<guid>/memory?search=`,
`PUT` and `DELETE /api/playbooks/<guid>/memory/<key>`, authenticated with the
playbook-scoped key in `X-API-Key` and nothing else: no transcript, no file
and no environment variable leaves. Before every write the cell repeats the
original's check — the anonymous metadata GET must answer 404 and an
authenticated read must succeed — so a playbook that turned public or
unlisted is refused rather than written to.

## Not ported

- **Mirroring the harness's own memory files.** Hermes keeps `MEMORY.md` and
  `USER.md` and mirrors their writes here; Nodecode has no such files.
- **The primary-agent gate and participant provenance.** A Nodecode session
  has no gateway participants to record as authors.
- **`hermes memory setup` / `hermes memory status`.** The section,
  `/settings` and `/agentplaybooks` are the same surface.
- **The four parallel recall workers.** One sequential pass under the same
  four-second budget and the same eight-record, 8,000-character limits.

MIT, Máté Benyovszky's.
