# nodecode-xmemo

XMemo cloud memory: semantic recall, durable facts and working state in a user's own account. A Nodecode rewrite of [yonro/hermes-xmemo-plugin](https://github.com/yonro/hermes-xmemo-plugin) by [yonro](https://github.com/yonro).

The model gets six tools — `xmemo_search`, `xmemo_get`, `xmemo_list`,
`xmemo_recall_context`, `xmemo_remember`, `xmemo_update_state` — that keep a
user's facts in their own XMemo account and find them again by meaning, and
`/xmemo` to show the backend or search it. Memories written by the account's
other agents are visible here, so one private memory layer serves them all.

## Get it

Tell the agent "add the xmemo cell", or run `nodecode add nc://xmemo`.

## Set it up

The cell needs an XMemo key, and the `xmemo` section of `config.jsonc` (or
`/settings`) names it. `XMEMO_KEY` in the environment is read without any
section at all:

```jsonc
{
  "xmemo": {
    "api_key_env": "XMEMO_KEY",  // or "api_key": "…", or just XMEMO_KEY in the environment
    "bucket": "work",            // where new memories are written
    "scope": "nodecode/default"  // and the scope they are written in
  }
}
```

A start with no key is refused, naming the member. A missing section key
that has a default needs nothing; an integer under its floor (`search_limit`
0, say) is refused the same way.

| Member | Default | What it does |
| --- | --- | --- |
| `api_key` | unset | the XMemo API key; `api_key_env` and `XMEMO_KEY` are read too |
| `api_key_env` | unset | the environment variable holding the key |
| `base_url` | `https://xmemo.dev` | the API address |
| `agent_id` | `nodecode` | the agent id every request is stamped with |
| `instance_id` | generated per start | this install's opaque provenance id |
| `bucket` | `work` | the bucket new memories are written to |
| `scope` | `nodecode/default` | the scope new memories are written to |
| `read_bucket` | `%` | the bucket reads span; `%` means every bucket |
| `read_scope` | every scope | the scope reads span |
| `search_limit` | `10` | how many memories one search asks for unless the call says otherwise |
| `max_items` | `5` | how many items one context pack carries unless the call says otherwise |
| `max_tokens` | `900` | the token budget of one context pack |
| `capture_timeline` | `false` | record each finished turn's request on the XMemo timeline |
| `timeout_seconds` | `20` | one request's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea and live in Oslo.

The model calls `xmemo_remember`. Later, "what do I drink?" makes it call
`xmemo_search` first, and XMemo answers with the fact and its id — the id
`xmemo_get` reads whole. `/xmemo` prints the backend, its address, the
agent and the bucket and scope writes land in; `/xmemo where do I live`
searches from the shell.

`xmemo_recall_context` is the pack the original builds before a turn: at most
`max_items` items inside `max_tokens`, ranked, with working state preferred.
Every read accepts the original's filters — `since`, `until` (RFC 3339, with
a timezone offset), `time_order`, `owner_timezone`, `reference_time`,
`temporal_axis_mode`, `query_mode` — and validates them before a request
leaves, so a malformed instant is the model's repair rather than a 500.

Search widens a query with the original's bilingual alias table: a question
mentioning memory, deletion, recovery, a plan or a discussion gains its
Chinese and English variants, so one mixed-language search finds what two
would.

`xmemo_update_state` saves the active task, the next action or a blocker
with a TTL, so a later session can resume. With `capture_timeline` on, each
finished turn's request is recorded as a timeline event.

## The wire

`X-API-Key` plus `X-Memory-OS-Agent-ID` and `X-Memory-OS-Agent-Instance-ID`
against `base_url`; `GET /v1/memories/search` for search and listing,
`GET /v1/memories/<id>/explain` for one memory whole, `POST /v1/remember`,
`POST /v1/update_state`, `POST /v1/recall/context`, and
`POST /v1/timeline/events` for the opt-in capture. Writes carry an
`Idempotency-Key`. Five read failures in a row pause reads for two minutes
instead of hammering the backend.

## Not ported

- **The local cache and the outbox.** The original caches recall packs in
  SQLite and retries a failed write from a durable outbox. Here a read that
  fails is the error the model sees, and a write answers what the backend
  said; the idempotency key still makes a retry safe.
- **The workflow, destructive and feedback tools** — timeline events,
  reminders, `complete_reminder`, `mark_used`, `forget`. The original keeps
  the first four opt-in and the last internal; only its six defaults are
  registered here.
- **The restart snapshot** on session end, and mirroring Hermes's built-in
  memory writes: both are seams of Hermes's provider interface, and a
  Nodecode cell has the `:frame` point instead (which the opt-in timeline
  capture uses).
- **The XMemo CLI credential file** as a fallback key: a cell reads its
  section and the environment.

MIT, XMemo contributors'.

## Licence

MIT. The original repository's LICENSE is bundled unchanged; see [NOTICE](NOTICE).
