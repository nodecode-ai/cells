# nodecode-past

Past.dev memory: recalls what past remembers before each turn, and keeps each session there. A Nodecode rewrite of [pastdotdev/hermes-plugin](https://github.com/pastdotdev/hermes-plugin) by [pastdotdev](https://github.com/pastdotdev).

The model gets `past_recall` (search past for what was said or decided
before), the operator gets `/past` (the connection, or a query), and each
turn carries, by itself, what the person's own words recall from earlier
sessions. Every finished turn is kept and filed in past as part of a
sitting; nothing is sent while a sitting is still going.

## Get it

Tell the agent "add the past cell", or run `nodecode add nc://past`.

## Set it up

Make a project key in the past.dev console, then fill the `past` section of
`config.jsonc` (or `/settings`):

```jsonc
{
  "past": {
    "api_key": "past_sk_…",      // or api_key_env, or just PAST_API_KEY in the environment
    "identity": "you@example.com" // who you are in past: recall is off until it is set
  }
}
```

A self-hosted past is `"api_url": "https://past.example.com"`; plain
`http://` is accepted only for a loopback address. A start without a key is
refused, naming the member.

| Member | Default | What it does |
| --- | --- | --- |
| `api_key` | unset | the past.dev project key; `api_key_env` (default `PAST_API_KEY`) is read when it is empty |
| `identity` | unset | who the recall is made as; recall is off until it is set |
| `api_url` | `https://api.past.dev` | the API origin, with no path |
| `recall` | `true` | put what past remembers in front of each turn |
| `recall_limit` | `8` | how many memories one recall may carry |
| `context_chars` | `4000` | how much of a recall one turn carries |
| `ingest` | `true` | keep each conversation in past, cut into sittings |
| `sitting_minutes` | `30` | a session is cut into a new sitting where it went quiet this long |
| `audience` | unset | the audience past files these memories under, when the project has more than one |
| `timeout_seconds` | `20` | one request's ceiling |

## Use it

- **Recall before a turn.** At a turn's first round the person's own words
  are the query, and what past answers rides the request as one block:
  `=== past · recalled from memory ===`, how many memories it carries, and
  one dated line each. It is labelled background, not instruction.
- **`past_recall {query, limit?}`** answers JSON — the query and the
  memories it found, each with its date — so the model can ask again with
  other words, or ask for more.
- **`/past`** prints the address, the key's last eight, the identity,
  whether recall and ingest are on, and how many sittings went. `/past where
  do I live` asks past from the shell.
- **Ingest.** A finished turn's two texts are filed in its session's
  buffer; a sitting is sent when a later turn closes it, when it has been
  quiet for `sitting_minutes`, or when the cell stops. Each sitting is
  content-hashed, so an unchanged one is never sent twice. Tool calls and
  their results are never sent, and the obvious shapes of keys and tokens
  (`past_sk_…`, `sk-…`, `ghp_…`, `Bearer …`, `PASSWORD=…`, private keys)
  are redacted from the query and the content alike.

## The wire

`POST /api/v1/recall` with `{query, identity, limit, level}` — and
`queryTimestamp` when a caller names one — answering `{results:
[{occurredAt, content}]}`. `POST /api/v1/ingest/batch` with `{items:
[{id, content, label, timestamp, identity?, audience?, metadata}]}`, the id
`nodecode:<session>` for a session's first sitting and
`nodecode:<session>:<n>` after it, so a resent sitting replaces its own.

## Not ported

- **The on-disk buffer.** The original keeps each session under
  `~/.past/hermes/` while it runs and sends what a crash left behind at the
  next start. Here the buffer lives in the cell: a sitting is sent when it
  closes or when the cell stops, so a crash loses the sitting that was
  running.
- **`~/.past/config.json`.** The original shares one config file with past's
  other connectors, and `hermes pastdotdev setup` / `status` write and read
  it. A cell takes its settings from its own section, `/settings` and `/past`
  are the same surface, and `PAST_API_KEY` is still read.
- **The messaging-gateway opt-out.** The original turns itself off on a
  gateway session unless the operator says it serves only them, because past
  recalls as one identity. This cell does not tell a shared room from a
  private session; a home that serves several people should set `recall` and
  `ingest` off, or use a project per person.

MIT, past's.
