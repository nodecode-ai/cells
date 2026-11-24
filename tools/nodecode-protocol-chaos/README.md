# nodecode-protocol-chaos

Protocol Chaos: the user's long-lived matters, on their own server, in context before the model answers and recorded after. A Nodecode rewrite of [chaos-xxl/protocol-chaos](https://github.com/chaos-xxl/protocol-chaos) by [chaos-xxl](https://github.com/chaos-xxl).

Protocol Chaos is one person's matters — what they are working toward, the
limits on it, the plan, the progress, the state, and the words they said —
kept on a server of their own, so every assistant they use reads the same
account of them. This cell changes a Nodecode session the way the original
changes a Hermes one: before a round the matter overview is added to the
turn's context and the matters a message names are read by id; after a
finished turn what the user said is recorded on the matter it belongs to.
Runs beside a memory cell, not instead of it.

## Get it

Tell the agent "add the protocol-chaos cell", or run `nodecode add nc://protocol-chaos`.

## Set it up

You need a Protocol Chaos server of your own (the repository's `server/`,
draft 0.2 or later), and the `protocol-chaos` section of `config.jsonc` (or
`/settings`) names it:

```jsonc
{
  "protocol-chaos": {
    "url": "https://example.org:8443/mcp",  // its MCP endpoint
    "token_env": "PROTOCOL_CHAOS_TOKEN",    // the token it issued for this client
    "actor": "nodecode",                    // the client name that token was issued for
    "capture": "auto"                       // auto | match | off
  }
}
```

A missing `url`, an address that is not http(s), or no token at all is
refused at the start, naming the member; `/protocol-chaos` then prints what
the cell is talking to and how many matters it found.

| Member | Default | What it does |
| --- | --- | --- |
| `url` | — | your server's MCP endpoint, such as `https://example.org:8443/mcp` |
| `token` | unset | the bearer token, written here; or `token_env` names the variable holding it (`PROTOCOL_CHAOS_TOKEN` is read either way) |
| `actor` | `nodecode` | the client name the token was issued for; every write is stamped with it |
| `capture` | `auto` | after each finished turn: `auto` the auxiliary model decides what to record and where, `match` the matter whose title shares the most words takes it, `off` records nothing |
| `overview` | `true` | add the overview of the open matters to a turn's first round |
| `related` | `3` | how many matters whose titles share words with the message one round may bring in |
| `timeout_seconds` | `8` | one read's ceiling; a write gets fifteen |
| `capture_max_chars` | `600` | how much of one message a recording send carries |

## Use it

Ask about your own plans and the model already knows them, because the
overview rides the turn:

> What has to happen before the Lisbon trip?

and say what changes, in your own words:

> The router keeps dropping the link in the evenings, so the provider is
> coming on Thursday.

The words are recorded on that matter after the answer, copied character
for character, and the next session — or another assistant on the same
server — reads them in the matter's progress.

The model has three tools: `protocol_chaos_matters` (the open matters as a
tree), `protocol_chaos_matter` (one matter in full: goal, limits, plan,
progress, parts, the last words), and `protocol_chaos_record` (keep words
now, on a matter or as a new one). `/protocol-chaos` shows the server and
its counts, and `/protocol-chaos router` lists the matters whose titles
share words with `router`.

When you ask it to organize the repository, the model follows the
organizer's order — `event_words_since` from a checkpoint, `context_pack`
for the directory, then `event_record_expression` per matter with its
`expectedVersion`, and `event_add_parent` for the hierarchy; the help topic
`(help :protocol-chaos)` states it, and the cron cell can run it nightly.

## What travels, and what does not

The message is matched to matter titles on this machine, so reading a
matter sends only its id; the message itself is never sent to the server to
find matters. With `capture` on, the message and the matter titles go to
the auxiliary model of the provider this session already runs on, and the
words it decides to record are sent verbatim to your server at `url`, with
your token. Nothing else is contacted; there logs hold ids, counts and
times, never content.

## Not ported

- **The per-message structure fold.** The original folds each recorded
  message into the matter's fields right away, with a structured-output
  side call and a `VERSION_CONFLICT` retry. A Nodecode side call answers
  prose, so the fold is the organizer's, in one pass, as the help topic
  states.
- **The bundled cron blueprint.** Schedule the organizer with the
  `nodecode-cron` cell.
- **The four overview placements.** The original could put the overview in
  the system prompt, in the first turn, or nowhere; a cell adds context to
  a turn, so it rides the first round, or `overview: false` turns it off.
- **`hermes memory setup` / `hermes memory status`.** The section,
  `/settings` and `/protocol-chaos` are the same surface.

Apache-2.0, Xue Xinglong's, as the NOTICE says.
