# nodecode-chronicle

Chronicle: local-first memory for a session, kept in one file on this machine. A Nodecode rewrite of [indigokarasu/chronicle-agent-context-and-memory](https://github.com/indigokarasu/chronicle-agent-context-and-memory) by [indigokarasu](https://github.com/indigokarasu).

The model gets nine tools — `chronicle_remember`, `chronicle_search`,
`chronicle_answer`, `chronicle_ask_about`, `chronicle_timeline`,
`chronicle_history`, `chronicle_correct`, `chronicle_forget` and
`chronicle_get_context` — over two tiers of memory: the beliefs it chose to
keep, and the raw turns the cell captured by itself. Nothing leaves the
machine: the store is one JSON document under the Nodecode home.

## Get it

Tell the agent "add the chronicle cell", or run `nodecode add nc://chronicle`.

## Set it up

Nothing needs setting. The store lives in `chronicle/` under the home with
capture on, and the `chronicle` section of `config.jsonc` (or `/settings`)
moves it or turns capture off:

```jsonc
{
  "chronicle": {
    "dir": "~/notes/chronicle/",   // where the store lives; chronicle/ under the home unless answered
    "capture": true,               // store each finished turn as a raw event
    "capture_chars": 600,          // how much of each half of a turn one raw event keeps
    "search_limit": 5,             // beliefs, and raw events, one search answers with
    "timeline_limit": 10,          // entries chronicle_timeline answers with
    "max_beliefs": 2000,           // beliefs kept; a pinned one survives every trim
    "max_events": 2000             // raw turn events kept, newest first
  }
}
```

An integer under its floor is refused at the start, naming the member; so is a
store that will not parse — a start never quietly replaces what it cannot
read.

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea and live in Oslo.

The model calls `chronicle_remember`, which answers the belief's id. Later,
"what do I drink?" makes it call `chronicle_search` first — or
`chronicle_answer`, which answers from memory or says it does not know:

> I don't know: nothing in memory answers "what is the weather".

Facts change without being lost: `chronicle_correct` supersedes a belief, the
old one stays readable through `chronicle_history`, and `chronicle_forget`
retracts one while keeping the record. `/chronicle` prints where the store is
and what it holds; `/chronicle green tea` searches it from the shell.

With `capture` on (the default), every finished turn is stored as a raw event
on the `:frame` point — the turn's two texts, at most `capture_chars` each,
once per turn — so a session four days later can search what was said without
anyone having thought to remember it. Capture off stores only what the model
chooses to keep.

## The two tiers

- **Beliefs** are what the model kept: a content line, a kind (`fact`,
  `episode`, `note`, `reference`, `procedure`), an optional entity and
  attribute, and a salience tier — `pinned` (never trimmed), `high`, `normal`
  or `incidental`. `chronicle_search` ranks by how many query terms a belief
  carries, then by tier, then by recency.
- **Raw events** are the captured turns: when, which turn, what was asked and
  what was answered. They answer a search the beliefs do not, and they are
  what `chronicle_timeline` walks.

A correction stores a new belief and links it to the old one
(`superseded_by`), so the history of a fact is always two reads away.

## The store

One file, `chronicle.json`, in the section's `dir`: `{"version", "next_id",
"beliefs": [...], "events": [...]}`, newest first in both arrays, written
atomically at mode 0600. It is plain JSON: read it, back it up, move it. An
id is minted inside the file under the cell's lock, so two sessions writing in
turn never take the same one. Nothing is sent anywhere; there is no service,
no key and no account.

## Not ported

- **SQLite and the vector index.** The original's event log, its FTS search,
  its optional local embedder and its vector cache are SQLite and a Python
  package; a cell keeps its memory in one JSON document and matches terms.
- **The extraction pass.** The original can ask a model for facts at the end
  of a turn; here the turn itself is the record, and beliefs come from the
  model's own tool calls.
- **Derivation, contradiction, federation, ACLs, consent and verification.**
  The rules engine, the contradiction queue, federated capability providers,
  principals and read grants, `withdraw_consent` and `verify` are the
  original's multi-agent layers; a cell that talks to one session keeps the
  memory that session needs.
- **The local-database query tool** (`chronicle_db_query`) and the web
  dashboard; `/chronicle`, the tools and the store file answer the same
  questions.
- **The context engine slot.** The original can also compress a Hermes
  conversation window around what it remembers. Nodecode has its own context
  cell and its own eviction; this cell contributes what it remembers, through
  `chronicle_get_context`, and leaves the window alone.

MIT, indigokarasu's.
