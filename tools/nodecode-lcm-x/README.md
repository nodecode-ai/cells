# nodecode-lcm-x

LCM-X: every turn of a session kept verbatim in a local store, so a long session loses nothing to a shorter window. A Nodecode rewrite of [electricsheephq/lcm-x](https://github.com/electricsheephq/lcm-x) by [electricsheephq](https://github.com/electricsheephq).

The cell writes each turn's ask, answer and tool results to one append-only
file per session as they land, and gives the model four tools over it:
`lcm_search` finds the blocks carrying a query's words, `lcm_expand` reads a
hit back verbatim (with its neighbours, if you ask), `lcm_recent` lists the
newest and `lcm_status` says what is held. Nothing is summarised, nothing
leaves the machine, and no key or server is needed.

## Get it

Tell the agent "add the lcm-x cell", or run `nodecode add nc://lcm-x`.

## Set it up

One setting: `dir`, the folder under the Nodecode home that keeps the
blocks. The cell refuses to start until it is named, so a session's turns
are written where the operator chose and nowhere else. Say it once in
`/settings` (or ask the agent to set up `lcm-x`):

```jsonc
{
  "lcm-x": {
    "dir": "lcm-x"        // ~/.nodecode/lcm-x/ — one <session>.jsonl per session
  }
}
```

The rest has defaults, and each is in `/settings` too:

| Setting | Default | What it does |
| --- | --- | --- |
| `capture` | `true` | keep each turn's ask, answer and tool results as they land |
| `max_chars` | `8000` | how much of one message or tool result is kept verbatim |
| `expand_chars` | `20000` | how much of one block `lcm_expand` answers |
| `search_limit` | `8` | how many hits one search answers unless the call says otherwise |

## Use it

The model reaches for the store by itself when a session has gone on long;
you can too:

| Tool | What it answers |
| --- | --- |
| `lcm_search` | blocks carrying every word of a query, newest first, each with the reference `lcm:<session>:<seq>` that reads it back; `scope: "all"` reads every session the store holds |
| `lcm_expand` | those references read back verbatim, with `around` neighbouring blocks each side |
| `lcm_recent` | the newest blocks of this session, one line each, optionally one `role` (user, assistant, tool) |
| `lcm_status` | how many blocks, in how many sessions, in which folder, and whether capture is on |

`/lcm QUERY` searches the store yourself; `/lcm` alone says what it holds and
shows the newest blocks. `(help :lcm-x)` is the manual the model reads.

Search is lexical: every word of the query must appear in the block, so a
word the tool output or the operator's sentence actually used is what finds
it. That is the trade for having no embedder, no key and no daemon.

The store is plain text: `~/.nodecode/lcm-x/<session>.jsonl`, one JSON object
a line. It is yours to read, copy or delete.
