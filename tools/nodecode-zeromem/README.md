# nodecode-zeromem

ZeroMem: every turn of every session kept word for word in one local file, and
past sessions recalled with provenance and no model call. A Nodecode rewrite of
[ptaranat/zeromem](https://github.com/ptaranat/zeromem) by
[ptaranat](https://github.com/ptaranat).

Nothing leaves the machine: the store is a folder of two JSONL ledgers under
the home, the ranking is BM25 over the turn's own words, and the only thing
that costs a token is what a model reads back.

## Get it

Tell the agent "add the zeromem cell", or run:

```
nodecode add nc://zeromem
```

## Set it up

Nothing is required: a fresh install keeps every turn under
`zeromem/` in the home. In `/settings` (or the `zeromem` section of
`~/.nodecode/config.jsonc`):

```jsonc
"zeromem": {
  "store": "zeromem/",   // the folder holding turns.jsonl and progress.jsonl
  "recall": true,         // put what past sessions answered into a turn
  "top_k": 5,             // how many turns one recall answers
  "chars": 700            // the most of one turn's words a recall line carries
}
```

## Use it

The model gets three tools, the original's own:

- `zeromem_recall` — past sessions' words, verbatim, each with its session,
  turn, speaker and day; JSON `{query, evidence[]}`. The calling session's
  own turns are never among them: they are already in its window.
- `zeromem_stats` — turns, sessions, and the oldest and newest day kept.
- `zeromem_forget_session` — drop one past session for good; it is not read
  in again, and the running session refuses.

In eval, the same three as strings, every refusal `ERROR: ZEROMEM-ERROR`:

```lisp
(zeromem:recall "the deploy script")   ; one line a turn, clipped
(zeromem:stats)                        ; "zeromem: 128 turns from 6 sessions, ..."
(zeromem:forget-session "s-3F5EH2ES")
(zeromem:import-turns)                 ; what this home already said, or :dry-run
```

`/zeromem` is the operator's read: typed alone it shows the counters,
`/zeromem import` previews what the sessions this home already had would add
and `/zeromem import yes` writes it, `/zeromem forget SESSION` forgets one.

At each turn's first round, up to `top_k` turns of past sessions that answer
its input ride into the request as one reminder —
`Recalled from past sessions (verbatim, with provenance):` and one bracketed
line a turn, `[s-3F5EH2ES #12 user 2026-11-08] ...` — clipped to `chars`
characters. Set `recall` to false to keep the store and stop the reminder.

## What it does not do

The original's Rust core embeds turns with a small ONNX model and retrieves
over an entity graph and hierarchical windows as well as lexically. This
rewrite keeps the lexical half only, so a paraphrase whose words differ from
the stored ones may not be found. NOTICE says what else was left out.

Run `.github/test-cell.sh tools/nodecode-zeromem` from the cells repository.
