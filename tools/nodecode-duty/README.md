# nodecode-duty

Duty Board: what is waiting on you, what is running and what broke, in one read-only board. A Nodecode rewrite of [xiaoyaner0201/hermes-duty-board](https://github.com/xiaoyaner0201/hermes-duty-board) by [xiaoyaner0201](https://github.com/xiaoyaner0201).

Work spreads across sessions: a conversation where the answer asked you something and waits, a turn that has been quiet for an hour, one that started and never came back, a cron job failing since Tuesday. The duty board puts all of it on one page, cheapest possible: it calls no model, reads no message text, and changes nothing.

## Get it

Tell the agent "add the duty cell", or run `nodecode add nc://duty`.

## Set it up

Nothing is required. The `duty` section has three members, all with defaults: how far back a conversation still counts as in flight (`window_hours`, 72), when a running turn is called stalled (`stall_minutes`, 10), and how many rows a section prints (`limit`, 20).

```jsonc
"duty": { "window_hours": 72, "stall_minutes": 10, "limit": 20 }
```

## Use it

Ask the agent what is waiting on you, what is stuck, or what broke while you were away -- it calls the `duty` tool -- or type `/duty` yourself. The command takes a section: `/duty waiting`, `/duty running`, `/duty unfinished`, `/duty cron`, `/duty goals`; `/duty counts` is the one-line count and `/duty` alone is all of it.

```
duty: 1 waiting on you, 1 running, 1 unanswered, 2 cron in trouble, 1 goal
  ◆ Waiting on you (1)
    ? Plan the offsite  [cli]  waiting 5m  "which venue?"
  ◆ Running (1)
    ◐ Refactor billing  [tui]  running 2h00m  quiet for 2h00m (stalled)
  ◆ Started, no answer (1)
    ✗ Nightly notes  [discord]  started 15m ago, no answer yet
  ◆ Cron trouble (2)
    ⚠ cron 'nightly-backup'  last run FAILED: code 1 stderr: no target
    ⚠ cron 'morning-digest'  paused  answer awaiting delivery
  ◆ Standing goals (1)
    ◎ goal g-1  [s1]  8 turns  the suite passes  check: make test
```

| Section | What appears |
|---|---|
| Waiting on you | A conversation whose newest answer asked the person something, oldest first, with the question the harness recorded and how long it has waited. |
| Running | A turn in flight now, with its running time, flagged `(stalled)` once it has been quiet past `stall_minutes`. |
| Started, no answer | A turn that began and left no ending: you wrote, and nothing answered. |
| Cron trouble | A job whose last run failed, one paused, or one whose answer was never delivered, with the last line of its failure and its next fire. |
| Standing goals | The goals of this home that have not ended, with their turn budget and their check. |

The first two sections always print, so an empty home reads as all clear. A source that could not be read -- no store, no cron jobs yet, an older image -- is named in the `Not read` footer instead of failing the read.

A script can read the board itself: `(nodecode-duty:snapshot)` answers a plist (`:waiting`, `:running`, `:unfinished`, `:cron`, `:goals`, `:sources`), `(nodecode-duty:duty-text :only "waiting")` the text, and `(nodecode-duty:counts-text)` the one line.

## What it reads

All read-only, through their public readers; nothing is written.

| Source | Store |
|---|---|
| Sessions | The session directory in the store: titles, sources, when each was last touched, which answer waits on the person, and which turn is the newest. |
| Cron | The `cron` cell's registry row, when jobs exist: state, last outcome, an answer still owed. |
| Goals | The `goal.set` facts of this home, minus those a `goal.ended` fact closed. |

## License

Apache-2.0, the original's (see `LICENSE` and `NOTICE`).
