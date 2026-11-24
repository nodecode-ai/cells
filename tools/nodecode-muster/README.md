# nodecode-muster

Muster: a live board of every subagent flight - working, stuck, failing on repeat or gone - with a stop and a message. A Nodecode rewrite of [jollyroger1480/muster](https://github.com/jollyroger1480/muster) by [jollyroger1480](https://github.com/jollyroger1480).

Every `(fork "task" ...)` a turn starts is a *flight*, and Nodecode already
publishes how each fork of a running call stands. Muster reads that one fact,
keeps a row per flight under the home, and answers the questions the original
answered: who is still working, who has gone quiet, which job keeps failing,
and which ones died with the image that ran them. It also acts on a row: a
flight can be stopped, or sent a message it reads at its next step.

Nothing is dialled and nothing leaves the machine: the ledger is one JSON
document under the Nodecode home, and the only actions are the kernel's own
in-process ones.

## Get it

Tell the agent "add the muster cell", or run `nodecode add nc://muster`.

## Set it up

Nothing is required. The board starts empty and fills itself. The `muster`
section of `~/.nodecode/config.jsonc` (`/settings`, or the file;
`config.example.jsonc` has the shape) moves where the ledger lives and what
counts as stuck or as a repeat failure:

```jsonc
"muster": {
  "stall_seconds": 300,        // no frame for this long reads stuck
  "spin_threshold": 3,         // failures of the same job that read as "failing on repeat"
  "spin_window_seconds": 1800,
  "show_goals": true,         // false keeps the jobs off a shared screen
  "board_rows": 40,
  "auto_requeue": true,       // re-run a job that ended badly, in its own session
  "max_requeues": 2,          // and no more often than this
  "dir": "~/notes/muster/"    // ledger folder; muster/ under the home unless answered
}
```

A value out of range refuses the start, naming the member; a ledger that
will not read refuses it too, rather than being quietly replaced.

## Use it

`/muster` draws the board:

```
muster: 2 working, 1 stuck

> working  "1f3c9a2b"  audit the retry loop
    busy - 12 tool calls, last heard 8s ago
! stuck    "7d0e1a44"  index the corpus
    silent for 11m with nothing in progress - probably stuck.
    stop it: /muster stop 7d0e1a44
```

- `/muster stop ID` stops that flight's turn through the kernel, and says what
  the kernel answered rather than claiming a stop it did not get.
- `/muster note ID MESSAGE` sends a running flight a message (a steer): it
  reads it at its next step.
- `/muster help` lists the verbs. `/muster-digest` is the report: the four
  counts, then only the flights that need a decision, one line each.

The model has the same two surfaces as tools: `muster_status` (the board, and
which flights answer to which command) and `muster_digest` (the report).
`(help :muster)` carries the whole of it.

What the states mean:

- **working** - the flight's call is still running and frames keep arriving.
- **stuck** - no frame for `stall_seconds` and nothing in progress. The next
  frame unsticks it: the badge is never sticky.
- **failing on repeat** - the same job failed `spin_threshold` times inside
  `spin_window_seconds`.
- **gone** - the row was still live when a previous image stopped: found at
  the next start, and it needs a re-run.
- **done / failed / cut short / stopped** - how it ended.

A job that ended badly is run again in its own session, up to `max_requeues`
times, because a retry is what the original did; set `auto_requeue` to false
to watch only.

## Test it

```sh
NODECODE_SRC=... .github/test-cell.sh tools/nodecode-muster
```

The tests start and stop the cell, feed it fork frames by hand, move a row's
clock to prove a quiet row is stuck and a fresh frame heals it, stub the
in-process ingress to prove one bad ending is run again exactly once, and read
the board, the command and the digest.

## Gaps

- No token or cost column: the fork progress frame carries no token counts,
  and a price read off nothing would be invented. The original's rates and
  ceiling settings have no counterpart here.
- No dashboard pane, desktop chip or status-bar counter: Nodecode's own
  transcript already draws the forks of a running call, and a cell does not
  paint the TUI.
- Flights are the forks of a running call. A turn a person starts by hand in
  another session is not a flight and does not appear.
