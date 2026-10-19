# nodecode-goal

A session keeps taking turns until its goal's check passes. You state when
the work counts as done; the session works, the check runs after each turn,
and while it fails the next turn starts on its own, told what the check said
-- until it passes or the goal's turns run out.

## Setting a goal

`/goal CONDITION` in a session's shell (or a channel room, where /goal is
the room operators') sets the goal and starts its first turn:

- `/goal the core suite is green --check just test` -- done when `just test`
  exits 0 in the session's folder.
- `/goal the README documents every flag` -- no command: a judge decides.
- `--turns N` caps the turns (`goal.turns`, 10, unless given).
- `/goal` alone reads the goal: its check, the turns checked, the last
  verdict; `/goal stop` ends it.

Through eval, `(goal:start "condition" :check "cmd" :turns n)`,
`(goal:status)` and `(goal:stop "why")` do the same for the calling
session: a goal set mid-turn is checked when that turn ends, and a session
that cannot go on without the operator stops its goal and says what it needs.
Every refusal is `ERROR: GOAL-ERROR` with the reason.

## The check

A **command** runs in the session's folder through `sh -c`, at most
`goal.check_seconds`; exit 0 is met, and anything else -- its status and the
tail of its output -- is what the next turn is told.

A **judge** is a fork of the session at the turn that just ended, asked
whether the goal holds now, told to verify what it can and change nothing,
and to answer with a first line that is exactly `MET` or `NOT MET` and its
evidence under it. It runs on the session's model (its cached prefix) under
its own budget (`goal.judge_seconds`, `goal.judge_calls`), is never named,
and stays as a session of its own (`<goal id>-check-<n>`), so every verdict
can be read back. A first line that says neither is not met.

## How a goal ends

Met, spent (its last turn checked and not met), cancelled (the operator
stopped its turn), failed (a turn or the judge failed), stopped (`/goal
stop`, or the session's own `goal:stop`) or replaced (a new goal). Met,
spent, cancelled and failed are said in the session as a `[goal]` note, a
settled exchange that a channel room posts.

## Where it lives

A goal is facts in the session's own log, on the goal's aggregate:
`goal.set`, `goal.judging`, `goal.checked` and `goal.ended`. Where a goal
stands is a fold of those and the session's turns, so a restart loses
nothing: the worker, woken by each turn's end, reads the log and takes the
one step each goal owes, and every turn it starts carries a command id made
of the goal and its number, so a step taken twice admits one turn.

## Settings

`goal` in `~/.nodecode/config.jsonc` (see `config.example.jsonc`): `turns`,
`check_seconds`, `judge_seconds`, `judge_calls`. An absent section runs on
the defaults.

## Tests

`.github/test-cell.sh tools/nodecode-goal` from the repository root: real
sessions, store, turn workers, shell and forks; the provider is the one stub.
