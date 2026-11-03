# nodecode-selffix

Idle self-fixes, observe level. When nobody is using the home, the failure
it records most often becomes a **finding**: a short report plus a test that
fails today, locked by a git commit. Nothing else changes: no code, no config,
no layer, and the cycle makes no model call of its own.

It is modeled on Ouroboros's evolution loop
([razzant/ouroboros](https://github.com/razzant/ouroboros) at `add0ce32`:
`supervisor/evolution_lifecycle.py`, `ouroboros/post_task_evolution.py`),
reduced to what any install already has.

## How a cycle runs

1. **Clock.** The loop has no clock of its own. It is one more source on the
   cron cell's wake job, so it runs only when the wake is due, inside the
   wake's own bounds and 24-hour allowance. A wake that is off runs nothing.
2. **Gates, in Lisp, before any work:** the operator's stop, the breakers,
   then idle: no turn at work in any session of this home for `idle_minutes`,
   and none still open. The wake's own session and forks do not count. Then an
   fcntl lock on `cycle.lock`, so two processes serving one home run one
   cycle; the kernel drops it when its holder dies, so a crash frees it at once.
   The cycle runs on a thread of its own, never on the cron ticker, so its
   sealed runs hold back no cron job; the wake says its news at the next due
   wake, read from the ledger since the wake's cursor.
3. **Pick.** The backlog (`backlog.lisp`) reads failed turns, experience
   harms and failed verifications, and `/qa` notes, groups them by fault and
   sorts them most frequent first. The ledger skips what was dropped or
   already found.
4. **Reproduce.** `repro.lisp` turns the captured case into one Lisp form
   that answers 1 while the failure stands, 0 once it is gone. `runner.lisp`
   asks it in a fresh process of the same build (its `trial` verb), in a
   throwaway home laid from the newest generation, under bwrap: environment
   cleared, no network, the real home hidden. No bwrap, no run.
5. **Record.** A form that answers 1 becomes `findings/<fp>.lisp`, with the
   generation where it turned and the `deftest` for the cell that owns the
   function, committed to that folder's own git. The commit is the lock: a
   later fix counts only when the committed form answers 0. The next wake's
   turn gets the finding as news, and the shell shows a notice at once.

## Limits the loop cannot change

- **Stop.** `(selffix:selffix :off)` is stored in the ledger and holds across
  restarts. Only `(selffix:selffix :on)` from a session that is not a cron
  job's lifts it.
- **Three failed cycles in a row** pause the loop until the operator starts it
  again.
- **An item tried three times** without a finding is dropped, and stays
  dropped.

## Use

```lisp
(cron:wake :on)              ; the clock the loop rides
(selffix:selffix :status)    ; on / stopped / paused, the idle gate, findings
(selffix:selffix :findings)  ; each finding, and whether its test is still locked
(selffix:selffix :now)       ; one cycle now, gates held
(selffix:selffix :off)       ; the operator's stop
```

`/selffix [status | findings | now | on | off]` does the same from a shell.

## Config

Absent, the cell installs nothing: the loop is off until the section exists.

```jsonc
{
  "selffix": {
    "idle_minutes": 30   // minutes with no turn in any session before a cycle may run
  }
}
```

## Files

Under `~/.nodecode/selffix/`: `ledger.sexp` (one readable row per outcome,
append only), `cycle.lock`, `cases/<fp>.lisp` (captured failures),
`findings/<fp>.lisp` (a git repository) and `scratch/` (the throwaway homes).

## Tests

```sh
.github/test-cell.sh tools/nodecode-selffix
```
