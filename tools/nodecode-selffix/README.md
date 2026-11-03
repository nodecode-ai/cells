# nodecode-selffix

Idle self-fixes, observe level. When nobody is using the home, the failure
it records most often becomes a **finding**: a short report plus a test that
fails today, locked by its hash. Nothing else changes: no code, no config,
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
   and none still open. The wake's own session and forks do not count. Then a
   lock file (`O_EXCL`), so two processes serving one home run one cycle.
3. **Pick.** The backlog (`backlog.lisp`) reads failed turns, experience
   harms and failed verifications, and `/qa` notes, groups them by fault and
   sorts them most frequent first. The ledger skips what was dropped or
   already found.
4. **Reproduce.** `repro.lisp` writes a test for the item and runs it in a
   throwaway home with keys and network off.
5. **Record.** A test that fails as the item says is copied to
   `~/.nodecode/selffix/findings/<fp>/` with `finding.md`; its SHA-256 goes in
   the ledger. A later fix counts only when that file, unchanged, passes.
   The wake's turn gets the finding as news, and the shell shows a notice.

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

Under `~/.nodecode/selffix/`: `ledger.jsonl` (one row per outcome, append
only), `cycle.lock`, `findings/<fp>/` and `scratch/<fp>/` (the throwaway homes).

## Tests

```sh
.github/test-cell.sh tools/nodecode-selffix
```
