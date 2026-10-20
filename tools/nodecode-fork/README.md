# nodecode-fork

A turn forks itself. `(fork:run "task" ...)` gives each of up to four tasks
to a fork of the calling session, made at the turn that calls it: each fork
holds that whole context, works its one task on the session's own model, and
answers. The call waits for them all and answers one plist a task, in order:

```lisp
(fork:run "check the restore path on a store copy"
          "measure the warm-attach close")
;; => ((:task "check the restore path on a store copy" :session "fork-turn-...-1"
;;      :status :completed :answer "..." :ran (";; copy the store" ";; restore"))
;;     (:task "measure the warm-attach close" :session "fork-turn-...-2" ...))
```

`:status` is `:completed`, `:failed`, `:cancelled` or `:timeout`; `:answer`
the fork's last words; `:ran` the first line (the note) of each eval it ran.

## When to fork

The line every request carries says it, and `(help :fork)` says it whole:
fork work that splits into branches that do not need each other, or one probe
whose output would bury the context; never a plain linear task, one read, or
steps that each need the one before. That rule is the cell's point: a
delegation primitive is used as often as its description invites, and the
frames this replaces were reached for in 15 of 2,376 turns, 13 of them plain
tasks.

## How it works

- A fork is an anchored child session (`nlk:create-session :parent
  :anchor-turn-id`): its history composes the parent's by reference through
  the running turn, and the create carries the parent's model and reasoning
  effort, so its first request extends the parent's cached prefix.
- That history ends in the eval call that is waiting on the fork, which the
  fold answers `no result recorded (interrupted)`. The fork's input says why,
  and that a fork cannot fork.
- The wait rides the `:frame` point: a fork's turn ending wakes its waiter,
  and the log says how it ended (`nlk:turn-terminal-kind`, `nlk:turn-answer`).
- A fork's turn runs under `seconds` (`nle:turn-budget`): past it its tools
  are refused and it answers where it stands; one still running a minute later
  is cancelled. The caller unwinding (an eval interrupt, its turn stopped)
  cancels every fork still running.
- Forks are never named (a model call each) and never reflected on: their
  turns' source is `fork`.

## Config

Nothing is required. `config.example.jsonc` shows the section:

| key | default | what |
| --- | --- | --- |
| `seconds` | 900 | seconds a fork's turn may run before its tools are refused (at least 30) |

## Tests

`.github/test-cell.sh tools/nodecode-fork` runs the slice: real sessions,
real anchored forks, real turn workers, a stubbed provider.
