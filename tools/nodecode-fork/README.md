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

## Going back

The same verb forks from the past, never a second primitive:

```lisp
(fork:run "what did we know before the rename?" :from -2)   ; two turns back
(fork:run :from 1100 :check '(fboundp 'nle::help))           ; generations 1100..newest
(fork:run :from '(1100 1142) :check "test -f README.md")      ; a range, a shell check
```

- `:from -N` (or a turn id) anchors the forks at that earlier turn: they hold
  what the session knew when it ended, and nothing after it.
- `:from G` (or `(OLD NEW)`) with `:check` is the way back through the
  generations this home sealed: each generation is laid into a throwaway home
  of its own (`nlk:build-generation`, its `cells/`) and asked in a fresh
  process of today's binary (`nlk:trial`), four at once, both ends first, until
  the step where the check turned false. A generation that answers nothing is
  stepped over, never read as the break. The answer is `(:bisect (:verdict
  :found|:clean|:program|:unanswered :good G :bad G :changes LINES :says
  WORDS ...))`: `:clean` means the newest passes (the break is outside what a
  generation records), `:program` that the oldest fails too (every generation
  ran on today's binary, so the program is to blame; rebuilding old binaries
  is not built). The step found is asked again, both its ends: a fresh boot
  that answers otherwise the second time is `:unsteady` and names no step --
  the first live run met one, a layer variable fetched over the network at
  load. Tasks given beside it are forked after, told what it found.

## Racing a name

```lisp
(fork:run "make it faster" "make it faster another way"
          :keep 'slow-function :check '(= 42 (slow-function 6)))
```

Fork n writes its version as `fork-n/slow-function`, one defun taking every
call the name takes. The parent tries each, and what stands, against the check
in a fresh process laid at the newest generation; the ones that pass are timed
on the image's own calls of the name, one version serving each live call (the
call itself the clock, so nothing runs twice; read on the monotonic clock, as
`get-internal-real-time` moves once a millisecond), 20 calls each within 300 s. The
fastest goes live if it is 5% faster than what stands, or passes where what
stands fails, HELD (`nlk:hold-definitions`): written to the layer after one
clean call made outside an eval, put back if that call signals; `(revert
'slow-function)` puts the old one back until then. Beside `:from G`, the name
as the last good generation's layer had it races too. A name with advice on
it, a held one, a generic function, a macro or a locked package's function is
refused.

## What a fork may define

A fork runs in the operator's own image. A form of a fork that defines or
forgets a name that already answers there, or one a sibling claimed, is
refused before it runs (`nlk:definition-escrow`); nothing a fork defines is
written to the layer (`nlk:record-definitions`), and no eval -- the fork's, or
any other open while it runs -- is told a fork's definitions changed but were
not kept (`nlk:report-unkept-changes`). What it defined stays live; the layer
gets what the parent keeps.

## Config

Nothing is required. `config.example.jsonc` shows the section:

| key | default | what |
| --- | --- | --- |
| `seconds` | 900 | seconds a fork's turn may run before its tools are refused (at least 30) |

## Tests

`.github/test-cell.sh tools/nodecode-fork` runs the slice: real sessions,
real anchored forks, real turn workers, a stubbed provider.
