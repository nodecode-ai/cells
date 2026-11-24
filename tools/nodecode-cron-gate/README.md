# nodecode-cron-gate

Cron Gate: skip scheduled agent runs that would do nothing. A Nodecode rewrite of [superlowburn/hermes-jev-cron-gate](https://github.com/superlowburn/hermes-jev-cron-gate) by [superlowburn](https://github.com/superlowburn).

A job that runs every hour to check on something usually wakes up, spends a whole
model run, and reports that nothing has changed. A gate asks
[TypeSafe](https://typesafe.ai) Jev one yes/no question about a small snapshot of
what the job reads -- the queue file, the last lines of a log, whether the
folder it works on changed, a localhost page -- and answers `:wake` or `:skip`
in about half a second, for a fraction of a cent.

```lisp
(cron-gate:gate "heartbeat"
                :question "Running the agent now would make real new progress, rather than re-reporting that it is still waiting on approval."
                :sources '((:file "~/proj/APPROVALS.md")
                           (:file "~/proj/log.jsonl" :tail 900)
                           (:changed ("~/proj/src"))))
(cron-gate:check "heartbeat")
;; => "cron-gate heartbeat: skip, useful=0.05", :SKIP
```

## Get it

Tell the agent "add the cron-gate cell", or run `nodecode add nc://cron-gate`.

## Set it up

A TypeSafe key, from [console.typesafe.ai](https://console.typesafe.ai): set
`TYPESAFE_API_KEY` in the environment, or save `api_key` in the cell's
`cron-gate` section. `base_url` points at a TypeSafe of your own; `model`
defaults to `jev-latest`.

**What leaves the machine.** Each decision posts the snapshot -- what those
sources hold, up to 6,000 characters, from the end -- and your question to
`api.typesafe.ai`, with the key. Point sources at what a person would glance at,
not at secrets.

## Use it

Put the check at the top of the gated job's prompt:

```text
Run (cron-gate:check "heartbeat"). If it answers :SKIP, reply "nothing to do" in one line and stop.
```

- `(cron-gate:gate NAME :question ... :sources ... :skip-below 0.2 :max-skips 4)`
  defines or updates a gate. Sources are `(:file PATH [:tail N])`,
  `(:changed (PATHS))` (modified since the last wake?), `(:url "http://127.0.0.1:PORT/...")`
  (localhost only; any other host is named and skipped), and `(:text "...")`.
- `(cron-gate:check NAME)` decides now, logs it, and answers the line and
  `:wake` or `:skip`.
- `(cron-gate:gates)` is every gate with its runs, skips and share skipped,
  read from `log.jsonl` under the home.
- `(cron-gate:forget NAME)` drops a gate's definition and counters.

The model can do the same through the `cron_gate` tool (`define`, `check`,
`status`, `forget`) -- so "gate my 9am job, look at APPROVALS.md" needs no Lisp
from you -- and `/cron-gate` lists the gates or checks one.

**It fails open.** No key, no config, a broken source, a network error, or an
answer of the wrong shape all wake the run. After `max-skips` skips in a row
(4 by default) the gate wakes anyway, so a job can never stall silently.

## What the rewrite carries, and what it does not

Carried from the original: the question over a state snapshot, the four source
kinds and the 6,000-character cap that keeps the end, the localhost-only rule,
the skip rate and the per-gate log, the skip cap, and failing open on every
path.

Not carried: Hermes cron's pre-run `--script` slot, which has no Nodecode
equivalent -- a scheduled job here runs a prompt, so the gate is a verb the
prompt calls first. The generated wrapper script, and `setup`/`remove` writing
it into the harness's script directory, go with it. `test` is `(cron-gate:check
NAME)`, which is the decision itself, and it waits one request like a real run
rather than a dry run of it.
