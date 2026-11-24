# nodecode-flightlog

Flightlog: records every turn locally and flags the ones that finished green but went wrong. A Nodecode rewrite of [piyushpradhan/flightlog](https://github.com/piyushpradhan/flightlog) by [piyushpradhan](https://github.com/piyushpradhan).

A turn can finish cleanly and still be wrong: the answer says the fix works but
no test ran after the last edit, a tool errored and the answer never mentions
it, the same call goes round forty times, a job that usually spends ten
thousand tokens spends forty, a job that usually makes twelve calls makes one
and answers from memory. Flightlog records what each turn did and says so,
afterwards, in plain words.

Nothing is dialled and nothing leaves the machine: the record is one JSON
document under the Nodecode home.

## Get it

Tell the agent "add the flightlog cell", or run `nodecode add nc://flightlog`.

## Set it up

Nothing is required. Recording starts when the cell starts, and the store lives
in `flightlog/` under the home. The `flightlog` section of
`~/.nodecode/config.jsonc` (or `/settings`, or the page's Settings tab) moves
the store and tunes what counts as wrong:

```jsonc
{
  "flightlog": {
    "dir": "~/notes/flightlog/",   // where the store lives; flightlog/ under the home unless answered
    "keep_runs": 200,              // runs kept, newest first
    "notice": true,                // raise a flagged run as a standing notice
    "loop_repeats": 5,             // the same tool with the same arguments this many times
    "cost_spike_pct": 300,         // tokens this percent of the job's median
    "effort_collapse_pct": 25,     // tool calls under this percent of the job's median
    "min_baseline_runs": 3         // healthy runs a job needs before its baseline speaks
  }
}
```

A value out of range refuses the start, naming the member; a store file that
will not parse refuses it too, rather than being quietly replaced.

## Use it

`/flightlog` lists the newest runs with a verdict each:

```
Flightlog · last 24h: 3 runs, 1 needs a look

⚠ Claimed it works, but never proved it
   chat · 4m ago · run 3f9a01c2 · 6 tools · 9.1k tokens

✓ Looks fine
   job cron-7 · 1h ago · run 71b2d3e4 · 11 tools · 12.4k tokens
```

- `/flightlog why RUN` — one run's report and its timeline: what ran, on
  what, how long, and every step that failed, plus the finding's sentence and
  why it matters.
- `/flightlog jobs` — each scheduled job's baseline: median tools, tokens and
  duration over its healthy runs.
- `/flightlog ack FINDING` — acknowledge a finding; that rule stays quiet for
  that job from then on.
- `/flightlog purge confirm` — forget every run, finding and silence.

When a run is flagged and `notice` is on, the verdict is also raised as a
standing notice naming the run, so it reaches you without opening anything.
The model can read the same record with the `flightlog_runs` tool.

### The rules

| Rule | Trips when | Default |
| --- | --- | --- |
| Claimed it works, but never proved it | A file was edited, the answer claims tests pass or the fix works, and no test, build or lint ran after the last edit — or the last one failed | any |
| Said done, but a step failed | A tool call failed and the answer never mentions it | 1 error |
| Stuck in a loop | The same tool, with near-identical arguments, again and again in one turn | 5 repeats |
| Cost far above normal | Tokens far above the job's median | 3× median |
| Did far less work than usual | Tool calls far below the job's median | under 25% |
| Answered without doing any work | No tool call at all, on a job that normally makes them | baseline over 0 |

A job is a scheduled turn: the cron cell runs every job in its own session,
`cron-<id>`, and a run in such a session is compared with that job's last
seven healthy runs. Everything else is a chat, and gets the three rules that
need no history. The baseline rules stay silent until a job has three healthy
runs.

`(help :flightlog)` in a session answers the same page.

## What is recorded

Per turn: each tool call (its name, the argument that matters — the command,
the form, the path — how long it took and whether it failed), each model
round with its tokens, the answer, and how the turn ended. A call's one
argument is clipped to a hundred characters and a failure's words to a
hundred and twenty, so the store stays a record and not a second transcript.

## Licence

MIT. The original is MIT, Copyright (c) 2026 Piyush Pradhan; see [NOTICE](NOTICE) and [LICENSE](LICENSE).
