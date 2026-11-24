# nodecode-escalation-governor

Escalation Governor: trips a session that is measurably stuck, and puts a packet of what was tried into the model's context. A Nodecode rewrite of [jusst01/escalation-governor](https://github.com/jusst01/escalation-governor) by [jusst01](https://github.com/jusst01).

A weak model does not know when it is out of its depth. Left alone, it
retries the same failing approach until the session dies, because the
decision to escalate was left to the model that is already failing. This
cell takes that decision away from the model and gives it to signals that
are deterministic -- no judging model, no prompt trick.

| Signal | Default | What it counts |
| --- | --- | --- |
| Consecutive tool errors | 3 | tool results that failed, one after another |
| Same call repeated | 3 | the same tool name with the same arguments, without progress |
| Provider errors | 3 | failed provider calls since the last successful tool result |
| Rounds without progress | 12 | model rounds since the last successful tool result |

Any successful tool result resets all four, so only a sustained failure
trips: a single unlucky call never does.

## What happens on a trip

The cell builds a packet -- the trip reason, the goal, and every attempted
call with how it ended -- and puts it in two places, at the two moments the
original chose: appended to the next tool result of the round, and recorded
into the next round's request. The packet tells the model to hand the
remaining work to a subagent or a stronger model, or, when there is none, to
stop and present it to the operator as the blocker report.

A successful tool result after a trip clears it: the session recovered on
its own, and the cell records that instead of escalating again. The operator
gets one notice per trip, and `/escalation-governor report` shows the lines
it wrote.

## Get it

Tell the agent "add the escalation-governor cell", or run

```sh
nodecode add nc://escalation-governor
```

## Set it up

Nothing is required. The thresholds are settings; set them in `/settings`
(or in the `escalation-governor` section of `~/.nodecode/config.jsonc`, as
`config.example.jsonc` shows), then `(restart-cells "nodecode-escalation-governor")`.
A setting of 0 turns that signal off; four signals at 0 make a cell that
watches and never trips.

## Use it

Once it is on, there is nothing to call: every tool call and every round of
every session passes through it.

| Verb | What it answers |
| --- | --- |
| `/escalation-governor` | the signals, the thresholds, and every session it tracks |
| `/escalation-governor report` | the last report lines it wrote |
| `/escalation-governor goal TEXT` | what this session is for, as its packet states it |
| `/escalation-governor reset` | forget every session it tracked |
| `(help :escalation-governor)` | the same, as prose |

A trip and a recovery are appended as one JSON line each to
`~/.nodecode/escalation-governor/reports.jsonl`: counts, tool names, and a
bounded hint of what each call aimed at (`read(src/x.lisp)`), never message
text.

## Test it

```sh
.github/test-cell.sh tools/nodecode-escalation-governor
```

10 tests, 0 failed. They funcall the three seams by hand -- a tool call, a
round, a provider call that failed -- so the slice runs offline.

## Gaps

- The goal is the operator's, stated with `/escalation-governor goal TEXT`.
  Hermes handed the plugin the session's first user message; a cell is not
  given the prompt text.
- Delegation is a directive, not an action: the cell names a subagent or a
  stronger model as the way out, and the delegation cell (or the operator)
  decides which.
- The token accounting of the original's report is gone with the round's
  usage object; nothing in the packet used it.
- Two cells watching the same session's tools is fine (each files under its
  own key), but two copies of this cell are not.
