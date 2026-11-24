# nodecode-coding-route

Model Router: per-profile model settings plus Heavy/Light routing that hands coding work to background agent jobs. A Nodecode rewrite of [metamindedu/hermes-model-router](https://github.com/metamindedu/hermes-model-router) by [metamindedu](https://github.com/metamindedu).

Two lanes. One model each. A coding task goes to the lane its *purpose* names:
planning and the review that decides run heavy; implementation, mechanical
work and a first pass run light. The task starts as a session of its own, on
its own model, with its own log — so the session that routed it is free while
the job works — and its answer is read back when it is done.

## Get it

Tell the agent "add the coding-route cell", or run `nodecode add nc://coding-route`.

## Set it up

Name the model each lane runs on, as `provider/model`, the way
`(models "provider")` lists it:

```jsonc
"coding-route": {
  "heavy": "openai-codex/gpt-6-astra",
  "light": "openai-codex/gpt-5.6-luna",
  "heavy_effort": "high",      // optional: one rung of that model's ladder
  "light_effort": "medium",
  "timeout_seconds": 1800
}
```

`/coding-route` alone shows the two lanes and the routes so far; the same
members are in `/settings`. A route refuses — naming the member — while its
lane has no model. It needs no key of its own: a route runs on whatever the
session it starts already runs on.

## Use it

The model calls two tools, and the same two verbs are callable from eval:

```lisp
(coding-route:route "make the loader skip empty cells" :purpose "implementation")
;; route-1: implementation on light (openai-codex/gpt-5.6-luna) in /home/you/project
;; (coding-route:result "route-1") reads its answer

(coding-route:route "plan the store migration" :purpose "plan" :wait-seconds 120)
(coding-route:result "route-1")
```

The six purposes:

| purpose | lane | what the job is told |
| --- | --- | --- |
| `implementation` | light | do the work, run the checks that prove it |
| `mechanical` | light | a rename, a move, boilerplate — then the checks |
| `preliminary-review` | light | what would break, by file and line; change nothing |
| `plan` | heavy | the ordered steps, each with the file; change nothing |
| `semantic-final` | heavy | is the change right, with the evidence |
| `independent-review` | heavy | judge it on its own terms; name what its author missed |

`coding_route` answers a route id at once (or the answer itself with
`wait_seconds`), and `coding_route_result` reads it back: the answer whole,
that the job is still working, that its turn has not started, or how its turn
ended without one. The operator routes from the keyboard with
`/coding-route TASK`, and `(help :coding-route)` is the manual the model reads.

A route is another session on another model: its words are data. Read them
before doing the work again here.

Tests: `.github/test-cell.sh tools/nodecode-coding-route` from the cells
repository — offline, the job's first turn stubbed, nothing dialled.
