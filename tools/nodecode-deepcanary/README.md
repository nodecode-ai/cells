# nodecode-deepcanary

DeepCanary: an attention inbox with C1-C3 levels, quiet hours and an hourly interrupt budget. A Nodecode rewrite of [Oscar-Williams/dsh-deepcanary](https://github.com/Oscar-Williams/dsh-deepcanary) by [Oscar-Williams](https://github.com/Oscar-Williams).

A long run needs you for very different things: an approval that blocks it,
a tool failing in a loop, a task that finished, a context window filling up.
Told all of it at the same volume, none of it is heard. This cell keeps one
local inbox and gives every item a level:

| Level | What it is | What happens |
| --- | --- | --- |
| **C3** | the work waits for you: a question, an approval | you are told at once, quiet hours and the budget included |
| **C2** | a failure or an abnormal state to look at | you are told, unless quiet hours or the hour's budget hold it |
| **C1** | an informational update | kept in the inbox as activity |

The level comes from the reason and from how the thing was seen. A guess
(`heuristic` authority) never makes C3: only evidence the harness itself
produced (`host`, `runtime`) can say the work is blocked.

## Get it

Tell the agent "add the deepcanary cell", or run:

```sh
nodecode add nc://deepcanary
```

## Set it up

One thing is required, and it is the whole point of the cell:

```jsonc
"deepcanary": {
  "level": "C2"        // the lowest attention that reaches you: C1, C2 or C3
}
```

The rest has defaults:

```jsonc
"deepcanary": {
  "level": "C2",
  "quiet_hours": false,          // hold C1 and C2 inside the window below
  "quiet_start": "22:00",
  "quiet_end": "08:00",          // equal times are the whole day
  "max_interrupts_per_hour": 3,  // C1 and C2 notices; a C3 is not counted
  "dedupe_window_minutes": 10,   // the same dedupe key bundles into one item
  "max_items": 500,
  "snooze_minutes": 30,
  "privacy_safe_summary": true   // redact a summary that names a token or a prompt
}
```

## Use it

Ask the agent, or call the verbs from eval:

```lisp
(deepcanary:raise "HUMAN_QUESTION_PENDING" :authority "host"
                  :summary "which branch to merge")
(deepcanary:inbox)        ; what is open, newest first
(deepcanary:status)       ; level, indicator, counts, quiet hours, budget
(deepcanary:explain 3)    ; why #3 has its level, and what holds it
(deepcanary:acknowledge 3)
(deepcanary:snooze 3 :minutes 60)
(deepcanary:mute 3)
(deepcanary:preview "TOOL_FAILURE_LOOP" "heuristic")
```

`/deepcanary` lists the inbox and opens a panel; the same verbs answer
`/deepcanary acknowledge 3`, `/deepcanary status`, and so on. The model has
`deepcanary_raise`, `deepcanary_inbox`, `deepcanary_acknowledge`,
`deepcanary_snooze`, `deepcanary_status` and `deepcanary_dry_run`.

An item you were told about also reaches the model at its next request, once,
as one reminder: it knows what it raised and that the operator has it. The
inbox itself is one JSON file under `~/.nodecode/deepcanary/`, mode 0600;
nothing here dials anything.
