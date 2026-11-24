# nodecode-handflow

Handflow: a visible step plan the agent writes before it works and marks as it goes. A Nodecode rewrite of [DECRUX9812/hermes-handflow](https://github.com/DECRUX9812/hermes-handflow) by [DECRUX9812](https://github.com/DECRUX9812).

One tool, `handflow`, keeps the plan for a multi-step task: an ordered list of
steps, each `not_started`, `in_progress`, `completed` or `blocked`, with a note
of what it produced. The model writes the plan before it begins, marks each
step as it goes, and replans mid-flight; every call answers the plan as it now
stands, so it never spends a turn asking where it is. The active plan also
rides every model request, so the plan is always in front of the model, and
`/handflow` puts it in front of you.

State is plain JSON under the Nodecode home, one file per plan plus a pointer
to the active one, written atomically. No model call, no network, no daemon.

## Get it

Tell the agent "add the handflow cell", or run:

```sh
nodecode add nc://handflow
```

## Set it up

Nothing is required: with an empty section the cell keeps plans under
`~/.nodecode/handflow/`. Two settings, in the `handflow` section of
`~/.nodecode/config.jsonc` (or `/settings`):

| setting | default | what it does |
| --- | --- | --- |
| `state_dir` | `""` | where plan JSON lives; empty means `handflow/` in the Nodecode home |
| `show_plan` | `true` | carry the active plan in every request; off leaves the tool and `/handflow` |

```jsonc
{
  "handflow": {
    "state_dir": "",
    "show_plan": true
  }
}
```

## Use it

The agent drives it. Ask for anything with more than a couple of steps and it
should plan first:

> **you:** port the price parser to the new adapter and prove it on last week's data
>
> **agent:** *(handflow start -- goal and five steps)* -> works -> *(handflow complete 1, note "ported, tests pass")*

You can call it yourself from chat, or from eval as `(handflow:plan-text)`:

| action | what it does |
| --- | --- |
| `start` | create the plan: `goal` and `steps`; the first step goes in progress |
| `plan` | show the active plan |
| `begin` / `complete` / `block` / `reopen` | move a step, with a `note` |
| `note` | attach a note to a step |
| `add` | insert a step (replanning); `position` is 1-based |
| `finish` / `abort` | close the plan; `finish` refuses while steps are open unless `force` is set |
| `list` / `use` | the recent plans / pin one as the active plan |

Steps are addressed by 1-based number, by id (`s02`), by a title substring, or
by nothing at all (the current step).

`/handflow` shows the active plan, `/handflow list` the recent ones, and
`(help :handflow)` has the whole rule. From eval: `(handflow:plan-text)` and
`(handflow:plans-text)`.

Rules worth knowing: starting a step completes the one left in progress;
completing the last step closes the plan; adding a step or reopening one
reopens it; a note is kept on the step it was written for.
