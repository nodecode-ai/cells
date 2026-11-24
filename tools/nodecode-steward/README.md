# nodecode-steward

Project Stewardship: one project, its autonomy level, the health its own checks measured, and the initiatives a person approves. A Nodecode rewrite of [Sahil-SS9/hermes-project-stewardship](https://github.com/Sahil-SS9/hermes-project-stewardship) by [Sahil-SS9](https://github.com/Sahil-SS9).

## Get it

Tell the agent "add the steward cell", or run `nodecode add nc://steward`.

## Use it

Ask the agent to open a project, or run `/steward` for the board.

- **`steward_open`** registers a project: its directory, its autonomy level, the checks that measure it, and the executables those checks may use. Opening it again changes what you name.
- **`steward_status`** answers one project (settings, last measurement, initiatives) or the whole board.
- **`steward_cycle`** runs the project's checks in its own directory and records what they measured: `healthy` when every check passed, `watch` when one failed, `degraded` when two failed or a high-severity one did, `unknown` when a check could not run.
- **`steward_propose`** files one initiative — title, rationale, expected outcome, risk — and leaves it for a person.
- **`steward_bind`** makes the session read its project's line at each request; `/steward bind PROJECT` does the same from the shell, and `/steward bind` stops it.

A person decides: `/steward approve PROJECT INITIATIVE` or `/steward reject PROJECT INITIATIVE`. The model proposes; it cannot approve its own work.

### Autonomy

The six levels are the original's, weakest first. Each adds to the one below, and nothing is ever taken away by a higher level:

| Level | Adds |
| --- | --- |
| `assistant` | observe |
| `investigator` | research, diagnostics |
| `planner` | create an initiative |
| `builder` | write code, run a command evaluator |
| `maintainer` | merge low-risk work |
| `steward` | merge anything, publish a release, notify a gateway, spawn sub-agents |

Two gates follow from it: a cycle needs `builder` or above, because running a check is running a command; a proposal needs `planner` or above, and a measured health that is neither `unknown` nor `critical`.

### What a check may run

A check is written as `name: command` (`!name: command` makes its failure high severity). The command is split on whitespace into an argv — there is no shell, so no quoting, no pipes, no redirection. Its first word must be named in the project's `allow` list, and the list is fail-closed: with no allowlist, nothing runs. Each check runs in the project's own directory, with a deadline (`timeout_seconds`, a minute by default) and no stdin, and only the head of its output is kept as evidence (`evidence_chars`).

Health is what those checks measured. It is never a model's opinion, and never a percentage.

## Set it up

Nothing is required. The section has no key and no server:

```jsonc
"steward": {
  "data_file": "steward/projects.json",   // under the Nodecode home
  "timeout_seconds": 60,                  // one check's ceiling
  "evidence_chars": 400                   // how much of a check's output is kept
}
```

The registry is one JSON file: the projects, and which session follows which project. Delete it and the cell starts empty again.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: the registry, the six levels, the check runner and its grading, the five tools, `/steward`, the round hook and the manual topic. |
| `test/cell-test.lisp` | Offline tests: lifecycle, the board, a cycle over a stubbed shell, the allowlist, the proposal gates, approval, and the round's line. |

## License

Apache License 2.0, the original's. See `LICENSE` and `NOTICE`.
