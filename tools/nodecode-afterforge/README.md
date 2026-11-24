# nodecode-afterforge

Afterforge: reviewed regression checks. A Nodecode rewrite of [ussyverse/afterforge](https://github.com/ussyverse/afterforge) by [ussyverse](https://github.com/ussyverse).

A failure a person cared about becomes a case; a case a person approved becomes a check; the check runs, and what it printed is kept as evidence. Everything stays on this machine: the shelf is a folder of one JSON file per case, in this home's `afterforge/` folder unless `home` names another.

## Get it

Tell the agent "add the afterforge cell", or run `nodecode add nc://afterforge`.

## Set it up

`afterforge.project` is required and names the folder the checks run in, usually the repository they cover. Without it the cell refuses to start, and `/cells` says which key is missing. The rest is optional:

| Setting | Default | What it is |
| --- | --- | --- |
| `project` | — | the folder the checks run in |
| `home` | this home's `afterforge/` | where cases and their evidence live |
| `timeout_seconds` | 120 | one check's ceiling |
| `output_chars` | 1200 | how much of a check's output the model is handed |

Set it in `/settings`, or in the `afterforge` section of `~/.nodecode/config.jsonc` (see `config.example.jsonc`).

## Use it

The loop, in the model's six tools:

| Tool | What it does |
| --- | --- |
| `afterforge_case` | record one failure or correction as a candidate case, with what happened and what should have happened |
| `afterforge_cases` | list the shelf, filtered by status |
| `afterforge_review` | approve or reject a candidate — a person's decision |
| `afterforge_recipe` | draft the check for an approved case: a shell command, the folder it runs in, the exit code it must give |
| `afterforge_verify` | run that check with the digest the recipe answered, and keep its output as evidence |
| `afterforge_report` | one case's evidence, or the whole shelf |

A case is `verified` when its command last passed and `regressed` when it last failed; the last five runs of a check are kept. A check runs only when the digest it was drafted with is passed back, so the command that runs is the command that was reviewed.

For the operator, `/afterforge status`, `/afterforge cases [STATUS]` and `/afterforge report CASE-ID` print the same shelf; `(help :afterforge)` is the primer, and the verbs are callable from eval too: `(afterforge:report "case-001")`.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the six tools, the shelf, the runner and the `/afterforge` command |
| `package.lisp` | the package, its verbs and its one condition |
| `config.example.jsonc` | the `afterforge` section, commented |
| `test/` | the offline tests: 13, no network |

## What the rewrite does not do

The original scanned Hermes' own `state.db` for corrections, captured failed tool calls with `post_tool_call` and `on_session_end` hooks, showed a local web page, managed its own Python runtime with `uv`, and ran pytest recipes. This cell is the same loop with none of that: cases are recorded by the model or the operator, the check is any shell command, and the evidence is the JSON on the shelf. See `NOTICE`.

## License

MIT, the original's: see `LICENSE` and `NOTICE`.
