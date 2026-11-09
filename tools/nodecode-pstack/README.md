# nodecode-pstack

Poteto's engineering rigor stack: one router, 17 playbooks and 52 skills that tell the agent how to investigate, design, review, verify and ship a change, read one at a time so none of them rides a request until it is wanted. A Nodecode rewrite of [Cloeille/pstack](https://github.com/Cloeille/pstack) by [Cloeille](https://github.com/Cloeille).

## Get it

Tell the agent "add the pstack cell", or run `nodecode add nc://pstack`.

## Set it up

Nothing to answer: the prose is in the cell's own folder and the cell reads it there. One setting, in the `pstack` section of the config (`/settings`, or the file):

| Key | Meaning |
| --- | --- |
| `max_chars` | How much of one playbook or skill a call returns (default 24000, minimum 200). A longer file is cut on a line and the answer names the rest. |

## Use it

Ask for engineering work, and the agent loads the playbook it needs before it starts:

- `pstack` with no name answers the catalogue: every playbook with the line it opens with, every skill with its own description.
- `pstack(name="feature")` answers that whole file: `feature`, `bug-fix`, `investigation`, `perf-issue`, `refactoring`, `figure-it-out`, `autonomous-run`, `eval`, `hillclimb`, `opening-a-pr`, `shipping`, `babysit`, `autopilot-full`, `autopilot-stack`, `orchestrate`, `session-pickup`, `authoring-a-skill`.
- `pstack(name="poteto-mode")` is the router: it carries the ladder, the principles, the autonomy rules and which task goes to which playbook. It is the one to read first.
- `pstack(name="how")`, `pstack(name="architect")`, `pstack(name="interrogate")`, `pstack(name="principle-prove-it-works")` and the rest of the 52 skills read the same way.
- `pstack(name="reflect/judgment-reviewer")` reads a reference file a skill names, by its path under `prose/skills/` (a skill's `references/` may be left out of the path).

The operator reads the same two answers from the shell: `/pstack` for the catalogue, `/pstack feature` for one file. `(help :pstack)` is the short version of this section, in the manual.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The cell: the catalogue, the reader, the `pstack` tool, the `/pstack` command, the manual topic. |
| `prose/skills/` | The original's `skills/` folder, unchanged: `poteto-mode/` with its 17 playbooks, the 51 other skills with their `references/` and `scripts/`. |

The playbooks are the original's text. Where one says `skill_view(name='pstack:X')` or names `playbooks/Y.md`, that is `pstack(name="X")` and `pstack(name="Y")` here; where one says `delegate_task` or `cronjob`, that is Nodecode's delegation and the cron cell. Every served file says so above it.

## License

MIT. The playbooks and skills are Copyright (c) 2026 poteto (adapted by Madeleine): see `LICENSE` and `NOTICE`.
