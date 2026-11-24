# nodecode-hades-coding-workflows

Hades coding workflows: three skills for planning a change, executing it through delegated subagents, and running a design–implement–verify harness. A Nodecode rewrite of [singhguri/hades-coding-workflows](https://github.com/singhguri/hades-coding-workflows) by [singhguri](https://github.com/singhguri).

The three skills are prose, not code, and they are read where they sit:

| Skill | What it is for |
| --- | --- |
| `writing-plans` | A plan as bite-sized tasks of two to five minutes each: exact file paths, complete code, the command that proves each task, the output it should print. DRY, YAGNI, TDD. |
| `subagent-driven-development` | Execute such a plan by giving each task to a fresh subagent with its own context, then two reviews: does it match the spec, then is the code good. |
| `dual-model-harness` | Design → plan → implement → verify, with the main agent doing the work and delegation only for genuinely independent parts; and the lesson that delegation overhead usually costs more than it saves. |

## Get it

Tell the agent "add the hades coding workflows cell", or run:

```sh
nodecode add nc://hades-coding-workflows
```

## Set it up

Nothing to set: no key, no service, no dependency. The folder is a knowledge cell, so Nodecode never loads it — the index reads the `SKILL.md` files as data.

## Use it

Ask the agent to plan a change, or to work through a plan; the index lists the three skills, and the agent reads one whole with `(help 'writing-plans)`, `(help 'subagent-driven-development)` or `(help 'dual-model-harness)`. Each carries its workflow, its prompt templates, its review gates and the pitfalls that cost the original author a session.

What the skills say about the harness is said in Hermes Agent's words — `delegate_task`, `todo`, `read_file`, `patch`. In Nodecode the same moves are:

| The skill says | In Nodecode |
| --- | --- |
| `delegate_task(goal=..., context=...)` | `(fork "task 1" "task 2")` for independent work in parallel, or a `(sh ...)`/`eval` step when it is serial; the `delegation` cell adds named model routes for subagents |
| `todo([...])` | the `board` cell, or `(context :note ...)` for what the turn must not forget |
| `read_file` / `patch` | `(sh "sed")`, `(read-file ...)`, `(edit path old new)` |
| `npx tsc --noEmit` | whatever your project's check is, run through `(sh ...)` — the point stands: the check is the evidence, not your summary of it |

An edit to a `SKILL.md` here is the skill at the next read, so a lesson learned in a session is kept by editing the file.

## What is in it

| Path | What it is |
| --- | --- |
| `nodecode-hades-coding-workflows.asd` | The folder as a knowledge cell: `:properties ((:knowledge . t))`, no component. |
| `skills/<name>/SKILL.md` | The three skills, unchanged from the original, each with the frontmatter that names it and the one-sentence description the index lists. |
| `skills/<name>/references/` | The four reference notes the skills point to, unchanged: GLM provider quirks, context-budget discipline, the gates taxonomy, and the server-action result pattern. |
| `test/hades-test.lisp` | Reads the folder as the index does: three skills, each found, named, described and read whole. |

## License

MIT. The skills are Copyright (c) 2026 Gurpreet Singh: see `LICENSE` and `NOTICE`.
