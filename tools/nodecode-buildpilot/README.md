# nodecode-buildpilot

BuildPilot: projects, their tasks and their progress checkpoints, kept locally. A Nodecode rewrite of [Justnevo/buildpilot](https://github.com/Justnevo/buildpilot) by [Justnevo](https://github.com/Justnevo).

A long job outlives one session. Open a project for it, file the tasks, and
after each stretch of work record a checkpoint — one or two lines on where it
stands. The next session asks for the project's context and reads the tasks
and the newest checkpoint, instead of guessing:

```
project #1  parser  (active, opened 2026-11-08 09:14)
  port the old parser
  3 tasks: 1 done, 1 in progress, 1 pending
    #1 [x] read the old one
    #2 [>] port it
    #3 [ ] prove it against the fixtures
  latest checkpoint (2026-11-08 11:40): the lexer is ported; the parser is next
```

Everything is one JSON file under the Nodecode home. Nothing is sent
anywhere: no key, no network, no service.

## Get it

Tell the agent "add the buildpilot cell", or run `nodecode add nc://buildpilot`.

## Set it up

Nothing is required. The section has two optional members:

```jsonc
{
  "buildpilot": {
    "state_dir": "",          // where the store lives; empty means buildpilot/ in the Nodecode home
    "checkpoints_kept": 50    // how many checkpoints one project keeps; the oldest are dropped past it
  }
}
```

## Use it

The model has six tools, the same six the original plugin gave it:

| Tool | What it does |
| --- | --- |
| `buildpilot_add_project` | open a project: a name and a line on what it is for |
| `buildpilot_list_projects` | every project, with its task tally and when it was last checkpointed; an optional `search` keeps those whose name or description has it |
| `buildpilot_add_task` | file a task under a project; it starts `pending` |
| `buildpilot_update_task` | move a task to `pending`, `in_progress` or `completed` |
| `buildpilot_checkpoint` | record where the work stands, in a line or two |
| `buildpilot_get_project_context` | the project, its tasks and its newest checkpoint, in one answer |

`/buildpilot` lists the projects and opens a picker with a row for each;
`/buildpilot 1` shows one in full. The same reads and writes are Lisp
functions in the `buildpilot:` package, called through eval:

```lisp
(buildpilot:projects)                        ; every project, one line each
(buildpilot:projects "parser")               ; those whose name or description says parser
(buildpilot:context 1)                       ; one project in full
(buildpilot:add-project "parser" "port the old parser")
(buildpilot:add-task 1 "prove it against the fixtures")
(buildpilot:update-task 3 "completed")
(buildpilot:checkpoint 1 "the lexer is ported; the parser is next")
```

`(help :buildpilot)` is the model's manual. Every answer is a string; a
refusal is `ERROR: BUILDPILOT-ERROR` with the reason.

## What it does not do

The original kept its three tables in a SQLite file under a profile's plugin
data; this cell keeps the same three lists in one JSON document under the
Nodecode home, so it needs neither SQLite nor the Python runtime. Nothing
else of the original's behaviour changed: same six verbs, same three task
states, same rollup of a project's tasks and its newest checkpoint. See
[NOTICE](NOTICE).
