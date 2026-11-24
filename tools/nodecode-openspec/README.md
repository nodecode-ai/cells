# nodecode-openspec

OpenSpec: spec-driven development over a project's `openspec/` folder — changes, tasks, specs, ideas and a board of what is in flight. A Nodecode rewrite of [FelineStateMachine/hermes-openspec](https://github.com/FelineStateMachine/hermes-openspec) by [FelineStateMachine](https://github.com/FelineStateMachine).

The model gets twenty `openspec_*` tools that read and write the same
artifacts the original plugin did: a change is a folder under
`openspec/changes/` holding `proposal.md`, `design.md`, `tasks.md` and
`specs/<capability>/spec.md`; its status is derived from the checkboxes in
`tasks.md`, never stored. Everything is plain markdown, so the tools work
with nothing installed.

## Get it

Tell the agent "add the openspec cell", or run `nodecode add nc://openspec`.

## Set it up

Set `workdir` to the project whose `openspec/` the tools work in, in
`/settings` or in the `openspec` section of `config.jsonc`:

```jsonc
{
  "openspec": {
    "workdir": "~/code/myproject"
  }
}
```

More projects can be named for `openspec_context`:

```jsonc
{
  "openspec": {
    "workdir": "~/code/myproject",
    "projects": ["api=~/code/api", "web=~/code/web"]
  }
}
```

`cli` names the `openspec` command when it is not on `PATH`; only
`openspec_cli` runs it. Every tool also takes a `workdir` of its own, so a
session can work in another project for one call.

## Use it

| Tool | What it does |
| --- | --- |
| `openspec_list` | The project's changes (status and task progress) or its specs |
| `openspec_show` | One change whole (proposal, design, tasks, delta specs) or one spec, parsed |
| `openspec_status` | Which artifacts a change has and which are missing |
| `openspec_task_list` / `openspec_task_set_status` | A change's checklist; tick or untick it by id or by text |
| `openspec_change_create` / `openspec_change_promote` | A change scaffold; then its task list and delta spec placeholder |
| `openspec_change_archive` / `openspec_change_unarchive` | Move a finished change into `changes/archive/`, or back |
| `openspec_idea_create` / `openspec_idea_enrich` / `openspec_idea_promote` | Capture an idea, evaluate it, turn it into a change |
| `openspec_spec_list` / `openspec_spec_show` / `openspec_spec_create` | Read and write a `spec.md` as requirements and scenarios |
| `openspec_spec_diff` | A change's delta spec against the baseline, by requirement and scenario |
| `openspec_validate` | The shape checks: a proposal with `## Why`, tasks, a delta spec, a scenario per requirement |
| `openspec_instructions` | The authoring guide for the proposal, design, tasks, specs, and how to apply a change |
| `openspec_context` | Resolve `project` or `project/change` into the change's content |
| `openspec_cli` | The `openspec` CLI itself, for the subcommands the files do not carry |

`/openspec` prints the project's board: every active change with its status
and `done/total` tasks. The tools are also callable from eval and from a
layer: `(opsx:status)`, `(opsx:validate :change "add-login")`.

A change's lifecycle, as the tools see it: `draft` (no tasks), `todo`,
`in_progress`, `done`, `archived`.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The `nodecode-openspec` package, nicknamed `opsx` |
| `model.lisp` | The `openspec/` folder as data: paths, tasks, specs, ideas |
| `cell.lisp` | The operations, the twenty tools, `/openspec`, and the cell declaration |
| `test/cell-test.lisp` | Fifteen tests over scratch projects, no network, no CLI |

## License

MIT. The rewrite is Nodecode's; the design and the guides come from
FelineStateMachine's plugin. See `LICENSE` and `NOTICE`.
