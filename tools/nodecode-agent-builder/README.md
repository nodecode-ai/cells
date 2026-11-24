# nodecode-agent-builder

Agent Builder: turns a plain-language brief into an editable specialist agent blueprint, with trials, version comparison and a portable export. A Nodecode rewrite of [itsflownium/hermes-agent-builder](https://github.com/itsflownium/hermes-agent-builder) by [itsflownium](https://github.com/itsflownium).

## Get it

Tell the agent "add the agent builder cell", or run `nodecode add nc://agent-builder`.

## Set it up

Nothing is required: the cell drafts on the model Nodecode already runs, through its auxiliary target, so it asks for no key of its own.

The `agent-builder` section of the config takes four members, all optional:

| Member | What it does |
| --- | --- |
| `max_brief` | the longest brief, refinement or trial prompt the cell accepts (default 12000) |
| `toolsets` | the toolsets a blueprint may pick from; empty means the model's choice is taken as it comes |
| `skills` | the skills a blueprint may pick from; empty means the same |
| `keep` | how many drafts `/agent-builder` lists (default 50) |

## Use it

Ask for a specialist, and the agent calls the `agent_builder` tool:

> "Build me an agent that reads tide tables and tells me when I can launch."

A draft comes back with a name, a slug, a description, its standing instructions, three conversation starters, three success checks and the toolsets and skills it needs. It is a draft, not a running agent: nothing in the config, the model or the tools changes until you use it.

The operator's command is `/agent-builder`:

```
/agent-builder                    the drafts in this home
/agent-builder BRIEF              a new draft from BRIEF
/agent-builder refine ID TEXT     the draft ID, refined by TEXT; the old version is kept
/agent-builder show ID            one draft whole
/agent-builder try ID TASK        the draft answering TASK, as the agent would
/agent-builder compare ID TASK    the last two versions answering TASK, side by side
/agent-builder drop ID            the draft's file, gone
```

Through eval the same verbs are `(agent-builder:build "...")`, `(agent-builder:refine ID TEXT)`, `(agent-builder:trial ID TASK)`, `(agent-builder:compare ID TASK)`, `(agent-builder:drafts)`, `(agent-builder:show ID)`, `(agent-builder:export-blueprint ID)` and `(agent-builder:drop ID)`. `(help :agent-builder)` has the same list while the cell runs.

Drafts are files under `~/.nodecode/agent-builder/`, one JSON object per draft, written atomically: `export-blueprint` answers the same object without the history, so a blueprint can go into a repository or another home.

## What is in it

| File | What it is |
| --- | --- |
| `blueprint.lisp` | the instructions the model reads, and the eight-field plan checked on the way back |
| `store.lisp` | one draft, one JSON file: read, written atomically, listed, forgotten |
| `cell.lisp` | the verbs, the `agent_builder` tool, `/agent-builder` and the section |

## License

MIT. The original is Copyright (c) 2026 Hermes Agent Builder contributors: see `LICENSE` and `NOTICE`.
