# nodecode-workflows

Workflows: agent-authored workflow graphs run in the background. A Nodecode rewrite of [jacobhausler/hermes-workflows](https://github.com/jacobhausler/hermes-workflows) by [jacobhausler](https://github.com/jacobhausler).

## Get it

Tell the agent "add the workflows cell", or run `nodecode add nc://workflows`.

## Set it up

The `workflows` section is optional and has defaults: `max_parallel` (3),
`tick_seconds` (2), `node_timeout_seconds` (900) and `output_chars` (4000).
See `config.example.jsonc`. Nothing else is needed: the cell runs nodes as
Nodecode turns in sessions of its own, so the model in use is the model they
take.

## Use it

The model writes a graph in JSON and hands it to the `workflow` tool:

```json
{"name": "check",
 "nodes": [{"id": "inspect", "type": "agent", "goal": "Inspect the target and answer with one JSON object."},
           {"id": "report", "type": "agent", "goal": "Write the report.", "after": ["inspect"]}]}
```

and ends its turn. One worker thread runs the graph: each agent node is one
turn in a session of its own, `after` edges order the nodes and hand every
child its direct ancestors' committed output under `## Inputs`, and the
session that started the run is woken with one line when the run settles.

| Action | What it does |
| --- | --- |
| `run` | validate a graph and start it; returns at once |
| `validate` | lint a graph, every problem in one answer, no writes |
| `status` / `list` | one run node by node / every run in the home |
| `show` | what one node committed |
| `stop` | nothing new starts; what committed stays |
| `answer` | answer a gate and let the run go on |

A graph is data, so a node can also be:

- `{"type": "echo", "echo": "text"}` — a constant, no turn spent;
- `{"type": "gate", "question": "Publish?", "options": ["yes", "no"]}` — the
  run holds there until `workflow{action:"answer"}` or `/workflow answer`;
- `{"fanout": {"items": ["a", "b"]}}` or `{"fanout": {"items_from": "plan.items"}}` —
  one lane an item, `id.1`, `id.2`, and `{"from": "plan"}` hands a later node
  the whole array;
- `{"inputs": ["plan.items"]}` — a dotted path from any ancestor;
- `{"schema": {...}}` — the reply contract, written into the prompt.

Every output is written under `workflows/<run-id>/` in the Nodecode home, with
the graph and each node's state in `run.json`: a node that committed is never
run again, so a run interrupted by a restart picks up where it stopped.

From the keyboard: `/workflow` lists the runs, `/workflow status ID`,
`/workflow show ID NODE`, `/workflow run PATH`, `/workflow stop ID`,
`/workflow answer ID NODE TEXT`. From Lisp through `eval`: `(workflows:start
GRAPH)`, `(workflows:status "id")`, `(workflows:show "id" "node")`,
`(workflows:runs)`, `(workflows:stop "id")`, `(workflows:answer "id" "node"
"text")`, `(workflows:validate GRAPH)`. `(help :workflow)` is the manual
page for the model.
