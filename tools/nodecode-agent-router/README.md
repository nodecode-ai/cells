# nodecode-agent-router

Agent Router: chooses one agent for a task from a list, explains the choice, abstains when none fits, and falls back exactly once. A Nodecode rewrite of [Pinutss/jev-agent-router](https://github.com/Pinutss/jev-agent-router) by [Pinutss](https://github.com/Pinutss).

A task arrives, several agents could take it, and one has to be chosen. The
choice is arithmetic over what the registry says about each agent and what the
call says it needs: the agent whose words reach the task, whose capabilities
cover it, that has held up and costs little. The answer is one of three —
`select`, `fallback` or `abstain` — and it carries the score, the reasons,
the runner-up, the alternatives, and every agent that was thrown out with the
reason it was.

The access policies come first and the score never overrides them. An agent
of another scope, a disabled one, one missing a capability or a permission
the caller required, one holding a permission the caller forbade: each is
rejected before anything is scored, and no weighting can bring one back.

It chooses; it runs nothing. The caller sends the task to the agent it named.

## Get it

Tell the agent "add the agent-router cell", or run:

```sh
nodecode add nc://agent-router
```

## Set it up

Nothing is required. With no registry the cell ranks five example agents
(coding, devops, research, support, security), so `/agents fix the CORS bug`
answers out of the box. There is no key, no network and no model call: the
registry is data and the ranking is arithmetic.

| Member | Default | Notes |
| --- | --- | --- |
| `min_confidence` | `15` | a percent: under it the answer is `abstain`. A call may raise this floor, never lower it |
| `max_alternatives` | `3` | how many runners-up one answer lists |
| `registry` | unset | a JSON file of agents — an array, or an object with an `"agents"` array. `~/.nodecode/config.jsonc` under `agent-router` |

`config.example.jsonc` beside this file is the whole section, commented. A
`registry` that is not a file refuses at start, so a typo is one notice, not
a surprise at the first call.

## Use it

The model has one verb, `agent_route`:

```
agent_route(task: "fix the CORS bug in the FastAPI API", agents: [
  {"id": "coding-agent", "description": "Fixes CORS bugs and writes Python.",
   "capabilities": ["coding", "python"], "permissions": ["read_code", "write_code"],
   "cost": 0.4, "latency": 0.4, "reliability": 0.85, "fallback_id": "research-agent"},
  {"id": "support-agent", "description": "Answers the customer ticket.",
   "permissions": ["read_tickets"], "reliability": 0.75}
])
```

An agent is an object: `id`, `name`, `description`, `capabilities`, `tags`,
`tools`, `permissions`, `cost`, `latency`, `reliability` (the three numbers
are 0.0 to 1.0), `enabled`, `fallback_id`, `max_actions`, `scope`. Only `id`
is required; the rest have the original's defaults. `agents: []` ranks the
configured registry instead.

| Constraint | What it does |
| --- | --- |
| `required_capabilities` | the agent must have all of them |
| `required_permissions` | the agent must hold all of these permissions |
| `forbidden_permissions` | an agent holding any of these is rejected |
| `min_confidence` | 0.0 to 1.0: abstain under it |
| `allow_fallback` | `false` leaves the selection alone |
| `failed_agent_id` | an agent that just failed: the answer is what to use instead, one hop, honouring its `fallback_id` |
| `max_alternatives` | how many runners-up to list |
| `scope` | which namespace of agents to consider |

The caller narrows, never the task. A requirement is the caller's own
statement; nothing in the task's words can add a permission to an agent.

In a shell, `/agents` prints the registry and what it holds, and
`/agents fix the CORS bug in the FastAPI API` routes that task over it. From
eval the same decision is two calls:

```lisp
(agent-router:route "fix the CORS bug")                  ; the lines /agents prints
(agent-router:route-json "fix the CORS bug" :min-confidence 0.4) ; the JSON the tool answers
(agent-router:agents-text)                                      ; the registry
```

`(help :agent-router)` carries the same manual into the conversation.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, the verbs, the one condition |
| `router.lisp` | the agents, the tokenizer, the access policies, the score, the decisions, the JSON and the lines |
| `cell.lisp` | the `agent-router` section, the example registry, the `agent_route` tool, `/agents`, `(help :agent-router)` |
| `test/` | offline tests: the lifecycle, the tool through its dispatch seam, every policy, the abstention, the one-hop fallback, the command and a registry file |

## Tests

```sh
.github/test-cell.sh tools/nodecode-agent-router
```

Offline: the cell has no network path at all. The tool is called through the
wire a provider sends, the agents are literals, and the registry file is one
the test writes in a scratch home.

## Licence

MIT. The original plugin is Copyright (c) 2026 Pinuts: see `LICENSE` and `NOTICE`.
