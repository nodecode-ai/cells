# nodecode-orquehelx

OrqueHelx: sends each subagent to the subscription you pick, one route per provider and model, and says plainly when one runs out of quota. A Nodecode rewrite of [TicoraX/OrqueHelx](https://github.com/TicoraX/OrqueHelx) by [TicoraX](https://github.com/TicoraX).

## Get it

Tell the agent "add the orquehelx cell", or run `nodecode add nc://orquehelx`.

## Set it up

No key of its own: a route names a provider you already have configured, so the
subagent runs on credentials that are already there. Write the routes in the
`orquehelx` section of `~/.nodecode/config.jsonc` (`/settings`, or the file;
`config.example.jsonc` has the shape):

```jsonc
"orquehelx": {
  "policy": "ask",
  "seconds": 900,
  "routes": [
    { "name": "claude", "provider": "anthropic", "model": "claude-haiku-4-5" },
    { "name": "codex",  "provider": "openai-codex", "model": "gpt-5.5" }
  ]
}
```

| Key | Meaning |
| --- | --- |
| `routes` | One object per route: `name` (lowercase letters, digits, `-`, up to 32), `provider` and `model`, both required. |
| `policy` | What the parent does when a route runs out of quota: `ask` (default), `parent_decides` or `wait`. |
| `seconds` | How long one delegated subagent's turn may run before it is stopped (default 900). |

A malformed route — no name, a name that is not lowercase letters, digits or `-`,
no provider, no model, an unknown field — is skipped rather than guessed at, and a
standing notice says which and why; a repeated name keeps the first. The section
is read when the cell starts, so `(restart-cells "nodecode-orquehelx")` is what
a rewritten route list needs.

## Use it

Ask for a task to go to a route. The agent calls `delegate_to` with the route's
name, the goal, and any context the subagent cannot see:

```
delegate_to(route: "codex",
            goal: "Find every caller of parse_header and say which ones pass a null buffer.",
            context: "src/parse.lisp, the header parser")
```

- The goal runs in a **fresh subagent session** pinned to the route's provider and
  model: its own transcript, the same folder, listed on `/sessions` under the
  session that asked. It never sees the parent's conversation.
- The answer is JSON: `status`, the `route` it ran on, the child's `summary`, and
  its `session` id.
- Only route names and the model behind each are in the tool's schema — the
  provider and the endpoint are never tool parameters, so the agent cannot
  invent a route, and a name outside the list is refused before anything runs.

`/ohx routes` lists every route with its provider and model. `/ohx quota` is one
line per route: a provider the cell has seen run out of quota, with the hour it
was seen and the reset it named, or `no data` — a figure is never estimated. What
happens after an exhaustion is the policy:

- `ask`: the answer says `status: out_of_quota`, names the reset it knows (or
  that it knows none) and the other routes, and tells the parent to ask you and
  wait. It cannot delegate again in that turn until your answer opens a new one;
  subagents already running still finish.
- `parent_decides`: the parent may resend the task on another route whose model
  suits it, and says which one it chose and why.
- `wait`: the parent tells you and does nothing else with the task.

A route counts as out of quota only when the provider's own words say so (a 429
that speaks of a usage limit or a quota, or a `RESOURCE_EXHAUSTED`), never on a
plain failure.

## What is not here

The measured usage windows Hermes has (`fetch_account_usage`, a percentage and a
reset time per provider): Nodecode has no such meter, so an exhaustion is
recorded from a delegated subagent's failure and `/ohx quota` prints no
percentage. Also not here: the `pre_tool_call` hook that blocks Hermes's own
`delegate_task` (a Nodecode subagent is launched by the cell itself), and the v0.1
Spanish config names.

## License

MIT. The original plugin is Copyright (c) 2026 TicoraX: see `LICENSE` and `NOTICE`.
