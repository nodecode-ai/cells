# nodecode-delegation

Named model routes for subagents: the agent picks a `fast`, a `deep` or a `local` you defined, and only the names and the one-line descriptions you wrote reach it. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the delegation cell", or run `nodecode add nc://delegation`.

## Set it up

No key of its own: a route names a provider and a model you already have configured, so the subagent runs on credentials that are already there. Write the routes in the `delegation` section of `~/.nodecode/config.jsonc` (`/settings`, or the file; `config.example.jsonc` has the shape):

```jsonc
"delegation": {
  "backend": "model_map",
  "default_route": "fast",
  "routes": [
    { "name": "fast", "description": "Cheap and quick: lookups, summaries, simple edits.",
      "provider": "openrouter", "model": "google/gemini-3-flash-preview" },
    { "name": "deep", "description": "Slow and careful: hard debugging and design work.",
      "provider": "anthropic", "model": "claude-opus-5", "reasoning_effort": "high" }
  ]
}
```

| Key | Meaning |
| --- | --- |
| `backend` | `model_map` offers the agent the route names; `default` keeps every subagent on the parent's model and refuses a route out loud. |
| `routes` | One object per route: `name` and `description` (what the agent picks and reads), `provider`, `model`, optional `reasoning_effort`. |
| `default_route` | What a task that picks no route runs on. Empty means the parent's own model. |
| `seconds` | How long one delegated subagent's turn may run before it is stopped (default 900). |

A malformed route — no name, no description, no model, a name that is not letters, digits, `.`, `_` or `-` — is skipped rather than guessed at, and a standing notice says which and why; a repeated name keeps the first. The section is read when the cell starts, so `(restart-cells "nodecode-delegation")` is what a rewritten route list needs.

## Use it

Ask for a task to go to a route. The agent calls `delegate` with the task and the route's name:

```
delegate(task: "Find every caller of parse_header and say which ones pass a null buffer.",
         route: "fast")
```

- The task runs in a **fresh subagent session** whose model selection is the route's: its own transcript, the same folder, listed on `/sessions` under the session that asked. It never sees the parent's conversation — the task's words are all of it — so write them whole.
- The answer is the subagent's last words, then the route it ran on and its session id:

  ```
  Three callers: src/parse.lisp:88, :141 and src/http.lisp:32 -- the second and third pass a null buffer.
  [route fast: openrouter · google/gemini-3-flash-preview · session delegate-3KQ7]
  ```

- A task with no route runs on `default_route` when one is set, else on the parent's own model.
- Only route names and descriptions are in the tool's schema — the provider, the model id and the endpoint are never tool parameters, so the agent cannot invent a route, and a name outside the list is refused before anything runs.

`/delegation` prints the mode, each route's name and line, and what it actually runs on, with the other mode as a picker row. `/delegation model_map` (or `models`) and `/delegation default` save the mode, and say that a restart puts the new tool list on; the switch is live for the mode's gate at once, which is what refuses a route of the mode you left instead of running it on the wrong model.

## What is not here

- **Profile routes** — running a subagent *as another Hermes profile*. A Hermes profile is a config inside one process; a Nodecode profile is a separate home with its own organism, so a subagent cannot become one in this image.
- **Daybreak** (`daybreak: true`), per-route `service_tier`, `fallback_providers`, `api_key`, `api_mode` and `base_url`. A Nodecode provider section carries the endpoint and the key, and the reasoning effort is the one per-route setting this cell passes on.

## License

MIT. The original plugin is Copyright (c) 2026 Nous Research: see `LICENSE` and `NOTICE`.
