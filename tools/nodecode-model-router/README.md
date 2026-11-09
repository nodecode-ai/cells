# nodecode-model-router

Model router: picks the LLM a task goes to, under quality, cost and latency limits, and says why. A Nodecode rewrite of [Pinutss/jev-model-router](https://github.com/Pinutss/jev-model-router) by [Pinutss](https://github.com/Pinutss).

## Get it

Tell the agent "add the model router cell", or run `nodecode add nc://model-router`.

## Set it up

Nothing is required. The cell ships the original's catalog of seven vendors (OpenRouter, OpenAI, Groq, Together, Fireworks, Mistral, Ollama, thirteen models) and ranks it with no key, no network and no model call. A key is only ever *named*: each row carries the environment variable that would hold its key, and the answer says whether this process has one (`has_key`).

Set a member in `/settings` or in `~/.nodecode/config.jsonc` under `model-router`, then `(restart-cells "nodecode-model-router")`.

| Member | Default | Notes |
| --- | --- | --- |
| `catalog_file` | unset | a JSON catalog merged over the presets: `{"providers": {name: {base_url, api_key_env, models: [...]}}}` |
| `default_prefer` | `balanced` | how a call that names no preference is ranked |
| `default_model` | unset | the row `prefer=named` pins when a call names none, as `vendor:model` |
| `max_alternatives` | `3` | runners-up a decision lists |
| `max_candidates` | `50` | rows ever shown to a judge |
| `require_keys` | `false` | leave out rows whose key variable is not set here |
| `show_rejected` | `true` | name the rows a decision threw out, and why |
| `judge_url` | unset | a decision endpoint taking `{task, models}` and answering `{scores:[{id, score}]}` |
| `judge_key_env` | unset | the variable holding the judge's bearer token |
| `judge_timeout` | `30` | seconds one judge call may take |
| `judge_weight_percent` | `40` | what the judge's score is worth against the strategy's own numbers |

`config.example.jsonc` beside this file is the whole section, commented. An empty or absent section starts: the ranking is the local one.

## Use it

Ask for a model and the agent calls `model_route`:

```
model_route(task: "summarise this letter", max_cost: 0.2)
```

It answers a decision - `select`, `fallback` or `abstain` - with the chosen row, its vendor's `base_url`, the name of the variable holding its key, its score and the reasons, the runner-up and the alternatives, and the rows that were thrown out with the constraint that threw each one. It never calls a model and never writes text; the caller sends the task to the row it names.

| Constraint | What it does |
| --- | --- |
| `required_capabilities` | the row must have all of them: `chat`, `tools`, `json`, `coding`, `vision` |
| `prefer` | `balanced`, `best`, `cheapest`, `fastest`, or `named` with a pin |
| `min_quality`, `max_cost`, `max_latency` | 0.0 to 1.0, the row's own declaration |
| `min_context` | smallest context window, in tokens |
| `provider`, `model` | one vendor, or one row (an id, a model, or `vendor:model`) |
| `failed_model_id` | a row that just failed: the answer is what to use instead, one hop |
| `allow_fallback` | `false` leaves the choice alone |
| `models` | rank your own rows instead of the catalog, same fields |
| `max_alternatives` | how many runners-up to list |

A pin filters like every other constraint, so `prefer: "named"` with a `model` leaves exactly that row, and abstains when the catalog cannot show it.

The same decision is one call in `eval`:

```lisp
(model-router:catalog-text)                          ; the catalog, with which keys are set
(model-router:route "write a haiku about rain")       ; the lines /route prints
(model-router:route-json "..." :prefer "cheapest")   ; the JSON the tool answers
```

In a shell, `/route` prints the catalog with a row a vendor, `/route a quick summary` decides, `/route -- cheapest a quick summary` decides the other way, and `/route -- provider openai` opens one vendor's rows.

`(help :model-router)` has the same while the cell runs.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, the verbs, the one condition |
| `catalog.lisp` | the seven vendor presets, a catalog file, and whether a row's key is set here |
| `router.lisp` | the tokenizer, the four strategies, the hard filters, the decisions, the JSON and the lines |
| `judge.lisp` | the optional judge: its seam, its HTTP call, and the redaction of the task |
| `cell.lisp` | the `model-router` section, the `model_route` tool, `/route`, `(help :model-router)` |
| `test/` | offline tests: the ranking and every decision on bare rows, then the lifecycle, the tool through its dispatch seam, the command, the judge and the catalog file |

The judge is asked only when `judge_url` is set, sees only the redacted task and the rows' numbers, and a judge that fails leaves the local decision standing, with `judge_unavailable` among the reasons.

## Licence

MIT. The original plugin is Copyright (c) 2026 Pinuts: see `LICENSE` and `NOTICE`.
