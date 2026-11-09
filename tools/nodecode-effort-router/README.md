# nodecode-effort-router

Effort Router: each turn is routed to the model and the reasoning effort a decision
model picks for that ask. A Nodecode rewrite of
[AlphaPerseii3000/jev-effort-router](https://github.com/AlphaPerseii3000/jev-effort-router)
by [AlphaPerseii3000](https://github.com/AlphaPerseii3000).

A conversation is not one kind of work. "what is the capital of Norway" wants a cheap
fast model with no thinking; a refactor across four files wants the strongest coding
model at high effort. The original plugin put a router between Hermes and its model
pick: one decision call per turn, two Choice questions — which model, how hard should
it think — and the request went out on the answer. This cell puts the same router at
the seam where Nodecode freezes a turn's provider settings, so what a turn runs on is
decided once, per turn, and every round of its tool loop shares that one decision.

## Get it

Tell the agent "add the effort-router cell", or run `nodecode add nc://effort-router`.

## Set it up

The router asks TypeSafe's Jev, through OpenRouter's Decisions API, for the decision.
That needs an OpenRouter key: https://openrouter.ai/settings/keys.

```
"effort-router": {
  "api_key_env": "OPENROUTER_API_KEY"
}
```

`api_key_env` is the name of the environment variable holding the key. The other
members, all optional:

| Member | Default | What it does |
| --- | --- | --- |
| `provider` | `ollama-cloud` | the one Nodecode provider whose turns are routed |
| `grid` | the measured six | the models the decision may choose, one `"model: what it is for"` per entry |
| `decision_model` | `typesafe/jev-1.13` | the decision model, pinned so an upstream swap cannot change routing |
| `endpoint` | OpenRouter's Decisions API | where the decision is asked for |
| `confidence_percent` | `50` | a decision under this confidence is dropped |
| `route_per_turn` | on | off, one decision serves a whole session |
| `timeout_seconds` | `8` | one decision's ceiling |
| `audit` | on | one line per decision in `routes.jsonl` under the cell's home |

A section naming no key refuses at start and says so; the tools and the command are
absent until it is set, and `/cells` shows the line to fix.

## Use it

Once it runs, nothing has to be asked for: every turn on the routed provider is
decided, and the turn runs on what was decided. Two tools let the model look:

- `effort_status` — the provider, the decision model, the grid, the confidence floor,
  how many decisions this boot made, and the newest one.
- `effort_route` — the decision for a text you hand it, without running a turn on it.

and `/effort-router` shows the same status with a row to route a text. In eval:

```lisp
(nodecode-effort-router:decide "prove this lemma about groups")
;; => the route, and why there is none (two values)
```

### What it will not do

The router never costs a turn. No key, a timeout, a connection that dies, an answer
that is not the shape it should be, a model off the grid, a confidence under the floor:
each of those leaves the request exactly the request it would have been. One decision
a turn, not one a round. And only the section's provider is routed: a turn on any
other provider is not even asked about, because the grid names models one provider must
serve.

The effort rung (`low`, `medium`, `high`) is what the decision picks; which sound each
model's wire takes for it is the provider lane's own clamp, so a model that has no
`medium` gets the nearest rung it does have, and a model that does not reason gets no
effort field at all.

## Credit

Rewritten from AlphaPerseii3000's jev-effort-router; see [NOTICE](NOTICE). The
original's MIT licence is kept in [LICENSE](LICENSE).
