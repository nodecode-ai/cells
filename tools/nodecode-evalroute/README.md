# nodecode-evalroute

Evalroute: route a task to a model and a reasoning effort, and label how the route did. A Nodecode rewrite of [keppy/hermes-plugin-evalroute](https://github.com/keppy/hermes-plugin-evalroute) by [keppy](https://github.com/keppy).

A task in your own words becomes a card: the lane it belongs to, the arm that lane names -- a model *and* an effort together -- the evidence that decided it, and what that lane has measured so far. You rate the outcome, and the lane's rate is what the next card reads. Everything happens on this machine: the classification is keywords, the ledger is a file, and no model is called to pick a model.

## Get it

Tell the agent "add the evalroute cell", or run `nodecode add nc://evalroute`.

## Set it up

The cell needs to know which model each lane runs on, because Nodecode names its own models:

```jsonc
"evalroute": {
  "models": "build=z-ai/glm-5, quick=z-ai/glm-5-air, review=<a strong model>, read=<a long-context model>, plan=<a reasoning model>",
  "default_lane": "read"
}
```

Set it in the `evalroute` section of `config.jsonc` or in `/settings`. A lane with no model still routes -- it says `no model named` and shows the pair to write. There is no key and nothing leaves the machine; `config.example.jsonc` documents every member.

## Use it

Ask the agent to route a task (`evalroute_route`), or in a shell:

```
/route refactor the parser and fix the regression in the class module
```

```
evalroute: build - implement, fix or refactor code
  arm: strong/coder, effort high
  decided by: rules match, 6 keyword hits: refactor, fix, parser, class, module, regression
  route id: r-20261109-1
  payoff: priors: 1 route, nothing rated for this lane yet
  next: /model strong/coder, /think high; after the work, /rate pass|fail
```

Then set the two switches the card names (`/model`, `/think`), do the work, and label it:

- `/rate pass`, `/rate fail`, `/rate skip`, with a word of note after it: `/rate pass shipped`. `--lane quick` files it under the lane you say it belonged to, which is a correction, not a loss.
- `(evalroute:report)` prints the books: every lane, its arm, and passes, fails, skips and unrated routes against it.
- `/route lanes` prints the table; `/route --lane quick a rename` pins a lane when you know better than the keywords.
- `(evalroute:route "...")`, `(evalroute:rate "pass")`, `(evalroute:lanes)`, `(evalroute:status)` through eval; `/rate` also takes `--route-id` and `--lane`.

A ledger nobody rates only grows: each card's `payoff:` line then says nothing is rated yet, which is the truth.

## Where it lives

The ledger is one JSON-lines file, `routes.jsonl`, in `data_dir`, by default the `evalroute/` folder under the home: one line a route, rewritten atomically under the cell's lock when a verdict lands, and trimmed to `max_routes` (200) newest. It outlives a restart and is the only state there is. Missing settings refuse at start; a write that cannot happen is a refusal, not a silent loss.

## License

MIT. Evalroute is Copyright (c) 2026 James Dominguez: see `LICENSE` and `NOTICE`. The rewrite left out the first-turn advice hook, the `dispatch` worker, `install-routes` and the Hugging Face `sync`/`contribute`; NOTICE says why.
