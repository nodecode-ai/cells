# nodecode-fallback

Fallback: heuristic model failover, ranking your own providers' models and keeping Nodecode's failover chain filled. A Nodecode rewrite of [tomkubik/hermes-fallback](https://github.com/tomkubik/hermes-fallback) by [tomkubik](https://github.com/tomkubik).

Nodecode already walks a chain of fallback models when a round's own retries are spent on a rate limit, an outage, a broken answer or a refusal. What it does not do is choose the models in that chain: that is a job for whoever is still awake. So this cell ranks every model your configured providers offer against what a turn needs -- a context window big enough, a model big enough, a recent one, a free one, one that can call tools, one on the provider you trust first -- and writes the best few into `failover.chain`. It makes no model call, so it works while the model you are on is the one that is down.

## Get it

Tell the agent "add the fallback cell", or run `nodecode add nc://fallback`.

## Use it

Ask for it, or call it:

- `fallback_resolve` -- the ranking, without writing anything.
- `fallback_apply` -- the ranking, written into `failover.chain`.
- `fallback_status` -- the primary model, the chain, and the providers this box can name models for.
- `/fallback status | resolve | apply | clear | providers [key=value ...]`, the same from the prompt, when the model that would answer is the one that just failed:

```
/fallback resolve min_context=128k sort_by=latest free_only=true
/fallback apply count=3
/fallback clear
```
- From eval: `(fallback:resolve)`, `(fallback:apply-chain :max-candidates 3)`, `(fallback:status)`, `(fallback:providers)`. `(help :fallback)` is the manual.

Criteria, the section's or a call's: `provider` (`auto`, `existing`, or one id), `providers` (a preference order, which also limits the search), `exclude_providers`, `sort_by` (`largest_parameter_count`, `greatest_context`, `latest`, `latest_flash`, `smallest`, `provider_preference`), `min_context`/`max_context` (`128k`, `1m`, `200000`), `min_params`/`max_params` (`30b`), `free_only`, `flash_only`, `require_tools`, `vendor`, `name_filter` (a substring, or a `*` glob), `prefer_current_provider`, `allow_provider_switch`, `max_candidates`.

Where the numbers come from, in this order: the model id itself (`70b`, `128k`, `:free`, `4o-mini`), then Nodecode's own models.dev row for the id, then a small table of families whose ids publish none. A number nobody published stays unknown, and a criterion that needs one leaves that model out rather than guessing; `require_tools` drops only a model whose catalog row says it cannot call tools.

## Set it up

Nothing is required. The section is optional, and `auto_sync` is on by default: while `failover.chain` is empty, the cell fills it at start from the section's criteria and says so. A chain somebody wrote by hand is never touched -- say `/fallback apply` to replace it.

```jsonc
{
  "fallback": {
    "sort_by": "largest_parameter_count",
    "min_context": 128000,
    "require_tools": true,
    "max_candidates": 3,
    "auto_sync": true
  }
}
```

No key, no endpoint and no account: it reads the providers you already configured and the catalog Nodecode already caches.

## The credit

MIT, Copyright (c) 2026 Tom Kubik: see `LICENSE` and `NOTICE`.
