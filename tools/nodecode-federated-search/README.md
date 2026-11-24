# nodecode-federated-search

Federated Search: one query asked of every search backend you configured, merged, deduplicated and ranked. A Nodecode rewrite of [kumaxs/hermes-federated-search](https://github.com/kumaxs/hermes-federated-search) by [kumaxs](https://github.com/kumaxs).

## Get it

Tell the agent "add the federated-search cell", or run `nodecode add nc://federated-search`.

## Set it up

The cell needs one thing: the search lines it fans out to. Three backends are known by name -- `tavily`, `firecrawl`, and a SearXNG instance of your own -- so a line for them is just its name (a SearXNG line also names its `base_url`). Any other endpoint that takes the query as a parameter is a line of its own.

In `/settings` (or the page's Settings tab), under `federated-search`:

```jsonc
{
  "federated-search": {
    "backends": [
      { "name": "tavily" },
      { "name": "minimax",
        "base_url": "https://api.minimaxi.com",
        "search_path": "/v1/coding_plan/search",
        "query_param": "q",
        "api_key_env": "MINIMAX_CN_API_KEY" }
    ]
  }
}
```

A line's keys are **environment variable names**, never values: export them, or set them in the same panel, and the cell reads them at request time. `tavily` reads `TAVILY_API_KEY`, `firecrawl` reads `FIRECRAWL_API_KEY`; a line of your own says which variable holds its key with `api_key_env`, or lists several under `credentials` to rotate and fail over between them. A key pasted where a name belongs is refused at start, with the name of the line it was on.

`/federated-search` with no argument shows every line, its endpoint, and whether each key is set.

## Use it

Ask for what you want; the model calls `federated_search`:

| Tool | Use |
| --- | --- |
| `federated_search` | one query across every configured line -- the merged, ranked list |

From the keyboard, `/federated-search <query>` runs one search and shows its results.

### What a line can say

| Key | Meaning |
| --- | --- |
| `name` | what the line is called in an answer; a name alone is a known backend |
| `base_url`, `search_path` | where its search API is |
| `query_param` | the field the query goes in (`q` by default; `query` for tavily and firecrawl) |
| `count_param` | the field the result count goes in (`max_results` by default; empty for none) |
| `method` | `POST` (the default) or `GET` |
| `auth_style` | `bearer` (the default), `x-api-key`, or empty for no key header |
| `api_key_env` or `credentials` | the environment variable names its keys are read from |
| `pool_strategy` | this line's own `fill_first`, `round_robin` or `random` |
| `required` | the whole search fails if this line returns nothing |

Three answer shapes are read: `{"organic": [{title, link, snippet}]}`, `{"data": {"web": [{title, url, description}]}}`, and `{"results": [{title, url, content}]}`. A line that answers in another shape is a line that answers no results.

### The section

| Key | Default | Meaning |
| --- | --- | --- |
| `backends` | -- | the lines; the one thing this cell cannot run without |
| `timeout` | 10 | seconds for the whole fan-out; what is still unanswered is dropped |
| `max_results` | 8 | the most ranked results one answer carries |
| `min_backends` | 1 | how many lines must return results for the search to count |
| `pool_strategy` | `fill_first` | the global default for a line that does not say its own |
| `max_chars` | 400 | how much of one result's snippet an answer carries |

### How a search comes back

One numbered entry per result -- a title, its url, then a snippet -- under a first line saying how many lines answered out of how many, and how long it took. A line that failed, timed out or had no key to spend is a `[note]` above the results, so a short list is a list one backend answered alone. The search fails as a whole only when a line marked `required` returns nothing, or when fewer lines answer than `min_backends` says.

Results are deduplicated by url, then ordered by how many of the query's terms each one carries in its title or description -- the original's keyword ranking. Ties keep the order the lines answered in.

### Keys that rest

A card (one environment variable) that fails -- a 401, 403 or 429, a 5xx, a connection error -- rests before it is spent again: an hour for a 429, five minutes otherwise, and a minute when it is the line's only card. A line with several cards fails over to the next one inside the same request. This state is in-process: a restart clears it.

## Not ported

- **The LLM re-ranking.** The original could send the merged results to an auxiliary model for relevance ordering. A cell would have to drive a model of its own to do it; keyword ranking is the original's own default and its fallback, and it is what this cell does.
- **The health-check HEAD probes** (`health_check`). They saved the timeout of a dead line; here a dead line costs its own share of the deadline, as it did with the probes off.
- **The desktop card editor** (the original's `desktop/plugin.js`, one card per line with L1/L2 membership checkboxes). The section's `backends` array is the same tree, edited as JSON.
- **The `k` alias for the line count**, and the `ranking_mode` / `ranker` / `ui_language` keys, which have nothing to choose between here.

## Gaps

- **The lines are asked one after another**, not on a thread pool, each with what is left of the section's deadline as its ceiling. The partial answer, the `required` rule and `min_backends` are the original's; only the wall-clock overlap of the requests is not.
- **No per-call line selection.** The model asks every configured line, as the original's provider did; a line is dropped only by being taken out of `backends`.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-federated-search
```

Every request is a stubbed `nlk:http`, so nothing reaches a search API and no key is needed.

Apache-2.0 licensed, as the original is.
