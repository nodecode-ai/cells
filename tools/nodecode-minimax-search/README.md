# nodecode-minimax-search

MiniMax web search: titles, links, snippets and dates for a query, on a MiniMax Token Plan key. A Nodecode rewrite of [pdonizete/hermes-minimax-search](https://github.com/pdonizete/hermes-minimax-search) by [pdonizete](https://github.com/pdonizete).

The original is a web search provider for a harness's own search backend: it
registers a `minimax` backend that answers a query from the MiniMax Token
Plan search endpoint. Nodecode's search cell keeps its engines in a fixed
list a cell cannot add one to, so this cell offers the same call as a tool of
its own, and `/minimax-search` for the operator.

## Get it

Tell the agent "add the minimax-search cell", or run:

```sh
nodecode add nc://minimax-search
```

## Set it up

One key, and it has to be a **Token Plan** key — the ones that start with
`sk-cp-`, from
<https://platform.minimax.io/user-center/basic-information/interface-key>.
An ordinary MiniMax model API key is refused by the search endpoint.

```jsonc
{
  "minimax-search": {
    // the variable the key is read from; or write the key here as "api_key"
    "api_key_env": "MINIMAX_CODE_PLAN_KEY",
    // "global" (api.minimax.io) or "cn" (api.minimaxi.com), where the key was minted
    "region": "global",
    "max_results": 5,
    "timeout_seconds": 30
  }
}
```

The original also read the key from `MINIMAX_CODING_API_KEY`,
`MINIMAX_OAUTH_TOKEN` or `MINIMAX_API_KEY`, in that order; this cell reads
the variable the section names first and those three after it. A section
that names a variable with nothing in it refuses at start, and the refusal
says which variable and where a key comes from.

## Use it

The model has one tool while the cell runs:

- `minimax_search {query, limit?}` — titles, urls, snippets and dates for
  a query, at most 10 rows. The endpoint has no count knob, so the payload
  is just the query and the cell lists the first `limit` rows of the answer,
  plus the related searches the endpoint suggests.

`(help :minimax-search)` is the primer behind it. From eval the same call is
`(minimax-search:search "query" :limit 5)`, which answers a string.
`/minimax-search QUERY` searches from the operator's side; with no argument
it says which host it asks and how many results a search lists.

Search is all it does: the endpoint returns snippets, never page bodies. Use
the websearch cell to fetch a page a result names.

## What changed

- The endpoint, its request body and its answer shape are the original's:
  `POST /v1/coding_plan/search` with `{"q": …}`, read back from `organic[]`
  as title, link, snippet and date, with `base_resp.status_code` recognised
  as the endpoint's own error channel.
- The original let `MINIMAX_REGION` and `MINIMAX_API_HOST` move its calls;
  here that is the section's `region` and `base_url`, so the operator sets
  it in one place.
- Its provider registration and setup-panel schema are the harness's
  onboarding, not a capability: the section, the tool and its refusal are
  this cell's.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-minimax-search
```

Every request is a stubbed `NLK:HTTP` and the key a stubbed
`NLE::CREDENTIAL-ENV`, so nothing reaches MiniMax or the network.

MIT licensed.
