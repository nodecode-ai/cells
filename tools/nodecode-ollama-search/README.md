# nodecode-ollama-search

Ollama Cloud Web Search: web search and page fetch through Ollama Cloud's native API, on the Ollama API key a Cloud profile already has. A Nodecode rewrite of [rriggs/hermes-plugin-ollama-cloud-search](https://github.com/rriggs/hermes-plugin-ollama-cloud-search) by [rriggs](https://github.com/rriggs).

The original is a web search provider for a harness's own search backend: it
advertises search and content extraction, and answers both by asking two
Ollama endpoints, `POST /api/web_search` and `POST /api/web_fetch`, with
`OLLAMA_API_KEY` as a bearer. Nodecode's search cell keeps its engines in a
fixed list a cell cannot add one to, so this cell offers the same two calls
as two tools of its own, and `/ollama-search` for the operator.

## Get it

Tell the agent "add the ollama-search cell", or run:

```sh
nodecode add nc://ollama-search
```

## Set it up

One key, and it is the one Ollama Cloud chat completions already use: a key
from <https://ollama.com/settings/keys>, in `OLLAMA_API_KEY`. A profile
serving Ollama Cloud models needs nothing else.

```jsonc
{
  "ollama-search": {
    // the variable the key is read from; or write the key here as "api_key"
    "api_key_env": "OLLAMA_API_KEY",
    // the host the two endpoints are asked on
    "base_url": "https://ollama.com",
    "max_results": 5,
    "timeout_seconds": 30,
    "fetch_timeout_seconds": 60
  }
}
```

A section that names a variable with nothing in it refuses at start, and the
refusal says which variable and where a key comes from. `base_url` moves
every call to another host, for a proxy or a compatible server.

## Use it

The model has two tools while the cell runs:

- `ollama_search {query, limit?}` — titles, urls and a snippet per result,
  at most 10, as Ollama's search returns them.
- `ollama_fetch {url}` — one page as the text Ollama fetched and extracted
  from it.

`(help :ollama-search)` is the primer behind them. From eval, the same
calls are `(ollama-search:search "query" :limit 5)` and
`(ollama-search:fetch "https://…")`, each answering a string. `/ollama-search
QUERY` searches from the operator's side; with no argument it says which
host and how many results a search asks for.

## What changed

- The endpoints, their request bodies and their answer shapes are the
  original's: `/api/web_search` with a query and a count capped at 10,
  `/api/web_fetch` with one url per call.
- The original also let `OLLAMA_BASE_URL` move its calls; here that is the
  section's `base_url`, so the operator sets it in one place.
- Its "free tier" badge and its setup-panel schema are the harness's
  onboarding, not a capability: the section and its refusal are this cell's.
- It honoured the harness's interrupt flag between calls; a Nodecode turn
  is cancelled by its own condition, which passes through the tools
  untouched.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-ollama-search
```

Every request is a stubbed `NLK:HTTP` and the key a stubbed
`NLE::CREDENTIAL-ENV`, so nothing reaches Ollama or the network.

MIT licensed.
