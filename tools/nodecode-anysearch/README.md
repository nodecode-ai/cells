# nodecode-anysearch

AnySearch: web search and page extract through AnySearch's REST API, with the
MCP extract fallback beside it. A Nodecode rewrite of
[vollegrewar/hermes-plugin-anysearch](https://github.com/vollegrewar/hermes-plugin-anysearch)
by [vollegrewar](https://github.com/vollegrewar).

The model gets two tools: `anysearch_search`, which returns titles, links and
an abstract per result, and `anysearch_extract`, which fetches each URL as
text. Both talk to AnySearch's own REST API. `/anysearch QUERY` is the
operator's own search, and `(help :anysearch)` is the model's page.

## Get it

Tell the agent "add the anysearch cell", or run `nodecode add nc://anysearch`.

## Set it up

Nothing is required: AnySearch answers without a key, at a lower quota.

A free key from [anysearch.com](https://www.anysearch.com/console/api-keys)
raises the quota to 1,000 requests a day. Put it in the environment:

```sh
echo 'ANYSEARCH_API_KEY=…' >> ~/.nodecode/.env
```

or in the cell's section, which `/settings` writes:

```jsonc
{
  "anysearch": {
    "api_key_env": "ANYSEARCH_API_KEY",   // or "api_key" written inline
    "max_results": 5,
    "max_urls": 5
  }
}
```

With an `Authorization` header present but the key invalid, the API answers
401 or 403 rather than falling back to the anonymous tier, and the tool says
so in its answer.

| Setting | Default | What it does |
| --- | --- | --- |
| `api_key` / `api_key_env` | none | the key, inline or by environment variable (`ANYSEARCH_API_KEY` is read either way) |
| `base_url` | `https://api.anysearch.com` | the API root, for a proxy of your own |
| `max_results` | `5` | results one search asks for, at most 10 |
| `max_urls` | `5` | URLs one extract fetches, one request each |
| `timeout_seconds` | `30` | one search's ceiling |
| `extract_timeout_seconds` | `60` | one page fetch's ceiling |
| `mcp_fallback` | `true` | ask the `/mcp` endpoint when `/v1/extract` answers no content or fails |

## Use it

- **`anysearch_search`** — `{query, limit?}`. Answers one line per result: its
  title, its URL and an abstract clipped to 400 characters. `limit` is
  clamped to 1..10; unset, the section's `max_results` decides.
- **`anysearch_extract`** — `{urls}`. One request per URL, in the order
  given, each page's text under its title. A URL that fails says so on its
  own line and the rest still answer, as the original's per-URL error objects
  do.
- **`/anysearch QUERY`** — the same search, for the operator. With no
  argument it says where the cell points and whether it has a key.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the endpoints and the MCP fallback, the two tools, `/anysearch` and `(help :anysearch)` |
| `test/cell-test.lisp` | lifecycle, both tools, the fallback, `/anysearch` and the refusals, every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Why two tools, and not a search backend

In Hermes the plugin registered itself as three web providers behind the
standard `web_search` and `web_extract` tools. Nodecode's `nodecode-websearch`
cell keeps its engines in a fixed list a cell cannot add one to, so AnySearch is
two verbs of its own here. The endpoints, the request shapes and the answers
are the original's; only the seam differs.

The plugin's own MCP extract fallback came with it: when `/v1/extract`
answers no content or fails, the same URL is asked of `/mcp`, whose
`content[].text` is a JSON string that is parsed once more. The two chains the
plugin also shipped, Exa → AnySearch and Firecrawl → AnySearch, were left out:
they rank another provider first, and that order belongs to the caller, not to
this cell.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-anysearch
```

## License

MIT. Copyright (c) 2026 vollegrewar: see `LICENSE` and `NOTICE`.
