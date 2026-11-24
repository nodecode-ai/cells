# nodecode-web-search-plus

Web Search Plus: multi-provider web search and clean page reading, as two tools the model calls. A Nodecode rewrite of [robbyczgw-cla/hermes-web-search-plus](https://github.com/robbyczgw-cla/hermes-web-search-plus) by [robbyczgw-cla](https://github.com/robbyczgw-cla).

The two tools are the original's:

- `web_search_plus` — search the web. With `provider: "auto"` the query's shape picks the first engine (Brave for a general question, Serper for security, shopping and news, Exa for documentation and papers) and the next configured engine answers when one fails or finds nothing. Recency (`freshness`), the news vertical, `include_domains`/`exclude_domains` and `mode: "research"` (search, then read the top pages) are the original's parameters.
- `web_extract_plus` — read pages as clean text, falling back from Tavily to Firecrawl to Exa to a plain fetch here that strips the HTML. `render_js` asks Firecrawl first, because it renders JavaScript.

A person can call the same functions from eval: `(web-search-plus:search "…")` and `(web-search-plus:extract "https://…")` taking a decoded JSON object; the model learns them from `(help :web-search-plus)`.

## Get it

Tell the agent "add the web-search-plus cell", or run `nodecode add nc://web-search-plus`.

## Set it up

One provider key is enough. Put it in the `web-search-plus` section of the config — in `/settings`, on the settings page, or by asking the agent to set it — or in the environment variable the key names:

```jsonc
{
  "web-search-plus": {
    // One engine is enough; more give the fallback somewhere to go.
    "brave_api_key": "…",          // BRAVE_API_KEY   https://api.search.brave.com/app/keys
    "serper_api_key": "…",         // SERPER_API_KEY  https://serper.dev/api-key
    "tavily_api_key": "…",         // TAVILY_API_KEY  https://app.tavily.com
    "exa_api_key": "…",            // EXA_API_KEY     https://dashboard.exa.ai/api-keys
    "firecrawl_api_key": "…",      // FIRECRAWL_API_KEY  extraction, and JavaScript pages
    // A self-hosted instance needs no key:
    "searxng_url": "https://searx.example.org",   // SEARXNG_INSTANCE_URL
    "provider": "auto",            // or brave, serper, tavily, exa, searxng
    "extract_provider": "auto",    // or tavily, firecrawl, exa, native
    "max_results": 5,
    "max_chars": 6000,
    "timeout": 20
  }
}
```

With no key and no `searxng_url` the folder refuses to start, in one line naming what to set.

## Use it

Ask for a search, or for a page to be read. The answer is one JSON object:

```json
{"query": "…", "provider": "brave", "count": 2,
 "results": [{"title": "…", "url": "…", "snippet": "…", "age": "…"}],
 "notes": ["serper: HTTP 401: …"]}
```

`notes` carry what happened on the way — which engine was skipped and why, that a domain list was applied here, that an engine has no news vertical — and never replace an answer. `mode: "research"` adds `pages`, the first pages' text. `web_extract_plus` answers `pages` (url, title, chars, truncated, text) and `notes`; a page nothing could read is a note, not a failure.

## What the rewrite leaves out

The original speaks to fourteen search engines and eight extractors. This cell keeps the ones whose API is one HTTP request — Brave, Serper, Tavily, Exa, SearXNG for search, Tavily, Firecrawl, Exa and a plain fetch for extraction — and leaves out You.com, Linkup, Parallel, Keenable, Serpbase, Querit, TinyFish, the Firecrawl search vertical and the DonSeTch CLI. Also left out: the search cache, the quality report, the routing diagnostics and health statistics, the semantic-span ranking, markdown output (every page is text), and the native Hermes web-provider bridge, which has no meaning outside Hermes.

## Tests

```
cd cells && NODECODE_SRC=… .github/test-cell.sh tools/nodecode-web-search-plus
```

Every provider is a stubbed `nlk:http`, so the suite needs no network and no key: the lifecycle, a missing key refusing, a search per engine, the fallback, the routing, the domain filter, research mode, the extraction chain, a page cut to its limit, and a url scheme that is never fetched.

## Licence

MIT, as the original. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
