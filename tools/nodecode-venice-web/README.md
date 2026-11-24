# nodecode-venice-web

Venice AI web search and page extract: privacy-first search and Markdown page
fetch, on a Venice API key. A Nodecode rewrite of
[MnrGreg/hermes-venice-web](https://github.com/MnrGreg/hermes-venice-web) by
[MnrGreg](https://github.com/MnrGreg).

The model gets two tools: `venice_search`, which returns titles, links and
snippets for a question, and `venice_extract`, which fetches each URL as
Markdown. Both talk to Venice's own `/augment` endpoints, which route a search
to Brave (zero data retention) or Google (anonymized). `/venice QUERY` is the
operator's own search, and `(help :venice)` is the model's page.

## Get it

Tell the agent "add the venice-web cell", or run `nodecode add nc://venice-web`.

## Set it up

A Venice API key from [venice.ai](https://venice.ai), in the environment:

```sh
echo 'VENICE_API_KEY=…' >> ~/.nodecode/.env
```

or in the cell's section, which `/settings` writes:

```jsonc
{
  "venice": {
    "api_key_env": "VENICE_API_KEY",  // or "api_key" written inline
    "search_provider": "brave"        // brave (zero data retention) or google (anonymized)
  }
}
```

`/venice` with no argument says where the cell points. Each search and each
scraped URL costs about $0.01 on that Venice account; the cell says so in
every answer's header and in the tool descriptions, and `max_urls` caps what
one `venice_extract` call may spend.

| Setting | Default | What it does |
| --- | --- | --- |
| `api_key` / `api_key_env` | none | the key, inline or by environment variable (`VENICE_API_KEY` is read either way) |
| `base_url` | `https://api.venice.ai/api/v1/augment` | the augment root, for a proxy of your own |
| `search_provider` | `brave` | the engine Venice searches with |
| `max_results` | `10` | results one search asks for, at most 20 |
| `max_urls` | `5` | URLs one extract fetches, each billed |
| `timeout_seconds` | `20` | one search's ceiling |
| `scrape_timeout_seconds` | `30` | one page fetch's ceiling |

## Use it

- **`venice_search`** — `{query, limit?, provider?}`. Answers one line per
  result: its title, its URL and a snippet clipped to 400 characters. `limit`
  is clamped to 1..20, `provider` to `brave` or `google`; unset, the section
  decides.
- **`venice_extract`** — `{urls}`. One request per URL, in the order given,
  each page's Markdown under its title. A URL that fails says so on its own
  line and the rest still answer, as the original's per-URL error objects do.
- **`/venice QUERY`** — the same search, for the operator.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the two endpoints, the two tools, `/venice` and `(help :venice)` |
| `test/cell-test.lisp` | lifecycle, both tools, `/venice` and the refusals, every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Why two tools, not a search backend

In Hermes the plugin registered itself as the harness's web search provider,
behind the standard `web_search` and `web_extract` tools. Nodecode's
`nodecode-websearch` cell keeps its engines in a fixed list a cell cannot add
one to, so Venice is two verbs of its own here. The endpoints, the request
shapes and the answers are the original's; only the seam differs.

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-venice-web
```

## License

MIT. Copyright (c) 2026 Greg May: see `LICENSE` and `NOTICE`.
