# nodecode-websearch

Gives the model the web as two functions in the `web:` package, called
through `eval`. It began as
[pi-web-access](https://github.com/nicobailon/pi-web-access) reduced to its
core. It registers no tool and no slash command: while the cell runs,
`(help :web)` answers the primer, and every session's help section names the
two calls.

```lisp
(web:search "query" &key n provider)            ; n 1..10, default 5
(web:fetch "https://..." &key offset limit reader)
```

`search` works without a key: DuckDuckGo, Firecrawl and keyless Exa answer
together. With a Brave, Exa, Tavily or DeepSeek key, that provider answers alone
and may put its own answer on top. DeepSeek is DeepSeek's own web search: one
`deepseek-v4-flash` turn with its native `web_search` tool, whose results are
the hits and whose closing answer gives each a one-line summary. A Firecrawl key
only lifts its limits.
`:provider "arxiv"` searches papers instead of the web.

`fetch` returns the title, the url, then up to `limit` characters of the page
(6000 by default, 7000 at most). A cut result ends with the exact call that
reads the next slice, and the page stays cached for an hour, so that call costs
no second download. HTML becomes text with its links kept, a PDF is converted,
and a page this cell can't read (a JavaScript app, a refusal) goes to
Firecrawl, then Exa. `:reader` picks one: `http` (that chain), `firecrawl`,
`exa` or `jina`.

## Config

```jsonc
"websearch": {"enabled": true,
              "provider": "brave",
              "providers": {"brave":     {"api_key": "..."},
                            "exa":       {"api_key": "..."},
                            "tavily":    {"api_key": "..."},
                            "deepseek":  {"api_key": "..."},
                            "firecrawl": {"api_key": "..."}},
              "max_results": 5,
              "fetch": {"max_bytes": 5242880, "limit": 6000, "reader": "http"}}
```

Keys sit inline, as in the core `providers` block, and are redacted from any
text the model sees. An empty section works too.

A key here is a search key: it pays for searches and nothing else, apart from
the key the machine's models run on. Another cell that searches on one of these
engines reads it through `(web:provider-key "deepseek")`, which answers the
configured key while this cell runs, else `NIL`.

A fetch that falls through to a second reader can take a minute, so the primer
tells the model to pass `yield_time_ms 60000`. Failures read
`ERROR: WEBSEARCH-ERROR: <detail>`.

Tests: `just websearch-check` in nodecode.
