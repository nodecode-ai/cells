# nodecode-umt

Universal MCP Toolkit: Hacker News, arXiv and npm lookups. A Nodecode rewrite of [Markgatcha/universal-mcp-toolkit](https://github.com/Markgatcha/universal-mcp-toolkit) by [Markgatcha](https://github.com/Markgatcha).

The plugin ships three MCP servers that need no configuration at all — Hacker
News, arXiv and the npm registry. This cell is those three, reimplemented in
Lisp against the same public APIs: eleven read-only tools the model can call,
the same lookups as functions in the `umt` package for eval, and `/umt` to ask
by hand. Nothing to install, nothing to sign in to, no key.

## Get it

Tell the agent "add the umt cell", or run `nodecode add nc://umt`.

## Set it up

Nothing is required: all three APIs are public. The `umt` section points at
them and bounds a call, and every default works as it is.

```jsonc
"umt": {
  "hn_api_base": "https://hacker-news.firebaseio.com/v0",
  "hn_search_base": "https://hn.algolia.com/api/v1",
  "arxiv_api_base": "https://export.arxiv.org/api/query",
  "npm_registry_base": "https://registry.npmjs.org",
  "default_category": "cs.AI",
  "timeout": 20
}
```

`default_category` is the arXiv category a recent-papers call reads when it
names none; a base points at a mirror or a proxy; `timeout` is one call's
ceiling in seconds.

## Use it

Ask in words ("what is on Hacker News", "find me arXiv papers on diffusion
models", "what does express depend on"), or call a verb through eval:

```lisp
(umt:top-stories :limit 10)                        ; the front page, by rank
(umt:new-stories :limit 10) (umt:best-stories)    ; newest, and HN's best
(umt:search-stories "common lisp" :limit 10)      ; Algolia keyword search
(umt:item-thread 12345 :depth 2 :max-children 20) ; one discussion tree

(umt:search-papers "diffusion models" :max 5 :sort "relevance")
(umt:paper "2405.12345")                          ; one paper, whole abstract
(umt:recent-papers :category "cs.AI" :max 5)      ; a category's newest

(umt:search-packages "mcp server" :limit 10)
(umt:package-metadata "express")                  ; latest, licence, dist-tags
(umt:package-versions "express" :limit 20)        ; newest first, with dates
```

Each answers text a reader or a model can use: a story with its points,
comments, author, date, url and item id; a paper with its authors, categories,
abs and pdf urls and abstract; a package with the registry's own metadata.
The model has the same eleven as tools — `hn_top_stories`,
`hn_search_stories`, `hn_item_thread`, `arxiv_search_papers`, `arxiv_paper`,
`npm_search_packages`, `npm_package_metadata`, and the rest.

`/umt` alone says where the three bases point; `/umt hn QUERY`,
`/umt arxiv QUERY` and `/umt npm QUERY` search without leaving the shell.

A call that fails answers `ERROR: UMT-ERROR: …` naming the host, the status
and what it said, so a retry is informed. Nothing is cached: a ranking is the
ranking now.

Credit is in [NOTICE](NOTICE); the original's licence is in [LICENSE](LICENSE).
