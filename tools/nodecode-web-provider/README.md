# nodecode-web-provider

A Nodecode cell for the keyless engines of oh-my-pi's `web` provider: Google,
Startpage, Ecosia and Mojeek scraped as omp scrapes them, DuckDuckGo, a
SearXNG instance of your own, and omp's `public` merge of the five scrapers.
With it the model can call, through `eval`,

```lisp
(engines:search "sbcl unix socket deadline")                    ; public: all five, merged
(engines:search "quri merge-uris" :engine "google" :recency "year" :n 5)
(engines:search "lisp !ddg" :engine "searxng")
```

and gets one numbered line per result (title, snippet, date when known) with
its url under it, an engine's own answer on top (SearXNG's), and a note for
an engine of the merge that failed or ran out of time. While the cell runs,
`(help :engines)` answers the manual, and every request's help section
carries one line naming the verb.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `web` provider
("Web search engines", api `web-search`). See [NOTICE](NOTICE).

## Why a verb, and why this name

omp's `web` provider is no model. Its catalog rows (kind `search`) are the
engines omp's web search tool may pick, by role, beside the session model's
own grounding: `public`, `google`, `startpage`, `duckduckgo`, `ecosia`,
`mojeek`, `searxng`, and the keyed `brave`, `exa`, `tavily`, `kagi`,
`parallel`, `perplexity`, `jina`, `tinyfish`, `synthetic`, `kimi`, `zai`,
`ollama`, `firecrawl`. A turn cannot run on any of them, so the port is a
search verb the model calls through `eval`, as the shipped websearch cell's
`web:search` is.

The names avoid what Nodecode already ships: the folder is
`nodecode-web-provider` (the core ships a cell named `nodecode-web`, the
browser tab), the section is `web-provider` (`web` is the core's own key,
`websearch` the websearch cell's), the package answers to `engines:` (not
`web:`), and the help topic is `:engines` (not `:web`).

## How it relates to the websearch cell

The shipped websearch cell (`web:search`, `web:fetch`) answers with a
keyed engine (Brave, Exa, Tavily) when it has a key, else its public floor:
DuckDuckGo's HTML endpoint, Firecrawl's keyless search and Exa's keyless MCP
endpoint, merged; arXiv on request; and it fetches pages. This cell adds what
omp's `web` provider has and that cell lacks, all without a key:

| engine | websearch | this cell |
| --- | --- | --- |
| Google (fetch-first, `udm=14`) | | yes |
| Startpage (homepage token, then the form) | | yes |
| Ecosia | | yes |
| Mojeek (independent index) | | yes |
| SearXNG (your instance, its token or Basic auth) | | yes |
| omp's public merge (the five scrapers, consensus-ranked) | its own floor (DDG, Firecrawl, Exa) | yes |
| DuckDuckGo | in its floor | alone, and in the merge |
| Firecrawl, Exa keyless; arXiv; fetching a page | yes | |
| Brave, Exa, Tavily with a key | yes | |

The two run side by side: neither reads the other's section or package.

## What is carried

- **google**: `GET https://www.google.com/search?q=&num=&hl=en&gl=us&udm=14&pws=0`
  (`tbs=qdr:d|w|m|y` for a recency); each `<h3>` inside a result link, its
  `/url?q=` redirect unwrapped and Google's own pages left out, its snippet
  the result's `VwiC3b` block (else `IsZvec`, `s3v9rd`), `Read more` cut; an
  automated-traffic page or the JavaScript challenge is a failure
- **startpage**: `GET https://www.startpage.com/` for the search form's hidden
  inputs (its `sc` token), then `POST /sp/search` with them, the query and
  `with_date`; a plain `GET /sp/search?query=` when the form cannot be read;
  `div.result > a.result-link` with its `h2`, `p.description`; the
  `a-bg-result` honeypot is no result; the CAPTCHA shell is a failure
- **duckduckgo**: `POST https://html.duckduckgo.com/html/` with
  `q`, `kl=us-en`, `df` and `b`; `result__a` (its `uddg` redirect unwrapped),
  `result__snippet`, the date in `result__extras__url`; the anomaly modal is
  a failure
- **ecosia**: `GET https://www.ecosia.org/search?q=`;
  `article[data-test-id=organic-result]`, its `result-title` link, its
  `web-result-description`; Ecosia's Cloudflare firewall is a failure
- **mojeek**: `GET https://www.mojeek.de/search?q=&t=&arc=none&lang=en&lb=en&theme=dark`
  (`since=` a recency); `ul.results-standard > li`: `a.title`, `p.s`;
  Mojeek's own pages left out; its automated-queries wall is a failure
- **searxng**: `GET <endpoint>/search?q=&format=json&pageno=1` with
  `time_range` (a week asks a month: SearXNG has none), `categories`,
  `engines` (shortcuts named through the instance's `/config`, kept for the
  process), `safesearch`, `language`; Basic auth over a bearer token; an
  external bang (`!!g`) stripped, since the instance answers it with a
  redirect; results, the instance's answers (up to three) and its
  suggestions; no results with engines that did not respond is a failure
  naming them
- **public**: the five scrapers at once, merged: one page (host without
  `www.`, path without a trailing slash, query kept) counted once, ranked by
  how many engines named it, then its best rank, then the engine order
  Startpage, Google, DuckDuckGo, Ecosia, Mojeek; the longest snippet kept;
  the merge answers when every engine has, or 5 s in with one answer, or at
  30 s with whatever it has; only every engine failing fails it
- every page asked with omp's fixed desktop Mac Chrome navigation headers,
  a same-origin referer where omp sends one; omp's default number of
  results (10 for one engine, 15 for the merge; at most 20 and 30)

## Install

Copy this folder into `~/.nodecode/cells/nodecode-web-provider/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "web-provider": {
    "engine": "public",                         // what a search asks by default
    "searxng_endpoint": "https://searx.example.org",
    "searxng_engines": "duckduckgo, br, sp"
  }
}
```

SearXNG's credentials are best left to `SEARXNG_TOKEN`, or
`SEARXNG_BASIC_USERNAME` and `SEARXNG_BASIC_PASSWORD`; `searxng_token` and
`searxng_basic_password` take them inline. Neither ever reaches the model.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-web-provider
```

## Gaps

- **The headless-browser fallback.** omp retries Google, Ecosia and Mojeek
  in a stealth headless Chrome (puppeteer) when a plain fetch is blocked, and
  solves Mojeek's ALTCHA there. Nodecode has no headless browser a cell can
  drive (the shipped chrome cell drives the operator's own Chrome through an
  extension, and cells share no code), so a blocked page is a failure here.
  The seam: a core primitive that loads a URL in a headless browser and
  answers the rendered HTML (`(nle:render-page url &key wait-for)`).

## Not ported

- omp's query parsing (`query.ts`): Google-style operators are passed to the
  engines as typed; omp rebuilds them per engine and filters the results
  after (relaxing a filter that would leave nothing).
- the randomized browser fingerprint (omp's `HeaderGenerator`): one fixed
  Chrome identity is sent, and `Accept-Encoding` asks gzip and deflate only.
- DuckDuckGo's paging (omp follows its next-page form until it has enough)
  and its locale codes (`kl` is `us-en`).
- the keyed engines and model groundings of omp's `web` rows (Brave, Exa,
  Tavily, Kagi, Parallel, Perplexity, Jina, TinyFish, Synthetic, Kimi, Z.AI,
  Ollama, Firecrawl with a key, the session model's own search): out of this
  cell's scope; the websearch cell carries Brave, Exa and Tavily.

MIT licensed.
