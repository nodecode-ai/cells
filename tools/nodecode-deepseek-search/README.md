# nodecode-deepseek-search

DeepSeek Search & Fetch: web search and page fetch on DeepSeek's own APIs, with your key. A Nodecode rewrite of [JasonSwindle/hermes-deepseek-search-fetch](https://github.com/JasonSwindle/hermes-deepseek-search-fetch) by [JasonSwindle](https://github.com/JasonSwindle).

The model gets two tools — `deepseek_search`, which finds pages and answers
ranked hits with the vendor's date, site, quality score and authority, and
`deepseek_fetch`, which reads one page as Markdown — plus the same two as
verbs called through eval, `(deepseek-search:search "query")` and
`(deepseek-search:fetch "https://…")`. It is the plugin's own pair of
endpoints: `POST https://api.deepseek.com/v1/tools/search` and
`/v1/tools/fetch`, billed by DeepSeek to your account.

## Get it

Tell the agent "add the deepseek-search cell", or run `nodecode add nc://deepseek-search`.

## Set it up

DeepSeek bills Search & Fetch per successful request — $0.75 per 1,000 calls,
with failed calls and empty results free — so the cell needs a key from
[platform.deepseek.com/api_keys](https://platform.deepseek.com/api_keys).
Nodecode keeps it out of the config file if you prefer:

```jsonc
{
  "deepseek-search": {
    "api_key_env": "DEEPSEEK_API_KEY"   // or "api_key": "…", or just the variable itself
  }
}
```

A start with no key anywhere is refused, and the refusal names the three ways
to give it one. Everything else has a default: `count` (5, at most 50) how
many hits a search asks for, `max_chars` (8000) how much of a fetched page
comes back, `timeout_seconds` (30) one request's ceiling, `need_content`
(off) whether every hit carries its page text, and `localization` (empty) an
IPv4 or IPv6 address to localise results to. See
[`config.example.jsonc`](config.example.jsonc).

## Use it

Ask for something that needs the web; the model calls the tools. From eval:

```lisp
(deepseek-search:search "kestrel hovering behaviour" :count 3)
(deepseek-search:fetch "https://en.wikipedia.org/wiki/Kestrel")
```

A search answers a numbered list — title, url, description, then whatever
signals the vendor sent (`date`, site, `rerank ~2.25`, `authority 3`).
A fetch answers the url, its title as a heading, and the page as Markdown,
cut at `max_chars` with a line saying where it was cut. Both are also tools,
so the model can call them in a turn without eval.

A refusal is a line, not a crash: `ERROR: deepseek-search: …` names what
went wrong — a missing key, HTTP 401, a 402 with the top-up sentence, a
transport failure — and the model repairs from it.

## What it sends, and where

Two endpoints on `api.deepseek.com`, a bearer key, and nothing else: no
telemetry, no update check, no second host. The key is never echoed into an
answer or a log; vendor error text is cut to 300 characters before the model
sees it.

`base_url` exists so a test can point the cell at a local server; it is a
setting, so anyone who can edit the section can redirect the key — the
original plugin made the same choice with its environment variable and
guarded it to the vendor origin or loopback.

## What the rewrite kept

- The 128-character query ceiling, clamped client-side at a word boundary,
  so a long question is never sent as-is.
- The search fields the vendor documents: `query`, `count`,
  `need_content`, `localization` — and all five hit signals, with an
  unknown authority (`-1`) left out.
- The fetch endpoint's backslash escaping, reversed: `\#` becomes `#`,
  `\[a link\](…)` becomes `[a link](…)`, while a page's own `\d` and
  `C:\path` survive, because only a backslash before ASCII punctuation is
  removed.
- Two retries on 408, 429 and any 5xx, waiting 1 s then 2 s plus a little
  jitter, with a `Retry-After` honoured instead when it asks for ten seconds
  or less, and a 402 answered once with the top-up sentence.
- One page's failure is one line in the answer; the other pages are still
  read. An empty result set is success with no hits.

## Not carried

- The harness-shaped parts: a provider base class, the settings surface, and
  the batch deadline that kept a multi-URL extract inside that harness's
  web-extract timeout. Here each request has its own ceiling, and the tool
  takes one URL (or a short list) at a time.
- The fallback to the host's keyless search ring when DeepSeek fails: that
  is the host's behaviour, and Nodecode's own `nodecode-websearch` cell is
  the separate choice for it.
- A hit's `site_icon` url, which a text answer cannot use.

## Test

From the cells repository's root:

```sh
NODECODE_SRC=<a nodecode source tree> .github/test-cell.sh tools/nodecode-deepseek-search
```

Every DeepSeek call is a stubbed `nlk:http` and every retry wait is stubbed,
so the suite is offline, keyless and unbilled.

MIT licensed. The original's licence and this folder's credit are in
[LICENSE](LICENSE) and [NOTICE](NOTICE).
