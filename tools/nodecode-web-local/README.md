# nodecode-web-local

Web Local: reads web pages from this machine and answers them as markdown, with no key and no service. A Nodecode rewrite of [enzo-adami/hermes-plugin-web-local](https://github.com/enzo-adami/hermes-plugin-web-local) by [enzo-adami](https://github.com/enzo-adami).

Given an address, it fetches the page itself — the request leaves from this machine — and reduces the HTML to the article: navigation, sidebars, ads, scripts and comments dropped, `<main>` or `<article>` preferred when the page names one, headings, list items, links, table cells and code blocks written as markdown, entities decoded. The page's own title and `<meta>` tags ride along: its site, its byline, its publish date and how many words it carries. Nothing is asked for and nobody else is contacted: no key, no extraction service, no telemetry.

The original was Hermes' extract backend, the reader behind its `web_extract` tool. Nodecode has no extract registry a cell may join, so the same reader is here a tool the model calls, a verb, and a command.

## Get it

Tell the agent "add the web-local cell", or run `nodecode add nc://web-local`.

## Set it up

Nothing. There is no key, and the section's three settings only bound what one call may cost.

## Use it

The model gets one tool:

| Tool | What it answers |
| --- | --- |
| `local_extract` | One to eight addresses, each page as the text below, in one answer. A page that could not be read is an ERROR line of its own. |

And the same call three ways:

```
/web-local https://example.org/an-article        # the operator's own read
```

```lisp
(web-local:extract "https://example.org/an-article")                 ; one page, as markdown
(web-local:extract "https://example.org/an-article" :format "text")  ; without markdown markers
(web-local:extract-many (list "https://a" "https://b"))              ; several, one answer
```

A page reads as its title, its address, one line naming what the page says about itself, its description quoted, then the article:

```
A Bridge in the Fog
https://example.org/a-bridge
Example · by J. Doe · published 2024-03-01 · 812 words

> How one town rebuilt its crossing.

# A Bridge in the Fog
...
```

`max_chars` (or `:max-chars`) clips the answer and says where it was cut; `0`, the default, keeps all of it. A call with no address, an address that is not `http(s)://`, one that points into a private network, or more than eight addresses answers `ERROR: WEB-LOCAL-ERROR` and names why. A page that could not be read never raises: it is an `ERROR:` line under its own address, so a batch keeps what worked.

**It reads, it does not search.** Find the address with the shipped websearch cell (`web:search`), then read it here.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `timeout_seconds` | `30` | How long one page's exchange may take. |
| `budget_seconds` | `120` | How long one call may take over all its pages; what is left is what the next page gets. |
| `max_bytes` | `10485760` | The most one page may weigh; past it the page is an error naming the cap. |

`(help :web-local)` has the same guidance the model is given.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The `web-local` package and the condition every refusal signals. |
| `cell.lisp` | The network seam, the address checks, the HTML-to-markdown reduction, the metadata read, the batch, the verb, the tool, `/web-local` and the section. |
| `test/` | Offline tests: lifecycle, the tool's answer for one and for several pages, the command, the text format, the clip, and every refusal. |

## What the rewrite does not do

- **No extract-backend registration.** Hermes picked this plugin as the backend of its `web_extract` tool; Nodecode has no such registry, so the reader is a tool, a verb and a command.
- **No readability library.** `trafilatura` is not vendored and no HTML parser is in the serving image, so the reduction is `cl-ppcre`'s: the same posture as the shipped websearch cell, coarser than trafilatura on an unusual page.
- **No `format: "html"`.** The plugin could answer readability-reduced HTML; here a page is markdown or plain text.
- **Redirects are not re-checked.** The image's one HTTP exchange follows up to five redirects itself, so the private-network check is made on the address the caller gave, not on every hop the way the plugin made it.
- **A private address is refused by name, not by resolution.** A hostname that resolves to a private address is not caught: there is no resolver check here.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-web-local
```

Every exchange is stubbed, so nothing needs a network.

MIT licensed. The original is Copyright (c) 2026 Enzo Adami: see `LICENSE` and `NOTICE`.
