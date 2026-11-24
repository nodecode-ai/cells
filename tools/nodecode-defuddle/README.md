# nodecode-defuddle

Defuddle: reads any URL as a clean article, its markdown and metadata, by the `defuddle` program on your machine. A Nodecode rewrite of [annalinneajohansson82/hermes-defuddle](https://github.com/annalinneajohansson82/hermes-defuddle) by [annalinneajohansson82](https://github.com/annalinneajohansson82).

[Defuddle](https://www.npmjs.com/package/defuddle) is a reader: given a page it keeps the article and drops the navigation, the sidebars and the ads, and prints the result as markdown with the title, the byline, the publish date and the word count the page carried. The program runs locally — no key, no service — so the only traffic is the page request itself.

## Get it

Tell the agent "add the defuddle cell", or run `nodecode add nc://defuddle`.

## Set it up

The cell drives the `defuddle` program, so that has to be there first:

```sh
npm i -g defuddle
```

It is found on `PATH`. If it lives somewhere else, give its path in the cell's `program` setting (`/settings`); a cell that cannot find it refuses to start and says so. Nothing else is needed.

## Use it

The model gets one tool:

| Tool | What it answers |
| --- | --- |
| `defuddle_extract` | A page as an article: its title and address, one line of metadata, the description, then the markdown. `max_chars` clips it. |

And the same call three ways:

```
/defuddle https://example.org/an-article     # the operator's own read
```

```lisp
(defuddle:extract "https://example.org/an-article")              ; the whole article
(defuddle:extract "https://example.org/an-article" :max-chars 4000)
```

A page it cannot read answers `ERROR: DEFUDDLE-ERROR` with the reason: the program's own message, a page that carries no article, one that needed JavaScript, or a read past `timeout_seconds`.

`(help :defuddle)` has the same guidance the model is given.

**It reads, it does not search.** Find the address with the shipped websearch cell (`web:search`), then read it here when `web:fetch`'s plain text is too noisy. The shipped fetch is the everyday reader; this one is the article reader.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `program` | `defuddle` | The program's name on `PATH`, or its path. |
| `timeout_seconds` | `60` | How long one page may take to be read. |

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The `defuddle` package and the condition every refusal signals. |
| `cell.lisp` | The program runner, the JSON read, the verb, the tool, `/defuddle` and the section. |
| `test/` | Offline tests: lifecycle, the argv, the answer's shape, every refusal, one real run of a stand-in program. |

## What the rewrite does not do

- **No extract-backend registration.** Hermes let a plugin be the backend of its `web_extract` tool; Nodecode has no such registry, so the reader is a tool, a verb and a command rather than something the dispatcher picks.
- **One address a call.** The plugin took a list of URLs and fetched them in parallel; a model calling a tool reads one page at a time.
- **No SSRF re-check after a redirect.** As in the plugin: `defuddle` follows redirects internally, so the address it lands on is not re-checked. What the cell does check is that the address it is given is `http://` or `https://`, so it can never be read as a flag of the program.
- **No raw HTML fallback.** A page with no markdown is an error naming it, not a dump of its HTML — the plugin's own choice.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-defuddle
```

Every run of the program is stubbed, so nothing needs `defuddle` installed and nothing reaches the network; one test starts a real `/bin/sh` stand-in and reads back the argv it was given.

MIT licensed. The original is Copyright (c) 2026 annalinneajohansson82: see `LICENSE` and `NOTICE`.
