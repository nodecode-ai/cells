# nodecode-jina

Jina Reader: any web page as clean Markdown, and Jina Search's sources, as two tools the model calls. A Nodecode rewrite of [Adolanium/hermes-jina](https://github.com/Adolanium/hermes-jina) by [Adolanium](https://github.com/Adolanium).

## Get it

Tell the agent "add the jina cell", or run `nodecode add nc://jina`.

## Set it up

Reading works with no key at all, at Jina's anonymous limit of roughly 20 requests a minute.
Searching needs one: make it at
[jina.ai/api-dashboard](https://jina.ai/api-dashboard/) and either export it:

```sh
export JINA_API_KEY=jina_...
```

or set it in `/settings` (or the page's Settings tab) under `jina`: `api_key` for
the key itself, or `api_key_env` for the name of the variable holding it. A key
pasted into the panel is kept in a 0600 file under `~/.nodecode/secrets/`; the
folder holds no secret.

Nothing else is required. `/jina` with no argument shows what the cell is using
and whether a key is set, and `(help :jina)` is the model's guide to the tools.

## Use it

| Tool | Use |
| --- | --- |
| `jina_read` | read pages as Markdown — the final url, the title, the text, and Jina's warning when it served a cache. No key needed |
| `jina_search` | find sources — numbered, with title, url and snippet. Needs `JINA_API_KEY` |

From the keyboard, `/jina <url>` reads that page and `/jina <words>` runs one
search.

One broken url is its own error under the pages that did read, as the plugin
answered a batch; a url that is not `http://` or `https://` is refused before a
request is made. `max_pages`, `max_chars`, `max_results` and `timeout_seconds`
in the section bound what one call costs.

Jina fetches the page on its own servers, so the urls the model reads and the
words it searches go to `r.jina.ai` and `s.jina.ai`. The key rides as a bearer
token to Jina and nowhere else, and an error names the status and Jina's own
message, never the key. That is the whole of what leaves this machine.

### The two tools are the plugin's two operations

The original registered a web backend with its harness, so the harness' own
`web_extract` and `web_search` reached Jina through it and it registered no tool
of its own. Nodecode has no such switchboard, so the backend's two operations —
`extract` and `search` — are two tools here, posting to the same two endpoints
with the same headers.

## Not ported

- **The backend switch.** In the plugin the operator chose Jina per operation,
  in the harness' `web_extract` / `web_search` settings. A Nodecode cell is
  present or absent, so once the folder is on, the model sees `jina_read` and
  `jina_search` beside whatever other web cell is on.
- **Parallel reading.** The plugin read several urls at once, 2 in flight
  without a key and 5 with one, to stay under Jina's rate limit. This cell reads
  them one after another, at most `max_pages` a call.
- **The harness' own web-tool plumbing** — per-profile secret scopes, the
  backend's `is_available` gate and its setup-schema rendering. The section's
  guide, the `/jina` status line and the cell's start notice say the same
  things here.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-jina
```

Every request is a stubbed `nlk:http`, so nothing reaches Jina and no key is
needed.

MIT licensed, as the original is.
