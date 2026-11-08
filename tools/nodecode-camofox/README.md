# nodecode-camofox

Camofox: drives a self-hosted Camoufox anti-detection Firefox server so the agent can browse, click, type and screenshot pages. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

[Camofox](https://github.com/jo-inc/camofox-browser) is a self-hosted Node server wrapping Camoufox, a Firefox fork with C++ fingerprint spoofing. This cell drives it over its REST API, so the browser is local and there is no cloud dependency. The server is yours to run: the cell never installs, starts or stops it.

## Get it

Tell the agent "add the camofox cell", or run `nodecode add nc://camofox`.

## Set it up

Run the server first — native Node, no Docker needed:

```bash
git clone https://github.com/jo-inc/camofox-browser
cd camofox-browser && npm install && npm start     # -> http://localhost:9377
```

`GET http://localhost:9377/health` should answer `ok: true`. (Docker:
`docker run -d -p 9377:9377 -e CAMOFOX_PORT=9377 ghcr.io/jo-inc/camofox-browser:latest`.)

Then give the cell its address in `/settings`, or in `~/.nodecode/config.jsonc`:

```jsonc
{
  "camofox": { "url": "http://localhost:9377" }
}
```

`api_key` is only needed when the server was started with `CAMOFOX_API_KEY`; it rides as the bearer token and is also what `browser_import_cookies` needs. Everything else has a default — `config.example.jsonc` in this folder lists every member with what it changes. A `url` that is not `http://` or `https://` refuses the start, and the folder's line in `/cells` says why.

## Use it

Ask the agent to open a page. It calls `browser_navigate`, which opens this session's tab and answers the title, the URL it landed on and an accessibility snapshot whose elements carry refs; everything else works on those refs.

| Tool | What it does |
| --- | --- |
| `browser_navigate` | Open a URL: the session's first tab, or a new page in it. Answers the title and a snapshot. |
| `browser_snapshot` | The page's accessibility tree again, refs included. |
| `browser_click` | Click the element a ref names (`@e5` or `e5`). |
| `browser_type` | Clear the field a ref names and type into it. |
| `browser_scroll` | Five steps up or down. |
| `browser_back` | The previous page in the tab's history. |
| `browser_press` | One key: Enter, Tab, Escape, a shortcut. |
| `browser_get_images` | Every image on the page, with its URL and alt text. |
| `browser_vision` | Screenshot the tab, save the PNG, answer its path — the model looks at it with `(look PATH)`. |
| `browser_console` | Evaluate JavaScript in the page and answer the value. |
| `browser_import_cookies` | Load a Netscape `cookies.txt` into this session's browser user. |

`(help :camofox)` carries the same rules while the cell runs, and every request carries one line naming the topic.

What the tools keep from the original:

- **A tab per session.** `browser_navigate` opens it; every other tool works in it, and a tab the server reaped (a 404 or 410) is replaced instead of failing every call from then on.
- **Refused before sent.** A URL carrying a key-shaped secret and one pointing at a cloud metadata endpoint (`169.254.169.254`, `metadata.google.internal`, `100.100.100.200`) are refused before the request leaves.
- **A bounded snapshot.** One longer than `snapshot_threshold` characters is cut at a line boundary, and the whole tree is written under `~/.nodecode/cache/camofox/` with the answer saying how to read it.
- **A masked typed value.** What looks like an API key is `[redacted]` in what the model reads back.
- **A cookie file kept in its place.** `browser_import_cookies` reads only under `cookies_dir`, refusing an absolute path or a `../`, capped at 5 MB and 500 cookies, with an optional domain suffix. The server's cookie route needs the API key, so the tool says so when there is none.

### Docker

A browser inside a container opens page URLs inside that container, where `localhost` is not your machine. Set `rewrite_loopback_urls: true` and, if `host.docker.internal` is not what your container reaches, `loopback_host_alias`: only `localhost`, `127.0.0.1` and `::1` page URLs are rewritten, `url` itself is untouched, and the tool result says what it rewrote.

### Keeping a browser profile

By default each session gets a new Camofox user, so cookies and logins do not survive. `managed_persistence: true` sends a userId derived from this home, so the server keeps one browser profile for it across sessions. With `user_id` set, the tools work inside a Camofox user another app made (its visible browser, for instance); `session_key` names the key tabs are opened under, and `adopt_existing_tab` takes an already open tab carrying it before opening a new one.

## What the rewrite does not do

- **No auxiliary vision model.** `browser_vision` answers a screenshot's path rather than a model's reading of it; the model looks at the image with `(look PATH)` and answers its own question. No numbered-label overlay either — Camofox serves none.
- **No console log capture.** The Camofox REST API exposes no route for it, so `browser_console` without an `expression` says so and evaluates JavaScript when given one.
- **No website blocklist.** The original consulted the harness's `security.website_blocklist`; here that is a guard's business, not this folder's.
- **No `hermes camofox setup|status`.** The section, `/settings` and `(cells)` are where a cell is configured and checked.
- **No `browser_vault` fill.** Nodecode fills no saved passwords into a page.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-camofox
```

Every exchange is a stubbed seam — the cell's one network call, `NODECODE-CAMOFOX::*HTTP*` — so nothing reaches a Camofox server. The tests prove the folder starts and stops cleanly, that a `url` which is not a web address refuses, that an unanswered `url` refuses every tool, and that each of the eleven tools answers over its route: a tab created, a reaped tab replaced, a snapshot cut, images parsed, a key-shaped typed value masked, a screenshot written, JavaScript evaluated, and a cookies file imported under its caps.

MIT licensed. See `NOTICE` for what came from the original and what did not.
