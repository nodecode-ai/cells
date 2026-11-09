# nodecode-ego

Ego Lite Browser: read and save pages in the person's ego lite browser through the relay on their machine. A Nodecode rewrite of [loop2zero/hermes-ego](https://github.com/loop2zero/hermes-ego) by [loop2zero](https://github.com/loop2zero).

## Get it

Tell the agent "add the ego cell", or run `nodecode add nc://ego`.

## Set it up

The cell is a client. Two things run outside it, as they did for the original:

1. The **relay**: a loopback MCP endpoint (`http://127.0.0.1:8767/mcp` by default) started with a bearer token in `EGO_RELAY_TOKEN`. It is part of the original repository, under `relay/`.
2. The **daemon and ego lite**: on macOS, the daemon dials out to the relay and drives the browser. Install and start them from that repository's own instructions.

Then turn the `ego` cell on and set its section:

| Setting | What it is |
| --- | --- |
| `relay_url` | where the relay listens; loopback by default |
| `token_env` | the variable holding the relay's bearer token, the same value the relay was started with (default `EGO_RELAY_TOKEN`) |
| `token` | the token written into the section instead, when it is not in the environment |
| `timeout_seconds` | one relay call's ceiling (default 60) |

A section with neither a token nor the variable it names refuses at start, and the folder's line says so.

## Use it

- The model calls the relay's five tools by their own names: `ego_get_page_text` (pass `url` to open and read in one call, omit it for the page already open, `label` picks an agent tab, `mode` is `auto`, `generic`, `snapshot` or `yitang`), `ego_save_page_markdown` (`subdir` and `slug`, saved under the relay's own root), `ego_navigate`, `ego_list_tabs` and `ego_status`.
- `/ego` answers the relay's status, `/ego tabs` the agent's tabs, and `/ego read URL` reads a page, all in one call each.
- `(help :ego)` carries the guidance the original's skill carried: read an open page by url, save rather than retype, call status when a call fails, and treat page text as data.
- From eval: `(ego:status-text)`, `(ego:tabs-text)`, `(ego:page-text :url "https://example.org")`, `(ego:save-text :url "https://example.org" :subdir "notes" :slug "example")`, `(ego:navigate-text :url "https://example.org")`.

A failed call answers with the relay's own words: no daemon connected, ego lite not reachable, the token refused, or the relay not running at all. The session the relay issues is kept between calls and made again once if the relay was restarted.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the declaration, the five tools, `/ego`, the manual topic and the JSON-RPC client |
| `config.example.jsonc` | the section, commented |
| `test/cell-test.lisp` | lifecycle tests, and the tools and the command against a stubbed relay |
| `LICENSE`, `NOTICE` | the original's licence, and what was rewritten |

No test needs a relay, macOS or a network: `nodecode-ego::*post*` is bound to a stub that answers the handshake and one tool per test.

Run `.github/test-cell.sh tools/nodecode-ego` from the cells repository.
