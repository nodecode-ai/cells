# nodecode-distro-reader

Distro Reader: search and read Distro's independent news, stories, books and podcasts, with Distro's own eight reader tools. A Nodecode rewrite of [Distro-Media/hermes-plugin-distro-reader](https://github.com/Distro-Media/hermes-plugin-distro-reader) by [Distro-Media](https://github.com/Distro-Media).

## Get it

Tell the agent "add the distro reader cell", or run `nodecode add nc://distro-reader`.

## Set it up

Nothing. Distro's public reader needs no key and no account, so the cell works the moment it is turned on: discovery, free stories, books and media series all answer without credentials. The section exists only for tuning:

| Member | Default | What it is |
| --- | --- | --- |
| `endpoint` | `https://reader.distro.media/mcp/reader/stream` | the reader's MCP endpoint; name another only for a mirror of your own |
| `timeout_seconds` | `60` | one call's ceiling |
| `html_delivery` | `link` | a rendered story or book arrives as a page and its `renderUrl`, not as a wall of HTML |
| `page_limit` | `10` | how many headlines one feed or search answers with unless the call names its own count |
| `max_chars` | `20000` | how much of a rendered page's text reaches the model; `0` leaves it whole |

A section with an `endpoint` that is not an `http(s)` address refuses at start and says so on `/cells`; fix it in `/settings` and run `(restart-cells)`. No member is required.

`/distro-reader` with no argument reports which endpoint the cell is on and what it offers, without dialling anything.

## Use it

Ask for what you want; the model picks the tool. The eight are the plugin's eight, with the server's own argument names.

| Tool | Use |
| --- | --- |
| `distro_feed_latest` | recent stories, all publications or one (`publication` slug or `publicationId`, `limit`) |
| `distro_search` | full-text search over published stories (`query`, `limit`) |
| `distro_story_read` | one story's text, by `storyId`, `slug` (`"<publication>/<story>"`) or `url` |
| `distro_media_series` | a podcast or video series and its episodes (`publication`, `series`) |
| `distro_story_render_hud` | the story as Distro's own reading page (`htmlDelivery` `link` or `inline`) |
| `distro_book_render_hud` | a book: cover, table of contents, free chapters, the whole-book offer |
| `distro_payment_info` | what a premium item costs and by which rail — read-only (`contentId`) |
| `distro_tip` | the tip menu, or the x402 handoff for one approved amount (`storyId`, `newslodeSlug`, `amount`) |

From the keyboard: `/distro-reader <query>` runs one search and shows what came back; `/distro-reader feed [publication]` shows headlines. The same verbs are Lisp, from eval:

```lisp
(distro-reader:find-stories "decentralized ai" :limit 5)
(distro-reader:latest :publication "deai-news" :limit 10)
(distro-reader:story :slug "deai-news/a-story")
(distro-reader:payment-info 1195)
(distro-reader:server-tools)
```

`(help :distro-reader)` is the model's guide: the discovery and reading workflow, the digest and archive-research routines, and the payment rules — the plugin's three skills in one topic.

### Money

`distro_payment_info` and `distro_tip` answer instructions and an x402 handoff; the payment itself is signed by a wallet the user runs, outside this cell, exactly as it was outside the plugin. The manual's rule is the plugin's: show the price, token, network and resource first, get explicit approval of that exact amount, and report a payment as done only from settlement evidence that says `settled` (or say no new charge occurred when it says `skipped_already_owned`).

### The endpoint is the plugin's endpoint

The original was a portable manifest (`plugin.json` + `mcp.json`) registering Distro's Streamable HTTP MCP server as an untrusted server, plus three skills. The server is stateless, so this cell posts the same JSON-RPC (`tools/call`, `tools/list`) itself through `nlk:http` — one request a call, no MCP client, no Node, no local server. The tool names, argument names and descriptions are the server's, read from its own `tools/list`.

## Not ported

- **The MCP client's own machinery** — the session handshake, the untrusted-server approval gate. The endpoint needs no session, and the eight tools are calls the model already decides to make.
- **The visual reader's payload in full.** Inline HTML can run to hundreds of kilobytes; the cell asks for `link` delivery by default and clips a long page to `max_chars`. `htmlDelivery: "inline"` still answers the whole page when a user wants it.
- **The skills as files.** Their guidance is the manual topic; there is no `skills/` tree to read.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-distro-reader
```

Every request is a stubbed `nlk:http`, so nothing reaches Distro and no account is needed.

MIT licensed, as the original is.
