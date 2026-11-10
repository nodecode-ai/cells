# nodecode-song-gift

Custom Song Gift: browse the gift occasions and musical styles of a personalized song studio, and get lyric storytelling advice for turning real memories into a song. A Nodecode rewrite of [GptsApp/custom-song-gift-mcp](https://github.com/GptsApp/custom-song-gift-mcp) by [GptsApp](https://github.com/GptsApp).

## Get it

Tell the agent "add the song-gift cell", or run `nodecode add nc://song-gift`.

## Set it up

Nothing. The cell talks to the studio's public catalog at
`https://customsonggift.org/api/mcp`: no key, no account, nothing to install.
`/song-gift` alone shows the endpoint and what the cell can ask it.

`/settings` (or the page's Settings tab) under `song-gift` has three members,
all optional: `endpoint` for an MCP server of your own, `timeout_seconds` for
one call's ceiling, and `max_chars` for how much of one answer is laid out.

## Use it

| Tool | Use |
| --- | --- |
| `list_occasions` | the gift occasions the catalog supports, each with the memories that make a song for it work |
| `list_styles` | the musical styles and the mood each carries; name an occasion to ask what suits it |
| `guide_story_and_lyrics` | how to prepare a song: who it is for, which details to gather, what the chorus says |
| `get_pricing_options` | current prices and what a finished song comes with |

From the keyboard: `/song-gift occasions`, `/song-gift styles`, `/song-gift
styles anniversary`, `/song-gift guide mothers_day`, `/song-gift pricing`.

Everything here is read-only: the tools list the catalog and give advice.
They sell nothing, take no payment, and do not make or deliver audio; the song
itself is made on the studio's site. `(help :song-gift)` is the model's guide
to the four tools and the way to work with them.

### How it is spoken

The original was a portable manifest over one hosted MCP server, and the
server serves its own argument schema at runtime, which the pinned commit does
not carry. So each tool declares the members worth naming and sends only what
a call filled in; any other argument the server grows goes through the tool's
`extra` object unchanged.

MCP's streamable HTTP is spoken directly: one call is `initialize`, then
`notifications/initialized`, then `tools/call` to
`https://customsonggift.org/api/mcp`, with the `Mcp-Session-Id` the
handshake issued echoed back. No MCP transport, no Node and no local server
are needed.

## Not ported

- **The plugin's interface block.** The manifest's display name, brand
  colours, composer icon and sample prompts are ChatGPT app-store metadata; a
  cell has its own listing instead.
- **The server-side schema.** The hosted server's argument lists are not in
  the pinned commit, so the declared members are this cell's guess at them and
  `extra` passes the rest through.
- **The catalogue itself.** The occasions, styles, guidance and prices are
  the server's, fetched at call time, never copied into the folder -- so they
  stay current.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-song-gift
```

Every request is a stubbed `nlk:http`, so nothing reaches the catalog and no
network is needed.

MIT licensed, as the original is.
