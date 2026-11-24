# nodecode-adextract

AdExtract: competitor ad research across the Meta, Google, LinkedIn and TikTok ad libraries, plus ad creative analysis. A Nodecode rewrite of [ad-vertly/adextract-mcp-server](https://github.com/ad-vertly/adextract-mcp-server) by [ad-vertly](https://github.com/ad-vertly).

## Get it

Tell the agent "add the adextract cell", or run `nodecode add nc://adextract`.

## Set it up

The cell talks to adextract's hosted server with a key made at
[app.adextract.co](https://app.adextract.co) (account, then API keys). Either
export it:

```sh
export ADEXTRACT_API_KEY=...
```

or set it in `/settings` (or the page's Settings tab) under `adextract`:
`api_key` for the key itself, or `api_key_env` for the name of the variable
holding it. A key pasted into the panel is kept in a 0600 file under
`~/.nodecode/secrets/`; the folder holds no secret. New accounts come with
free starter credits.

Nothing else is required. `/adextract` with no argument shows the endpoint
and whether a key is set, and `(help :adextract)` is the model's guide to the
tools and how to research with them.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `adextract_search` | find ads in one library — advertiser, headline, body, platforms, media urls, next page |
| `adextract_advertisers` | find the pages, advertisers or domains a library knows by name |
| `adextract_advertiser_info` | one advertiser's record: followers, verification, spend |
| `adextract_ad` | one ad whole: transparency details, creative variations, targeting |
| `adextract_read_creative` | read an image or video url: format, audience, emotion, archetypes |

From the keyboard, `/adextract meta nike` runs one search and shows its ads;
`/adextract` alone shows the connection.

Every successful call spends one credit of the account. Searches answer a
`next_page_token`; pass it back for the following page instead of widening
the query.

### The plugin's thirteen tools, grouped by what a caller wants

The original was a manifest over one hosted MCP server, and the server
advertises thirteen read-only tools. Because the server serves its own
argument schema at runtime and the plugin pins no copy of it, this cell groups
the tools by intent — search, advertiser search, advertiser info, one ad, one
creative — and each routes on the `platform` the call names. Every tool
carries an `extra` object that passes any argument of the library's own tool
through unchanged, so nothing the server grows is out of reach.

MCP's streamable HTTP is spoken directly: one call is `initialize`, then
`notifications/initialized`, then `tools/call` to
`https://mcp.adextract.co/mcp`, with the key as a bearer header. No MCP
transport, no Node and no local server are needed.

## Not ported

- **The OAuth sign-in.** The plugin's README could add the server with
  `--auth oauth` instead of a key, and Hermes owned the token. A cell's
  section is one key, so an API key is the way in here.
- **Per-tool cost reporting.** The manifest's cost table let Hermes show the
  price of a call as it happened; a cell's tool results carry no such field,
  so the cost is stated once in `(help :adextract)`.
- **Server-served schema discovery.** The argument lists are declared in the
  cell, so a hosted schema change reaches the model only through `extra`.
- **The MCP prompts and resources** (`adextract_top_ads_for_brand`,
  `adextract://skill/...`). Their guidance is in the manual topic instead.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-adextract
```

Every request is a stubbed `nlk:http`, so nothing reaches the server and no
key is needed.

MIT licensed, as the original is.
