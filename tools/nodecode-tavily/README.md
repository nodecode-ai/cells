# nodecode-tavily

Tavily: web search, page extraction, site maps, crawls and research reports, as five tools the model calls. A Nodecode rewrite of [tavily-ai/tavily-hermes-plugin](https://github.com/tavily-ai/tavily-hermes-plugin) by [tavily-ai](https://github.com/tavily-ai).

## Get it

Tell the agent "add the tavily cell", or run `nodecode add nc://tavily`.

## Set it up

The cell talks to Tavily's API with a key from
[app.tavily.com](https://app.tavily.com). Either export it:

```sh
export TAVILY_API_KEY=tvly-...
```

or set it in `/settings` (or the page's Settings tab) under `tavily`:
`api_key` for the key itself, or `api_key_env` for the name of the variable
holding it. A key pasted into the panel is kept in a 0600 file under
`~/.nodecode/secrets/`; the folder holds no secret.

Nothing else is required. `/tavily` with no argument shows which endpoint and
key the cell is using, and `(help :tavily)` is the model's guide to the
tools.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `tavily_search` | find sources on the web — title, url, snippet, date, and Tavily's short answer |
| `tavily_extract` | read the pages you already have urls for |
| `tavily_map` | list a site's urls without reading them |
| `tavily_crawl` | read a bounded part of a site |
| `tavily_research` | a cited multi-source report on one question (slow, paid) |

From the keyboard, `/tavily <query>` runs one search and shows its sources.

Every call spends the account's credits. `search_depth` and `extract_depth`
`advanced` cost more than `basic`, and `tavily_research` with `model` `pro` is
the most expensive thing here.

### The five tools are the plugin's five tools

The original was a portable manifest pointing at Tavily's hosted MCP server;
the server advertised exactly these five tools, each one a Tavily REST
endpoint. This cell posts to those endpoints itself, so it needs no MCP
transport, no Node, and no local server.

## Not ported

- **The OAuth sign-in.** The plugin could hand token storage and refresh to
  Hermes (`auth: oauth` in a native `mcp_servers` entry). A cell's section is
  one key, and Nodecode's OAuth plumbing belongs to the provider cells, so an
  API key is the way in here.
- **The MCP connection's own settings** — `strict_redirect_headers`, the
  per-server `timeout` the plugin bumped to 900 seconds for research. The
  section's `research_timeout_seconds` (600 by default) is the same idea in
  this cell's own terms.
- **Schema discovery from the remote server.** The argument lists are declared
  in the cell, from the plugin's own skill references, so a hosted schema
  change no longer reaches the model automatically.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-tavily
```

Every request is a stubbed `nlk:http`, so nothing reaches Tavily and no key
is needed.

MIT licensed, as the original is.
