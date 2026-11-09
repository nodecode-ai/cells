# nodecode-crawl4ai

Crawl4AI Web Extract: turns any page into clean markdown through your own self-hosted Crawl4AI instance. A Nodecode rewrite of [mblauser/hermes-plugin-crawl4ai](https://github.com/mblauser/hermes-plugin-crawl4ai) by [mblauser](https://github.com/mblauser).

[Crawl4AI](https://github.com/unclecode/crawl4ai) is an open-source crawler that renders a page in its own browser and answers it as LLM-friendly markdown, so a JavaScript-heavy site extracts as readily as a static one. It runs on your machine or your own server: no page, and no text, leaves your network. The server is yours to run; the cell never installs, starts or stops it.

## Get it

Tell the agent "add the crawl4ai cell", or run `nodecode add nc://crawl4ai`.

## Set it up

Run the server first:

```sh
docker run -d -p 11235:11235 unclecode/crawl4ai
```

It prints a JWT on startup when auth is on. Then give the cell its address in `/settings`, or in `~/.nodecode/config.jsonc`:

```jsonc
{
  "crawl4ai": { "url": "http://localhost:11235", "api_token": "the-jwt-it-printed" }
}
```

`url` is required and must be `http://` or `https://`; `api_token` is optional, for a server started with auth off. `timeout_seconds` (default 60) is how long one page may take — the server renders it in a browser first. `config.example.jsonc` in this folder lists every member. A `url` that is not an http address refuses the start, and the folder's line in `/cells` says why.

The route it calls is the one Crawl4AI serves: `POST /md` with `{"url": ...}` and a bearer token. A server behind a path prefix works — put the whole prefix in `url`, without a trailing slash.

## Use it

Ask the agent to read a page, or call the tool yourself:

| Tool | What it does |
| --- | --- |
| `crawl4ai_extract` | One or more page addresses in, each page's title, address and markdown out — or one `ERROR` line for the page that failed. |

`/crawl https://example.com` extracts one page without asking the model, and typed alone it asks for the address.

A page address pointing into a private, loopback or link-local network is refused before the request goes out, as the original plugin's SSRF check refused one. The bearer token never appears in an answer: every refusal is filtered for it.

The cell answers the same three shapes the original read: a `markdown` string, a `markdown` object carrying `raw_markdown`, and the title taken from the page's first `# ` heading. A failed page is answered as `ERROR:` with the status and the server's own detail, so the model can repair the call instead of repeating it.

To switch it off, drop the folder or set `"crawl4ai": {"enabled": false}`: nothing is registered then.

