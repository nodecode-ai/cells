# nodecode-cloudflare-web-search

Cloudflare Web Search: Ceramic, Exa or Linkup through your own Cloudflare AI Gateway, as one model tool. A Nodecode rewrite of [Adolanium/hermes-cloudflare-web-search](https://github.com/Adolanium/hermes-cloudflare-web-search) by [Adolanium](https://github.com/Adolanium).

## Get it

Tell the agent "add the cloudflare-web-search cell", or run `nodecode add nc://cloudflare-web-search`.

## Set it up

Two things come from Cloudflare, and the cell refuses to start without both:

1. **An account id** — the 32-character id on your Cloudflare dashboard.
2. **An API token** with **Account > Workers AI > Read** and **Account > AI
   Gateway > Read**. [Cloudflare's
   guide](https://developers.cloudflare.com/web-search/how-to-use/) walks
   through making one.

Either export them:

```sh
export CLOUDFLARE_WEB_SEARCH_API_TOKEN=...
export CLOUDFLARE_ACCOUNT_ID=...
```

or set them in `/settings` (or the page's Settings tab) under
`cloudflare-web-search`: `api_token` for the token itself, or `api_token_env`
for the name of the variable holding it, and `account_id`. A token pasted into
the panel is kept in a 0600 file under `~/.nodecode/secrets/`; the folder
holds no secret.

Searches are paid for by the AI Gateway they route through: either the
gateway's credits, or a provider key stored on the gateway under an alias you
name in `byok_alias`. `gateway_id` (default `default`) picks the gateway,
`provider` (`ceramic`, `exa` or `linkup`, default `ceramic`) picks what routes
a call that names none, and `base_url` is Cloudflare's API root, changed only
for a proxy of your own.

`/cloudflare-web-search` with no argument shows the endpoint, the route and
whether a token is set, and `(help :cloudflare-web-search)` is the model's
guide.

## Use it

| Surface | Call |
| --- | --- |
| tool | `cloudflare_search` — `query`, plus `max_results` (1-10) and `provider` when the question needs them |
| command | `/cloudflare-web-search <query>` — one search the operator reads |
| eval | `(cloudflare-web-search:search "latest Rust release notes")` |

An answer is one block a result — a numbered title, its url, its description —
under a header naming the provider and gateway that answered, the gateway's
latency and its request id. A search with no hits says so; a refused token or
a malformed answer is an `ERROR:` line in Cloudflare's own words, which the
model repairs from.

This cell searches; it does not read a page. Ask for a page's content with
whichever fetch tool the organism has, or search again for a different
question.

## Not ported

- **The harness' search cache and its keyless rescue.** Hermes owns both, and
  the original documented a cache-key limitation and a switch to keep searches
  on its own backend. A cell's tool has no harness cache and no fallback
  ladder, so the limitation cannot arise here.
- **The plugin settings panel.** The section declares the same members the
  plugin's `config_schema` carried, so `/settings` or the Settings tab is the
  same surface.
- **The provider registration itself.** Hermes plugins plug into one
  `web_search` tool; a cell contributes tools of its own, so the same call is
  `cloudflare_search` and nothing else in the organism changes.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-cloudflare-web-search
```

Every request is a stubbed `nlk:http`, so nothing reaches Cloudflare and no
token is needed.

MIT licensed, as the original is.
