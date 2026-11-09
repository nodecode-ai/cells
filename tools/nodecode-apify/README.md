# nodecode-apify

Apify Actors: discover one in the Store, start runs, collect results, plus Apify web search and page fetch. A Nodecode rewrite of [apify/apify-hermes-agent-plugin](https://github.com/apify/apify-hermes-agent-plugin) by [apify](https://github.com/apify).

[Apify](https://apify.com) is a marketplace of ready-made scrapers, "Actors":
one takes an input, runs on Apify's machines, and writes its results into a
dataset. The cell gives the model the three tools the original plugin gives
it — `apify_discover`, `apify_start`, `apify_collect` — and two eval verbs,
`(apify:search "query")` and `(apify:fetch "url")`, that are the web search
and page extraction the plugin registered with Hermes.

## Get it

Tell the agent "add the apify cell", or run `nodecode add nc://apify`.

## Set it up

A token comes from Apify: Console, **Settings**, **Integrations**. Put it in
the `apify` section of `config.jsonc` (or `/settings`):

```jsonc
{
  "apify": {
    "api_token": "apify_api_…",   // or leave it out and export APIFY_API_TOKEN
    "search_results": 5,           // web results one (apify:search …) answers
    "search_timeout": 90           // seconds one search may take
  }
}
```

`api_token_env` names an environment variable instead, and `APIFY_API_TOKEN`
is read either way, so a token exported in a shell needs no section at all.
There is no free tier in the cell: Actor runs and web searches are billed to
that token by Apify's own pay-as-you-go pricing, so a run costs money the
moment it starts. `/apify` answers whose token it is and at which address.

## Use it

The model calls the tools by name; you can call them too, through `(help :apify)`
and `eval`:

- `apify_discover` — `query` searches the Store (id, title, run count);
  `actor_id` fetches one Actor's input schema and README. Actor ids are
  `username~actor-name`, never a slash.
- `apify_start` — up to ten runs at once, `{actor_id, input, label}` each;
  it answers a `run_id`, `actor_id` and `dataset_id` per run immediately.
- `apify_collect` — the refs `apify_start` answered, until `all_done`; a
  finished run's dataset items come back as text wrapped in
  `<<<EXTERNAL_UNTRUSTED_CONTENT>>>` markers, capped at `max_chars`.
- `(apify:search "query" :n 5)` — the web, through `apify~rag-web-browser`:
  numbered lines of title, url and snippet.
- `(apify:fetch "https://…" :format "markdown")` — one page through
  `apify/web-fetch`: its title, its address, then its text.

`/apify` shows the account; `/apify instagram` searches the Store and lists
what it found.

### Actors worth knowing first

The original ships a routing table so the model does not search for what is
already known. The short form, by platform — prefer an `apify`-tier Actor
over a community one, and fetch its input schema before spending a run:

| Task | Actor |
| --- | --- |
| Instagram, all of it | `apify~instagram-scraper` (profiles, posts, comments, hashtags: `apify~instagram-*`) |
| Facebook | `apify~facebook-posts-scraper`, `apify~facebook-ads-scraper` |
| TikTok | `clockworks~tiktok-scraper`, `clockworks~tiktok-profile-scraper` |
| YouTube | `streamers~youtube-scraper`, `streamers~youtube-comments-scraper` |
| X/Twitter | `apidojo~tweet-scraper` |
| LinkedIn | `harvestapi~linkedin-profile-scraper` |
| Google Maps | `compass~google-maps-extractor`, `compass~Google-Maps-Reviews-Scraper` |
| Google Search and Trends | `apify~google-search-scraper`, `apify~google-trends-scraper` |
| a page as clean text for a model | `apify~website-content-crawler`, `apify~rag-web-browser` |
| reviews across platforms | `tri_angle~hotel-review-aggregator`, `tri_angle~yelp-scraper` |

## What it does not do

`hermes apify-setup` is gone: the section, `/settings` and `/apify` do its
work. The attribution header the original sends (`x-apify-integration-platform`)
is gone too, and the actor-routing table is a README table and a primer rule
rather than a loadable skill. See [NOTICE](NOTICE).
