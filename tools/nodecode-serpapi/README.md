# nodecode-serpapi

SerpApi: web search plus Maps, News, Shopping, Hotels, Flights, Travel Explore and page extraction, as eight tools the model calls. A Nodecode rewrite of [serpapi/serpapi-hermes-plugin](https://github.com/serpapi/serpapi-hermes-plugin) by [serpapi](https://github.com/serpapi).

The tools are the original's, on the same engines:

- `serpapi_search` — the web, on Google Light, the engine the plugin's `web_search` provider served.
- `serpapi_maps_search` — places, businesses, restaurants, with ratings, addresses and coordinates.
- `serpapi_news_search` — recent reporting, with publication and date.
- `serpapi_shopping_search` — products, merchants, prices and delivery.
- `serpapi_hotels_search` — stays with nightly rates, bounded by check-in and check-out dates.
- `serpapi_flights_search` — one-way and round-trip fares, with Google's own airport ids.
- `serpapi_travel_explore_search` — destination and date ideas from a departure point.
- `serpapi_extract` — scrape one page, map a site's urls, or crawl part of a site.

Each answers SerpApi's own Markdown by default, which a model reads directly, or the
structured answer when the call says `output: "json"`. `serpapi_extract` answers
compact Markdown, or the full Extract JSON with `compact_mode: false`.

A person reads the connection with `/serpapi` and searches with `/serpapi <query>`.

## Get it

Tell the agent "add the serpapi cell", or run `nodecode add nc://serpapi`.

## Set it up

The cell talks to SerpApi with your own key, made at
[serpapi.com/manage-api-key](https://serpapi.com/manage-api-key). Either export it:

```sh
export SERPAPI_API_KEY=...
```

or set it in the `serpapi` section of `~/.nodecode/config.jsonc`, as `"api_key"` or as
`"api_key_env": "THE_VARIABLE_HOLDING_IT"`. Every call spends one search credit, and a
crawl spends one per page.

## Use it

Ask for what you want and the model picks the tool:

- "coffee shops near Borough Market" → `serpapi_maps_search`
- "what happened in the election this week" → `serpapi_news_search`
- "a kettle under 40 euros that ships free" → `serpapi_shopping_search`
- "hotels in Oslo, 10 to 12 May, two adults" → `serpapi_hotels_search`
- "LHR to CDG on 10 May, back on the 14th" → `serpapi_flights_search`
- "where can I go from Lisbon in June for a week" → `serpapi_travel_explore_search`
- "read this page, or list this site's urls" → `serpapi_extract`

`(help :serpapi)` carries the same guidance to the model in one manual topic.

## Tests

```sh
NODECODE_SRC=~/nodecode/nodecode .github/test-cell.sh tools/nodecode-serpapi
```

Every request is a stubbed `nlk:http`, so the tests are offline and need no key.
