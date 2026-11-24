# nodecode-grounder

Grounder: licensed web search across many providers, one deduplicated list. A Nodecode rewrite of [grounder-labs/grounder-hermes-plugin](https://github.com/grounder-labs/grounder-hermes-plugin) by [grounder-labs](https://github.com/grounder-labs).

## Get it

Tell the agent "add the grounder cell", or run `nodecode add nc://grounder`.

## Set it up

Make a search key on your Grounder account's [Search keys](https://grounder.net/account/keys) page, then paste it into `api_key`, or leave that empty and let the cell read the variable `api_key_env` names (`GROUNDER_API_KEY` by default).

```jsonc
{ "grounder": { "api_key": "your-key", "max_results": 5 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the Grounder search key; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `GROUNDER_API_KEY` | the variable read when `api_key` is empty |
| `base_url` | `https://api.grounder.net` | Grounder's API origin; `/v1/search` is appended |
| `max_results` | `5` | results a call that names none asks for; a call may ask for 1 to 20 |
| `description_chars` | `800` | how much of each result's text Grounder is asked to fit; a row never carries more |
| `timeout_seconds` | `45` | seconds one search may take, across every provider it reaches |

A section that names no key refuses the start and says so, and the tool is not
registered until one is set. Then `(restart-cells "nodecode-grounder")`, or set
it in `/settings`. Say `/grounder offshore wind capacity 2026` to try the key.

The account is metered — 50 searches a day free, no card — so one call asks for
what the question needs.

## Use it

Ask for anything current that one search engine alone may not hold: a score, a
price, odds, a flight, or a recent page. The model calls `grounder_search`, gets
one deduplicated list of licensed results with their urls and the passages that
answer the query, and cites them.

From eval, the same call:

```lisp
(grounder:search "offshore wind capacity 2026")    ; the section's max_results
(grounder:search "ticket prices" :limit 10)        ; a call's own count
```

`(help :grounder)` answers the same guidance the model reads; `*primer*` is the
text, so a layer can reword it. The cell searches and does not fetch a page: for
that, a fetch cell such as `nodecode-websearch` reads the urls these results
carry.
