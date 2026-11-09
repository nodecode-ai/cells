# nodecode-brave-search

Brave Search: live web results as one model tool, with your own key. A Nodecode rewrite of [rabilrbl/hermes-brave-search-plugin](https://github.com/rabilrbl/hermes-brave-search-plugin) by [rabilrbl](https://github.com/rabilrbl).

## Get it

Tell the agent "add the brave-search cell", or run `nodecode add nc://brave-search`.

## Set it up

Make a subscription token at [api-dashboard.search.brave.com/app/keys](https://api-dashboard.search.brave.com/app/keys), then paste it into `api_key`, or leave that empty and let the cell read the variable `api_key_env` names (`BRAVE_API_KEY` by default).

```jsonc
{ "brave-search": { "api_key": "your-token", "count": 10, "safesearch": "moderate" } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the subscription token; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `BRAVE_API_KEY` | the variable read when `api_key` is empty |
| `count` | `10` | results a call that names none asks for; a call may ask for 1 to 20 |
| `safesearch` | `moderate` | `off`, `moderate` or `strict` |
| `country` | empty | a country code such as `us`; empty leaves it to Brave |
| `search_lang` | empty | a result language code such as `en`; empty leaves it to Brave |
| `freshness` | empty | `pd` a day, `pw` a week, `pm` a month, `py` a year; empty leaves it to Brave |
| `timeout_seconds` | `20` | seconds one search may take |

A section that names no key refuses the start and says so, and the tool is not
registered until one is set. Then `(restart-cells "nodecode-brave-search")`, or
set it in `/settings`. Say `/brave latest Rust release notes` to try the key.

## Use it

Ask for anything current: a release note, a changelog, a fact you would
otherwise guess. The model calls `brave_web_search`, gets ranked results with
their urls and descriptions, and cites them.

From eval, the same call:

```lisp
(brave:search "latest Rust release notes")             ; the section's count
(brave:search "monsoon onset" :country "in" :count 5)  ; a call's own parameters
(brave:search "llm releases" :freshness "pw")          ; only the past week
```

`(help :brave)` answers the same guidance the model reads; `*primer*` is the
text, so a layer can reword it. The cell searches and returns what Brave
ranked: it does not fetch a page. For that, a fetch cell such as
`nodecode-websearch` reads the urls these results carry.
