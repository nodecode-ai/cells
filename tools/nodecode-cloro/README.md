# nodecode-cloro

Cloro: live Google results as one model tool, with your own key. A Nodecode rewrite of [cloro-dev/hermes-plugin-cloro](https://github.com/cloro-dev/hermes-plugin-cloro) by [cloro-dev](https://github.com/cloro-dev).

## Get it

Tell the agent "add the cloro cell", or run `nodecode add nc://cloro`.

## Set it up

Make an API key at [dashboard.cloro.dev/api-keys](https://dashboard.cloro.dev/api-keys), then paste it into `api_key`, or leave that empty and let the cell read the variable `api_key_env` names (`CLORO_API_KEY` by default).

```jsonc
{ "cloro": { "api_key": "your-key", "country": "US", "count": 5 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `api_key` | — | the cloro key; one of `api_key` or a set `api_key_env` is required |
| `api_key_env` | `CLORO_API_KEY` | the variable read when `api_key` is empty |
| `country` | `US` | the 2-letter ISO 3166-1 code the results are located in, e.g. `DE` |
| `count` | `5` | results a call that names none asks for; a call may ask for 1 to 20 |
| `timeout_seconds` | `120` | seconds one search may take; cloro loads the live results page, so it is slow |

A section that names no key refuses the start and says so, and the tool is not
registered until one is set. Then `(restart-cells "nodecode-cloro")`, or set it
in `/settings`. Say `/cloro latest Rust release notes` to try the key.

## Use it

Ask for anything current: a release note, a changelog, a fact you would
otherwise guess. The model calls `cloro_search`, gets Google's organic results
with their urls and snippets, and cites them. Each search spends credits on your
cloro account, and cloro opens the live results page while it answers, so a call
takes seconds.

From eval, the same call:

```lisp
(cloro:search "latest Rust release notes")                    ; the section's count and country
(cloro:search "monsoon onset" :country "IN" :count 5)         ; a call's own parameters
```

`(help :cloro)` answers the same guidance the model reads; `*primer*` is the
text, so a layer can reword it. The cell searches and returns what Google
ranked: it does not fetch a page. For that, a fetch cell such as
`nodecode-websearch` reads the urls these results carry.
