# nodecode-index-search

Index Search: search and health-check a Meilisearch index you run, and watch one saved query for new documents. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-index-search](https://github.com/TakeshiTGAL/hermes-plugin-index-search) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the index-search cell", or run `nodecode add nc://index-search`.

## Set it up

Run a [Meilisearch](https://www.meilisearch.com/) server of your own, with
documents already in it -- this cell searches an index you filled, not the
files on disk. Then give the cell its origin and a search-only key:

```jsonc
{ "index-search": { "host": "http://127.0.0.1:7700", "api_key": "your-search-only-key" } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `host` | — | the origin of your server: scheme, host, optional port, and no path |
| `api_key` | — | a search-only key, sent as a bearer token on search and never on the health check |
| `api_key_env` | `MEILI_API_KEY` | the variable read when `api_key` is empty |
| `id_field` | `id` | the hit field `/index-search` uses when you name none |
| `title_field` | empty | a hit field copied as a title; empty for none |
| `timeout_seconds` | `10` | seconds one request may take |

A `host` that is present and is not an origin refuses the start and says so;
a `host` that is absent leaves the cell running and every verb answers that it
is not set, which is what the setup panel and the model read. Set it in
`/settings`, then `(restart-cells "nodecode-index-search")`.

`api_key` is a `:secret`, so a key pasted in `/settings` is kept in a 0600
file under the home and the section names that file. Use a search-only key:
over plain `http` it crosses the network unencrypted, and a master key is sent
as given.

## Use it

Ask for something you know you indexed: "search my notes index for cats".

- `index_search(index, q, id_field, title_field)` -- one page of the index:
  every hit's id, its title when `title_field` names one, and
  `estimatedTotalHits`. At most 20 hits; a page that is not exactly a search
  result is refused rather than half-read.
- `index_health()` -- `GET /health` on the same origin, with no key. It
  answers whether the server is up, and nothing else.
- `index_watch(action, index, q, id_field, title_field)` -- `save` stores one
  query and a baseline, `status` shows it, `check` reads the page and answers
  the ids it has not reported yet. The saved query is one JSON file under the
  home (`index-search/watch.json`); nothing else is written, and nothing is
  scheduled for you.

From the operator's side, the same three:

```
/index-search notes cats      search that index for those words
/index-search check          read the saved watch and answer what is new
/index-search status         what is saved, and how to run it on a schedule
```

`(help :index-search)` answers the whole manual, and the same functions are
reachable from eval: `(nodecode-index-search::search-one "notes" "cats" "id" "")`.

Nothing else is dialled: every request goes to `host`, at
`/indexes/<uid>/search` or `/health`.

## Run the tests

```sh
.github/test-cell.sh tools/nodecode-index-search
```

The tests stub the cell's one network call, so they never dial a server.

## Credit

The original plugin, its manifest and its Japanese skill aliases are
TakeshiTGAL's; see [`NOTICE`](NOTICE). Licence: MIT, the original's.
