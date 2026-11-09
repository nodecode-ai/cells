# nodecode-yandex

Yandex Search: web results and grounded answers from Yandex Cloud's Search
API. A Nodecode rewrite of
[akinfold/hermes-yandex-search-api](https://github.com/akinfold/hermes-yandex-search-api)
by [akinfold](https://github.com/akinfold).

The original gave a Hermes agent two things: a backend behind the harness's
own `web_search`, and a `yandex_generative_search` tool for a cited answer.
Nodecode has no `web_search` backend to plug into, so both arrive as tools of
their own, plus the same two calls in the `YANDEX` package for `eval`:

```lisp
(yandex:search "погода в Москве")                       ; classic web results
(yandex:search "sbcl unix socket" :n 10 :page 0)
(yandex:answer "when did the first metro open")          ; one grounded answer
(yandex:answer "glm release" :sites '("example.org"))    ; sources kept to a domain
```

`/yandex QUERY` is the operator's own search, and `(help :yandex)` is the
model's page.

## Get it

Tell the agent "add the yandex cell", or run `nodecode add nc://yandex`.

## Set it up

Yandex Search is a **paid** Yandex Cloud service, billed to the folder that
owns the key. You need two things, and the cell refuses to start without
them:

1. An API key: in the Yandex Cloud console, a service account with the
   `search-api.webSearch.user` role, then a key for it.
2. The **folder id** that account belongs to.

Put them in the environment (or paste them into the cell's `api_key` and
`folder_id` settings, which win):

```sh
echo 'YANDEX_API_KEY=…' >> ~/.nodecode/.env
echo 'YANDEX_FOLDER_ID=…' >> ~/.nodecode/.env
```

Settings, in the `yandex` section:

| Setting | What it does |
| --- | --- |
| `api_key`, `api_key_env` | the key, pasted or named by a variable (default `YANDEX_API_KEY`) |
| `folder_id`, `folder_id_env` | the folder it bills, pasted or named by a variable (default `YANDEX_FOLDER_ID`) |
| `search_type` | the market a query runs against: `SEARCH_TYPE_RU` (default), `TR`, `COM`, `KK`, `BE`, `UZ` |
| `max_results` | how many results a search that names none asks for (default 5; at most 100) |
| `base_url` | where the API answers, for a gateway of your own |
| `timeout_seconds` | seconds one request may take (default 30) |

## Use it

- **`yandex_web_search`** takes `query` and `n` (1–100) and answers one
  block per result: a numbered title, its url and domain, then the snippet
  Yandex's passages give.
- **`yandex_generative_search`** takes `query` and `sites` (up to five
  domains) and answers the synthesised text, the sources it cites (title,
  url, the text Yandex used), the queries Yandex ran, and any spelling
  correction — or the note that Yandex flagged the answer as rejected.

Both spend the operator's key, so the cell's primer asks the model to read an
answer before asking again.

The classic report arrives as Base64-encoded XML; this cell reads it with a
small scanner and refuses the document whole on a DTD, a byte that is not
UTF-8, or more than 2 MiB — the guards the original put in front of its XML
parser.

## Test it

```sh
.github/test-cell.sh tools/nodecode-yandex
```

The tests stub the cell's one HTTP call: no key, no network, no Yandex.
