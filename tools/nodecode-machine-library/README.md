# nodecode-machine-library

Machine Library: research search over papers, books, patents and standards, with citable source links. A Nodecode rewrite of [SpaceFrontiers/hermes-machine-library](https://github.com/SpaceFrontiers/hermes-machine-library) by [SpaceFrontiers](https://github.com/SpaceFrontiers).

## Get it

Tell the agent "add the machine-library cell", or run `nodecode add nc://machine-library`.

## Set it up

The cell talks to Machine Library's API with a key from
[machinelibrary.ai/keys](https://machinelibrary.ai/keys). Searches spend the
account's prepaid credits. Either export it:

```sh
export MACHINELIBRARY_API_KEY=...
```

or set it in `/settings` (or the page's Settings tab) under
`machine-library`: `api_key` for the key itself, or `api_key_env` for the
name of the variable holding it. A key pasted into the panel is kept in a
0600 file under `~/.nodecode/secrets/`; the folder holds no secret.

Nothing else is required. `/machine-library` with no argument shows which
endpoint and key the cell is using, and `(help :machine-library)` is the
model's guide to the tools.

## Use it

Ask for what you want; the model picks the tool:

| Tool | Use |
| --- | --- |
| `machine_library_search` | find documents — title, page url, byline, the matching passage, and the identifier to cite |
| `machine_library_extract` | read a document the library holds: its `/r/...` page, or a DOI, arXiv, ISBN or standard identifier |

From the keyboard, `/machine-library <query>` runs one search and shows its
sources.

A search answers with a `https://machinelibrary.ai/r/...` page for each
document and the identifier behind it — `doi://10.48550/arXiv.1706.03762`,
`iso://1219-1:2012`, `isbn://...` — so an answer can cite where a claim
came from. Extract reads what the library stores for a document. A url the
library does not hold fails on its own line: pair the cell with a general
page fetch (the shipped `websearch` cell fetches the open web) for anything
else.

Every call spends the account's credits, and `max_chars` bounds how much of
one document a tool result carries.

### The two tools are the plugin's two verbs

The original registered a web search provider with Hermes, so that
`web_search` and `web_extract` asked Machine Library. A cell registers
tools of its own instead, so the same two calls are the two tools above,
against the same API and with the same request shapes: `POST /v2/search/`
for search, and `GET /v2/documents/by-uri/...` or
`GET /v2/documents/{id}` for extract.

## Not ported

- **The provider plug-in point itself.** Hermes let a plugin take over its
  `web_search` and `web_extract` verbs; Nodecode's web search is its own
  cell, so this cell registers two tools rather than replacing a verb.
- **The `web.backend` selection** in Hermes's config. Here the section is
  the switch: a present `machine-library` section is an enabled cell.
- **The search index choice.** The provider sent no index selection either,
  so nothing is lost; the API's own default profile answers.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-machine-library
```

Every request is a stubbed `nlk:http`, so nothing reaches Machine Library
and no key is needed.

MIT licensed, as the original is.
