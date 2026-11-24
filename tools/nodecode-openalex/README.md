# nodecode-openalex

OpenAlex: 250 million scholarly works, their authors, venues, topics and citation graph, with the price of each call shown before it is spent. A Nodecode rewrite of [Adolanium/hermes-plugin-openalex](https://github.com/Adolanium/hermes-plugin-openalex) by [Adolanium](https://github.com/Adolanium).

OpenAlex bills by call shape rather than by bytes, and the spread is a factor of
a hundred: a lookup by id is free, a list call is $0.0001, a search $0.001, and
a text classification or a full-text download $0.01. The anonymous allowance is
$0.10 a day, which ten classifications exhaust. So this cell carries the price
model rather than a thin wrapper: every answer says what it cost, the ledger
refuses a call that would pass the ceiling *before* it is sent, and the free
calls keep working when the budget is gone.

## Get it

Tell the agent "add the openalex cell", or run `nodecode add nc://openalex`.

## Set it up

A key is optional. Without one, every call runs on the anonymous tier; a free
key from openalex.org/settings/api raises the daily allowance tenfold and lets
`openalex_account` report what is left of it.

```jsonc
"openalex": {
  "api_key": "…",                        // or name the variable in api_key_env
  "api_key_env": "OPENALEX_API_KEY",
  "verbosity": "summary",                // summary, detail or raw
  "budget_usd": "0.05",                  // this cell's ceiling; free calls work past it
  "per_page": 25,
  "allow_text_classification": false,    // openalex_classify costs $0.01 a call
  "timeout_seconds": 30,
  "max_result_chars": 24000
}
```

Nothing here is required, and every default works as it stands. `budget_usd` is
a string because it is money: `"0.05"` is five cents, `"0"` allows the free
calls only.

## Use it

Ten tools, cheapest first:

| Tool | Price | What it answers |
| --- | --- | --- |
| `openalex_resolve` | free | a name as an id you can filter on, with the filter key to use next |
| `openalex_get` | free | records by OpenAlex id, DOI, ORCID, ROR, ISSN or PMID, up to 20 at once |
| `openalex_account` | free | the daily allowance, the live price table, this run's spend |
| `openalex_fields` | free, local | every valid filter and select name, and the vocabularies |
| `openalex_count` | $0.0001 | how many, and how they split by year, type, institution, journal, country or access |
| `openalex_related` | $0.0001 | the citation graph from one work: cited_by, references, related |
| `openalex_search` | $0.001 | the records themselves, filtered and sorted |
| `openalex_harvest` | $0.0001 a page | cursor paging, bounded and priced before the first request |
| `openalex_classify` | $0.01 | OpenAlex topics for text that is not in the index yet; off unless allowed |
| `openalex_fulltext` | free, then $0.01 | whether the full text exists, and the TEI's prose on `confirm=true` |

The order is the advice. Resolving a name to an id and filtering on that id is
free and exact, while searching the name is neither; a count answers "how many,
which years, which institutions" for a tenth of a search; and several id
lookups cost nothing where one search that returns the same records costs a
thousandth of a dollar each time. On works the plain `search` parameter is full
text, which is twice as broad as most people expect, so
`search_field="title_and_abstract"` is what bibliographic searching wants.

`/openalex <id|doi|name>` fetches or resolves from the keyboard, and
`/openalex budget` shows the allowance; `(help :openalex)` is the same guidance
the model reads, and `(openalex:...)` calls the tools' own functions from eval.

## Test

```sh
.github/test-cell.sh tools/nodecode-openalex
```

The run has no network: every request is a stubbed `nlk:http`, so the tests
prove the urls, the headers, the select lists, the refusals and the answers
without spending a cent.

## Credit

The tool names, the argument names, the cost model and the phrase-keeping that
makes a paper list readable are Adolanium's. See [NOTICE](NOTICE).
