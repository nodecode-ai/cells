# nodecode-patsnap

Patsnap: search global patents and scientific literature through Patsnap's hosted MCP server, with the search workflow its skill taught. A Nodecode rewrite of [NousResearch/hermes-labs-plugins](https://github.com/NousResearch/hermes-labs-plugins) by [NousResearch](https://github.com/NousResearch).

## Get it

Tell the agent "add the patsnap cell", or run `nodecode add nc://patsnap`.

## Set it up

One member is the step left: a Patsnap key. Create one at [open.patsnap.com](https://open.patsnap.com/marketplace/mcp-servers/patsnap-search) (new accounts get free credits), then paste it into `/settings` or put it in the config:

```jsonc
{
  "patsnap": { "api_key": "your key" }
}
```

Or name the variable that holds it and leave `api_key` unset, as the original did:

```jsonc
{
  "patsnap": { "api_key_env": "PATSNAP_API_KEY" }
}
```

Then `(restart-cells "nodecode-patsnap")`. Ask `/patsnap` to see which server the cell is talking to, what it registered, and how many calls it has spent this start.

| Member | Default | Notes |
| --- | --- | --- |
| `endpoint` | Patsnap's hosted server | the Streamable-HTTP MCP endpoint; another instance answers at another path |
| `api_key` | unset | sent in an `apikey` request header, never in the URL |
| `api_key_env` | `PATSNAP_API_KEY` | the variable read when `api_key` is unset |
| `tool_prefix` | `patsnap` | the prefix the ten tools take, so another cell can list the same server |
| `language` | `en` | the language the detail tools answer in: `en`, `zh-CN`, `zh-TW` or `ja` |
| `limit` | `10` | what a search asks for when the call names no limit; a small one keeps credits down |
| `timeout` | `60` | seconds one MCP call may take |

The key is read from the section or the variable it names, sent to the endpoint you named and nowhere else, and a refusal never prints it back. Without either, the cell still starts: the tools are there, and the first call refuses naming the member to write.

`config.example.jsonc` beside this file is the whole section, commented.

## Use it

Ask for patents or papers. The ten tools are on the model from the first turn, so "patents from the last five years on solid-state battery electrolyte-interface stability" reaches them without being told to.

| Tool | Patsnap's own tool | What you get |
| --- | --- | --- |
| `patsnap_search_patent` | `search_patent` | ranked patents from WIPO/PCT, EPO, USPTO, CNIPA, JPO, KIPO and more, by semantic query, keywords, field filters or a Boolean query |
| `patsnap_search_literature` | `search_literature` | ranked papers from PubMed, Nature, arXiv and others |
| `patsnap_build_patent_boolean_query` | `build_patent_boolean_query` | a full intent turned into Patsnap's Boolean syntax |
| `patsnap_describe_search_capability` | `describe_search_capability` | the fields and operators of a specialised domain (`ls`, `material`) |
| `patsnap_get_standard_assignee_name` | `get_standard_assignee_name` | a company name resolved to the `company_id` / `ans_id` a search filters on |
| `patsnap_get_corptree_hierarchy` | `get_corptree_hierarchy` | one confirmed company's ancestors, children, descendants or root |
| `patsnap_fetch_patent` | `fetch_patent` | one patent's bibliography, claims and description |
| `patsnap_fetch_patent_lifecycle` | `fetch_patent_lifecycle` | one patent's legal events, annuities, transfers, licences and litigation |
| `patsnap_fetch_literature` | `fetch_literature` | one paper by the id a search returned |
| `patsnap_tools` | (this cell) | the server's own tool descriptions, or one tool's exact argument schema |

Patents and papers are separate tools: one call returns one record type. The workflow the original shipped as a skill rides along in three places: the tool descriptions, `(help :patsnap)`, and the fixed rules below.

- Resolve a company name with `patsnap_get_standard_assignee_name` before filtering on it: `company_id` and `ans_id` are not interchangeable, and a parent or subsidiary from the corporate tree does not widen a search until its ids are added.
- Pass only the parameter the chosen strategy uses, and search small (`limit` 10-20) before paging: every call spends credits on the account.
- Take domain fields and operators from `patsnap_describe_search_capability`, and identifiers from earlier results; never invent one.
- `patsnap_fetch_patent` already includes the full text: `include` takes only `relations` and `media`, and legal status is `patsnap_fetch_patent_lifecycle`.
- An authentication error means the key is wrong, expired or out of credits: report it and stop rather than retrying in a loop.
- Not for statistics, trends or Top-N counts, and never a substitute for a legal opinion or a freedom-to-operate conclusion.

Every call spends credits on the Patsnap account the key belongs to. The same answers are in the `patsnap` package, for a prompt you would rather type in `eval`:

```lisp
(patsnap:search-patent :semantic-query "solid-state electrolyte interface stability" :limit 5)
(patsnap:fetch-patent :patent-id "US-11234567-B2")
(patsnap:patent-lifecycle :patent-id "US-11234567-B2")
(patsnap:tools "search_patent")     ; the server's own schema, verbatim
```

Each answers the text the tool answers, and a refusal is `PATSNAP-ERROR` carrying the HTTP status and Patsnap's own sentence. `(help :patsnap)` has the same in the manual.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the `patsnap` section, the ten tool registrations, `/patsnap`, `(help :patsnap)` |
| `mcp.lisp` | the MCP client: initialize, `notifications/initialized`, `tools/list`, `tools/call`, a JSON or event-stream answer, and the session a server issues |
| `tools.lisp` | the server's nine tools, the schema each registers, and the verbs behind them |
| `test/` | offline tests: the lifecycle, each tool through the dispatch seam against a stubbed server, the session, the key's header, and every refusal |

Nothing is installed and no dependency is added: `nlk:http` is the one seam that reaches outside, and it reaches one endpoint.

The MCP server is Patsnap's own ([patsnap/patent-literature-search-mcp](https://github.com/patsnap/patent-literature-search-mcp), Apache-2.0). This cell only points at it.

## Licence

MIT. The original plugin is Copyright (c) 2026 Nous Research: see `LICENSE` and `NOTICE`.
