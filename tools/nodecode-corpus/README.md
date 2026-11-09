# nodecode-corpus

Corpus: live US-law search with verbatim citations, and a US company-formation intake, as model tools. A Nodecode rewrite of [teakesdev/corpus-agent-kit](https://github.com/teakesdev/corpus-agent-kit) by [teakesdev](https://github.com/teakesdev).

## Get it

Tell the agent "add the corpus cell", or run `nodecode add nc://corpus`.

## Set it up

`corpus.endpoint` is the one setting the cell needs, and it is Corpus's own address: `https://corpuslaw.us/api/mcp`. Without it the cell refuses to start and says so, with the address to paste.

No key is required: the anonymous floor answers, and the formation side is free. To raise the research limit, take a free self-serve key at <https://corpuslaw.us/settings> and name it in `corpus.api_key_env` (default `CORPUS_API_KEY`), or paste it as `corpus.api_key` in `/settings`. `corpus.jurisdiction` is the state or city a search assumes when a call names none.

```jsonc
"corpus": {
  "enabled": true,
  "api_key_env": "CORPUS_API_KEY",
  "jurisdiction": "Austin, TX"
}
```

## Use it

Ask a question about current US law, or ask to start an LLC. The model reaches for the tools itself; `(help :corpus)` is the primer it reads.

- `law_search {query, jurisdiction}`: official text with citations, called before the model answers what the law says.
- `law_get_node {citation, jurisdiction}`: one provision's full text, by the citation a search returned.
- `law_coverage {}`: which jurisdictions are indexed, so a gap is reported instead of guessed.
- `corpus_account {}`: credit balance and limits, free.
- `corpus_call {tool, arguments}`: any other Corpus tool by its dotted name and its own JSON object, the formation family included (`formation.requirements`, `formation.lookup_naics`, `formation.compare`, `formation.handoff`, `formation.checkout`, `formation.payment_status`).

From eval, `/corpus QUERY` and the `corpus` package call the same verbs:

```lisp
(corpus:search "food truck from a residential driveway" :jurisdiction "Austin, TX")
(corpus:provision "Tex. Loc. Gov't Code Sec. 217.042")
(corpus:coverage)
(corpus:account)
(corpus:call-tool "formation.requirements" (nlk:json-object "entityType" "llc" "state" "MS"))
```

Corpus is not a law firm and does not give legal advice.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The `corpus` section, the five tools, `/corpus`, the manual topic. |
| `mcp.lisp` | Corpus's streamable-HTTP MCP server, spoken to over JSON-RPC with the image's own HTTP call. |
| `tools.lisp` | The verbs, their boundary contracts and the primer. |
| `test/` | Offline tests: a stubbed MCP server, no network. |

The original is a configuration-only package for other harnesses: one MCP URL and two prose skills. This cell speaks that protocol itself, with no Node, no MCP client library and nothing fetched at run time.

## License

Apache-2.0, the original's licence: see `LICENSE` and `NOTICE`.
