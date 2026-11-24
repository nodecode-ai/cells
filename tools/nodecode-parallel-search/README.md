# nodecode-parallel-search

Parallel Search: keyless web search and page fetch through Parallel's free MCP server, as two model tools. A Nodecode rewrite of [parallel-web/search-mcp](https://github.com/parallel-web/search-mcp) by [parallel-web](https://github.com/parallel-web).

## Get it

Tell the agent "add the parallel-search cell", or run `nodecode add nc://parallel-search`.

## Set it up

Nothing is required. The endpoint is anonymous: no account, no API key, no environment variable. The cell registers two tools, `web_search` and `web_fetch`, and answers a manual topic, `/parallel` for the operator.

```jsonc
{ "parallel-search": { "max_results": 5, "timeout_seconds": 60 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `endpoint` | `https://search.parallel.ai/mcp` | the MCP server's address; point it at your own if it speaks the same two tools |
| `max_results` | `5` | what a search asks for when the call gives no count |
| `timeout_seconds` | `60` | seconds one search or one page read may take |

Then `(restart-cells "nodecode-parallel-search")`, or set it in `/settings`. Say `/parallel fastapi response model` to try it.

The free tier is rate limited (HTTP 429) and meant for personal agents; an authenticated Parallel account raises the limits, and the cell says so when the limit is hit. If another cell already registers `web_search` and `web_fetch`, keep one of them.

## Use it

Ask for anything current: official documentation, a release note, a fact you would otherwise guess. The model calls `web_search` and gets ranked results with excerpts and their urls; it cites them and fetches a page only when the question needs the page itself.

From eval, the same two calls:

```lisp
(parallel:search "fastapi response model")            ; one query
(parallel:search (list "fastapi response model" "pydantic v2 model_config")
                 :objective "what the latest FastAPI docs say")
(parallel:fetch "https://modelcontextprotocol.io/introduction")
```

`(help :parallel)` answers the same guidance the model reads; `*primer*` is the text, so a layer can reword it.
