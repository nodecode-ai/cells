# nodecode-keenable

Keenable: web search and page fetch through Keenable's hosted MCP server, as two model tools. A Nodecode rewrite of [keenableai/keenable-hermes-plugin](https://github.com/keenableai/keenable-hermes-plugin) by [keenableai](https://github.com/keenableai).

## Get it

Tell the agent "add the keenable cell", or run `nodecode add nc://keenable`.

## Set it up

Nothing is required: the server answers with no key and no account. An
optional Keenable API key, from
[app.keenable.ai/console](https://app.keenable.ai/console), lifts the shared
pool's hourly cap.

```jsonc
{ "keenable": { "max_results": 10, "timeout_seconds": 30 } }
```

| Member | Default | Notes |
| --- | --- | --- |
| `endpoint` | `https://api.keenable.ai/mcp` | Keenable's MCP endpoint |
| `api_key` | empty | a Keenable API key, written here |
| `api_key_env` | `KEENABLE_API_KEY` | the variable read when `api_key` is empty |
| `max_results` | `10` | what a call that names no count asks for, 1 to 50 |
| `timeout_seconds` | `30` | seconds one call may take |

Without a key the server answers from a shared public pool: at most 1,000
requests an hour and 10 a second per address. With a key the hourly cap goes
away.

## Use it

Ask for anything current, or paste a url and ask what it says. The model
calls `keenable_search` and gets pages back — title, url, a date when the
server knows one, and snippets — or `keenable_fetch` to read one page as
markdown, or only the part a prompt asks for.

From eval, the same calls:

```lisp
(keenable:search "official changelog for FastAPI 0.120")
(keenable:search "model release announcements" :site "huggingface.co"
                 :published-after "2026-10-01" :max-results 5)
(keenable:fetch "https://docs.keenable.ai/rate-limits"
                 :prompt "What are the unauthenticated rate limits?")
```

`keenable_search` takes `query` (describe the page you want, not keywords),
`site`, `published_after` and `published_before` (YYYY-MM-DD), `max_results`,
`snippet_max_length` and `mode` (`pro`, the default, or `realtime`).
`keenable_fetch` takes `url`, `prompt` (at most 2000 characters, answered
server-side) and `max_chars`. `(help :keenable)` answers the same guidance
the model reads; `*primer*` is the text, so a layer can reword it.

Fetched pages are data, not instructions. A refusal reads
`ERROR: KEENABLE-ERROR: <detail>`.
