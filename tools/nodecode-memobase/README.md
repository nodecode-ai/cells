# nodecode-memobase

Memobase: a self-hosted user profile and event timeline as two model tools. A
Nodecode rewrite of [fred0m/hermes-memobase-provider](https://github.com/fred0m/hermes-memobase-provider)
by [fred0m](https://github.com/fred0m).

The cell talks to a [Memobase](https://github.com/memodb-io/memobase) server
you run yourself: a FastAPI + PostgreSQL + pgvector backend that keeps one
user's ontology-based profile and an append-only event timeline, extracted
offline by an LLM when its buffer flushes. The original was a Hermes Agent
memory provider: it recalled that memory on every turn and wrote each
finished turn back. Here the same store is one verb away:

- `memobase_search` — the profile and the events around one question,
  assembled by the server under a token budget;
- `memobase_profile` — the whole profile it holds;
- `(memobase:remember "..." :assistant "...")` — write one exchange, which
  the server's own extractor folds into the profile in the background.

Same requests, same user id: `uuid5(NAMESPACE_DNS, user_name +
"memobase_client")`, as the original derived it, so this cell and the
Memobase MCP server read and write one store.

## Get it

Tell the agent "add the memobase cell", or run `nodecode add nc://memobase`.

## Set it up

Two things, in `~/.nodecode/config.jsonc` (or `/settings`):

```jsonc
{
  "memobase": {
    "base_url": "http://memobase.local:8019/api/v1",  // your server, /api/v1 included
    "api_key": "",                                     // its bearer token
    "api_key_env": "MEMOBASE_API_KEY"                  // or the variable holding it
  }
}
```

`base_url` is the one member no call can work without: a section naming none
refuses the start, and `/cells` shows it as `REFUSED` until it is set. A
server with no token needs no `api_key`; the original's `MEMOBASE_BASE_URL`
and `MEMOBASE_API_KEY` are the same two values, and the token is still read
from `MEMOBASE_API_KEY` when none is pasted in.

The rest is optional: `user_name` (which profile to read and write, default
`user`), `max_tokens` (the budget one recall asks for, 2000),
`time_range_days` (how deep the event window goes, 180),
`fill_window_events` (whether the recent window rides along, on) and
`timeout_seconds` (8).

## Use it

The model reaches the memory on its own:

```
memobase_search("what did we decide about the build")
memobase_profile()
```

The operator has the same store from the slash line:

```
/memobase                       where the cell points, and what it gives
/memobase profile               the whole profile
/memobase the launch date       a recall, answered as the tool answers it
/memobase remember build is green on main
```

and the same calls from `eval`, which is what the tools call:

```lisp
(memobase:search "what did we decide about the build" :max-length 2000)
(memobase:profile)
(memobase:remember "the build is green on main" :assistant "noted")
```

`(help :memobase)` carries the same summary while the cell runs.

### What a recall sends

`GET <base_url>/users/context/<user id>?chats_str=[{"role":"user","content":QUERY}]&max_token_size=…&time_range_in_days=…&fill_window_with_events=true`,
with `authorization: Bearer <token>` when one is set. `memobase_profile`
reads `GET <base_url>/users/profile/<user id>`, and the write posts a chat
blob to `<base_url>/blobs/insert/<user id>?wait_process=false`. Nothing else
is dialled: the one host is the server in `base_url`.

A non-zero `errno` in an otherwise-200 answer, and any HTTP status past 400,
are refusals naming Memobase's own sentence.

## Not ported

The original's hybrid retrieval — the local BM25, entity and RRF legs over
the returned event window, with its temporal boost and its optional
cross-encoder rerank — and its per-turn lifecycle (prefetch caching, the
circuit breaker, skipping writes from subagents). The server's own
`/users/context` assembly answers the recall here, and a turn is written
back with `memobase:remember` rather than by a memory-provider hook. See
[NOTICE](NOTICE).

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-memobase
```

Every Memobase endpoint is a stubbed `NODECODE-MEMOBASE::HTTP`, so nothing
reaches a server.

MIT licensed; the original's [LICENSE](LICENSE) covers this rewrite.
