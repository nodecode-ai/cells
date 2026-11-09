# nodecode-upstash

Upstash Memory: durable facts and searchable chat transcripts in your own Upstash Redis database. A Nodecode rewrite of [upstash/agentkit](https://github.com/upstash/agentkit) by [upstash](https://github.com/upstash).

The model gets four tools -- `upstash_memory_save`, `upstash_memory_search`,
`upstash_memory_forget`, `upstash_chat_search` -- and `/upstash` to show
the database or search it. Before each turn the memories nearest the user's
message are recalled into the prompt; after each turn the exchange is
appended to the session's searchable transcript. Both live in your own
database, and both are laid out the way `@upstash/agentkit-sdk` writes them,
so an AgentKit app on the same database shares the same facts.

## Get it

Tell the agent "add the upstash cell", or run `nodecode add nc://nodecode-upstash`.

## Set it up

Make a database at the Upstash console and copy its REST address and token;
the `upstash` section of `config.jsonc` (or `/settings`) holds them:

```jsonc
{
  "upstash": {
    "url": "https://<name>.upstash.io",   // UPSTASH_REDIS_REST_URL is read when absent
    "token": "…",                          // UPSTASH_REDIS_REST_TOKEN is read when absent
    "user_id": "default",                  // whose memories these are, the same across sessions
    "namespace": "agentkit"                // the keys @upstash/agentkit-sdk apps write
  }
}
```

A start with no address or no token is refused, naming the member. The
other members default to recalling five memories into the prompt
(`recall`, `recall_limit`) and saving each finished turn (`capture`), with
a 15-second request ceiling (`timeout_seconds`); `telemetry` adds the
opt-in `Upstash-Telemetry-Sdk` header the original sends, to the same
database and nowhere else.

## Use it

```
upstash_memory_save {text}              store one durable fact, verbatim
upstash_memory_search {query, limit?}   the facts nearest a question, with their ids
upstash_memory_forget {id}              delete one fact by the id a search returned
upstash_chat_search {query, limit?}     past sessions matching a query, with excerpts
/upstash                                 the database, user and namespace in use
/upstash QUERY                           what the database holds about QUERY
```

Ask the agent to remember a preference and it calls `upstash_memory_save`;
ask what it knows about you in a later session and `upstash_memory_search`
finds it, while the automatic recall puts the nearest memories in front of
the model before it answers. `upstash_chat_search` reads across sessions:
"where did we decide to put the cache?" finds the conversation, not just a
fact about it.

Memory keys are `<namespace>:memory:<user>:<id>` holding
`{text, userId, createdAt, …}`, transcript keys are
`<namespace>:chat:<user>:<session>`, and the namespace's one search index
covers both prefixes, so this works on the Upstash Free plan's one index
per database. `(help :upstash)` is the manual page while the cell runs.

## Tests

Run `.github/test-cell.sh tools/nodecode-upstash` from the cells repository.
Fourteen tests, no network - every Upstash call is stubbed - covering install,
stop and a refused config, a database and a user id being required, save and
search over the index, forget repairing a failed delete, past-session search,
`/upstash` with and without a query, recall into the harness block at
`turn.input_committed`, and the transcript written at `turn.completed`.
