# nodecode-memos

MemOS memory: durable facts in one local file, six tools, found by their words. A Nodecode rewrite of [Markgatcha/memos](https://github.com/Markgatcha/memos) by [Markgatcha](https://github.com/Markgatcha).

## Get it

Tell the agent "add the memos cell", or run `nodecode add nc://memos`.

## Set it up

Nothing is required. The facts live in `~/.nodecode/memos/memories.jsonl`,
one JSON object a line, and the file is plain text: back it up, grep it,
read it.

```jsonc
"memos": {
  "limit": 10,     // how many a search answers unless the call says less
  "standing": 0    // characters of the newest facts in every prompt; 0 for none
}
```

`"file"` names another path. `"standing"` above zero puts the newest facts
in every request, the way the original's SessionStart summary does; at its
default the tools recall them, so a fact costs nothing until it is asked for.

## Use it

The model keeps facts with the six tools, and finds them again:

| Tool | What it does |
| --- | --- |
| `memos_store` | Keep one durable fact: `text`, `memory_type` (preference, fact, context, entity, relationship), `namespace`, `tags`. Answers its id. |
| `memos_search` | The facts nearest a query, best first, each with its `[mem:xxxxxxxx]` id, type, namespace and date. |
| `memos_context_pack` | The same, packed into a `tokens` budget and ready to read, saying how many did not fit. |
| `memos_supersede` | Correct a fact: the corrected text is kept and the old one marked superseded, so the history still reads both. |
| `memos_forget` | Stop answering a fact; the file keeps it. |
| `memos_history` | How a fact came to read as it does. |

From the shell, `/memos QUERY` packs what is relevant, `/memos store TEXT`
remembers one, `/memos recent` lists the newest, and `/memos` alone shows the
state of the store. In eval the same verbs are `(memos:store "…")`,
`(memos:recall "…")`, `(memos:context-pack "…")`, `(memos:history "mem:…")`.

Ranking is by the query's terms in the fact's words, in its tags (double), in
its type and namespace, and the whole phrase at once; there are no embeddings
and no server, so nothing leaves the machine and nothing needs Node.

Run `.github/test-cell.sh tools/nodecode-memos` from the cells repository.

## Licence

MIT. The plugin is Copyright (c) 2026 Markgatcha: see `LICENSE` and `NOTICE`.
