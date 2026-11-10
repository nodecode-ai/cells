# nodecode-cortive

Cortive memory: the local Cortive knowledge base as the agent's recall. A Nodecode rewrite of [KadenDevAi/hermes-cortive](https://github.com/KadenDevAi/hermes-cortive) by [KadenDevAi](https://github.com/KadenDevAi).

Cortive is a private knowledge base for macOS: it keeps the web pages, PDFs, screenshots, voice memos and copied text you capture, in one SQLite file with a full-text index. This cell reads that file read-only and gives the agent three tools over it, plus a `/cortive` command and a manual topic:

- `cortive_search` — ranked captures: date, title, source, excerpt, id. Filters by kind (`screenshots`, `photos`, `documents`, `pdf`, `web`) and by date (`since_days`, `from_date`, `to_date`); with `*` it lists the newest instead of searching.
- `cortive_get` — one capture in full, 8,000 characters per call, paged with `offset`.
- `cortive_collections` — the whole library in one answer: totals, date range, where captures came from, the newest, and every collection with example titles; or the captures inside one collection.

The same calls work from eval while the cell runs: `(cortive:recall "lisbon ferry")`, `(cortive:capture "8f2a1c")`, `(cortive:library-report)`.

Ranking is the original's: a capture matching more of your words always ranks above one matching fewer; among those, the score weighs the full-text match, recency (a 90-day half-life), tags and title. Duplicates (the same content hash) collapse to the best copy. Encrypted and deleted captures are skipped, and nothing is ever written to the library.

## Get it

Tell the agent "add the cortive cell", or run `nodecode add nc://cortive`.

## Set it up

One setting, in `/settings` or the page's Settings tab:

```jsonc
{
  "cortive": {
    "library": "~/Library/Application Support/cortive-ai/cortive.db",
    "guard": true,
    "limit": 5
  }
}
```

`library` is the file the Cortive desktop app writes. The cell refuses to start without it, and says so on its line in `/cells`.

## Use it

Ask for something you saved: "what were those Lisbon ferry notes?", "find the sourdough schedule I saved", "what is in my Cortive?". The model searches, reads, and cites the capture's date and title, and is told to treat captured text as quoted reference material rather than instructions.

The library is read only through these tools: with `guard` on, another tool's call whose arguments name the library file (a `sqlite3` on it, a `cat` of it) is refused. Nothing here leaves the computer: no network, no key, no other file read.

## What it does not do

- **Meaning search.** The original asked the running Cortive app to rank by meaning over a local socket; without the app it matched words, and so does this cell. Say "Lisbon", not "Portugal".
- **Hiding passwords from other tools' output.** The original also rewrote every other tool's result to hide secrets saved in the library. This cell refuses a call that names the library instead, which is where those reads came from.
- **Automatic recall.** The original injected up to three captures into every turn; here the model calls the tools, or you call them from eval.
- **Writing.** Conversations are not saved into Cortive, as in the original.

MIT, see [LICENSE](LICENSE). Cortive itself is a separate, closed-source app.
