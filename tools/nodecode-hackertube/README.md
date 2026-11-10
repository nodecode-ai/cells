# nodecode-hackertube

HackerTube: a focused media search across Hacker News, Apple Podcasts, Open Library and LibriVox, with a local library of what you keep. A Nodecode rewrite of [rcmiller01/hackertube](https://github.com/rcmiller01/hackertube) by [rcmiller01](https://github.com/rcmiller01).

Two tools the model calls — `hackertube_search` and `hackertube_library` — and one slash command, `/hackertube`.

## Get it

Tell the agent "add the hackertube cell", or run `nodecode add nc://hackertube`.

## Set it up

Nothing to set up: four of the five sources answer without a key, and they are the ones enabled by default. The `hackertube` section of `config.jsonc` has five settings:

| Setting | Default | What it is |
| --- | --- | --- |
| `sources` | `["hn", "itunes", "openlibrary", "librivox"]` | which sources a search asks: `hn`, `itunes`, `openlibrary`, `librivox`, `youtube` |
| `max_results` | `10` | how many media lines one search lists |
| `timeout_seconds` | `10` | how long one source may take before the block names it as failed |
| `openlibrary_contact` | `""` | a contact string Open Library asks for; it rides that one request's user agent and nothing else |
| `youtube_key_env` | `"YOUTUBE_API_KEY"` | the variable holding a YouTube Data API key, read only when `youtube` is in `sources` |

YouTube is opt-in: putting `youtube` in `sources` without a key in that variable refuses the cell at start, naming the setting, and the other four sources still work as soon as it is taken out. No secret is ever written to a file.

## Use it

The model calls `hackertube_search` when a question wants something to watch, listen to or read: `hackertube_search(query="ursula le guin", sections=["books", "audiobooks"])` answers one line a record — the format, the title, its contributors, its duration, the source, the original https url and the id — plus a line per source that failed. `sections` is any of `videos`, `podcasts`, `books`, `audiobooks`; all four when absent.

The block is ordered, not just listed. A record that a Hacker News story links leads, because the original treats a discussion as the strongest signal; then a greedy pass spreads the list across sources and formats, so one source does not fill every line. Records that name none of the query's words are dropped — unless nothing matches, because an empty block is worse than a loose one.

You can search too: `/hackertube ursula le guin` answers the same block in the conversation, `/hackertube saved` lists the library, and `/hackertube` alone offers a row to type the query in.

`hackertube_library(action="save", item_id="ol:OL123W")` keeps a result — the id is the one its line ended with — `action="list"` shows what is saved, and `action="forget"` takes one out. The library is one JSON file under the Nodecode home, written readable only by its owner; nothing else is written anywhere, and an id that a restart forgot answers the repair (search again, then save) instead of failing.

Every line is a metadata record a source answered, not an endorsement and not a guarantee of relevance: a title is quoted data, and the block says so, because a title may hold anything its source put there.

The peers are `hn.algolia.com`, `itunes.apple.com`, `openlibrary.org`, `librivox.org` and, when enabled, `www.googleapis.com`. No cover, preview or media file is downloaded, nothing is spawned, and the only key that travels is YouTube's, in the request that needs it.

## What is in it

| File | What it is |
| --- | --- |
| `sources.lisp` | The five peers: one request each, its params, and how the answer becomes records. |
| `cell.lisp` | The gate, the ranking, the block, the library, the two tools, `/hackertube` and the one declaration. |
| `package.lisp` | The package and the cell's condition. |
| `test/` | Offline tests over a scripted network seam. |

## License

MIT, the original's: see `LICENSE` and `NOTICE`.
