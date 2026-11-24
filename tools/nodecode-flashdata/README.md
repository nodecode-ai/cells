# nodecode-flashdata

FlashData: Google web, news, image, shopping, places and video search plus YouTube research, through your own FlashData key. A Nodecode rewrite of [rocosen/flashdata-mcp](https://github.com/rocosen/flashdata-mcp) by [rocosen](https://github.com/rocosen).

The model gets three tools: `flashdata_search` for the six Google verticals, `flashdata_youtube` for search, metadata, transcript, captions, channel, suggestions and the Creative Commons label, and `flashdata_call` for any of the server's eighteen operations by name. Every one is also a verb in the `flashdata` package, callable from eval, and `/flashdata QUERY` is the operator's own search. `(help :flashdata)` is the model's page.

## Get it

Tell the agent "add the flashdata cell", or run `nodecode add nc://flashdata`.

## Set it up

Make a key at [flashdata.dev/app/api-keys](https://flashdata.dev/app/api-keys) and put it in the cell's section — `/settings`, the Settings tab, or `config.example.jsonc` — or export `FLASHDATA_API_KEY`. Installation is free; every data query spends that account's paid FlashData Credits, and `(flashdata:pricing "google_search")` reads a rate without spending one.

```jsonc
"flashdata": { "api_key": "...", "timeout_seconds": 60, "max_items": 10, "max_chars": 6000 }
```

A section with no key refuses to start and names the variable. Nothing here is a Nodecode key: it is yours, and it never appears in an answer the model reads.

## Use it

Ask for what you want:

> Search Google for battery recycling research in the US, in English, ten results, as a table of position, title, url and snippet.
>
> What does the video dQw4w9WgXcQ say? Give me the transcript with timestamps.

From eval, one verb per operation — `(flashdata:search "battery recycling" :num 10)`, `(flashdata:news "...")`, `(flashdata:images "...")`, `(flashdata:shopping "...")`, `(flashdata:places "...")`, `(flashdata:videos "...")`, `(flashdata:youtube-search "...")`, `(flashdata:transcript "dQw4w9WgXcQ")`, `(flashdata:channel "@NASA")`, `(flashdata:pricing "google_news")` — and `(flashdata:call "youtube_download" '(("video_id" . "...")))` for an operation without a verb. Add `:full t` when a long answer is cut.

Asynchronous answers carry a `job_id` (`flashdata:job`), chunked ones a `result_id` and `next_offset` (`flashdata:result`). A snippet is not the page, a price is a snapshot, and a Creative Commons label is not a grant of rights.
