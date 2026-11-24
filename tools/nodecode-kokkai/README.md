# nodecode-kokkai

Japan Diet Records: search Japan's National Diet proceedings, read speech excerpts, and watch for new meetings once a day. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-kokkai](https://github.com/TakeshiTGAL/hermes-plugin-kokkai) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the kokkai cell", or run `nodecode add nc://kokkai`.

## Set it up

Nothing to set up: the National Diet Library's Diet Proceedings Search System is open, needs no key, and the cell asks for no account. Five settings exist if you want them, in the `kokkai` section of `config.jsonc`:

| Setting | Default | What it is |
| --- | --- | --- |
| `hour` | `9` | the hour the saved watches are checked, on this machine's clock (0 through 23) |
| `lookback_days` | `30` | how many days back a watch reads when you do not say (1 through 30) |
| `timeout_seconds` | `30` | how long one request may wait for the API |
| `interval_minutes` | `15` | how often the worker looks to see whether the check hour has come |
| `watch` | `true` | whether the daily check runs at all |

An `hour` outside 0 through 23 refuses the cell at start, with the refusal naming the setting.

## Use it

The model calls the tools on its own when a question is about Japan's parliament, and you can ask the same things.

`kokkai_search` lists meetings: the house, the meeting, the session and issue, the date, the official `meetingURL`, the speech count and up to 30 speaker names. `any` is space-separated AND, `speaker` space-separated OR, `house` is 衆議院, 参議院, 両院 or 両院協議会, and `from` and `until` go together as `YYYY-MM-DD`. `maximum_records` is 1 to 30. When more meetings match than fit in one page, the call fails and says how many there are — it never reports 0 meetings — so you narrow the dates or the words and ask again.

`kokkai_speech` answers who said what: the first 240 characters of each matching speech with `…` when there is more, the speaker, the meeting, the date and the `speechURL`. At most 10 rows, 5 by default. Speech text is never stored: the excerpt is what came back, and the URL is where to read the rest.

`kokkai_watch` saves one query — `add` with `any` or `speaker` (a date range alone is not a watch) — and marks what the window already holds as seen, so the first check announces only what comes after it. `list` shows the saved ones with their ids, `run` checks now, `remove` takes one away by `watch_id`. A saved watch is checked once a day after the hour you set, and what is new appears as a standing notice naming the meetings to open.

`/kokkai 予算` runs the same search in the conversation; bare, it offers a row to type the words in.

Every answer that fetched something ends with the credit line the open-data licence asks for, in Japanese, naming the meeting or speech URLs that were used. Names and speeches are copied as the National Diet Library's API sent them, in Japanese.

The only peer is `https://kokkai.ndl.go.jp`, over GET, with no key. One request is in flight at a time and the next waits five seconds after the last answer, which is the API's own pace. The one file the cell writes is its watch state, `kokkai/state.json` under your Nodecode home; remove a watch, or delete that file, and nothing is kept.

## What is in it

| File | What it is |
| --- | --- |
| `wire.lisp` | The two API paths, the page checks, the excerpt, the credit line. |
| `watch.lisp` | The saved watches, the state file, the daily check and its notice. |
| `cell.lisp` | The three tools, the `/kokkai` command, the settings and the one declaration. |
| `test/` | Offline tests over a scripted network seam. |

## License

MIT, the original's: see `LICENSE` and `NOTICE`. What a search or a speech returns is the National Diet Library's, under PDL 1.0; the credit line every answer carries names it.
