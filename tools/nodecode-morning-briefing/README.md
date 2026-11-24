# nodecode-morning-briefing

Morning Briefing: the day's weather and the last 36 hours of headlines as one printable newsprint page. A Nodecode rewrite of [drkpxl/morning-briefing](https://github.com/drkpxl/morning-briefing) by [drkpxl](https://github.com/drkpxl).

## Get it

Tell the agent "add the morning briefing cell", or run `nodecode add nc://morning-briefing`.

## Set it up

A latitude and a longitude, in the `morning-briefing` section (`/settings`, or the page's Settings tab):

```jsonc
"morning-briefing": {
  "latitude": "44.05",
  "longitude": "-121.31",
  "place": "Bend, Oregon",
  "subreddits": ["news", "worldnews", "technology", "science"],
  "hours": 36,
  "max_items": 12
}
```

`radar_station` adds the National Weather Service's color radar image for that station (`KRTX`); `output_dir` picks where the page is written, this home's `briefings/` otherwise. No key is needed: both sources are public.

## Use it

Ask for the briefing, call the tool, or type `/briefing`:

```
morning_briefing topics=["science", "technology"] hours=36 max_items=12
/briefing
/briefing science technology
```

It writes `briefing-YYYY-MM-DD.html` and answers the path plus a plain-text digest: the dateline, the weather, then one line a headline. Open the file in a browser and print it (Letter, no headers) for the page the original printed every morning.

Where each section of the page comes from:

| Section | Source |
| --- | --- |
| Weather strip | the National Weather Service's forecast API, at the section's coordinates |
| Radar image | `radar.weather.gov` for `radar_station`, when it is set |
| Headlines | each subreddit's own Atom feed (`/r/<name>/new/.rss`), timestamped and filtered to the window |

A source that does not answer costs only its own section: the page says so in place rather than leaving a gap. A headline with no timestamp inside the window is not printed: a thin news day is honest, stale news is not.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the two reads, the newsprint page, the tool and the command. |
| `test/cell-test.lisp` | Lifecycle, the page, the tool through its contract, the refusal without a place: every request stubbed. |

## License

MIT. The original is MIT; see `LICENSE` and `NOTICE`.
