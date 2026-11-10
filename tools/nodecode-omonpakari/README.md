# nodecode-omonpakari

Omonpakari: a considerate health journal — barometric pressure, cycle windows and sourced absorption checks — kept as a quiet picture before each reply. A Nodecode rewrite of [tamamo510/omonpakari](https://github.com/tamamo510/omonpakari) by [tamamo510](https://github.com/tamamo510).

The original was built for living alongside someone whose body moves in waves:
care that remembers her cycle, her sleep and what she drinks in the morning
lands as care; care that ignores them lands as friction. The same idea here,
as seven tools and one standing section of context — no reminders, no
check-ins, no streaks.

| Tool | What it does |
| --- | --- |
| `omonpakari_log` | The journal in the person's own words: symptoms, medications, supplements, food, notes; `list`, `delete`, `set_limit`, `purge`. |
| `omonpakari_pressure` | Barometric pressure now, the past window and the forecast ahead, with the level and the trend (OpenWeather). |
| `omonpakari_cycle` | Period marks, observed lengths, and the next window as a **range of dates** — never one predicted day. |
| `omonpakari_interactions` | Journal entries taken close to an iron entry that inhibit or promote it, with the fact and its source, and the person's own alternative. |
| `omonpakari_life` | Standing facts in their words: the morning drink, what warms them up, and what each is their alternative to. |
| `omonpakari_waves` | The composed states — pressure, cycle, symptoms, sleep, nutrition — with the counts behind each. |
| `omonpakari_correlate` | Entries against the pressure hours they fell in: counts, baseline share, lift. |

Before each reply the cell adds a short picture of today (at most 1,200
characters) to the standing context, closing with the instruction that it is
background, not a message: weigh it, prefer the person's own alternatives, do
not propose something flagged under interactions today near its conflicting
entry, and never recite the numbers. `/omonpakari` says the same picture to
the person who asks.

## Get it

Tell the agent "add the omonpakari cell", or run `nodecode add nc://omonpakari`.

## Set it up

Nothing is required: the journal, the cycle, the life notes and the
interaction checks work with no key at all. For pressure, name the
environment variable holding an OpenWeather key (the free tier covers the two
endpoints used), and the place to read it for:

```jsonc
"omonpakari": {
  "openweather_api_key_env": "OPENWEATHER_API_KEY",  // the variable, never the key
  "latitude": "59.91",                                // decimal degrees
  "longitude": "10.75",
  "window_h": 24,                                     // hours for the change and the forecast
  "quiet_context": true                               // the picture before each reply
}
```

The journal is one JSON file under `~/.nodecode/omonpakari` (or the
`data_dir` you name). Nothing is written inside the cell's folder, and no
key is ever stored there.

## Use it

Ask "how did I sleep the week my period started?", "does the weather affect
my headaches?", or say "I take iron in the mornings" — the model files the
facts with `omonpakari_log`, keeps the standing ones with `omonpakari_life`,
and checks combinations before proposing them. Pressure needs the person to
give a place, or the section to name one.

Reading the picture costs nothing but context: it is at most 1,200 characters,
and it is built from the local store, so the hook itself never calls the
network. `quiet_context: false` keeps the tools and leaves the prompt alone.

The original plugin's skill text, its refusal to nag and its sourced
interaction table are kept; see NOTICE for what was left out and why.
