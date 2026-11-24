# nodecode-garmin

Garmin Connect: the wearer's own sleep, HRV, Body Battery, stress, steps, heart rate, activities, training load and fitness age, read-only, with deterministic baselines. A Nodecode rewrite of [adrianlaborda/hermes-garmin](https://github.com/adrianlaborda/hermes-garmin) by [adrianlaborda](https://github.com/adrianlaborda).

## Get it

Tell the agent "add the garmin cell", or run `nodecode add nc://garmin`.

## Set it up

Make a Personal API key in Garmin Connect on the web: Settings, Personal API Key. Garmin shows it once, and it expires after the 30, 90, 180 or 365 days you pick. Then either

- paste it in the cell's `api_key` setting in `/settings` (or the page's Settings tab), or
- put it in the environment variable `api_key_env` names, `GARMIN_PERSONAL_API_KEY` by default.

`api_key` wins when both are set. `region` picks Garmin's global server or its China one. Nothing else needs setting: a cell with no reachable key refuses at start with a notice naming the key page, and `(restart-cells "nodecode-garmin")` picks it up once the key is there.

The key is exchanged once for a short-lived token, which lives in memory only. Garmin sees the key and the token; nothing else does.

## Use it

The agent has fourteen read-only tools, all prefixed `garmin_`:

| Tool | What it reads |
| --- | --- |
| `garmin_today` | Steps, resting heart rate, stress, Body Battery, last night's sleep and HRV, in one call. |
| `garmin_sleep` | Sleep per night: score, total, stages, heart rate, SpO2, sleep stress; `detail` for one night. |
| `garmin_hrv` | Overnight HRV per night with Garmin's status and the person's own balanced range. |
| `garmin_body_battery` | Charged, drained, peak, low and latest, per day. |
| `garmin_stress` | The stress score and the time spent in each band. |
| `garmin_steps` | Daily steps against the goal, and the distance. |
| `garmin_heart_rate` | Resting, minimum and maximum heart rate, with the 7-day resting average. |
| `garmin_activities` | Recorded activities, newest first, by date, type or a word in the name. |
| `garmin_activity` | One activity in full, heart-rate zones included. |
| `garmin_activity_fit` | One activity's original ZIP, saved under the home's nodecode folder. |
| `garmin_body` | Weight, BMI, body fat, water and muscle mass, and logged water intake. |
| `garmin_trends` | Baselines over the last N days: mean, 7-day against 28-day, z-score, and plain-language flags. |
| `garmin_training_load` | Minutes per heart-rate zone, the easy/moderate/hard split, and intensity minutes against the goal. |
| `garmin_fitness_age` | Garmin's fitness age against the wearer's own age, and the inputs behind it. |

Ask in your own words -- "how did I sleep", "should I train today", "how hard was this week" -- and the agent picks the tool. `/garmin status` says where the key comes from and which server it uses, `/garmin today` gives the overview without a turn, and `/garmin help` prints the manual, which is also `(help :garmin)`.

The trend flags are deterministic, not a model's impression: a flag needs both a change of at least `threshold_pct` (10% by default) against the person's own baseline and a z-score of at least one, so ordinary day-to-day noise is not reported.

## What it does not do

- It reads; it never writes to Garmin (no workouts pushed, no activities renamed).
- Gaps below 28 days: the baseline needs at least five earlier days to say anything.
- No desktop panel, no `/garmin` daily context in every chat, no morning-brief automation: those were Hermes surfaces.

## Test

```sh
.github/test-cell.sh tools/nodecode-garmin
```

Every Garmin request is stubbed, so the run touches no network.

## License

MIT, the original's: see `LICENSE` and `NOTICE`.
