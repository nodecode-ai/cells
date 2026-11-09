# nodecode-health-insights

Health Insights: weekly trends, rule-based concern checks and data coverage from an Apple Health receiver database. A Nodecode rewrite of [mwdearing/hermes-health-insights](https://github.com/mwdearing/hermes-health-insights) by [mwdearing](https://github.com/mwdearing).

The original is a skill set over a Python command line tool. This cell is the
same analysis, in Lisp, inside the organism: it reads the SQLite database a
HealthRelay or Health Bridge receiver writes, and the model explains what the
numbers say.

## Get it

Tell the agent "add the health insights cell", or run `nodecode add nc://health-insights`.

## Set it up

Name the database once. It is the file the receiver on your Mac writes; the
cell reads it with `SELECT` and never writes to it.

```jsonc
"health-insights": {
  "db": "~/Health/receiver.sqlite",   // the receiver's SQLite database
  "utc_offset_hours": -6,             // your offset from UTC, so a day is your day
  "weight_unit": "kg",                // kg | lb, display only; the database stores kg
  "gap_days": 3                       // days of silence before data is called stale
}
```

`utc_offset_hours` is what the original took from the system time zone: every
day boundary — a daily mean, "last seven days", a night's sleep — uses it, so
a reading at 23:30 your time lands on the day you had it.

No key, no account, no network: everything is computed locally.

## Use it

Ask the agent for a health check, or type `/vitals`:

- `/vitals` — the weekly report: sleep, resting heart rate, HRV, weight, SpO2,
  heart rate and blood pressure, each as its 7-day mean against the 28-day
  median, with up/down only when the change reaches one robust sigma (the
  MAD-based spread) of that baseline. A metric with too little data says so.
- `/vitals concerns` — the rule findings, worst first: a severe blood pressure
  reading, a 7-day average in stage 2, resting heart rate above its baseline,
  an HRV run below it, short sleep, low oxygen saturation, a metric that
  stopped syncing, a database gone stale. Levels are worth watching, discuss
  with a clinician, urgent.
- `/vitals coverage` — what the database holds, per metric: how many days, the
  first and last, how old the newest day is, and when the receiver last ran.
- `/vitals daily resting_heart_rate` — one metric's daily values with its 30-
  and 90-day medians.

The model has the same four through its `health_insights` tool. Every answer
carries the same limit: deterministic thresholds, informational only, never
medical advice.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The declaration: the section, the tool, `/vitals`, the manual. |
| `report.lisp` | The arithmetic: daily means, the two windows, the concern rules, the SQLite read. |
| `test/cell-test.lisp` | The lifecycle, the reports over a stub, and one real receiver database built in the test. |

## License

Apache-2.0, as the original. See `LICENSE` and `NOTICE`.
