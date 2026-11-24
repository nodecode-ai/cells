# nodecode-apple-health

Apple Health: seven tools that read your own Apple Health export on this machine — a day, a week, the records, the workouts. A Nodecode rewrite of [davidmosiah/apple-health-mcp](https://github.com/davidmosiah/apple-health-mcp) by [davidmosiah](https://github.com/davidmosiah).

Nothing here dials anything. The export is a file you exported from the iPhone and unzipped on this machine; the cell reads that one file, one XML element at a time, and answers from it.

| Tool | What it answers |
| --- | --- |
| `apple_health_connection_status` | Is the export there and readable: its path, size and age, the privacy mode, and the next step when it is missing. |
| `apple_health_data_inventory` | One scan: how many records of each type, which workout activities, and the first and last day the export covers. |
| `apple_health_daily_summary` | One day: steps, distance, active energy, heart rate average/min/max, resting heart rate, HRV, sleep hours, that day's workouts. |
| `apple_health_weekly_summary` | The same per day for a run of days, plus the totals over the window. |
| `apple_health_records` | Individual records, bounded, filtered by type and by day. |
| `apple_health_workouts` | The workouts, newest first: activity, minutes, distance, energy. |
| `apple_health_privacy_audit` | What the cell reads, and what it does not. |

There is also a `/health` command a person can type: `/health 7` answers the last seven days of the export as plain text.

## Get it

Tell the agent "add the apple-health cell", or run `nodecode add nc://apple-health`.

## Set it up

1. On the iPhone: **Health** → your profile picture → **Export All Health Data**, and transfer the archive (`export.zip`) to this machine.
2. Unzip it. The archive holds `apple_health_export/export.xml` — that file is what the cell reads.
3. Point `export_path` at it: the file itself, or the folder that holds it.

```jsonc
"applehealth": {
  "export_path": "~/Health/apple_health_export/export.xml",  // or the folder
  "privacy_mode": "structured",                               // structured | raw
  "max_records": 25                                           // the cap on one list answer
}
```

`privacy_mode` decides what a record list carries. `structured` (the default) answers the type, the value, the unit and the day. `raw` adds the export's own type names, the source device or app, and the full timestamps.

A query scans the export each time: a big export takes a second or two per call, and nothing is cached.

## Use it

You do not call the tools; the model does, when a question is about your health data. "How did I sleep this week?", "what were my workouts last month?", "is my resting heart rate going up?" — the model calls the summary tools, or `apple_health_records` for the underlying numbers.

A person can call the same functions from eval, in the `apple-health` package:

```lisp
(apple-health:list-records
 (apple-health:decode-record-query
  (nlk:decode-json "{\"type\":\"StepCount\",\"limit\":5}")))
```

`(help :apple-health)` is the model's manual for the seven verbs, kept while the cell runs.

### What this rewrite reads, and what it does not

- **No network, no cache.** The export is opened read-only on every call; the cell writes nothing and keeps nothing between calls.
- **No zip.** Unzip the export yourself; the cell reads `export.xml`, not `export.zip`.
- **No live HealthKit.** The numbers are as fresh as the last export you imported.
- **A day is the day the export wrote.** Apple writes each timestamp in your local time with its offset, and the cell groups by that day, so a record made just after midnight belongs to the day it felt like.
- **Not medical advice.** It is your own data, for reflection and planning, not a diagnosis.
