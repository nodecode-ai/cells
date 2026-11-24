# nodecode-pocket-watch

Pocket Watch: what the scheduled jobs are doing, as a report — counts, one line a job, a health score each, and the day, week and month grids. A Nodecode rewrite of [indigokarasu/hermes-pocket-watch-plugin](https://github.com/indigokarasu/hermes-pocket-watch-plugin) by [indigokarasu](https://github.com/indigokarasu).

## Get it

Tell the agent "add the pocket-watch cell", or run `nodecode add nc://pocket-watch`.

## Set it up

Nothing is required. With no configuration the reports read the jobs the
[`nodecode-cron`](https://github.com/nodecode-ai/cells/tree/main/tools/nodecode-cron)
cell keeps in the store, so a cron job that is failing shows up here the moment it
does. The section `pocket-watch` in `~/.nodecode/config.jsonc` tunes it:

- `jobs_file` — a cron jobs JSON to report on instead, the way a harness keeps
  one: `{"jobs": [...]}`. Absent, the store is read.
- `limit` (50) — the most jobs one report lists.
- `report_chars` (4000) — the longest report, in characters.
- `warn_below` (80) and `crit_below` (40) — where the healthy, degraded and
  critical bands fall.

## Use it

The model calls the `pocket_watch` tool, and the operator types `/pocket-watch`:

- `view: "status"` — how many jobs there are, enabled, paused, ok, in error,
  never run, by kind, and the average health score.
- `view: "jobs"` — one line a job: id, state, kind, the schedule as it was set,
  next and last run, health score. `status:` and `kind:` narrow the list.
- `view: "health"` — every job, worst score first, then how many are healthy,
  degraded and critical.
- `view: "daily"`, `"weekly"`, `"monthly"` — the schedules as a grid: one
  line an hour, a column a day of the week, a column a day of the month.

From eval, the same reports are functions:
`(pocket-watch:report "health")`, `(pocket-watch:daily-text)`.

A health score is 100 for a job that last ran clean: 20 off per consecutive
failure (at most 60), 30 off when the last run failed, 10 off when it never ran,
5 off when it never completed; a paused job scores 0. Nothing here writes:
the jobs are read, and a job whose registry keeps only its newest outcome is
scored on that outcome.
