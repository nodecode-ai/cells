# nodecode-cron-plan

Cron Consolidator: a read-only plan for tidying a crowded cron list — which jobs are really one job,
which fire in the same minute, which keep failing and which deliveries were never confirmed. A Nodecode
rewrite of [Moodtuner997/hermes-cron-consolidator](https://github.com/Moodtuner997/hermes-cron-consolidator)
by [Moodtuner997](https://github.com/Moodtuner997).

Cron lists grow one job at a time: a digest for Monday, another for Wednesday, the same pull-request
chase for three repositories, five jobs that all fire at 09:00. The plan reads a cron folder and says
which of them look like one job written several times. It changes nothing: you read the plan and decide.

## Get it

Tell the agent "add the cron plan cell", or run `nodecode add nc://cron-plan`.

## Set it up

One setting is required — the home whose cron folder holds the plan's input:

```jsonc
{
  "cron-plan": {
    "home": "~/.hermes",   // a Hermes home: the plan reads <home>/cron/jobs.json
    "days": 14             // how far back usage_audit.jsonl is counted
  }
}
```

It reads the two files the original plugin read: `<home>/cron/jobs.json` (required: the jobs, their
prompts, skills, destinations, schedules and failure streaks) and `<home>/cron/usage_audit.jsonl`
(optional: what each fire cost, one record a line). Point `--jobs` at any jobs.json, or `--home` at
a copied home, to look at one without changing the section. Nothing needs a key and nothing is written.

## Use it

In a session:

```text
/cron-plan                                    # the plan for the configured home
/cron-plan report --json                      # the same plan as one JSON object
/cron-plan report --days 30                   # a longer usage window
/cron-plan report --profile work              # <home>/profiles/work/cron
/cron-plan report --home /path/to/that/home   # a copied home, for a dry look
```

Or through eval, the same words: `(cron-plan:plan "report --json")`.

The plan answers:

- **Merge candidates** — jobs that are one job written several times: prompts that are
  near-duplicates (85% or more after numbers and spaces are normalised), jobs sharing the same skills
  and destination, jobs running the same script. Each candidate names the job to keep (the one that
  fired most, then the oldest), what folds into it and why, and the skills to carry over.
- **Up to N prompt tokens/day** — a ceiling, counted from `usage_audit.jsonl` over the window, from
  the fires it saw. A merged prompt is longer and its output still has to be produced, so the figure
  is a ceiling and not a forecast; where there is no usage data the plan claims nothing.
- **Clock-time jobs that fire in the same minute** — with days of week, month and day of month taken
  into account (Monday 09:00 and Tuesday 09:00 do not collide), and pollers such as `*/30` left out,
  which would otherwise chain every job into one group.
- **Failing jobs** (a streak of three or more, with the last error) and **deliveries never confirmed**.
- **Schedules not understood** — listed as written rather than guessed.

## What it does not do

It applies nothing: there is no `--apply`, because a merge changes what an agent does and that stays
your decision. It writes nothing — no file, no table — and makes no network call and no model call.
`cron/executions.db` is not read: a failure streak is the number `jobs.json` records. The one text it
repeats is the first 160 characters of a failing job's last error; prompt text is compared, never
printed.

## Files

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, `/cron-plan`, `(help :cron-plan)` and the plan verb. |
| `cronfields.lisp` | A five-field cron expression as the sets it fires on, and when two overlap. |
| `plan.lisp` | Reading a cron folder: jobs, usage, prompt similarity, grouping, collisions. |
| `report.lisp` | The plan's numbers, its Markdown, its JSON and its argument words. |

## License

MIT. Copyright (c) 2026 Moodtuner997 — see `LICENSE` and `NOTICE`.
