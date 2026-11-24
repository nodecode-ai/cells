# nodecode-crontab

Crontab Reference: what a cron expression fires, field by field, with the runs it fires next. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

A cron line is short and easy to get wrong, for people and for models: `0 9 1 * 1` is not "the first Monday", `* 9 * * *` runs sixty times an hour, `*/7` restarts at `:00`, and `@daily` is not a five-field expression. This cell reads one out in plain words, puts each field on its own row, lists the runs it fires next in the zone you are in, and adds the notes people usually need -- so the model gets the reading done instead of working it out in its head.

```
Cron expression "0 9 * * 1-5", read the way a scheduler reads it
  At 09:00 on Monday through Friday.
  minute        0    at minute 0 of the hour
  hour          9    09:00-09:59
  day of month  *    any day
  month         *    any month
  day of week   1-5  Monday through Friday
Next 5 runs, in Asia/Kolkata (the crontab timezone setting)
  Mon 2026-11-09 09:00 UTC+05:30  in 1 day 14 hours
  Tue 2026-11-10 09:00 UTC+05:30  in 2 days 14 hours
  ...
```

Nothing leaves the machine: no request, no subprocess, nothing written, and no job scheduled. The only file read is a zone of the machine's own tz database, and only when a timezone is named.

## Get it

Tell the agent "add the crontab cell", or run:

```sh
nodecode add nc://crontab
```

## Set it up

Nothing is required. In `/settings`, the section `crontab` carries:

| Setting | Default | What it does |
|---|---|---|
| `timezone` | (empty) | an IANA zone name such as `Asia/Tokyo`; empty reads this machine's own zone, daylight saving included |
| `zoneinfo_dir` | `/usr/share/zoneinfo` | where the tz database lives; only read when `timezone` names a zone |
| `runs` | `5` | how many next runs a reading lists, 1 to 20 |

A zone name the database does not hold refuses at start, in words that name the setting, rather than quietly answering the wrong hours.

## Use it

Ask the model, and it calls the `explain_cron` tool:

> What does `0 9 1 * 1` actually fire? `explain_cron` it for me in Asia/Tokyo.

The tool takes the `expression` and an optional `timezone`; omitted, the zone is the section's, else this machine's own. Its reading comes back as the sentence, the rows, the next runs and the notes.

`/crontab` does the same for you, in the section's zone:

```
/crontab "0 9 * * 1-5"
/crontab */7 * * *
/crontab @daily
```

Typed alone, it shows four examples with their readings. An expression with spaces goes in quotes.

What the reading carries:

- **A sentence.** `0 9 * * 1-5` -- "At 09:00 on Monday through Friday."; `*/15 9-17 * * *` -- "Every 15 minutes during 09:00-17:59 (first 09:00, last 17:45)."
- **One row per field**, the written field beside what it means.
- **The next runs** (five by default) with the weekday, the date, the offset of the zone, and how far off each is. A wall-clock schedule stays at its clock time across a daylight-saving change, and a wall time the zone skips does not run.
- **Notes** for the cases that surprise people: day of month and day of week both set run on a day matching either; `*` in the minute field means every minute; a step that does not divide the hour or the day leaves a short gap at the turn; days 29-31 skip the months without them; `L` is the last day of the month; `7` in day of week is Sunday; a 6th field is seconds and a 7th is the year; and a date that never comes says `Next runs: none.`

`(help :crontab)` carries the same guidance for the model. This cell only explains: to run something on a schedule, use the cron cell, and paste the same expression there.

## Notes

- A shortcut (`@daily`, `@hourly`, ...) is read as the five fields it stands for, and the reading shows them: `same as 0 0 * * *`.
- Month and weekday names are read (`mon-fri`, `JAN`), so is a range, a list and a step.
- The zone comes out of the tz database's own transitions, so an offset is the real one for the instant, and the summer/winter switch is where the database puts it.
- A bad expression is answered in a sentence that names the field to repair, never as a signal.

## Tests

```sh
.github/test-cell.sh tools/nodecode-crontab
```

Offline: the clock is pinned and the tz database is a file the tests write byte by byte (a fixed offset, and an offset that changes mid-week), plus the section lifecycle, the tool through its declared contract, `/crontab`, and the refusals.

## Credits

A rewrite of olric3015's Hermes Agent plugin, MIT, at commit `c5a412f46ab5cc14bb82528481a46c8f8bb3fe2a`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
