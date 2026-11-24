# nodecode-holidays

Public Holidays: a country's public holidays for a year, and the workday math
on them -- is a date a workday, how many workdays a range holds, and the date
a deadline's N workdays land on. A Nodecode rewrite of
[Akloenx123/hermes-public-holidays](https://github.com/Akloenx123/hermes-public-holidays)
by [Akloenx123](https://github.com/Akloenx123).

The model gets the original's four tools -- `holidays_list`, `workday_check`,
`workdays_count` and `workdays_add` -- and the same four verbs in the
`holidays:` package for eval. Holiday data is [Nager.Date](https://date.nager.at),
which needs no key; the weekends, the regions and all the counting are done
here, so the model never works a calendar out in its head. `/holidays` is the
operator's way in, and `(help :holidays)` is the model's page.

## Get it

Tell the agent "add the holidays cell", or run `nodecode add nc://holidays`.

## Set it up

Nothing is required: Nager.Date is a public API with no key. What the
section can say, `/settings` writes:

```jsonc
{
  "holidays": {
    "subdivision": "DE-NI",          // the region every call counts unless it names one
    "weekend": "Saturday, Sunday",   // names or 0-6 (Monday 0), comma separated
    "base_url": "https://date.nager.at/api/v3"
  }
}
```

| Setting | Default | What it does |
| --- | --- | --- |
| `base_url` | `https://date.nager.at/api/v3` | the API root, for a proxy of your own |
| `subdivision` | empty | the ISO 3166-2 region every call counts when it names none, e.g. `DE-NI`, `US-CA` |
| `weekend` | `Saturday, Sunday` | the days off, as weekday names or 0-6 |
| `timeout_seconds` | `10` | one API call's ceiling |
| `max_span_days` | `3660` | the longest range one `workdays_count` walks |

## Use it

- **`holidays_list`** — `{country, year?, subdivision?, types?}`. One line a
  holiday: date, weekday, its name (the English one, with the local one
  beside it when they differ), and the regions that observe it when it is not
  nationwide. `types` defaults to `Public`; `Bank`, `School`, `Authorities`,
  `Optional` and `Observance` are the others.
- **`workday_check`** — `{date, country, subdivision?, weekend?, types?}`.
  The weekday, whether the date is a workday, and the holiday that makes it
  one. Without a subdivision only nationwide holidays are days off, and a
  regional holiday falling on that date is named as not counted.
- **`workdays_count`** — `{start, end, country, ...}`. Both ends inclusive:
  the calendar days, the workdays, the weekend days, and the holidays that
  fell on a weekday.
- **`workdays_add`** — `{start, workdays, country, ...}`. The date that many
  workdays away, forward or back, the start day itself not counted, and the
  holidays the walk stepped over.
- **`/holidays COUNTRY [YEAR]`** — the same list, for the operator. With no
  argument it says where the cell points and which days it treats as the
  weekend.

A country Nager.Date does not cover is a refusal naming it, never an empty
calendar: an empty calendar would turn every one of its holidays into a
workday.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | the section, the one endpoint, the weekly and calendar arithmetic, the four tools, `/holidays` and `(help :holidays)` |
| `test/cell-test.lisp` | lifecycle, the four tools, `/holidays` and the refusals, every request a stubbed `nlk:http` |
| `config.example.jsonc` | the section, commented |

## Test

From the cells repository's root, offline:

```sh
.github/test-cell.sh tools/nodecode-holidays
```

## License

MIT. Copyright (c) 2026 Alexander Konieczny (Akloenx123): see `LICENSE` and `NOTICE`.
