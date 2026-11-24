# nodecode-penguin

Penguin: time across zones, and the windows a whole team's meeting fits. A Nodecode rewrite of [Crystal32378/hermes-penguin](https://github.com/Crystal32378/hermes-penguin) by [Crystal32378](https://github.com/Crystal32378).

A meeting across zones is a wall-clock problem in several places at once, and a
model that adds offsets in its head gets it wrong on the two days a year the
clocks move, on the half-hour zones, and on an overnight shift. This cell does
the arithmetic on the machine's own tz database. One explicit local time is
resolved to an instant and read in every zone you ask for; a team's hours are
tested minute by minute for the windows where the whole meeting fits for
everyone, whatever their zone.

```
penguin_convert  2026-11-08T14:30  Asia/Taipei  ->  Europe/Berlin
  2026-11-08T14:30 Asia/Taipei (UTC+08:00) is 2026-11-08T06:30Z
    Europe/Berlin  2026-11-08T07:30 (UTC+01:00, CET)
```

Nothing leaves the machine: no request, no subprocess. The files it reads are a
zone of this machine's tz database, and the teams it saved under the home; the
one file it writes is that teams file. It books nothing and reads no calendar.

## Get it

Tell the agent "add the penguin cell", or run:

```sh
nodecode add nc://penguin
```

## Set it up

Nothing is required on Linux or macOS, where the tz database sits at the path
the default names. In `/settings`, the section `penguin` carries:

| Setting | Default | What it does |
|---|---|---|
| `zoneinfo_dir` | `/usr/share/zoneinfo` | the tz database folder zone names are read from |
| `max_slots` | `5` | how many meeting windows a search offers, 1 to 20 |

A folder that is not there refuses at start, in words that name the setting,
because a cell that cannot read a zone file cannot say an hour. On a machine
without a tz database, point `zoneinfo_dir` at one (the IANA tzdata
release, or a copy another program keeps) before turning the cell on.

## Use it

The model calls four tools:

| Tool | What it answers |
|---|---|
| `penguin_convert` | one local date and time in one IANA zone, read in up to 30 others, as UTC plus every zone's local time, offset, abbreviation and weekday |
| `penguin_overlap` | the windows where a meeting of a given length fits inside the whole of every member's hours, plus the earliest starts to offer |
| `penguin_save_team` | a team kept under a short id, when you say to remember it |
| `penguin_get_team` | a saved team, by its id |

A member is their name, their zone and their working windows: a start and an
end as `HH:MM` in their own zone, and the weekdays it covers, Monday `0`
through Sunday `6`. An end before its start is a shift that runs overnight on
the start day. A window whose start and end are equal keeps no hours.

Ask in your own words:

- "What time is 14:30 in Taipei for the Berlin and São Paulo people?" -- `penguin_convert`, and it asks which reading you mean on the day the clocks go back.
- "When can Ana, Bo and Chen all meet for an hour next week?" -- `penguin_overlap`, each of them with their own hours.
- "Remember this team as evening-standup." -- `penguin_save_team`, then `team_id` instead of repeating the team.

Or type the conversion yourself:

```
/penguin 2026-11-08T14:30 Asia/Taipei Europe/Berlin America/Sao_Paulo
```

The manual is `(help :penguin)`.

### What it refuses, and why

- A local time the clocks skipped (they jumped forward over it) is refused with a sentence saying so -- there is no instant to convert.
- A local time that occurs twice (the clocks went back) is refused until the call says `earlier` or `later`, so the model asks which one you meant instead of picking.
- A zone name the database does not hold, a date outside 2000-2100, or two members of one name are refused the same way: a sentence naming what to fix, answered to the model rather than thrown.

Daylight saving comes from the zone files themselves, so a search that spans
the weekend the clocks move is still right: both readings of a repeated hour
count, and a spring-forward gap holds no instant.

## Credit

The engine, the four tools and their arguments are Crystal32378's; this is
Crystal32378's plugin rewritten in Common Lisp against Nodecode's cell
contract. See [NOTICE](NOTICE) for what was rewritten and what was left out,
and [LICENSE](LICENSE) for the MIT licence it keeps.
