# nodecode-live-time

Live time: every turn's context carries the real current clock. A Nodecode rewrite of [chenfeijiang95-ui/hermes-live-time](https://github.com/chenfeijiang95-ui/hermes-live-time) by [chenfeijiang95-ui](https://github.com/chenfeijiang95-ui).

A session stamps the day it started, once. Leave one open overnight and the model keeps answering as though it were still that afternoon: it calls yesterday "today", misses that it is 3 a.m., cannot tell a five-minute reply from a two-day gap. This cell puts the real date, clock, weekday, zone and UTC offset at the head of every turn's context, read at that turn's first request -- so the model knows what time it is without calling anything, and a `current_time` call reads the clock again in the middle of a long turn.

```
[LIVE-TIME] Now: 2026-11-08 14:32:05 (Weekday 6/7, Sat), TZ Europe/Berlin, UTC+01:00.
Read at this turn's first request by the live-time cell: this is the authoritative
current time for every today/now/elapsed/date judgment, and an earlier session-start
stamp is history. current_time reads the clock again mid-turn.
```

Nothing leaves the machine: no request, no subprocess, nothing written. The only file it reads is a zone of the machine's own tz database, the same bytes `TZ=Europe/Berlin` reads.

## Get it

Tell the agent "add the live-time cell", or run:

```sh
nodecode add nc://live-time
```

## Set it up

Nothing is required. In `/settings`, the section `live-time` carries:

| Setting | Default | What it does |
|---|---|---|
| `timezone` | (empty) | an IANA zone name such as `Europe/Berlin` or `Asia/Tokyo`; empty reads this machine's own zone, daylight saving included |
| `zoneinfo_dir` | `/usr/share/zoneinfo` | where this machine's tz database lives; only read when `timezone` names a zone |

A zone name the database does not hold refuses at start, in words that name the setting, rather than quietly answering the wrong hour; `/time Some/Zone` fails the same way in a sentence. A macOS, Linux or BSD machine has the database at the default path. Leave `timezone` empty on a machine without one.

## Use it

Nothing to type: the block is in front of the model at every turn. What it is for:

- "How long since we started?" / "What's today's date?" / "Is it still Friday?" -- answered from the block, with no tool call.
- A session left open across a night or a weekend picks up knowing the gap.
- `current_time` (a tool the model calls) answers the same reading with an optional `timezone` argument, for the middle of a long turn or for a zone the setting does not name.
- `/time` answers it to you, and `/time Asia/Tokyo` answers for a zone you type.
- `(help :live-time)` carries the guidance the model reads.

The reading rides the standing harness section (`nle::read-harness-sections`), which the engine reads at the first request of a turn and freezes for that turn's rounds -- that is what keeps the prompt cache intact. A turn that runs long past a reading uses `current_time` for a fresh one; the block says so.

## Notes

- Weekday labels are English on purpose: the block reads the same whatever language the conversation is in.
- The offset is the real one for the instant: daylight saving comes out of the zone's transitions, not from a fixed number.
- The zone file is read on each reading (a few kilobytes) and nowhere cached, so a tz database update is picked up at the next turn.

## Tests

```sh
.github/test-cell.sh tools/nodecode-live-time
```

Offline: the clock is pinned and the tz database is one file the tests write byte by byte -- a version 2 file and a version 1 file, both read through their transitions -- plus the section lifecycle, the tool, `/time`, the machine's own zone, the fallback when a zone file goes away, and the refusals.

## Credits

A rewrite of chenfeijiang95-ui's Hermes Agent plugin, MIT, at commit `f5e4edcefb19f68f11c0e3faf5f6983577500c75`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
