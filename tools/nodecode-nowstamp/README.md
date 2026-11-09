# nodecode-nowstamp

Nowstamp: the real clock, and the real gap since the conversation last moved, in every turn. A Nodecode rewrite of [zakiyys/hermes-nowstamp](https://github.com/zakiyys/hermes-nowstamp) by [zakiyys](https://github.com/zakiyys).

A session is stamped with the day it started, once. Leave one open overnight and the model keeps answering as though it were still that afternoon, and nothing in what it was sent says when any message arrived -- so five minutes and a day both come back as "as we discussed". This cell puts two facts at the head of every turn's context: the clock, read at that turn's first request, and how long ago this conversation last moved.

```
[NOWSTAMP] Now: Fri 2026-11-08 19:42 UTC+01:00 | previous message: 5 min ago (today 19:37). Read at this turn's first request: this is the clock for every now, today or how-long-ago judgment, and the gap is the real one -- an older timestamp in the conversation is history.
```

The gap is a calendar comparison, not a count of hours: a message from 23:50 is "yesterday" at 00:10, and a gap of minutes on the same day is said as minutes.

Nothing leaves the machine: no request, no subprocess. It reads one zone of this machine's own tz database, and keeps a small table of turn instants under this home so a gap survives a restart.

## Get it

Tell the agent "add the nowstamp cell", or run:

```sh
nodecode add nc://nowstamp
```

## Set it up

Nothing is required. In `/settings`, the section `nowstamp` carries:

| Setting | Default | What it does |
|---|---|---|
| `timezone` | (empty) | an IANA zone name such as `Europe/Berlin` or `Asia/Tokyo`; empty reads this machine's own zone, daylight saving included |
| `zoneinfo_dir` | `/usr/share/zoneinfo` | where this machine's tz database lives; only read when `timezone` names a zone |
| `format` | `%a %Y-%m-%d %H:%M %Z` | the stamp's shape, in the tokens `%a %A %Y %m %d %e %H %M %S %Z %z %%` |
| `language` | `en` | weekday names: `en` or `id` (Bahasa Indonesia) |
| `show_elapsed` | `true` | also say how long ago the conversation last moved |
| `stamp_tool_results` | `false` | also put the time on a tool result during a long turn |
| `stamp_interval_seconds` | `60` | the least number of seconds between tool-result stamps; `0` stamps every one |

A zone name the database does not hold refuses at start, in words that name the setting, rather than quietly answering the wrong hour; `/now Some/Zone` fails the same way in a sentence. Leave `timezone` empty on a machine without a tz database.

## Use it

Nothing to type: the block is in front of the model at every turn. What it is for:

- "What time is it?" / "What's today's date?" -- answered from the block, with no tool call.
- "How long have I been away?" -- the `previous message: 5 min ago (today 19:37)` part is the real gap, so "yesterday" is used only when it was yesterday.
- A session left open across a night or a weekend picks up knowing the gap; the table of instants is a file under this home, so a restart keeps it.
- `now` (a tool the model calls) reads the clock again mid-turn, with an optional `timezone` argument for a zone the setting does not name.
- `/now` answers it to you, and `/now Asia/Tokyo` answers for a zone you type.
- `stamp_tool_results`, off by default, also puts a `[Current time: ...]` line on a tool result once `stamp_interval_seconds` have passed since the model was last given the time, so a turn that runs long does not drift. It rides the `:TOOL` point, which is only hooked when the setting is on, so nothing else is touched.
- `(help :nowstamp)` carries the guidance the model reads.

The reading rides the standing harness section (`nle::read-harness-sections`), which the engine reads at the first request of a turn and freezes for that turn's rounds -- that is what keeps the prompt cache intact. A turn that runs long uses `now` or the tool-result stamp for a fresh reading; the block says so when stamping is on.

## Notes

- Weekday labels follow the `language` setting; the rest of the block is English on purpose, so it reads the same whatever language the conversation is in.
- The offset is the real one for the instant: daylight saving comes out of the zone's transitions, not from a fixed number.
- The state file holds session ids and instants only, capped at the 200 most recent conversations, and is written through a temp and one rename.
- A session this cell has never seen gets no gap on its first turn -- there is nothing to measure from yet.

## Tests

```sh
.github/test-cell.sh tools/nodecode-nowstamp
```

Offline, with the clock pinned: the standing section, the gap and the words it is said in, the language and the format settings, the tool and `/now`, the refusal of a zone the tz database does not hold, the tool-result stamp behind its switch and its interval, and a restart that keeps the gap.

## Credits

A rewrite of zakiyys's Hermes Agent plugin, MIT, at commit `26148cca897f0798ca2cfc035e9eb2e00349bcf8`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
