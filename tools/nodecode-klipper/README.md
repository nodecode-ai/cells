# nodecode-klipper

Klipper Print Watch: watch and steer one Klipper 3D printer over its Moonraker host. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-klipper-print-watch](https://github.com/TakeshiTGAL/hermes-plugin-klipper-print-watch) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

A print runs for hours and everything you want to know is one HTTP call away.
This cell makes those calls for you: what the job is doing, how hot the
hotend and the bed are, how long the slicer thinks is left, and what changed
since the last look. It can pause, resume or cancel, and it says so only
when a second reading shows the printer really moved.

## Get it

Tell the agent "add the klipper cell", or run `nodecode add nc://klipper`.

## Set it up

One required setting, in the `klipper` section of `~/.nodecode/config.jsonc`:

| Setting | What it is for |
| --- | --- |
| `url` | The one Moonraker origin, such as `http://printer.local:7125`. No path, query, user or password. |
| `api_key` | Sent only as the `X-Api-Key` header, and only to that origin. Leave it empty when this machine is in Moonraker's `trusted_clients`. |
| `stall_minutes` | How long the file position may stand still while printing before a watch calls it stalled (10 by default). |
| `snapshot_max_bytes` | The largest webcam still to keep (2000000 by default, up to 5000000). |
| `watch_minutes` | How often a background check runs, in minutes. `0` (the default) is off. |

## Use it

- **`klipper_status`** reads `print_stats`, `virtual_sdcard`, `display_status`,
  `extruder`, `heater_bed` and `webhooks` in one query, asks Moonraker for the
  file's slicer metadata, and answers the state, progress, remaining time,
  temperatures and Klipper's own state. A stopped Klipper (shutdown or error)
  is named as such, because `print_stats.state` is not to be trusted until it
  restarts. `include_files` counts the gcode files; `include_snapshot` saves
  one webcam still.
- **`klipper_control`** takes `action` (pause, resume or cancel). Without
  `confirm` it tells you what would be sent and what the state is now, and
  sends nothing; with `confirm` it reads the state, sends the move, and reads
  the state again. It sends nothing when the printer is already there, and it
  reports `moved` only when the second reading agrees.
- **`klipper_watch`** compares this check with the stored one and reports
  `klippy_shutdown`, `complete`, `error`, `paused` (only when the previous
  state was printing) and `cancelled` once, plus `stalled` when the file
  position, progress and filename have all stood still for `stall_minutes`.
- **`/klipper status|files|watch|snapshot`** is the operator's own reading;
  the command never moves the printer.
- With `watch_minutes` above zero, a background check raises its events as a
  standing notice, so a finish or a Klipper stop reaches you without asking.
- **(help :klipper)** answers the same while the cell runs.

## What it reaches

One host: the Moonraker origin in `klipper.url`, over `http` or `https`.
`/printer/objects/query`, `/printer/print/{pause,resume,cancel}`,
`/server/files/metadata`, `/server/files/list`, `/server/webcams/list`, and
the webcam `snapshot_url` Moonraker itself lists -- refused when it points
away from that origin. The API key, when set, is sent only as `X-Api-Key` to
that origin and is never echoed in a message. The watch state and the stills
live in this cell's own folder under the Nodecode home.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the three tools, `/klipper`, the manual, the snapshot, the watch state |
| `watch.lisp` | One sample against the last: the events, the stall, the per-print generation |
| `test/` | Offline tests: Moonraker is stubbed, nothing dials |

## Tests

```sh
.github/test-cell.sh tools/nodecode-klipper
```

Offline: `nlk:http` answers from a canned Moonraker, so the run asserts on
what the cell asked and what it made of the answer.

## Credits

MIT, Copyright (c) 2026 TakeshiTGAL. A rewrite of
[TakeshiTGAL/hermes-plugin-klipper-print-watch](https://github.com/TakeshiTGAL/hermes-plugin-klipper-print-watch)
at commit `1dd1391c171afd92825ec07a81b1efbaca5c64e0`; see [NOTICE](NOTICE).
