# nodecode-octoprint

OctoPrint Print Watch: watch one OctoPrint 3D printer and pause, resume or cancel its job on approval. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-print-job-watch](https://github.com/TakeshiTGAL/hermes-plugin-print-job-watch) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

A print runs for hours. This cell answers what it is doing right now: the job
state, the filename, how far along it is, OctoPrint's own printTimeLeft
estimate, and how hot the hotend and the bed are. It can pause, resume or
cancel, and it says the printer moved only when a second reading agrees. It
can save one webcam still, and it can tell you what changed since the last
look.

## Get it

Tell the agent "add the octoprint cell", or run `nodecode add nc://octoprint`.

## Set it up

Two required settings, in the `octoprint` section of `~/.nodecode/config.jsonc`:

| Setting | What it is for |
| --- | --- |
| `url` | The one OctoPrint origin, such as `http://printer.local:5000`. No path, query, user or password. Plain `http` sends the key unencrypted. |
| `api_key` | An OctoPrint user API key, sent only as the `X-Api-Key` header and only to that origin. With access control off OctoPrint treats a missing key as an admin, so the cell refuses to call without one. Never the global key from the settings page. |
| `snapshot_url` | Optional. An `http(s)` webcam still on the printer's own host; another port is allowed, another host is not. JPEG or PNG only. |
| `stall_minutes` | How long the file position may stand still while printing before a watch calls it stalled (10 by default). |
| `snapshot_max_bytes` | The largest webcam still to keep (2000000 by default). |
| `watch_minutes` | How often a background check runs, in minutes. `0` (the default) is off. |

## Use it

- **`octoprint_status`** reads `/api/connection`, `/api/job` and `/api/printer`
  and answers the job state, connection state, filename, completion, file
  position, `printTimeLeft` (which may be 0) and the `tool0` and `bed`
  temperatures. A disconnected printer is named disconnected, not paused;
  `Error` and `Offline after error` are errors even when `/api/printer`
  answers 409. `include_snapshot` saves one webcam still.
- **`octoprint_control`** takes `action` (pause, resume or cancel). Without
  `confirm` it says what would be sent and what the state is now, and sends
  nothing; with `confirm` it reads the state, posts the move, and reads the
  state again. Pause goes from `Printing` or `Printing from SD` only, resume
  from `Paused` only, and a job in `Pausing` is left alone until it settles.
  Cancel ends the print and cannot be resumed.
- **`octoprint_watch`** compares this check with the stored one and reports
  `printer_disconnected`, `error`, `complete`, `paused` (only when a print was
  running), `cancelled`, and `stalled` when the file position, completion and
  filename have all stood still for `stall_minutes`. A printer that cannot be
  reached is reported once per cause, then once a day, and once when it
  answers again.
- **`/octoprint status|watch|snapshot`** is the operator's own reading; the
  command never moves the printer.
- With `watch_minutes` above zero, a background check raises its events as a
  standing notice, so a finish, a failure or a stall reaches you without
  asking.
- **(help :octoprint)** answers the same while the cell runs.

## What it reaches

One host: the OctoPrint origin in `octoprint.url`, over `http` or `https`.
`/api/connection`, `/api/job` and `/api/printer` for the readings,
`POST /api/job` for a confirmed move, and the webcam still at
`octoprint.snapshot_url` -- refused when it points away from that host. The
API key, when set, goes only to that origin as `X-Api-Key` and is never
echoed in a message. The stills and the watch state live in this cell's own
folder under the Nodecode home.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the three tools, `/octoprint`, the manual, the still, the watch and its worker |
| `watch.lisp` | One sample against the last: the events, the stall, the per-print generation |
| `test/` | Offline tests: OctoPrint is stubbed, nothing dials |

## Tests

```sh
.github/test-cell.sh tools/nodecode-octoprint
```

Offline: `nlk:http` answers from a canned OctoPrint, so the run asserts on
what the cell asked and what it made of the answer.

## Credits

MIT, Copyright (c) 2026 TakeshiTGAL. A rewrite of
[TakeshiTGAL/hermes-plugin-print-job-watch](https://github.com/TakeshiTGAL/hermes-plugin-print-job-watch)
at commit `61cf86ed8c1a348c2d9956368e1abf7f51bb40fa`; see [NOTICE](NOTICE).

