# nodecode-woof

WOOF weather simulations on this machine's GPU: size a run, launch it, watch it, stop it, show and compare its maps, search WOOF's own documentation, run a campaign of arms one at a time, and score a finished run against observations. A Nodecode rewrite of [recastsystems/hermes-woof](https://github.com/recastsystems/hermes-woof) by [recastsystems](https://github.com/recastsystems).

## Get it

Tell the agent "add the woof cell", or run `nodecode add nc://woof`.

## Set it up

It needs the WOOF command on the machine: `pip install recast-woof` in a virtual
environment of its own, or WOOF's own install guide, which also fetches the
model's tables and geography. WOOF runs on Linux x86_64 with an NVIDIA GPU; a
machine without one can still size a plan, search the manual and read the
catalog, but nothing runs and nothing scores.

Then point the cell at the command and, if you want, at the folder runs live in:

```jsonc
{
  "woof": {
    "enabled": true,
    "woof": "~/woof-env/bin/woof",   // or just "woof" when it is on PATH
    "runs_dir": "~/woof-runs",       // unset: woof-runs/ under your home
    "python": "",                     // unset: the python3 beside the woof command
    "max_images": 4
  }
}
```

`woof_score` needs `python`: it runs WOOF's own observation doors and its
battery scorer inside the WOOF environment, the way the original does. Unset,
the cell takes the `python3` beside the command it was given, and answers a
setup question rather than a score when it finds none.

## Use it

Ask for a forecast, and the agent sizes it, starts it and shows the maps as
they arrive. The verbs, which the agent finds in its index and reads with
`(help :woof)`:

- `woof_doctor` — the command it found, its version, and WOOF's own doctor
  report. Run it first: without the command nothing else works.
- `woof_catalog` — what WOOF offers here, from WOOF itself: start-data
  sources, physics suites, historical cases, research recipes.
- `woof_plan` — one WOOF run plan in, WOOF's resolver and estimator out: the
  grid, the memory fit, the download and the wall time it predicts. Nothing
  runs.
- `woof_launch` — start the plan, detached, and answer a run id at once.
- `woof_status` — a run as it stands, or every run this cell started.
- `woof_stop` — terminate a run it started, by the pid it recorded.
- `woof_compare` — two runs side by side through WOOF's own pair renderer,
  difference maps or sheets, answered as `MEDIA:` lines.
- `woof_report` — everything a run left behind in one answer: setup, ending,
  maps, log tail, event tail and the score when one was written. Give it a
  `campaign_id` and it reports a campaign's arms instead.
- `woof_show` — the maps so far, absolute paths and `MEDIA:` lines to copy
  into the reply.
- `woof_docs` — WOOF's own documentation, searched offline by BM25 over the
  pack the original ships: physics options, the run plan, outputs,
  verification and limits.
- `woof_campaign` — several runs of one plan that differ along one axis
  (`plan`, `start`, `status`, `stop`): the arms are sized together, then run
  one at a time, because one card runs one forecast at a time. Each `status`
  call starts the next arm once the one before it has ended.
- `woof_score` — a finished run against MRMS reflectivity, Stage-IV
  precipitation and ASOS surface reports, fetched and scored through WOOF's own
  doors under the one-off registration WOOF's registration library builds for
  the run's length. It writes `score.json` beside the run, which
  `woof_report` carries.

`/woof` answers the same from the shell: with no argument, the command and the
runs; with a run id, that run.

A run is a folder under `runs_dir`: `runs/<run_id>/plan.json`, `woof.log`, the
`events.jsonl` WOOF appends, the maps WOOF renders, and `score/` once it is
scored. A campaign is `campaigns/<campaign_id>/campaign.json` and one plan per
arm. Nothing else is kept, so a restart loses nothing.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the contracts, the manual and the verb that drives the CLI. |
| `docs.lisp` | The documentation search: the tokeniser, the index and BM25. |
| `campaign.lisp` | Campaigns: the spec, the per-arm sizing and the queue. |
| `score.lisp` | Scoring: the observation doors, the registration and the scorer's record. |
| `package.lisp` | The one package. |
| `docs/index.json`, `docs/MANIFEST.json` | WOOF's documentation pack, carried from the original (see `NOTICE`). |
| `test/cell-test.lisp` | Offline tests: the command line is stubbed, so no WOOF, no GPU and no network are needed. |

## What it does not do

The original also installs WOOF itself, drives a GPU host over ssh or WSL, and
keeps a watcher that pushes run events into the chat as messages. This cell runs
on the machine it is on, through the command that machine has, and the verbs are
polled instead of pushed: twelve of the original's twelve tool verbs, none of
its watcher.

## License

Apache-2.0. Rewritten from recastsystems's `hermes-woof`: see `NOTICE` and `LICENSE`.
