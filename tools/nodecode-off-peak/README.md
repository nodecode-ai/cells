# nodecode-off-peak

Off-Peak Queue: Ollama Cloud's off-peak window, its countdown, and a ledger of approved work. A Nodecode rewrite of [onkanat/hermes-offpeak-bekci](https://github.com/onkanat/hermes-offpeak-bekci) by [onkanat](https://github.com/onkanat).

Ollama Cloud is cheaper off-peak: outside 12:00-18:00 UTC on weekdays, and all day at the weekend. The cell says which side of that line you are on, how long until it moves, and what your own ledger of queued work holds, so bulk work waits for the cheap window instead of guessing.

## Get it

Tell the agent "add the off-peak cell", or run `nodecode add nc://off-peak`.

## Set it up

Point `off-peak.queue_file` at your ledger in `/settings` (or in the `off-peak` section of `config.jsonc`; `config.example.jsonc` is the whole section, commented). Until it is set, every verb says so, naming the setting. `off-peak.remind_minutes` (default 30, at least 1) is how early the one-shot notice fires.

The ledger is markdown you write, one item a line -- a table row `| ID | job | owner | evidence | status | window |` or a bullet `- id: K-001 | status: awaiting | ...`. Statuses are `awaiting`, `approved`, `ran` and `skipped`; the Turkish words `durum`, `onaylı`, `koşuldu` and `atlandı` read as the same. Nothing enters the ledger by itself and nothing but `/off-peak approve` writes it.

## Use it

- `/off-peak` -- the window, the countdown to its next change, and the ledger's counts: how many approved, awaiting, ran and skipped. While the window is peak, approved items wait; while it is off-peak, they may run.
- `/off-peak json` -- the same as one JSON line, for a script or a badge. The `off_peak` tool answers exactly this, so the model can read the window before it starts a long run.
- `/off-peak approve K-001,K-004` -- mark those items approved: only you approve. The file is rewritten atomically, and the answer says which ids changed, and which were not there or already approved.
- `/off-peak plan` -- the cron job the window suggests, in full, with the hash of the plan. Nothing is installed: hand the printed prompt to the shipped cron cell, and it runs the approved items at 18:05 UTC on weekdays.

A worker says once, `remind_minutes` before the window opens, that it is about to -- as a standing notice, so a shell attached later still sees it.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The window arithmetic, the ledger reader and its approval, `/off-peak`, the `off_peak` tool and the reminder. |
| `test/cell-test.lisp` | The window at fixed instants, both ledger shapes, approval that leaves every other line alone, the verbs, and the refusal when the ledger is not set. |
| `config.example.jsonc` | The section, commented: documentation, never config. |

## What the rewrite leaves out

The original's status-bar chip and title-bar badge (a Hermes desktop surface), and its `llm_execution` middleware that measured -- and could sleep -- Ollama Cloud calls inside the peak window: Nodecode's cells have no per-call seam to measure, so the countdown and the ledger carry the same intent, and `/off-peak approve` is the only writer. The cron install is a printed plan here, not a request injected into the session: the shipped cron cell installs it.

## License

MIT. The rewrite is a port of onkanat's plugin at commit `e566f91`: see `NOTICE` and `LICENSE`.
