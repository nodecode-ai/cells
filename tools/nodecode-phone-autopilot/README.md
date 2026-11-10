# nodecode-phone-autopilot

Phone Autopilot: a phone task saved once as an approved script, then replayed on the phone with no model in the loop. A Nodecode rewrite of [jupist/phone-autopilot](https://github.com/jupist/phone-autopilot) by [jupist](https://github.com/jupist).

## Get it

Tell the agent "add the phone-autopilot cell", or run `nodecode add nc://phone-autopilot`.

## Set it up

The routines run on phones the `phone_exec` tool drives, so this cell needs
[phone-harness](https://github.com/phone-harness/phone-harness) on `PATH` - the
same runner the [phone-harness cell](../nodecode-phone-harness) uses. Install it
once if you have not:

```sh
uv tool install phone-harness     # or: pipx install phone-harness
```

Then `/phone-autopilot check` runs its `--version` and says whether it starts.
If the runner lives elsewhere, name it in the `phone-autopilot` section of
`~/.nodecode/config.jsonc` (or in `/settings`):

```jsonc
{
  "phone-autopilot": {
    "command": "phone-harness",   // or "uvx phone-harness"
    "directory": "phone-autopilot",  // where routines and run records live
    "approve": true,              // ask before code is saved or run
    "ask_before_apps": false,     // keep a run out of apps you have not allowed
    "allowed_apps": ["com.spotify.music"],
    "timeout_seconds": 300
  }
}
```

## Use it

Ask for something on your phone, once, with the phone-harness cell's
`phone_exec`. When it works, say "save that as a routine": the model calls

```
routine action=save name=claim-reward title="Claim the daily reward" code='...
checkpoint("home", text="Home")
open_app("com.example.app")
checkpoint("reward", text="Claim")
tap_text("Claim")
done(claimed=True)'
```

A save asks you to approve the code - the question names the phone helpers it
calls and shows its head - and your approval is bound to those exact bytes. A
routine edited on disk afterwards is refused until it is saved again.

`routine action=run name=claim-reward params={...}` replays it. The answer is
JSON: `ok`, `duration_s`, the events, and on a failure `failed_checkpoint`
with the name, the screen it expected, `seen_text` and the screenshot it left
(`(look PATH)` shows it), plus `next_steps`. The prompts are saved under
`routines/` and the runs under `runs/` in that `directory`, so a routine and
its history outlive the session; a task you want on a schedule is a prompt for
the [cron cell](../nodecode-cron) that runs the routine.

`/phone-autopilot` lists what is saved; `show`, `run`, `delete`, `approve`
and `deny` name one; `apps`, `allow` and `revoke` move the allowlist, and
`check` runs the runner's version. `(help :phone-autopilot)` is the manual:
the prelude helpers, the settings and the run record.

### What a routine can call

The script runs as Python with the phone-harness helpers in scope, plus:

- `PARAMS` - this run's values: the saved defaults with the run's laid over.
- `ROUTINE_DIR` - the routine's own folder, for anything it writes.
- `checkpoint(name, text=..., any_of=..., app=..., timeout=10, exact=False)` -
  poll until the screen shows it; a miss saves a screenshot, reports what was
  on screen and stops the run.
- `step(name)` and `done(**summary)`.

Every helper is guarded: a run stops, and says so, at Android's own permission
dialog, and - with `ask_before_apps` on - in an app that is not allowed.

## Differences from the plugin

- **The runner is yours.** Hermes installed `phone-harness` into its own
  environment; a cell runs the copy on `PATH` (or what `command` names).
- **Approval is a question from the page that answers questions**, not a Hermes
  tool-approval prompt; a save nobody answered is kept but unapproved, and
  `/phone-autopilot approve` is the operator's own yes.
- **No live phone pane.** The plugin's dashboard showed the phone and the run
  records and offered Allow / Not now cards; here the tool's JSON and the run
  records are the record, and the allowlist is a command.
- **No image refs.** The plugin cropped a screenshot into refs at save time and
  matched them on the phone; a routine here takes its own screenshots.
- **One phone, one run.** A run holds the phone's lock; a second one says the
  phone is busy.

## Tests

Run `.github/test-cell.sh tools/nodecode-phone-autopilot` from the cells
repository. Nine tests, no network and no phone: every run goes through a
stubbed runner and the routines live in a scratch folder. They cover start and
stop, a save keeping its version and sha, the script the runner is handed, a
passed run and its record, a missed checkpoint with its screenshot, a refusal
to run bytes nobody approved, a routine edited on disk, list / show / runs /
delete, a runner that cannot start, a timeout, and the command.

## License

MIT. phone-autopilot is Copyright (c) 2026 Aman Jayesh: see `LICENSE` and `NOTICE`.
