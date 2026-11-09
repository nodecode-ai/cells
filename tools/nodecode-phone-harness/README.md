# nodecode-phone-harness

Phone Harness: drives a real phone from one scripted tool - an iPhone through the Mac's iPhone Mirroring window, an Android over adb, or a Phone Harness Cloud phone - with taps, typing, screen reads and screenshots. A Nodecode rewrite of [phone-harness/phone-harness](https://github.com/phone-harness/phone-harness) by [phone-harness](https://github.com/phone-harness).

## Get it

Tell the agent "add the phone-harness cell", or run `nodecode add nc://phone-harness`.

## Set it up

The cell runs the `phone-harness` Python package you already have; it bundles
nothing and installs nothing.

```sh
uv tool install phone-harness     # or: pipx install phone-harness, pip install phone-harness
```

That puts `phone-harness` on your `PATH`, which is what the cell runs by
default. If you would rather not install it, or it lives elsewhere, say which
command runs it in the `phone-harness` section of `~/.nodecode/config.jsonc`
(or in `/settings`):

```jsonc
{
  "phone-harness": {
    "command": "uvx phone-harness",   // or "phone-harness", or "python3 -m phone_harness.run"
    "platform": "auto",               // auto, ios or android
    "ask": false,                     // question before each script; needs a page that answers
    "timeout_seconds": 300           // the default bound for one script, 5 to 1800
  }
}
```

Then `/phone check` runs the runner's `--version` and says whether it starts.

- **iPhone**: macOS 15+ with iPhone Mirroring paired, and Accessibility plus
  Screen Recording for the app that runs Nodecode.
- **Android**: `adb` on `PATH`, the device authorized.
- **Cloud phone**: the package's own sign-in, once - `phone-harness cloud login`.

`/phone` answers which phone the cell will drive, where its workspace is, and
whether the runner starts.

## Use it

Ask for something on your phone. The model calls one tool:

```
phone_exec(code='''
ensure_device()
open_app("Weather")
wait_stable()
print([o["text"] for o in screen()][:12])
''')
```

The script runs as Python with the phone-harness helpers pre-imported, and the
result is JSON: `success`, `exit_code`, `output` (what it printed, up to 50,000
characters), `stderr` (its end, up to 4,000), `screenshots`, and `workspace`.
A script that runs past its bound is killed with everything it started. Each
call is a fresh interpreter, so variables do not carry over; the phone keeps
its state.

`screenshot()` prints the PNG path it saved. The cell reports the last absolute
image path in the output that exists and was written during the call, and
`(look PATH)` shows that image to the model and to you. A relative path, or a
file that was already there, is not reported.

`/phone check` is the setup question: it runs the runner's `--version` and
tells you how to install it if it does not start.

`(help :phone)` is the manual: the helpers, the settings, and what the result
holds. `phone-harness skill` prints the packaged skill in full - which phone to
drive, the working method, every helper with its arguments.

### Before a script runs

`ask` is off by default. With it on, every script is a question - "Allow once",
"Allow for this session", or "Deny" - answered from a page that answers
questions (the web page or the desktop app). An answer of "Allow for this
session" covers the rest of that session; with nobody attached to answer, the
script does not run, and the result says so. Remember that a script is full
Python: it can read the whole machine, not only the phone. The child gets no
`*_API_KEY`, `*_TOKEN` or `*_SECRET` variable, and `PH_CLIENT=nodecode` says
which harness asked.

Turn `ask` off when the phone is unattended, or when you run from a terminal
alone.

## Differences from the plugin

- **The runner is yours, not installed by the agent.** Hermes installed
  `phone-harness==0.3.3` into its own environment and ran that copy; a cell has
  no Python of its own, so it runs the copy you have on `PATH` (or whatever
  `command` names).
- **The question is a setting.** The plugin asked on every call and offered
  "session" and "always" answers; here `ask` is off unless you turn it on, and
  the answers come from a page that answers questions.
- **A screenshot is named, not attached.** The result carries the path and the
  image comes through `(look PATH)`, which Nodecode's own vision path draws for
  the model and for you.
- **The skill is a manual topic.** `(help :phone)` carries the helper list the
  plugin registered as an agent skill, and `phone-harness skill` prints the
  packaged skill whole.
- **No classifier auto-run.** The plugin could skip the question for a script a
  heuristic judged phone-only; the check was bypassable, and here `ask` on
  means asked.
- **The workspace is the cell's own.** Scripts run in
  `~/.nodecode/phone-harness/agent-workspace`, passed as `PH_AGENT_WORKSPACE`;
  the package seeds its helpers there as it always did.

## Tests

Run `.github/test-cell.sh tools/nodecode-phone-harness` from the cells
repository. Twelve tests, no network: every run goes through a stub runner, and
they cover install and stop, a script run through the runner, a failed script's
stderr tail, the call bound and the scrubbed child, the image named only when
this call wrote it, a timeout and a missing runner, the tool resting on its
declaration, the question refused when nobody answered, and `/phone`'s state.

## License

MIT. phone-harness is Copyright (c) 2026 shawn pana: see `LICENSE` and
`NOTICE`.
