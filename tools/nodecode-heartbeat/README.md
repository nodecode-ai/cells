# nodecode-heartbeat

HeartBeat 2: wakes and watchers. A timed check-in, or a file, url or command watched out of band, that wakes the session that armed it. A Nodecode rewrite of [electricsheephq/heartbeat2](https://github.com/electricsheephq/heartbeat2) by [electricsheephq](https://github.com/electricsheephq).

## Get it

Tell the agent "add the heartbeat cell", or run `nodecode add nc://heartbeat`.

## Use it

Ask for what you want watched, and the agent arms it with its `pulse` tool. The operator can do the same from the command line in a session: `/heartbeat` lists what stands, `/heartbeat pause|resume|show|check|remove ID` acts on one.

- **A clock watch** (`kind: "every"`, or `"after"` for one) is a wake: a prompt into the session that armed it, on that session's own model, with the note the operator wanted said. Nothing wakes a chat nobody is in: the prompt waits in that session.
- **A probe watch** (`file`, `url`, `command`) is checked out of band and costs no model turn until it fires. `fire_on` says what is worth a wake: `failure` (a break, said once until it recovers), `change` (any new value) or `success` (a recovery). Every wake carries the receipt: what was checked, what it answered, when.
- **There is no destination.** A watch wakes the session that armed it and nothing else.
- **It survives a restart.** The registry is a JSON file in the home, `heartbeat/watches.json`.

Verbs, callable from eval, each answering a string:

```lisp
(heartbeat:add :title "nightly build" :kind "command"
               :command '("gh" "run" "list" "--limit" "1") :every "10m"
               :fire-on "failure" :note "say what broke and where")
(heartbeat:watches)            ; one line each
(heartbeat:show "nightly-build")
(heartbeat:pause "nightly-build") (heartbeat:resume "nightly-build")
(heartbeat:forget "nightly-build") (heartbeat:check "nightly-build")
```

A command watch runs an argv vector, never a line of shell: one more argument to a program already on the machine. It runs as you, so it may use whatever that program already reads: a `git` or `gh` credential, a local service, a system tool. Keep to read-only commands.

## Set it up

Nothing is required. The `heartbeat` section of the config has three optional bounds:

| Setting | Default | What it does |
| --- | --- | --- |
| `tick_seconds` | 15 | how often the ticker looks at the watches |
| `max_watches` | 25 | how many may stand at once; the agent cannot go past it |
| `check_timeout_seconds` | 10 | one url check's ceiling |

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the `nodecode-heartbeat` (nick `heartbeat`) package and the one condition |
| `watch.lisp` | what a watch is, the registry, duration parsing, and the JSON file it survives in |
| `run.lisp` | the three probes, the fire rule, the wake, and the ticker's one pass |
| `cell.lisp` | the manual, the `pulse` contract and tool, the verbs, `/heartbeat`, `START-CELL` |
| `test/` | the offline slice: lifecycle, probes, the clock, the boundary |

## Credit

A rewrite of [electricsheephq/heartbeat2](https://github.com/electricsheephq/heartbeat2) at commit `8a1fc6efe9ad340941d92d7bf39583e8dc2dc8bc`, Apache-2.0. See [NOTICE](NOTICE).
