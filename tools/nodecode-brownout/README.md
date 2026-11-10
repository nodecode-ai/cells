# nodecode-brownout

Brownout: notices when this machine loses mains power, measures battery runway, and parks tool calls until power is back. A Nodecode rewrite of [Osiyomeoh/brownout](https://github.com/Osiyomeoh/brownout) by [Osiyomeoh](https://github.com/Osiyomeoh).

Built for places where the grid is unreliable, and for anyone running a
laptop, a home server or a UPS as the machine the agent works on.

Brownout watches the host's power and, when mains power drops:

1. **Says so**, in a standing notice every attached shell shows and the
   agent reads in its next request, with the charge and the OS's own
   time-left estimate.
2. **Measures the real drain** from the percent drops it has seen, and says
   the runway a few minutes later — the number the cell acts on.
3. **Parks the work**: reply `/brownout park` and every tool call is held,
   or it parks itself at `critical_pct` (or `critical_minutes` of measured
   runway) when nobody answers.
4. **Picks up again** when power returns, or on `/brownout resume`. Nothing
   is killed, deleted or stopped mid-write; a park is only a hold.

## Get it

Tell the agent "add the brownout cell", or run:

```sh
nodecode add nc://brownout
```

## Set it up

Nothing is required. The defaults poll every 15 s and park themselves at 15%
or 10 minutes of runway.

```jsonc
{
  "brownout": {
    "poll_seconds": 15,       // how fast a power loss is noticed
    "critical_pct": 15,       // park itself at or below this charge
    "critical_minutes": 10,   // ...or below this measured runway
    "auto_resume": true,      // let tool calls through when power returns
    "ups": "",                // a NUT UPS name for upsc; empty reads the OS
    "allow_when_parked": []   // tools that still run while parked
  }
}
```

There is no key and no account: the reading comes from the host itself —
`pmset -g batt` on macOS, `/sys/class/power_supply` on Linux, or
`upsc <name>` for a UPS that `nut` already knows. A machine with no battery
(most desktops, containers, remote boxes) has no reading, and the cell says
so instead of guessing.

## Use it

| Command | What it does |
| --- | --- |
| `/brownout` | State, charge, measured runway, and the verbs |
| `/brownout park` | Hold every tool call until resume or power back |
| `/brownout keep` | Carry on: no automatic park during this battery stretch |
| `/brownout resume` | Let tool calls through again |
| `/brownout test` | Stand a test notice, to see the path end to end |
| `/brownout replay TRACE` | Run a recorded trace through the engine, nothing performed |

The model has the same verbs as one tool, `brownout` (`status`, `park`,
`keep`, `resume`, `test`), so it can check the host, or park itself before a
long job.

**Do this:** unplug the charger. **See this:** at the next poll the notice
"power lost on this machine. Battery …" stands, and the runway follows a few
minutes later. `/brownout park`, and the next tool call comes back refused;
plug in and the call runs again.

No hardware to unplug? Record a trace and replay it with no side effects:

```sh
# one line per reading: {"t": <seconds>, "on_ac": true|false, "pct": <0-100>}
/brownout replay traces/laptop-outage.jsonl
```

A trace is a plain JSONL of readings, so a trace recorded on one platform
replays on another; the replay performs nothing and posts no notice.
