# nodecode-gateway-guardian

Gateway Guardian: the gateway's health in one line — OK, DEGRADED or BLOCKED, with the fault counts behind it. A Nodecode rewrite of [kawsthingz/kawsthingz-hermes-gateway-guardian](https://github.com/kawsthingz/kawsthingz-hermes-gateway-guardian) by [kawsthingz](https://github.com/kawsthingz).

A chat bot can go quiet without an error: two pollers on one bot token (the messenger answers 409), a gateway that froze, a lock left behind by a restart, a delivery that never landed. The original watched one agent runtime's gateway log for exactly those faults and answered one state with the exact cause. This cell reads the same faults out of the log files you name, counts them, and answers the same three states.

## Get it

Tell the agent "add the gateway-guardian cell", or run `nodecode add nc://gateway-guardian`.

## Set it up

Nothing is required. With no `gateway-guardian.logs` named, the newest `*.log` under this home's `logs/` and `Logs/nodecode/` is read. Point it at a file when your gateway writes somewhere else:

```jsonc
"gateway-guardian": {
  "logs": ["~/.nodecode/logs/gateway.log"],
  "tail_lines": 400,
  "tail_bytes": 262144,
  "watch_processes": true
}
```

A systemd install keeps its log in the journal rather than a file: name a file, or redirect the journal to one.

## Use it

- `gateway_guardian_status` — the model's tool: one state (`OK`, `DEGRADED`, `BLOCKED`), the cause, and the count of each fault pattern (409/polling conflict, freeze, stale lock, missed delivery) in the lines it read. It names the files it scanned and the gateway pids it found; it never quotes a log line and keeps no message body.
- `gateway_guardian_recover` — the approved checklist for `status`, `restart-gateway`, `clear-stale-lock` or `rebind-telegram`. It never restarts, kills or rebinds anything: it prints the steps the operator runs, and appends the chosen action to `gateway-guardian/recovery-intents.log` in this home.
- `/gateway-guardian` — the same health in one line; `/gateway-guardian recover restart-gateway` prints that checklist.

From eval: `(gateway-guardian:status)` and `(gateway-guardian:recover "clear-stale-lock")`, both strings.

The states are the original's thresholds: three or more 409/polling conflicts read `BLOCKED`; any conflict, or two warnings of one kind, read `DEGRADED`; with `watch_processes` on, a clean log and no running gateway reads `DEGRADED` too (`no gateway process found`).

Run `.github/test-cell.sh tools/nodecode-gateway-guardian` from the cells repository.
