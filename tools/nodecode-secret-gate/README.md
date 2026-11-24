# nodecode-secret-gate

Secret Gate: scans text for leaked credentials before it is shared and gates the outbound share, fail-closed. A Nodecode rewrite of [MSAbhishek22/hermes-secret-gate](https://github.com/MSAbhishek22/hermes-secret-gate) by [MSAbhishek22](https://github.com/MSAbhishek22).

## Get it

Tell the agent "add the secret-gate cell", or run `nodecode add nc://secret-gate`.

## Set it up

Nothing has to be set: the rules are the original's, every tool result is audited, and the receipt ring holds a hundred. The section is there if you want it:

- `secret-gate.audit_tool_results` — scan every tool result and warn on a suspicious one (default `true`).
- `secret-gate.max_receipts` — how many receipts the ring holds, newest kept (default `100`).
- `secret-gate.min_entropy` — bits per character a long token needs before it is called suspicious: `"3.0"`, `"4.0"` (the default) or `"5.0"`.

## Use it

- `secret_scan` — `CLEAR`, `REVIEW` or `LEAKED` for text the agent is about to hand to a person, a file or a service, with the rule that fired, the line, and a mask of the token's public prefix and length. The secret itself is never returned.
- `secret_gate_check` — `ALLOW`, `WARN` or `BLOCK` plus a receipt id: the last check before an outbound share. A blocked receipt can never be approved; the only fix is to remove the secret and re-scan.
- `secret_gate_receipts` — the recent decisions, masked, as an audit trail.
- `/secret-gate` — how many receipts are held and what is audited; `/secret-gate TEXT` — that text as the gate sees it.
- `(help :secret-gate)` — the manual, for the model.

**LEAKED** is a known provider format: an AWS access key id, a GitHub, Slack or OpenAI-style token, a private-key block, a JWT, a key or password assignment. **REVIEW** is a suspicious shape and not a conclusion: a long high-entropy string, or a path to a credential file. **CLEAR** is nothing matched. The scanner never raises — an internal error is one REVIEW finding, so a broken scan cannot silently pass.

An audit on every tool result runs at the `:tool` point: a result that looks like it carries a secret lands as a receipt and a warning, and the result itself is handed back unchanged.

Everything is local: the rules are string matching and entropy arithmetic in this folder. No network, no service, no model, no file read, no subprocess.

## Tests

```sh
.github/test-cell.sh tools/nodecode-secret-gate
```

Offline, in a scratch home: lifecycle, the rules, the gate, the audit on tool results, the bounded receipt ring and the command.
