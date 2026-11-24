# nodecode-dangerous-patterns

Custom Dangerous Patterns: your own regex rules over the shell commands an agent runs. A deny rule stops a matching call at once; a block rule asks you Allow once or Deny first. A Nodecode rewrite of [scross01/hermes-custom-dangerous-patterns-plugin](https://github.com/scross01/hermes-custom-dangerous-patterns-plugin) by [scross01](https://github.com/scross01).

## Get it

Tell the agent "add the dangerous-patterns cell", or run `nodecode add nc://dangerous-patterns`.

It ships no rules of its own: with an empty section it lets every call run.

## Set it up

The rules are two arrays in the `dangerous-patterns` section of `~/.nodecode/config.jsonc` — set in `/settings`, or by hand:

```jsonc
{
  "cells": ["nodecode-dangerous-patterns"],
  "dangerous-patterns": {
    "allow": ["\\brm\\s+-rf\\s+/tmp/scratch-"],
    "deny": [
      {"id": "deploy", "pattern": "\\bmake\\s+deploy\\b", "reason": "deploy"}
    ],
    "block": [
      {"id": "push", "pattern": "\\bgit\\s+push\\b", "reason": "push"}
    ]
  }
}
```

- `deny` stops a matching call before it runs. The model reads a refusal and
  hands you the command to run yourself. No prompt, and `--yolo` does not
  skip it.
- `block` raises the usual question card — **Allow once** or **Deny** — and
  the call runs only on Allow once. Nobody to ask (no page or app open),
  Deny, or no answer within `ask_seconds` refuses it.
- Each rule has an `id` (named in the refusal and the question), a `pattern`
  (a regular expression, case ignored), an optional `reason`, and an
  optional `read`:
  - `"commands"` (the default) reads only the commands the call runs, one
    per line, so `rg -n deploy src` passes a `\bmake\s+deploy\b` rule and
    `cd x && make deploy` does not.
  - `"call"` reads the whole call, its strings included — for a rule about
    a file being written, like `\bdelete-file\b`.
- `allow` exempts the text it matches; a match covered by it earns nothing.

Backslashes are doubled because the patterns are JSON strings. A rule whose
`pattern` is not a string refuses the section at start, and the cell stays
off until it is fixed; a rule with no `pattern` is skipped.

Patterns are compiled when the cell starts: after editing the section,
restart it — press space on it twice in `/cells`, or run `(restart-cells)`.

## Use it

The agent keeps working as it did; a call the rules stop comes back as

```
ERROR: refused by nodecode-dangerous-patterns: rule deploy (deploy) matched "make deploy"; ...
```

and the session records it as a failed tool call.

`/patterns` shows the rules in force and how many are deny and how many
block:

```
custom dangerous patterns: 1 deny, 1 block
deny (stopped at once, no prompt)
  deploy (deny) -- deploy [the commands a call runs] \bmake\s+deploy\b
block (asks Allow once or Deny)
  push (block) -- push [the commands a call runs] \bgit\s+push\b
```

`/patterns test <command>` says what one command would earn — ignored, asked
about, or stopped — which is the quick way to check a new pattern before
trusting it:

```
/patterns test make deploy
make deploy would be stopped at once by a deny rule: rule deploy (deploy) matched "make deploy"
```

It is a speed bump, not a security boundary: the agent's eval tool has your
machine's full authority, and a command written in a way no pattern sees
passes. `nodecode-guard`, which ships with Nodecode, is the built-in family
of rules; this cell is for yours.

## Testing

From this repository, `.github/test-cell.sh tools/nodecode-dangerous-patterns`
runs the slice offline against a pinned Nodecode tree: the lifecycle, both
actions, what each `read` sees, `allow`, and `/patterns`.
