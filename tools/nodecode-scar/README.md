# nodecode-scar

Scar: the rm shape you denied on an approval card is stopped the next time it comes back. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-scar](https://github.com/TakeshiTGAL/hermes-plugin-scar) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

A gate that asks you before it lets a risky command run forgets your answer the
moment the call decides. Scar remembers one: when you say Deny on a command
that carries an rm shape, the shape — the token set `rm`, `recursive`, `force`,
never the path — is written to this home's `scar.json`, and a later call whose
own command carries every one of those tokens does not run. `rm -rf build/`
today and `rm -fr ./out` tomorrow are one shape.

The general gates read patterns you wrote beforehand. This one remembers a
decision you took.

## Get it

Tell the agent "add the scar cell", or run `nodecode add nc://scar`.

## Set it up

Nothing is required. With the cell on, a Deny you give the guard, a block
pattern of your own, or any other gate that asks through the organism's own
question seam leaves a scar. The `scar` section only tunes it — set it in
`/settings`, or by hand:

```jsonc
{
  "cells": ["nodecode-scar"],
  "scar": {
    "max_scars": 200,      // how many scars the ledger holds before a new one is refused
    "ask_seconds": 120,    // how long a scar_forget question waits for an answer
    "record": true,        // keep the shape of a command you deny
    "file": ""             // the ledger file; empty is scar.json in this home
  }
}
```

## Use it

The model has four tools, and you have `/scar`:

| | |
| --- | --- |
| `scar_check` | one shell command, without running it: would a scar stop it, and which |
| `scar_deny` | store a shape now, when you said in words that it must not run again |
| `scar_list` | every scar, its receipt and the redacted preview of the denied command |
| `scar_forget` | forget one, and only after you say so |
| `/scar` | the same from your own hand: `list`, `check <command>`, `deny <command>`, `forget <id>`, `help` |

A blocked call answers a person's decision in words, not a rule id: which scar,
when it was recorded, the token set, and that the shape came back — and that
the command is not to be paraphrased or run again.

### What matches

`rm -rf`, `rm -fr`, `rm -r -f`, `rm --recursive --force`, a flag after the
path, `/bin/rm`, a wrapper in front (`nice`, `nohup`, `timeout`, `xargs`,
`env` with its assignments, up to two other leading words such as `git rm`),
`find -exec rm -rf`, and the same rm inside a shell `-c` body, eight shells
deep. A scar that kept `force` is not matched by `rm -r`.

### What does not

Only a command is read: `echo rm -rf` is not one, nor is a list of file names,
nor `python -c`, `node -e`, a script file, command substitution or a `--`
before `-c`. A call whose command is hidden behind a variable runs, because it
could not be read.

### The ledger

One JSON line per scar in this home's `scar.json`, capped at `max_scars`; a
full ledger refuses a new scar rather than dropping an old decision. A ledger
that cannot be read blocks an rm command and nothing else, so a person can move
the file aside to start again. The preview keeps the command's first line with
secret-shaped words masked (an assignment whose name says secret, a known token
prefix, a long hex run, a URL's password). Nothing leaves this machine, and no
model is called. A scar is a speed bump, not a boundary: `eval` has this
machine's full authority.

## Credit

A rewrite of [hermes-plugin-scar](https://github.com/TakeshiTGAL/hermes-plugin-scar)
by [TakeshiTGAL](https://github.com/TakeshiTGAL), MIT. See NOTICE for what was
rewritten and what was left out.
