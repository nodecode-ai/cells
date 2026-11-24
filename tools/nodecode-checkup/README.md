# nodecode-checkup

Checkup: what the built-in `/doctor` leaves out — a config section still half
filled, a cell folder that refused at start, what the organism's parts are
saying is wrong right now — each finding with its fix. A Nodecode rewrite of
[iap/hermes-guide](https://github.com/iap/hermes-guide) by [iap](https://github.com/iap).

`/doctor` is the first thing to run on this organism and this cell never
replaces it: the model pin, the store, the disk, the build on disk, every
declared section, the default provider's key and a release waiting are the
doctor's. The checkup is the other half of the same question — which
declared section is *unanswered* and what member it wants, which folder
loaded and then refused to start and in what words, and which part is
standing a notice up right now. Read-only: it reads state another owner
already holds and writes nothing, and it never restarts anything.

## Get it

Tell the agent "add the checkup cell", or run `nodecode add nc://checkup`.

## Set it up

Nothing to set up. The section has two optional members, both with
sensible defaults:

```jsonc
"checkup": {
  "scopes": ["sections", "cells", "notices"],
  "max_findings": 20
}
```

`scopes` is what a bare `/checkup` runs; `max_findings` is how long the
report may get before it says how many findings it left out. A wrong value
refuses at start and the folder says so on `/cells` and in the notices
block.

## Use it

- `/checkup` in a shell prints the report and opens a panel with one row
  per check; `/checkup cells` runs one check.
- `(checkup:scan)` and `(checkup:scan "notices")` answer the same text
  through eval.
- `run_checkup` is the tool the model calls, with `scope` (`all`,
  `sections`, `cells`, `notices`) and `all_findings`.

```
checkup: 2 findings across sections, cells, notices
  [warn] sections · channels.telegram — bot_token_env is required
        -> fill channels.telegram in /settings, then (restart-cells)
  [fix] cells · nodecode-acme — refused at start: profile is required
        -> fix that section of config.jsonc, then (restart-cells)
```

With nothing to report it says so in one line and points at `/doctor` for
the half it does not read.

The section is `checkup`; the manual topic is `(help :checkup)`.

## What the rewrite leaves out

The original's 31 troubleshooting playbooks (a skills tap) and its
Hermes-only scopes — versions, MCP, hooks, memories, storage, update — are
not here: Nodecode's `/doctor` already reads its own equivalents, and a
playbook that names Hermes's paths would be prose about the wrong program.
This cell is the one portable half: the checks the harness's own doctor
does not run.

Run `.github/test-cell.sh tools/nodecode-checkup` from the cells repository.
