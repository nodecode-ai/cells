# nodecode-plan-mode

Plan Mode: enforced plan-first work - the agent reads, writes a plan and changes nothing until you approve it. A Nodecode rewrite of [100yenadmin/hermes-plan-mode](https://github.com/100yenadmin/hermes-plan-mode) by [100yenadmin](https://github.com/100yenadmin).

## Get it

Tell the agent "add the plan mode cell", or run `nodecode add nc://plan-mode`.

## Use it

Start it, and ask for the change in the same breath:

```
/plan on add a --dry-run flag to the import command
```

From then on the agent reads, asks you a question or two, and writes a plan
as Markdown under `.nodecode/plans/` in the working directory. Its commands,
its edits and everything else that would change something are refused with a
message saying so; the plan file is the one write that gets through. When it
has shown you the plan it submits it, and you answer one card:

- **Approve, implement** - plan mode turns off, and the agent implements the
  plan on your next message.
- **Keep planning** - it revises the plan and submits a complete new
  revision.

If nothing answers the card (a headless lane, a turn that ended first), the
plan stands waiting and you approve it by name.

| Command | What it does |
| --- | --- |
| `/plan on <task>` | Turns the mode on and gives it the task. `/plan <task>` does the same. |
| `/plan status` | The phase, the plans directory and the plan files. |
| `/plan show [file]` | Prints the plan, up to 64 KiB. |
| `/plan edit [file]` | Names the plan's path for you to change and save. |
| `/plan approve [file]` | Takes the plan; the agent implements it on your next message. |
| `/plan reject <note>` | Keeps planning, with your note read at the next revision. |
| `/plan done` | Ends an approved plan once it is implemented. |
| `/plan off` | Leaves the mode without approving anything. |

The model has the same mode from its side, as the `plan_mode` tool: `on`,
`status`, `submit` (ask for the approval), `off` (only before it submits -
from then on the mode is yours to end). It finds the tool in its tool list;
the mode's own note rides every request while it is on, so it knows what to
do without being told twice.

A plan file is `<plans_dir>/<timestamp>-<slug>.md`. The plans directory is
`.nodecode/plans/` under the working directory unless the `plan-mode`
section's `plans_dir` names another one, and a write the form does not show
a path for is refused, so the mode fails closed.

## Configure it

Nothing is required. In `~/.nodecode/config.jsonc`:

```jsonc
{
  "plan-mode": {
    "plans_dir": "",          // empty: .nodecode/plans under the working directory
    "ask_seconds": 180,        // how long the approval card waits for you
    "allow_commits": false     // off: the agent plans no commit and makes none
  }
}
```

## What it does not do

It is a speed bump, not a jail: a call whose form hides its own path is
refused, but the mode cannot stop a form you approve by hand, and anything
run outside the tool seam - a cell's own worker, a channel - is not held.
Hermes's "approve and review each file edit" is not offered, because
Nodecode's edits are EVAL forms rather than a pair of write calls to
interrupt; the approval is one card or the typed command. The mode is per
session and lives in memory, so a restart of the organism forgets it (the
plan files stay).

## License

MIT. The original plugin is Copyright (c) 2026 100yenadmin: see `LICENSE`
and `NOTICE`.
