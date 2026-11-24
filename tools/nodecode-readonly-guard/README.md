# nodecode-readonly-guard

Read-Only Guard: per-session read-only, plan and writable modes, enforced in code, with a writable lease. A Nodecode rewrite of [mciocchi/hermes-readonly-guard](https://github.com/mciocchi/hermes-readonly-guard) by [mciocchi](https://github.com/mciocchi).

## Get it

Tell the agent "add the readonly-guard cell", or run `nodecode add nc://readonly-guard`.

## Use it

Nothing is required. A session starts read-only, and `/readonly` is the switch:

```
/readonly status              # the mode in force here, where it came from, the lease
/readonly writable            # this session may write again
/readonly readonly            # and back
/readonly plan                # read, and write the plan markdown, nothing else
/readonly toggle              # read-only <-> writable
/readonly on | off            # the same two, for the older muscle memory
```

| Mode | Runs | Refused |
| --- | --- | --- |
| `readonly` | inspection calls -- `(help ...)`, `(index)`, `(recall-entry ...)`, `(list-evals)`, the form's own readers -- and one read-only command through `(sh "...")` | every write, definition, assignment, other tool, and every command that is not a single read-only program |
| `plan` | the same, plus a write of the plan markdown under `.nodecode/plans/` | everything else |
| `writable` | everything | nothing |

A refusal reaches the model as the call's failure, in words that say the mode and
the way on, and the call does not run. The agent has no tool that can turn its own
guard off: the switch is the command, which is the operator's.

While a session is read-only or in plan mode, a note rides every request saying
so, so the model does not have to be reminded.

### The switch is one session's

Modes are per session, in one small table. Loosening one chat leaves another
exactly where it was -- the failure a machine-wide switch guarantees eventually.

### Writable is a lease

A writable session goes back to read-only once you have been silent for thirty
minutes, so the dangerous mode is the one you have to keep choosing. The clock
starts at the beginning of each of your turns, so a turn that is already running
always finishes; once the limit has passed the grant is gone and `/readonly
writable` re-arms it. `0` keeps a writable session writable.

## Set it up

No key, no service. In `~/.nodecode/config.jsonc`, all three keys are optional:

```jsonc
{
  "readonly-guard": {
    "default_mode": "readonly",         // readonly | plan | writable
    "writable_expiry_minutes": 30,      // 0 keeps writable on until you change it
    "plans_dir": ""                     // empty: .nodecode/plans in the working directory
  }
}
```

An unrecognised `default_mode` reads as read-only, so a typo can only tighten the
guard.

## What a probe is

In read-only and plan mode, `(sh "...")` runs only when the command is one
program from a read-only list, with read-only flags, and the string carries no
composition (`;`, `&&`, `|`, `>`, `<`), no newline and no shell expansion
(`$(...)`, `${...}`, `$NAME`, a backtick). The list is `ls`, `cat`, `head`,
`tail`, `wc`, `file`, `stat`, `pwd`, `tree`, `du`, `df`, `ps`, `uname`,
`whoami`, `id`, `which`, `realpath`, `readlink`, `cut`, `tr`, `sort`, `nl`,
`column`, `diff`, `grep`, `rg`, `jq`, `find` (with read-only primaries only)
and `git` (`log`, `status`, `diff`, `show`, `blame`, `rev-parse`, `ls-files`,
and the listing forms of `branch`, `tag`, `stash`, `remote`, `worktree`, `reflog`).
A writing flag (`sort -o`, `tree -o`, `file -C`, `tail -f`, `rg --pre`,
`git --output`), a value flag that runs a program, and `git -c` config
injection are refused, as is an allowlisted program this machine does not have.

## What it does not do

- **It is a speed bump, not a sandbox.** Nodecode's `eval` has this machine's
authority and the guard only vetoes the calls that reach it: a background process,
a worker a cell started, or a channel is not held.
- **It refuses by reading a form, so a form it cannot read is refused.** Anything
built at run time from a variable fails closed -- which is the intent, but a
useful call sometimes is refused too.
- **A probe can read anything you can read**, credential files included. The
mode is about writes and execution, not confidentiality.
- **The mode lives in this image.** A restart forgets it and sessions start at
`default_mode` again; Hermes's state file, dashboard panel, desktop chip and
keybinds are not part of this rewrite (see `NOTICE`).

## Testing

From this repository, offline against a pinned Nodecode tree:

```sh
.github/test-cell.sh tools/nodecode-readonly-guard
```

The tests cover the three modes, a form's readers against its writers, the probe
list and its refusals, the plan file as the one write that gets through, the lease
lapsing, and a session's mode not reaching its neighbour.

## License

MIT. The original plugin is Copyright (c) 2026 Matt Ciocchi: see `LICENSE` and
`NOTICE`.
