# nodecode-sluice

Sluice: runs the agent's commands in a staged copy of a project folder and applies only the changes you approve. A Nodecode rewrite of [orjinameh/airlock](https://github.com/orjinameh/airlock) by [orjinameh](https://github.com/orjinameh).

In Hermes the plugin was a terminal backend: every command and file edit the
agent made ran in a copy of the workspace, and you reviewed the result hunk by
hunk before it landed. Nodecode's shell is its own, so here the stage is
reached through four tools and one command, and the copy is what you ask to
work in, not every shell call.

## Get it

Tell the agent "add the sluice cell", or run `nodecode add nc://sluice`.

## Set it up

One setting is required: `workspace`, the project folder to stage. In
`~/.nodecode/config.jsonc`:

```jsonc
{
  "sluice": {
    "workspace": "/work/project",
    "copy_git": true,
    "max_file_mb": 25,
    "max_workspace_mb": 1024,
    "allow_paths": ["/tmp"]
  }
}
```

Git must be on `PATH` only if you want `git status` and `git diff` to work
inside the stage; the cell itself needs nothing but the image. `copy_git`
copies `.git` into the stage so history commands work there and the real
repository is untouched. `allow_paths` names outside paths that are written
without the NOT CONTAINED label.

## Use it

- `sluice_run(command)` runs one shell command in the copy. The workspace's
  paths in the command are rewritten to the copy and back, so the answer
  reads like the real folder.
- `sluice_diff()` numbers every change: each block of edited lines, and each
  created, deleted, renamed, replaced or mode-changed file.
- `sluice_apply(selection)` lands what you name: `all`, `1,3-5`, or a changed
  file's path. Per file, all or nothing, written atomically. A file that
  moved in the real folder meanwhile is a conflict, reported and left alone.
- `sluice_discard()` throws the copy away; the real folder was never touched.

The operator reads the same review with `/sluice` (`status`, `diff [path]`,
`apply all`, `apply 1,3-5`, `discard`, `log`), and `(help :sluice)` has the
primer.

A command a copy cannot contain is held and labelled **NOT CONTAINED**: an
install, a `git push`, a network write, a write or delete outside the
workspace, `sudo`, `curl | sh`. `sluice_run` answers with the finding and runs
nothing until the call says `allow: true`.

This is not an operating-system sandbox: commands still run on your machine
as you. The copy contains file changes inside the workspace; what you let past
the gate really happens.
