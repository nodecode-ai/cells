# nodecode-chronos

Chronos Time Travel: snapshots the workspace before each file edit or shell call, so a bad change can be diffed and rolled back. A Nodecode rewrite of [DevTarlow/hermes-chronos](https://github.com/DevTarlow/hermes-chronos) by [DevTarlow](https://github.com/DevTarlow).

A write, a refactor or a shell command that goes wrong leaves the workspace in a state nobody asked for. This cell records a checkpoint of the whole workspace before every tool call that can change it, keeps the checkpoints in a private git repository inside the workspace, and puts the files back when asked. The project's own git history never moves, and nothing leaves the machine.

## Get it

Tell the agent "add the chronos cell", or run:

```sh
nodecode add nc://chronos
```

## Set it up

`git` on `PATH` is all it needs, and every setting has a default. All of them are optional, in `~/.nodecode/config.jsonc`:

```jsonc
{
  "chronos": {
    "workspace": "/work/project",   // absolute; this process's own directory when unset
    "auto_snapshot": true,         // a checkpoint before every file-editing or shell call
    "max_checkpoints": 50,         // how many /chronos lists
    "clean_untracked": true,        // a rollback deletes files created since the checkpoint
    "ignored": ["node_modules"],    // extra gitignore patterns no checkpoint covers
    "max_diff_chars": 400000,       // how much diff text one answer carries
    "timeout_seconds": 30           // one git run's ceiling; its process tree is killed past it
  }
}
```

## Use it

Nothing to type: before a `(write-file ...)`, an `(edit ...)`, a `(sh ...)` or an `eval`, a checkpoint is taken unless the workspace is unchanged since the last one.

The model has four tools:

- `chronos_checkpoint(description)` — take one before a risky step.
- `chronos_list_checkpoints(limit)` — id, time, the tool that took it, its description.
- `chronos_diff(checkpoint_id | from_checkpoint | to_checkpoint, max_chars)` — what one checkpoint changed, what changed between two, or what changed since one up to the files on disk (`to_checkpoint: "WORKTREE"`, the default). The answer carries the diff, its stat, and the untracked files a worktree diff does not cover.
- `chronos_rollback(checkpoint_id, clean)` — put the files back the way that checkpoint left them. A safety checkpoint of the state now is taken first, so the rollback can itself be rolled back. `clean` (the default) deletes files created since the checkpoint; `clean: false` writes them back and leaves them untracked.

The operator types the same:

- `/chronos` — the checkpoints, newest first.
- `/chronos diff ID` — what that one changed.
- `/chronos rollback ID` or `/chronos rollback latest` — restore it; `--no-clean` keeps the files created since.
- `/chronos help`, and `(help :chronos)` is the manual the model carries.

Every answer is JSON with `status` (`ok` or `error`), so a failed call is a result the model reads, not a condition it has to catch.

## What it costs, and what it does not cover

A checkpoint is `git add -A` plus a commit in the shadow repository: the second one of a quiet workspace costs nothing, because no commit is written when nothing changed. The repository lives at `<workspace>/.nodecode-chronos/`, holds every non-ignored file -- untracked ones such as `.env` included, which is why it stays inside the workspace -- and carries a `.gitignore` of `*` so the operator's own `git status` never shows it. `.git`, `.nodecode-chronos`, `node_modules`, `.venv`, `venv` and `__pycache__` are never covered, nor is anything the `ignored` setting names.

Not covered: ignored files, empty directories, file modes and symlink targets beyond what git stores; and a shell command that leaves the workspace half-written is only put back by a rollback, never mid-command. A restore runs without a prompt — that is the point of a one-verb undo — and it is safe to run twice, since the second one finds nothing to restore.

## Test it

```sh
.github/test-cell.sh tools/nodecode-chronos
```

The tests stub the one git seam, so they need no repository, no git install and no network.
