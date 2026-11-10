# nodecode-time-machine

Time Machine: restore points for a folder, a rewind that previews, and the effects a rewind cannot undo. A Nodecode rewrite of [ddaikodaiko/hermes-time-machine](https://github.com/ddaikodaiko/hermes-time-machine) by [ddaikodaiko](https://github.com/ddaikodaiko).

A rewind puts files back. It cannot un-push a commit, un-publish a package or
un-apply a plan. This cell keeps both halves of that picture: checkpoints of the
folder, content-addressed under the home, and a ledger of the calls that left
the machine, each with the command that compensates it.

## Get it

Tell the agent "add the time-machine cell", or run `nodecode add nc://time-machine`.

## Use it

Nothing to type once it is on. Before the first call of a turn that changes
files — a write, an edit, a shell command carrying `rm`, `mv`, `git checkout`,
`pip install` — the cell takes a restore point of the protected folder, and the
tool result says where it is. A command that crossed a point of no return
(`git push`, `npm publish`, `docker push`, `kubectl apply`, `terraform apply`,
`gh release create`, `fly deploy`) is written into the effect ledger with its
compensation, and the result says so.

The tools the model calls:

- `checkpoint` — a restore point now, with a label; answers its id.
- `list_checkpoints` — the restore points of this folder, newest first.
- `rewind` — a preview by default: what it would revert, restore and remove,
  and the effects recorded since that a rewind will not touch. `confirm: true`
  performs it; `paths` narrows it to some files or folders.
- `fork_checkpoint` — the folder as of a checkpoint, copied into a sibling
  folder, so two approaches can be compared.
- `list_effects` — what left the machine since a checkpoint, with the command
  that compensates each one.

And the command:

```
/time-machine                  the folder, the newest checkpoint, what a rewind would leave standing
/time-machine list [N]         the restore points
/time-machine diff ID          what a rewind to ID would change
/time-machine rewind ID        the preview
/time-machine rewind ID --yes [PATH...]   apply it
/time-machine fork ID [NAME]   a sibling folder as of ID
/time-machine effects [ID]     the ledger of what already left
```

A checkpoint copies a file's bytes, never directories, modes or what a shell
wrote beyond the files themselves. What a rewind replaces or removes is copied
under the store's `trash/` first, so a rewind is undoable by hand.

## Set it up

The section is optional. `workspace` names the folder to protect; empty means
the folder the session runs in. Nothing here needs a key.

```jsonc
"time-machine": {
  "workspace": "",                 // empty: the folder the session runs in
  "auto_checkpoint": true,         // a restore point before a call that changes files
  "mutation_cooldown_seconds": 120,
  "retention": 20,                 // automatic checkpoints kept; manual ones are never pruned
  "max_file_mb": 50,
  "max_files": 5000,
  "exclude_git": true,             // leave .git out: faster, and a rewind keeps the history
  "annotate_results": true
}
```

The store lives under the organism's home: `time-machine/checkpoints/` for the
manifests, `time-machine/objects/` for the bytes, `time-machine/effects.sexp`
for the ledger, `time-machine/trash/` for what a rewind replaced. `retention`
prunes automatic checkpoints only; the objects nothing points at are dropped
with them.

## Gaps

- The original asks the human through the harness's own approval prompt before a
  call crosses the point of no return. Nodecode has no approval seam at the tool
  point, so the cell writes the effect into the ledger and puts the note and the
  compensation command in the tool result the model reads, in the same breath as
  the call. It never blocks a call.
- The original's `pre_llm_call` and `post_compaction` hooks remind the model of
  effects after a rollback or a compaction. Here the reminder rides the result
  of the call that made the effect, and `/time-machine status` states what a
  rewind to the newest checkpoint would leave standing.
- A rewind restores files, not directories, modes or symlinks, and a fork copies
  files only.
- `.git` is excluded by default, so a rewind leaves the history alone; the
  original includes it unless told otherwise.
