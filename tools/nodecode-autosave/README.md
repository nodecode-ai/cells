# nodecode-autosave

Autosave: a pre-edit version of every file a tool changes. A Nodecode rewrite of [xiaoyaner0201/hermes-git-autosave](https://github.com/xiaoyaner0201/hermes-git-autosave) by [xiaoyaner0201](https://github.com/xiaoyaner0201).

## Get it

Tell the agent "add the autosave cell", or run `nodecode add nc://autosave`.

## Use it

Nothing to type: once the cell is on, every tool call that is about to change a file leaves a version of what the file held, and the call runs either way.

- In a git work tree, the whole work tree is saved as one snapshot commit on a local shadow ref, `refs/nodecode-autosave/<branch>`, with a reflog. Git stores only what changed since the last snapshot, and `HEAD`, the branches, the real index and the working tree never move; nothing is pushed. A work tree whose snapshot is unchanged since the last one costs two `git` calls and no new commit.
- Anywhere else, the file itself is copied, `keep` versions deep (20 by default), under the cell's data directory.

The model reads the same thing through Lisp:

```lisp
(autosave:versions "src/app.py")        ; ((:TIME ... :PLACE :GIT :ID "a1b2c3d4") ...) newest first
(autosave:restore "src/app.py")         ; version 1 back on disk
(autosave:restore "src/app.py" 3)       ; the third newest
(autosave:paths)                        ; every path and repository held, with how many
(autosave:prune)                        ; drop copies beyond `keep`
(autosave:status)                       ; what is held, where, and the settings
```

The operator types the same from the slash bar: `/autosave`, `/autosave list`, `/autosave find PATH`, `/autosave restore PATH [N]`, `/autosave prune`. `(help :autosave)` is the manual the model carries.

The hook reads a call before it runs: the paths a `(write-file ...)`, `(edit ...)`, `(delete-file ...)`, `(rename-file ...)` or `(copy-file ...)` form names, the string a `($ "name")` resolves to, and whether a `(sh ...)` or `run-program` command changes anything at all. A command it cannot read the targets from (`rm`, `mv`, a redirection, an in-place `sed`, a `git checkout`) still saves the work tree it runs in. Targets that are credentials by name (`.env`, `auth.json`, `.ssh`, `.aws`, `.kube`, `id_rsa*`, `*.pem`) and dependency directories are never copied.

Settings, under `"autosave"` in the config:

| Setting | Default | Meaning |
| --- | --- | --- |
| `keep` | `20` | versions kept per file |
| `max_file_mb` | `10` | biggest file copied outside git |
| `storage_dir` | unset | where copies and the ledger go; default `autosave/` under the home |
| `skip` | `[]` | extra path components never saved |
| `git` | `true` | keep a work tree's snapshots as local shadow refs |

It is a recovery net, not a backup service: a snapshot that hits its own error is logged in the cell's `ledger.txt` and the call still runs, a file bigger than `max_file_mb` is not copied, and a command built out of variables can hide its targets. Nothing here blocks, approves or rewrites a tool call, and nothing is sent anywhere: the cell makes no network request at all.
