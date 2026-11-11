# nodecode-safe-swap

Safe Swap: destructive shell commands become reversible twins — `rm -r` moves into a quarantine, `git push --force` becomes a lease, a hard reset snapshots first — with a receipt for each and one command to undo it. A Nodecode rewrite of [itsolaph/hermes-safe-swap](https://github.com/itsolaph/hermes-safe-swap) by [itsolaph](https://github.com/itsolaph).

## Get it

Tell the agent "add the safe-swap cell", or run `nodecode add nc://safe-swap`.

## Set it up

Nothing is required: the cell starts on the day it is installed, with every rule on and its quarantine in `~/.safe-swap/trash`. Its section in `~/.nodecode/config.jsonc` turns a rule off, moves the quarantine, or names branches where a force push is left alone (`config.example.jsonc` shows the shape). `restart-cells` after an edit.

## Use it

Every command an eval snippet runs through `(sh "...")` is seen by one hook on `sh`, and a destructive one is rewritten before it runs:

| Command | Runs as | Undo |
| --- | --- | --- |
| `rm -r build`, `rm *.log` | a move into `<quarantine>/<receipt>/<n>`, with the file and byte count in the note | `/safe-swap undo <id>` puts it back |
| `git push --force`, `-f`, `+ref` | `--force-with-lease --force-if-includes` | not undoable: the lease only refuses an unsafe push |
| `git reset --hard`, `git checkout -- <paths>`, `git restore`, `git clean -f`, `git stash drop` | a snapshot first: a stash commit under `refs/safe-swap/<id>`, the HEAD, or a tar of what `clean` may delete | `/safe-swap undo <id>` re-applies it |
| `git branch -D <name>` | the tip is recorded first | `/safe-swap undo <id>` recreates the branch |

A command that uses `sudo`, `env`, `xargs`, `eval`, `sh -c`, `$( )`, backticks, a subshell, a redirection, an unreadable option, or a protected target (`/`, `~`, a system directory, the quarantine itself) runs exactly as written, and the model is told so, with the reason. Nothing is ever blocked: a hook that fails calls `sh` as the model wrote it.

What the model can call through eval:

```lisp
(safe-swap:plan "rm -rf build")   ; what it would run as, and nothing executed
(safe-swap:receipts)             ; the receipts this quarantine holds, newest first
(safe-swap:undo "4dc78a")        ; put back what that receipt took; "last" for the newest
(safe-swap:purge "4dc78a")       ; delete that receipt's quarantine directory (nothing else is ever deleted)
(safe-swap:status)               ; the quarantine, the rules that are on, how many receipts there are
```

The operator has `/safe-swap list`, `/safe-swap undo <id|last>`, `/safe-swap purge [id]` and `/safe-swap status`, and `(help :safe-swap)` carries the same text the model reads.

A receipt is a directory of plain files (`manifest.tsv`, `stash.sha`, `branches.tsv`, `clean.tar`, `stash.shas`, `original`, `kind`, `stats`), so an undo can be finished by hand, without this cell.

The one thing to know: a swapped `rm` no longer looks like a recursive delete to a guard that reads the command — `nodecode-guard` will not stop it, and it is reversible from the quarantine instead.
