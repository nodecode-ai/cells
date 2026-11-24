# nodecode-edit-ledger

Edit Ledger: keeps a local ledger of the turns that changed files, and answers `/edit-log` with which models change files, how many per turn and of which types. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

Every turn the agent changes a file in, the cell appends one line to a
ledger of its own: when the turn ended, which model ran it, how many files
it changed and of which types (`.lisp`, `.md`, `.tsx`). Never a path, never
a file name, never a word the agent wrote. `/edit-log` reads that ledger
back:

```
/edit-log             last 24 hours, by model
/edit-log session     the most recent session
/edit-log all         everything in the ledger
/edit-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
/edit-log types       add to any of the above to list file types (or days)
/edit-log recent      add to any of the above to list the newest 10 turns
/edit-log recent 25   the newest 25 instead (1 to 100)
/edit-log clear       delete the ledger
```

`/edit-log all` answers the table the original prints:

```
Turns that changed files, all recorded:

model      turns  files  median  top type    last
one-model      2      4       2       .py   3s ago

Counts files changed by the agent's write-file and edit verbs, not by shell commands.
```

- **turns** counts turns that changed at least one file, **files** the files they changed.
- **median** is the middle number of files per turn.
- **top type** is the file type changed most often, **last** how long ago the newest turn was. The model with the most turns comes first.
- `types` lists file types instead (`type`, `files`, `turns`, `last`), `days` one calendar day per line, newest first.
- `recent` lists the newest turns, newest first, with the three most changed types of each.

A file type is the file's extension in lower case; `(none)` is a file
without one (`Makefile`, `.env`) and `(other)` an extension that is not a
short plain one. A time window counts back from now; `session` and `all`
take precedence over one. The ledger keeps its newest 5,000 rows, so a long
window on a busy install reaches back only as far as those rows do.

## Get it

Tell the agent "add the edit-ledger cell", or run:

```sh
nodecode add nc://edit-ledger
```

## Set it up

Nothing has to be set: the cell records by itself, keeps its ledger in its
own folder under the home, and needs no key and no network. The section is
there when you want a different ledger or a longer default window:

```jsonc
{
  "edit-ledger": {
    "file": "",            // another ledger file; empty keeps the cell's own
    "keep_rows": 5000,     // rows kept when the ledger is trimmed
    "max_bytes": 2000000,  // the size at which it is trimmed
    "default_hours": 24    // the window /edit-log shows with no argument
  }
}
```

## Use it

Ask after a session that touched files, or once a week: `/edit-log 7d types`
says what kind of work the week was, `/edit-log all` which model does the
editing. `(help :edit-log)` carries the same manual into the conversation.

## What it counts

The files a tool call changed on disk — the workspace diff its result
carries — and, where a call left no diff, the path named by the `write-file`
or `edit` receipt it stamped. A file changed by a shell command is counted
when the workspace diff saw it and not otherwise, and a file written twice
in one turn counts once.

## Tests

```sh
.github/test-cell.sh tools/nodecode-edit-ledger
```

Offline: a turn is a `:FRAME` op carrying the fact the core would publish,
so the row a turn leaves, the deduplication, the trimming and every
`/edit-log` answer are asserted without a session or a provider.
