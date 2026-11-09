# nodecode-undo

Undo Effects: snapshot what an agent is about to delete or overwrite, so it can be put back. A Nodecode rewrite of [Moodtuner997/hermes-undo-effects](https://github.com/Moodtuner997/hermes-undo-effects) by [Moodtuner997](https://github.com/Moodtuner997).

A `write-file`, an `edit`, or a simple `sh "rm ..."` in Nodecode changes files without a copy of what was there. This cell keeps that copy: every such call snapshots its paths into a store under the home before it runs, reads them again after, and drops the snapshot when the call changed nothing. `/undo` lists those calls and puts the files back.

## Get it

Tell the agent "add the undo cell", or run `nodecode add nc://undo`.

## Use it

- `/undo` — every snapshot, newest first, with its id and the paths it touched.
- `/undo show ID` — one entry: each path, what it was, what the call left.
- `/undo restore ID` or `/undo restore last` — put the paths back. A path changed since the call by anything else is a conflict and is left alone; `--force` puts it back anyway, `--dry-run` only says what it would do. A restore snapshots what it overwrites first, so it can itself be undone.
- `/undo prune` — drop entries past the retention and the bytes nothing points at.

Covered: the bytes of a file. A path that does not exist yet is recorded as absent, so a restore removes the file the call created. Not covered: directories, file modes, and anything a shell command writes other than the simple `rm` and `mv` that name their targets — `/undo show` says which, rather than promising more.

Files that look like credentials (`.env`, `*.pem`, `id_*`, `auth.json` and the like) are not copied into the store.

## Settings

In the `undo` section of `~/.nodecode/config.jsonc` (see `config.example.jsonc`):

| Key | What it is |
| --- | --- |
| `max_file_mb` | largest file a snapshot keeps (default 20); a bigger one is listed as not covered |
| `retention_days` | how long an entry is kept (default 14) |
| `keep_last` | how many entries survive whatever their age (default 200) |

The store lives under `<home>/undo-effects/`, one file a snapshot entry and one content-addressed copy of the bytes.

## Credit

The idea, the entry/restore model, the "restore only what still looks the way the call left it" guard, the protecting of credential-looking files and the content-addressed store are Moodtuner997's, from the Hermes Agent plugin this is a rewrite of. See [NOTICE](NOTICE).
