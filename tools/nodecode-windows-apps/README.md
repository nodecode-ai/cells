# nodecode-windows-apps

Windows Apps: what is installed on this Windows machine, from the four Uninstall registry keys behind Apps & Features. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

Ask "is Python installed, and which version?", "what is taking the disk?" or
"what would uninstall this?" and the model picks the tool.

| Tool | What it does |
| --- | --- |
| `win_app_table` | Every program the four Uninstall keys carry, with version, publisher, size in MB, install date and which view it lives in. A query narrows by name or publisher; `view` keeps one of `machine-64`, `machine-32`, `user-64`, `user-32`; `include_system_components` shows what the publisher hid from Settings. |
| `win_app_detail` | One program in full: every field its publisher wrote, its install location and whether that folder is still on disk, help and about links, comments, and a reading of its uninstall command — an MSI product code, the publisher's own setup program, an appx removal or a script. |

The same two are callable from eval:

```lisp
(windows-apps:win-app-table :query "python")
(windows-apps:win-app-detail "OBS Studio")
```

`/windows-apps python` prints the inventory from the shell.

## Get it

Tell the agent "add the windows-apps cell", or run `nodecode add nc://windows-apps`.

## Set it up

Nothing, on Windows: the cell asks `reg.exe`, the registry tool Windows ships.
Its section has two settings, and both have defaults:

```jsonc
"windows-apps": {
  "reg": ["reg"],
  "timeout_seconds": 30
}
```

`reg` is the argv prefix of the registry tool, when it is not the one on PATH;
`timeout_seconds` bounds one query, and its process tree is killed past it.

On a machine without the registry tool — Linux, macOS — the tools still load
and answer with what `reg` said, because the keys they read exist only on
Windows.

## Use it

Four keys are read, in this order: the machine's 64-bit and 32-bit Uninstall
views and the user's. That is a superset of what Settings shows, because it
includes the per-user installs the machine page never lists and the entries
publishers asked to hide.

- The same name registered in two views is one row with both views named: the
  32/64-bit dual install Apps & Features itself shows once.
- `SystemComponent` = 1 means the publisher hid the entry from Apps & Features
  (runtimes, update rollups). Those are filtered out unless asked for, and the
  notes say how many were.
- `EstimatedSize` is a DWORD in kilobytes; the table says megabytes.
- `InstallDate` is written as `YYYYMMDD` or a slashed date; both read as
  `YYYY-MM-DD`.

Read-only, and only the four keys: nothing is uninstalled, modified or run.
The uninstall command is classified and quoted, never executed. The original
plugin chose the registry over WMI for a reason that still holds —
`Win32_Product` sees only MSI packages and reconfigures each one while
enumerating — so no WMI query happens here either.

Run `.github/test-cell.sh tools/nodecode-windows-apps` from the cells
repository. Every test stubs the registry tool: no Windows, no registry and no
`reg.exe` is needed to prove what the cell does with what it prints.
