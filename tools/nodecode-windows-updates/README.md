# nodecode-windows-updates

Windows Updates: the update history the Settings app shows and the OS-level quick-fix list, read-only, with each update's outcome in words. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

## Get it

Tell the agent "add the windows-updates cell", or run `nodecode add nc://windows-updates`.

## Set it up

Nothing is required on a stock Windows: the cell runs `powershell.exe`, which is already there. Two settings in the `windows-updates` section of `~/.nodecode/config.jsonc` are worth knowing:

| Key | Default | What it is |
| --- | --- | --- |
| `powershell` | `powershell.exe` | the executable the two queries run through; point it at `pwsh` for PowerShell 7 |
| `timeout_seconds` | `90` | how long one query may take before its process tree is killed (at most 600) |

## Use it

Ask what Windows installed lately, why a machine is stuck on an old build, whether a driver update actually took, or when the OS was last patched. The agent has two tools for it:

- `win_update_history` — the update history the Settings app shows, newest first: Windows, Defender and driver updates, each with its outcome in words (`2` succeeded, `3` succeeded with errors, `4` failed, `5` aborted) and, when it did not end clean, the failure's code in the `0x` form a person can paste into a search box. Filter it with `only_failures`, or by a title substring such as `Defender` or a KB number, and raise `count` when the window is too small (1 to 500).
- `win_hotfix_table` — the OS-level quick-fix list: KB ids, security updates, install dates and installers, newest install date first. It carries OS-level updates only, so apps, Defender signatures and drivers are `win_update_history`'s.

`/windows-updates` is the same read in the operator's own words: bare it is the status — how many updates the history holds, how many of the newest did not end clean, the newest failure's code, and when the OS was last patched — and `history [COUNT]` and `hotfixes [QUERY]` are the two tables.

Nothing is searched for, downloaded, installed or uninstalled: `QueryHistory` reads what was already installed or attempted (the searcher's `Search()` is the call that reaches the network, and no script calls it), and `Win32_QuickFixEngineering` is a CIM read. No file is written.

Windows only. Elsewhere the first query says what failed.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The two scripts, the result-code table, the two tools, `/windows-updates` and the section. |
| `test/` | Offline tests: a stubbed PowerShell answers canned output, so nothing runs a command. |

## License

MIT, Copyright (c) 2026 Yicheng-Mark. See `LICENSE` and `NOTICE`.
