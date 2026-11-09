# nodecode-windows-tasks

Windows Tasks: what Windows' Task Scheduler runs on its own, and whether it worked, as two read-only tools. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

Windows starts a great deal of what it does, and what installed software wants it to do, through Task Scheduler: at logon, at startup, every hour, or during automatic maintenance. When one of those starts failing, the only place that says so is Task Scheduler's "Last Run Result" column, as a number — `0x80070002` is a program that is no longer there, `0x800710E0` a task that needed a desktop it did not have. These tools read that column and translate it.

## Get it

Tell the agent "add the windows-tasks cell", or run `nodecode add nc://windows-tasks`.

## Set it up

Nothing to set up on Windows: the tools ask the Scheduler's own cmdlets through `powershell.exe`, which Windows ships, and there is no key, no service and no network. The cell refuses to start when it cannot find the program, and both the refusal and the `program` setting name why — point `program` at `powershell.exe` (or `pwsh.exe`) when it is not on `PATH`. Windows only.

## Use it

The model gets two tools:

| Tool | What it answers |
| --- | --- |
| `win_scheduled_tasks` | Every task with its path, state, the account it runs as, its first action's command line, its triggers in words, its last and next run, its missed runs, and its last result code in hex with what it means. Filter by path substring, by state, or to the tasks whose last run failed. |
| `win_scheduled_task_detail` | One task in full: every trigger with its start boundary, repetition and schedule, every action with its arguments and working directory, the principal, and the settings that decide whether it runs on battery or when late. |

And the same two calls from Lisp, through eval:

```lisp
(windows-tasks:tasks :query "backup")                 ; what looks like a backup
(windows-tasks:tasks :failed-only t :include-microsoft t) ; what failed, Windows' own included
(windows-tasks:tasks :state "disabled")                 ; what was switched off
(windows-tasks:task-detail "\\Microsoft\\Windows\\Defrag\\ScheduledDefrag")
```

Or from the operator's side:

```
/windows-tasks                                   # every task whose last run failed
/windows-tasks \Backup\Offsite                   # that one task in full
```

Both answer JSON, as the plugin answered its tools: `ok`, the rows (or the one task), and `notes` that say what the filters left out — `"12 of 20 matching task(s) listed, 195 registered"`, `"stopped at limit=100; raise it or narrow the query"`, `"Windows' own tasks are left out; set include_microsoft=true to see them"`. A call that cannot read the Scheduler answers `ok: false` with a `code` (`tasks_unavailable`, `not_registered`), what PowerShell said, and for a path nothing is registered at, the paths like it.

A task with no triggers but maintenance settings runs during automatic maintenance, and the notes say so instead of leaving an empty trigger list to be misread.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `program` | `powershell.exe` | The Windows PowerShell program: its name on `PATH`, or a path to it. |
| `timeout_seconds` | `90` | How long one call may take. One call lists every task; a second exports one definition. |

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | The `windows-tasks` package and the condition every refusal signals. |
| `xml.lisp` | A task definition read as data: triggers, actions, principal, settings, and the sentence each trigger and action is told in. |
| `cell.lisp` | The PowerShell runner, the listing and its filters, the detail, the two tools, `/windows-tasks` and the section. |
| `test/` | Offline tests: the lifecycle, the listing through a stubbed PowerShell, the detail through an exported definition, every refusal, and one real run of a stand-in program. |

## What the rewrite does not do

- **No elevation and no writing.** Every call is a query: `Get-ScheduledTask`, `Get-ScheduledTaskInfo` and `Export-ScheduledTask`. Nothing creates, starts, changes or deletes a task, here as in the plugin.
- **No console window suppressed.** The plugin passed a create-no-window flag to its subprocess; SBCL's `run-program` takes no such flag, so a shell that starts a call from a windowless process may show a console for the length of the call.
- **No cache**, deliberately: scheduled tasks change when software is installed, and a stale answer here is worse than a slow one.
- **Windows only**, as the plugin was: the tools read Task Scheduler, and the cell says so when it finds no PowerShell.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-windows-tasks
```

Every run of PowerShell is stubbed with the JSON it would have printed, and one test starts a real `/bin/sh` stand-in for the argv and the reading of its answer. Nothing here needs Windows or a real PowerShell.

MIT licensed. The original is Copyright (c) 2026 Yicheng-Mark: see `LICENSE` and `NOTICE`.
