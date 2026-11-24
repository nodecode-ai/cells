# nodecode-process-table

Process Table: what is still running on this machine and who started it — every process with its parent, its start time, its image, and a flag saying whether the parent it points at is still the one that spawned it. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

## Get it

Tell the agent "add the process-table cell", or run `nodecode add nc://process-table`.

Nothing to configure: the cell reads the machine's own process table.

## Use it

Ask a question about what is running — "what is left over from the gateway I just quit", "which of these thirty `node` processes is the live one", "did this start before or after the thing I am debugging" — and the agent calls the `process_table` tool. The answer is one JSON object: the processes the call's filters left, oldest first, each with `pid`, `parent_pid`, `process`, `path`, `started`, `age_seconds`, `threads`, `parent_process` and `parent_state`, plus the counts for the whole table and the notes that say what you are looking at.

`parent_state` is the judgement worth having. A system records a parent **PID**, not a handle: once the parent exits, its number is free again, and a child that keeps running points at an unrelated newer process — which is what a session that leaked children looks like after a restart.

| State | What it means |
| --- | --- |
| `alive` | the parent is in the table and started before the child |
| `gone` | no process holds that PID now |
| `reused` | the PID belongs to a process that started *after* the child |
| `root` | the parent PID is 0 |
| `unknown` | no start time to compare |

Filters: `pid`, `pids`, `process` (a substring of the name or the path), `parent`, `orphans_only` (only the `gone` and `reused` ones), `include_system` (false leaves out the system's own session and bootstrap processes, whose parent exits by design), `with_ancestry` (also return the parent chain, as context rows) and `limit` (default 100, max 500).

The same rows as text:

- `/process-table` — the table, oldest first, with the notes under it.
- `/process-table orphans` — only the leftovers.
- `/process-table node` — only processes whose name or path holds `node`.

Or from eval: `(process-table:describe-table :process "node")` prints the table, and `(process-table:report :orphans-only t)` answers the JSON the tool answers.

## What it reads

Nothing is signalled, opened for writing or started, and no file is written. The table is read once per call:

| System | Where the rows come from |
| --- | --- |
| Linux | `/proc/PID/stat`, `/proc/PID/status` and the `/proc/PID/exe` link |
| macOS, BSD | `ps -axo pid=,ppid=,etime=,comm=` — no thread count |
| Windows | `Get-CimInstance Win32_Process`, through `powershell -NoProfile` |

A process this account cannot open comes back with no path and no start time instead of failing the call.

## Set it up

Only if the defaults do not fit: the section `process-table` in `config.jsonc` takes `reader` (`auto`, `proc`, `ps` or `powershell`), `limit`, `clock_ticks` (for the Linux start times; 100 is Linux's own value), `timeout_seconds`, and `system_prefixes` — the path prefixes that mark an image as the system's own. See `config.example.jsonc`.

## What is in it

| File | What it is |
| --- | --- |
| `nodecode-process-table.asd` | The system and its test system. |
| `package.lisp` | The package, its nicknames and the one condition. |
| `read.lisp` | The three readers, and the seam a test binds. |
| `table.lisp` | Rows into one table: the parent judgement, the filters, the text. |
| `cell.lisp` | The call's contract, the tool, `/process-table` and the manual. |
| `test/` | The cell's tests: a written-in table, the filters, the wire, the real reader. |

## License

MIT. The original plugin is Copyright (c) 2026 Yicheng-Mark: see `LICENSE` and `NOTICE`.
