# nodecode-windows-eventlog

Windows event log: ask Windows why the process died. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

Three read-only tools over the Windows Event Log, the original plugin's, plus
`/windows-eventlog` to read one channel into the few lines an operator wants
first:

| Tool | Answers |
| --- | --- |
| `win_crash_events` | Application Error / Application Hang / Windows Error Reporting / .NET Runtime records in a window, with the faulting module, its path, the offset and the exception code decoded (`0xC0000005 (ACCESS_VIOLATION)`), plus groups that collapse repeated crashes of one app and signature |
| `win_shutdown_events` | Kernel-Power 41 unexpected restarts, bugchecks decoded to names (`0x9F` → `DRIVER_POWER_STATE_FAILURE`), EventLog 6008 dirty shutdowns, 1074 operator restarts with the process and the comment, and the 6005/6006/12/13 start and stop markers |
| `win_eventlog_query` | any channel, filtered by event ids, levels, exact provider and a payload substring, answering structured records instead of raw XML |

```
win_crash_events {"hours": 24, "process": "hermes"}
win_crash_events {"hours": 168, "kinds": ["application_error"]}
win_shutdown_events {"hours": 720}
win_eventlog_query {"channel": "System", "event_ids": [7031, 7034], "contains": "gateway"}
```

## Get it

Tell the agent "add the windows-eventlog cell", or run `nodecode add nc://windows-eventlog`.

## Set it up

Nothing is required on Windows 10 or 11: the cell runs `wevtutil`, which is
already in `System32`, and reads the Application and System logs the account
is allowed to read. The `windows-eventlog` section of `config.jsonc` (or
`/settings`) has four settings, all with working defaults:

```jsonc
"windows-eventlog": {
  "wevtutil": ["wevtutil"],      // or ["C:/Windows/System32/wevtutil.exe"]
  "timeout_seconds": 60,          // one read's ceiling
  "scan_cap": 5000,               // records one read asks Windows for
  "assume_windows": false         // try anyway from a process that is not on Windows
}
```

A section with no `wevtutil` at all refuses at start rather than at first
use, and the refusal names the setting.

## Use it

Ask why something vanished ("why did the gateway die last night", "did the
machine restart or did we crash"), and the model picks the tool. Or call it
from eval, as `(windows-eventlog:crash-events (windows-eventlog:crash-events-request ...))`,
or just read a channel:

```
/windows-eventlog System
/windows-eventlog Microsoft-Windows-TaskScheduler/Operational
```

An answer is one JSON object. `{"ok": true, ...}` carries `window_hours`,
`matched`, `scanned`, `events` newest first, `groups` where the tool groups,
and `notes`; `{"ok": false, "error": "..."}` is what a read that could not
happen answers — a channel that needs elevation (exit 5), a channel that is
not there, a `wevtutil` that is not on PATH, a process that is not on
Windows. Nothing raises.

## What it gets right, and what it does not do

**A record is classified by provider as well as id.** Application-specific
providers reuse 1000 and 1001 for informational events, so a record outside
the real crash family is dropped instead of reported as a crash with empty
fields.

**A field name is matched case-blind.** Windows builds disagree with
themselves: `Faulting Module Name` beside `Faulting module name`, and the
provider's own `param1` beside `Process`.

**Filters are applied twice.** The event ids, levels and provider go into the
XPath Windows runs and are checked again on what comes back, so a channel
that ignores a filter still answers what was asked for. Channel and provider
names are validated before they are interpolated, so a quote cannot break out
of the query.

**A window stops at its start.** Records are read newest first and the walk
stops as soon as the window's start is crossed, or at `scan_cap` records;
that second case is said in `notes`, never passed off as the whole window.

**It is not a debugger.** It reports what Windows recorded. It reads no
minidump and no Windows Error Reporting report file (those are ACL'd to the
machine's administrators), writes no log, clears no log and configures no
channel. Times are UTC, ISO 8601 with microseconds. Provider text is whatever
the machine's locale produced, untranslated.

**Windows only.** Off Windows every tool answers `{"ok": false}` with the
reason, as the original did.

## The rewrite

The original reached `wevtapi.dll` in-process through `ctypes`. This asks the
same API through `wevtutil`, the tool Windows ships, and parses the XML it
renders with the small reader in `xml.lisp` — one bounded child process per
call, no library, no dependency beyond Nodecode itself. The classification,
the decoding tables, the grouping, the window walk and the answer shape are
the original's. See NOTICE.

Run `.github/test-cell.sh tools/nodecode-windows-eventlog` from the cells
repository: no test needs Windows or a log. Every read goes through one stub
that answers the XML a test writes, and the tools are called through the real
dispatch and their contracts.
