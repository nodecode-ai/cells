# nodecode-windows-services

Windows Services: what runs on this machine, why one failed, and what it depends on. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

## Get it

Tell the agent "add the windows-services cell", or run `nodecode add nc://windows-services`.

## Set it up

Nothing, on Windows. The cell finds `powershell.exe` (or `pwsh.exe`) on PATH the
first time it starts; nothing else is needed, and there is no key, no account and
no network call.

If PowerShell lives somewhere unusual, or the cell refuses with "no PowerShell
found", name it in the `windows-services` section of `/settings`:

```jsonc
"windows-services": {
  "powershell": "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe",
  "timeout_seconds": 90,
  "max_rows": 100
}
```

`timeout_seconds` bounds one query (the listing or the event log); `max_rows`
bounds how many services one listing lays out before the rest are counted only.

Nothing is started, stopped, reconfigured or deleted: the cell only reads.

## Use it

Ask for what you want and the model picks the tool.

| Tool | Use |
| --- | --- |
| `win_service_table` | every service with its state, start mode, pid, account, binary, exit code, and its dependency edges both ways; filter by `query`, `state`, `start`, `nonzero_exit`, `limit` |
| `win_service_detail` | one service in full: the failure policy Windows applies (restart the service, restart the computer, run a command, after how long, when the count resets), whether its binary is still on disk, and the Service Control Manager's own recent events |

```text
> what died after the last update?
> which services would not start if I reboot now?
> why is FooSvc stopped?
```

The numbers are the point. An exit code is a Win32 error number: `1077` has not
tried since the last boot (not a failure), `1053` did not answer in time, `1075`
waits on a dependency that failed first, `2` its binary is gone. The events say
what the Service Control Manager saw: `7000` failed to start, `7009` a start
request that timed out, `7024` a service-specific error, `7031` an unexpected
end, `7045` an install.

The operator has `/windows-services` for the same thing without asking the model:
with no argument it says how many services are registered, how many run, and
which ones carry a failed exit code; with a name it gives that service in full.
From eval the same answers are `(windows-services:table :state "stopped" :start
"auto")` and `(windows-services:detail "wuauserv")`, and `(help
:windows-services)` is the model's guide.

Windows' own two sources are read: one `Get-CimInstance Win32_Service` call,
whose property names and enum values do not change with the display language
(`sc.exe query` localizes both), and the registry under
`HKLM\SYSTEM\CurrentControlSet\Services`, which is where the dependency edges and
the `FailureActions` blob really live -- CIM carries a `DependOnService` property
it never populates, and the failure policy exists nowhere in CIM at all. The
blob is decoded in Lisp: its 32-bit `SERVICE_FAILURE_ACTIONS` layout, reset
period, action count, then (type, delay) pairs.

The output names services, accounts, command lines and paths, so treat it like a
Services window opened on the machine, and say so to whoever is reading.

## Tests

Run `.github/test-cell.sh tools/nodecode-windows-services` from the cells
repository. Every PowerShell call is stubbed, so the suite needs no Windows, no
PowerShell and no network: it covers the section and its refusal, the two tools
through the seam the model calls them by, the filters, the failure-policy blob
decoded on its own, and `/windows-services`.
