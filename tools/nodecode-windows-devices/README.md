# nodecode-windows-devices

Windows Devices: which device on this Windows machine is unhappy, what its problem code means and what driver is behind it. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

## Get it

Tell the agent "add the windows-devices cell", or run `nodecode add nc://windows-devices`.

## Set it up

Nothing is required on Windows: the cell reads CIM through the PowerShell the machine already has, unelevated, and asks nothing of Device Manager. Off Windows it says so instead of spawning anything, and takes a PowerShell that reaches a Windows host when you name one:

```jsonc
{
  "windows-devices": {
    "powershell": "",   // the executable to run; empty uses powershell.exe on Windows, pwsh elsewhere
    "timeout": 120       // seconds one CIM query may take, at least 10
  }
}
```

There is no key, no account and no network call: everything read is read from this machine.

## Use it

Ask the agent which device is unhappy, or type the command yourself:

- `win_device_table` lists every present PnP device with its class, status, manufacturer, device id and its problem code in words. Filter with `query` (a substring of the name or the id, e.g. "camera", "usb", "LENOVO"), `class` ("Net", "HIDClass", "Display"), `broken_only`, `include_absent` for devices that are not plugged in any more, and `limit` (1 to 300, 100 by default).
- `win_device_detail` is one device in full: everything the table shows, plus the driver behind it -- name, version, date, provider, signer, INF file, and whether it is signed -- or the absence of one, which for problem code 28 is the same fact said twice.
- `/windows-devices` typed alone answers what is here and what needs attention, with a row that opens one device; `/windows-devices NAME` prints one device in full.

From eval the same reading is a function call: `(windows-devices:devices)` is the raw table as plists, `(windows-devices:detail-of "camera")` a device and the driver behind it.

What the codes mean, in Device Manager's own order of worry: `28` no driver is installed, `10` cannot start, `43` Windows stopped it after it reported a failure, `1` configured incorrectly, `39` its driver is missing or corrupt, `22` the person switched it off on purpose (not a breakage).

## Read-only

The tools query `Win32_PnPEntity` and `Win32_PnPSignedDriver` and return JSON; nothing is enabled, disabled, updated or reinstalled, no file is written, and the plugin's own words -- "no network, no file writes" -- hold here too. The only process is one `powershell.exe -NoProfile -NonInteractive -Command` per query, the script a constant and no caller's text ever part of it, bounded by `timeout`.
