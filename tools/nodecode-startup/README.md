# nodecode-startup

Startup: what launches itself when this machine signs in, from where, whether the person switched it off, and whether the binary it names is still there. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

The Task Manager → Startup tab question, answered without Task Manager, and answered on the machine the cell runs on: the original read Windows' six Run/RunOnce keys, Explorer's `StartupApproved` verdicts and the two Startup folders; this rewrite does the same, and reads the XDG autostart folders on Linux and the launchd agents and daemons on macOS.

## Get it

Tell the agent "add the startup cell", or run `nodecode add nc://startup`.

## Use it

Ask the agent what starts at sign-in, or call the verbs from eval (or `/startup` in the shell):

| Verb | What it does |
| --- | --- |
| `/startup` | Every autostart entry, one line each: enabled, source, name, command, then the notes that read them. |
| `/startup NAME` | One entry in full. The name is what the table showed, case-insensitive. |
| `/startup --json` | The whole payload, as the tool answers it. |
| `(startup:startup-table)` | The table, as text. |
| `(startup:startup-detail "NAME")` | One entry, as text. |
| `(startup:entries)` | Every entry as a plist — the reading the tools are built on. |

The model has the same two entry points as tools, `startup_table` (with `query`, `scope`, `include_disabled` and `limit`) and `startup_detail` (one `name`).

What an entry is made of:

| Field | Where it comes from |
| --- | --- |
| `name`, `command` | The Run value's name and data, the folder file's name and path, an XDG `Name=` and `Exec=`, a launchd `Label` and `Program`, verbatim |
| `source` | One of six registry keys (`registry-hkcu-run`, `registry-hklm-run-32`, ...), `folder-user` / `folder-common`, `xdg-user` / `xdg-system`, `launchagent-user` / `launchagent-common` / `launchdaemon-common` |
| `scope` | `user` or `machine` |
| `enabled`, `changed` | On Windows, Explorer's `StartupApproved` verdict: enabled for state bytes 2 and 3, disabled for 6 and 7, with the FILETIME of the last change; any other byte is reported as unknown with the byte, never guessed. On Linux, `Hidden=true` or `X-GNOME-Autostart-enabled=false`; on macOS, `Disabled`. An entry with no record runs, which is each platform's default |
| leftover approvals | On Windows, records for entries that are gone — uninstalled programs leave them behind — reported, never folded into a verdict |

`startup_detail` adds the resolution of the command: quoting, arguments and environment variables folded in, the binary the loader would find, and whether it is still on disk. An unquoted command with blanks in its path is resolved the way a loader resolves it — the longest whitespace prefix that names a real file — and a command whose path is relative is reported unchecked rather than guessed.

Nothing is enabled, disabled or deleted: the tools enumerate. Turn an entry off where the platform does it — Task Manager, the desktop's own Startup settings, or the file itself — and ask again.

## Set it up

Nothing has to be set. Two members, both optional, in `/settings` or in `~/.nodecode/config.jsonc`:

```jsonc
{
  "startup": {
    "max_rows": 100,        // rows one table answer carries, at most 400
    "home": "/home/someone" // read user-scope sources from another home, a mounted disk or another profile
  }
}
```

`home` moves the user-scope sources only: the Windows registry is always the live one.

### What the rewrite does not do

- **It is not Windows-only.** The original answered on Windows; this answers on Linux (XDG autostart folders) and macOS (launchd agents and daemons) too, so its two tool names lost their `win_` prefix. A platform's own source set is narrower than Windows': systemd user units and macOS login items are not read, and a directory entry or a login item the desktop keeps elsewhere is not seen.
- **The registry is read through `reg query`.** The image has no registry reader, so on Windows the cell runs `reg.exe` — one process per key, read-only. On the other two platforms it reads files and starts nothing.
- **A binary plist on macOS is named but not parsed.** A launchd job stored as a binary `.plist` is reported by its file name, with a note, rather than decoded.

MIT licensed. See `NOTICE` for what came from the original and what did not.
