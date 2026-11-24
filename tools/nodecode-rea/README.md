# nodecode-rea

REA: reverse engineer binaries, apps and core dumps through REA's MCP server. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

The plugin was a launch command and two workflow skills. The cell is both: it
launches REA's MCP server itself (`npx -y rea-agents@6.7.0 mcp`, the version
the plugin pinned, with the two environment variables it forwarded), speaks
the protocol over its stdio, and puts the workflow on the manual at
`(help :rea)`.

## Get it

Tell the agent "add the rea cell", or run `nodecode add nc://rea`.

## Set it up

Node.js 22.19+, 24.11+ or 26+ with `npx` on PATH is what REA itself needs;
the first call unpacks the pinned package into npm's cache. Then set
`server` to `rea-agents@6.7.0` — the pin the plugin shipped — in `/settings`,
or ask the agent to set up `rea`. A newer pin works the same way; nothing
else changes.

A core dump also needs a Python with pwntools 4.15.0, pyelftools 0.33 and
Unicorn 2.1.2: name its interpreter in `pwntools_python`, which rides to the
server as `REA_PWNTOOLS_PYTHON`. Nothing else is required for JavaScript and
Electron apps, ASAR and archive inventory, Go and Rust metadata, PE resources,
.NET metadata and saved HAR captures. Native decompilation wants an engine of
your own — Ghidra with a JDK, Hopper, IDA, or rizin, objdump and gdb on PATH;
Android code wants JADX and Java; firmware wants binwalk or unblob. REA finds
the ones on PATH by itself and never installs any.

```jsonc
"rea": {
  "server": "rea-agents@6.7.0",
  "npx": "npx",
  "call_timeout_seconds": 300,
  "ghidra_startup_timeout_ms": 900000,
  "pwntools_python": ""
}
```

## Use it

Ask for a target: "open ./parser and tell me how it reads the file header", or
"what made ./core crash?". The agent finds `rea` in its index and reads it
with `(help :rea)`: the workflow, the routing by target, and the verbs.

```lisp
(rea:tools)                             ; every tool the server carries, or (rea:tools :search "android")
(rea:schema "open_binary")             ; one tool's arguments
(rea:call "open_binary" :path "/tmp/parser")
(rea:call "inspect_android_package" :path "/tmp/app.apk")
(rea:status)  (rea:reconnect)
```

The model gets two tools rather than REA's 207 schemas (some 800 KB): one to
find a tool, one to run it. `/rea` answers the connection's state, `/rea
tools android` lists tools, `/rea restart` reconnects.

`open_binary` validates a target; the first Ghidra-backed query imports and
analyses it and can take minutes. Raise `call_timeout_seconds` for a big
executable — the plugin's own budget was 300 seconds with Ghidra's internal
import deadline at 900 — and keep one connection open, because reopening
discards the analysis.

Some tools run or change something: a command under a PTY, a browser or
native window, an Android device, Frida, gdb or rizin, an arbitrary rizin
command, a file overwritten at a path you name. The primer tells the agent to
name the exact command, app or device and ask before any of them.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the verbs, the two model tools, `/rea`, and the workflow at `(help :rea)`. |
| `transport.lisp` | One MCP server over stdio: a JSON-RPC line out, its answer matched by id, a call's deadline. |
| `package.lisp` | The package and the refusal every verb answers with while the cell is not started. |
| `test/cell-test.lisp` | The lifecycle, both tools and the command against a stubbed server: no npx, no network, no Node. |

## Test

```sh
.github/test-cell.sh tools/nodecode-rea
```

## Credits and licence

MIT, Copyright (c) 2026 Nous Research: see `LICENSE` (unchanged, including
the two skill licences it reproduces) and `NOTICE`. REA itself is
[morluto/rea](https://github.com/morluto/rea), and the workflow skill is
adapted from it; the crash-triage workflow from
[mingleiw/crashlens](https://github.com/mingleiw/crashlens). The primer in
`cell.lisp` condenses those two workflows.
