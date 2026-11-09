# nodecode-ports

Who holds a local port. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

A bind failure names a port and nothing else. One call here answers the
rest: the local TCP and UDP endpoints, the owning PID, its image name and
its path, and who else is on the same binary. The rows are the ones
`netstat -ano` prints, with the process already joined to them, so nothing
depends on console output or on the machine's display language.

The plugin read Windows' tables only. This cell keeps that path and adds the
two others from the same question: `/proc` on Linux and `lsof` on macOS. All
three are read-only — nothing is opened, bound or connected, and on Windows
and Linux no child process runs at all.

## Get it

Tell the agent "add the ports cell", or run `nodecode add nc://ports`.

## Set it up

Nothing is required. Two settings in the cell's `ports` section shape the
answer, both under `/settings` or the Settings tab:

| Setting | Default | What it does |
| --- | --- | --- |
| `limit` | 100 | the most endpoints one answer carries; a call may lower or raise it, to 500 |
| `show_paths` | on | whether an answer carries each process's image path |

macOS needs `lsof`, which ships with the system. Linux reads `/proc`, and a
process whose descriptors this user cannot read keeps its endpoint and
reports no owner rather than failing.

## Use it

Ask in words — "what is holding port 3000", "is anything listening on 8086",
"which process owns this connection" — and the model calls `win_port_owners`
(the name the original plugin used, so a prompt written for it still
works). Or call it yourself from eval:

```lisp
(ports:owners 3000)                      ; the endpoint holding port 3000
(ports:owners :process "node")            ; whatever node holds
(ports:owners :state "listen")            ; every listener
(ports:owners :ports '(3000 8086))        ; two ports at once
(ports:owners :protocol "udp" :limit 20)  ; the UDP table, twenty rows
```

The answer is JSON, field for field the original's:

```json
{"ok": true, "matched": 1, "listeners": 1, "total_endpoints": 121,
 "entries": [{"protocol": "tcp", "address": "127.0.0.1", "port": 3000,
              "state": "listen", "pid": 496, "process": "node",
              "path": "/usr/local/bin/node",
              "remote_address": "0.0.0.0", "remote_port": 0}],
 "by_process": {"node": 1},
 "notes": ["1 endpoint matched out of 121 in the table"]}
```

Filters, the same six the plugin took: `port` and `ports` (local endpoints),
`process` (part of the image name or path, case-insensitive), `protocol`
(`tcp`, `udp`, `both`), `state` (`listen`, `established`, any TCP state by
name, `all`; a UDP row reports `bound`), `include_ipv6` (on by default) and
`limit`.

`/ports` prints the same rows as lines: `/ports 3000`, `/ports node`,
`/ports listen`, `/ports` for the whole table. `(help :ports)` has the rest.

## What changed in the rewrite

| The plugin | Here |
| --- | --- |
| Python, ctypes, `iphlpapi.dll` | Common Lisp, `sb-alien`, the same two tables and the same `QueryFullProcessImageName` right (`PROCESS_QUERY_LIMITED_INFORMATION`) |
| Windows only | Windows, Linux (`/proc` and its socket inodes) and macOS (`lsof -F`) |
| a tool only | the tool, `/ports`, and `(help :ports)` |
| everything in one answer | `limit` and `show_paths` in the cell's section |

The answer's shape, the six filters, the `by_process` summary and the
notes are unchanged. macOS has no in-process table without a kext, so that
one platform runs `lsof`.

## License

MIT, Copyright (c) 2026 Yicheng-Mark: see `LICENSE` and `NOTICE`.
