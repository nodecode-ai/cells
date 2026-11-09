# nodecode-wslc

WSL Containers sandbox: run commands in a Linux container on Microsoft's WSL Containers runtime, the one that ships with WSL 3.0.3+, with no Docker Desktop anywhere in it. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

In Hermes the plugin swaps the terminal the agent's tools run in, so every
`sh`, file and `execute_code` call lands in a `wslc` container. Here it is one
tool, `wslc_exec`, and one set of verbs: your own shell is untouched, and
what you send to the sandbox is the work you meant to isolate.

## Get it

Tell the agent "add the wslc cell", or run `nodecode add nc://wslc`.

## Set it up

One setting is required, and it is the image the sandbox runs:

```jsonc
{ "wslc": { "image": "alpine:3.20" } }
```

- `image` (required) — a Linux image the runtime can pull. WSL Containers does
  not run Windows images, and it pulls through your own registry configuration,
  not Docker Desktop's; `wslc pull <image>` by hand first if the registry needs
  credentials.
- `binary` — the `wslc.exe` to drive, when it is not on `PATH`. Unset, the cell
  looks on `PATH` for `wslc`, `wslc.exe` or `container.exe`, then in
  `%ProgramFiles%\WSL`.
- `sandbox`, `persistent` — the sandbox's own name, and whether its container
  stays between sessions. A persistent sandbox (the default) is **stopped**, not
  removed, when the cell stops, and the next session starts it again with its
  files; a non-persistent one is a fresh container per start and is removed at
  the end of it.
- `network`, `volumes`, `forward_env`, `env` — outbound network (on unless
  `false`), bind mounts (`host:container[:ro]`), the host environment
  variables to copy in (only the names you list), and an `env` object of
  variables of its own, which wins over `forward_env`.
- `cpus`, `memory_mb`, `cwd`, `timeout_seconds` — the container's cores and
  memory (0 leaves it to the runtime), where commands start, and how long one
  may run.

The cell is not a Docker backend's equal: `wslc run` rejects `--cap-drop`,
`--security-opt`, `--pids-limit`, `--init` and `--storage-opt`, so the sandbox
does not drop capabilities. `/wslc` says so, naming the flags this wslc build
lacks, and dangerous-command approval stays on.

## Use it

The model calls `wslc_exec(command, cwd, timeout)` and reads the command's
output and exit status together; a nonzero exit is an answer, not a refusal.
From eval:

```lisp
(wslc:exec "uname -a; apk add --no-cache curl")  ; (:exit-code 0 :output "...")
(wslc:sandboxes)                                ; the containers this cell started
(wslc:remove-sandbox "nodecode-wslc-default")   ; one it started, files and all
(wslc:binary) (wslc:version)                    ; the CLI it drives
```

`/wslc` reports the CLI, its version, the image, the sandbox's state and what
this build cannot harden; `ps` lists the runtime's containers, `mine` only this
cell's, `start` creates the sandbox now, and `rm NAME` removes one.
`(help :wslc)` teaches the model the same, while the cell runs.

The container is created on the first command, not at boot, so a machine
without the `wslc` CLI still starts the cell and says what is missing.

## What it needs

The `wslc` CLI, which ships with WSL 3.0.3+ (`wsl --update`), and a Linux
image. Nothing else: the cell runs on what Nodecode already carries.

## License

MIT, as the original. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
