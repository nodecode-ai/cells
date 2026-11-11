# nodecode-smol

Smol Machines: runs shell commands in a local Linux microVM. A Nodecode rewrite of [smol-machines/hermes-smolmachines](https://github.com/smol-machines/hermes-smolmachines) by [smol-machines](https://github.com/smol-machines).

A smol machine is a microVM on this computer with its own Linux kernel, booted
from an OCI image. The boundary is hardware virtualization, not a container
sharing your kernel: nothing of this computer is mounted into it, so a build, a
package install, an untrusted script or a `rm -rf` runs there and leaves your
files alone. The plugin sent Hermes's whole terminal into one; this cell offers
the same machine as a verb you can call beside the others.

## Get it

Tell the agent "add the smol cell", or run `nodecode add nc://smol`.

## Set it up

One prerequisite: the `smolvm` CLI, the engine's own, from
[smolmachines.com](https://smolmachines.com) (`brew install
smol-machines/tap/smolvm`, its install script, or a release binary). It runs on
macOS on Apple Silicon, and on Linux on x86_64 or arm64 with glibc 2.34 or newer
and `/dev/kvm`. The cell finds it on `PATH`; if it lives elsewhere, name it in
the `smol` section under `/settings` or the Settings tab:

| Setting | Default | What it does |
| --- | --- | --- |
| `binary` | `smolvm` | the CLI: a name on `PATH`, or a path to it |
| `image` | `python:3.12-slim` | the OCI image a one-off machine boots from |
| `network` | on | whether a machine this cell boots gets outbound network |
| `timeout_seconds` | 120 | one command's ceiling |
| `max_output` | 20000 | the characters of output one answer carries |
| `extra_args` | none | arguments added to every `machine run` and `machine create`, for a flag this cell does not name |

The first command in a new image pulls it from its registry (Docker Hub for the
default), so the first boot of `python:3.12-slim` takes longer than the rest.

## Use it

Ask in words -- "run this in a sandbox", "test that install in a clean Linux" --
and the model calls `smol_run`. Or call it yourself from eval:

```lisp
(smol:run "uname -a")                          ; a one-off machine, removed when it exits
(smol:run "pip install pandas" :image "python:3.12-slim")
(smol:run "ls /root" :machine "dev")            ; a named machine, made on first use
(smol:run "make" :machine "dev" :timeout-seconds 600)
(smol:machines)                                ; the machines this computer holds
```

`/smol CMD` runs a command the same way and prints its output and exit code,
`/smol` lists the machines, `/smol stop NAME` and `/smol rm NAME` free one, and
`(help :smol)` has the rest. `smol_machines` (or `(smol:machines)` with a
name) reports on the machines, the CLI's own listing in `raw` beside the names.

The answer is JSON:

```json
{"ok": true, "exit_code": 0, "output": "Linux ...", "clipped": false,
 "machine": "dev", "image": "python:3.12-slim", "network": true,
 "seconds": 0.9, "notes": []}
```

A command that failed still answers, with `ok` false and its own `exit_code`; a
command past its ceiling is killed and answers 124, the code `timeout(1)` uses.
The machine runs `bash -c CMD`, so an image without bash (`alpine`) takes
`apk add bash` first.

## What changed in the rewrite

| The plugin | Here |
| --- | --- |
| a terminal backend: Hermes's every tool ran in the machine | `smol_run`, `smol_machines`, `/smol` and `(help :smol)`, beside the tools you already have |
| the Python SDK, installed as a plugin dependency | the engine's `smolvm` CLI, so nothing Python is bundled |
| a machine per Hermes task, kept or deleted by `container_persistent` | a one-off machine by default, or a named one that keeps its files |
| a Python result object, an 11 MiB stream cap | a JSON answer with the exit code, the output, the machine, its image, the network, the seconds and notes |

The machine is the same one: local, hardware-isolated, booted from the image
you name, with nothing of the host mounted into it.

## License

MIT, Copyright (c) 2026 smol machines: see `LICENSE` and `NOTICE`.
