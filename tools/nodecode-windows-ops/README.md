# nodecode-windows-ops

Windows Ops: Windows and WSL triage — processes that survive a quit, argv that reaches `cmd.exe` as shell syntax, a config with a UTF-8 BOM, path and console-encoding traps. A Nodecode rewrite of [Yicheng-Mark/hermes-windows-plugins](https://github.com/Yicheng-Mark/hermes-windows-plugins) by [Yicheng-Mark](https://github.com/Yicheng-Mark).

## Get it

Tell the agent "add the windows ops cell", or run `nodecode add nc://windows-ops`.

## Set it up

Nothing to set: no key, no service, no dependency. The folder is a knowledge cell, so Nodecode records it and never loads it — the index reads `windows-ops.lisp` as data and `(help 'windows-ops)` reads the skill back.

## Use it

Ask about a machine that misbehaves only on Windows or under WSL:

> Something leaves a pile of python processes behind every time I quit. Use windows-ops to find out which of them are actually orphaned before killing anything.

> On WSL, opening a link with an `&` in it ran something else. Walk windows-ops's interop section and tell me what to capture.

The skill sorts the symptom into one of five classes before any fix — interop, process lifecycle, path, encoding, terminal — and gives each class its cause and the evidence that tells them apart. It establishes which layer you are in first (native Windows, WSL, or a shell in WSL driving a Windows binary), because the same symptom has a different cause in each. Everything is read-only until the fourth step, which asks before it changes anything; the two commands in its reference that kill a process are commented out and marked "Destructive, ask first".

`references/diagnostic-commands.md` is the copy-paste half: the layer check, the orphan sweep that reports `parentAlive` per candidate, the `cmd.exe` command line as it actually arrived, the first bytes of the config read with `Format-Hex`, the active code page, and the job-object state. It names Nodecode's own home and config — `~/.nodecode`, `%USERPROFILE%\.nodecode\config.jsonc` — so the path in a command is the one on this machine.

An edit to `windows-ops.lisp` is the skill at the next read, so a Windows trap you learn the hard way is kept by editing the file.

## What is in it

| Path | What it is |
| --- | --- |
| `nodecode-windows-ops.asd` | The folder as a knowledge cell: `:properties ((:knowledge . t))`, no component, so there is nothing to start. |
| `windows-ops.lisp` | The skill, one prose `define-skill`, whole: the quick-reference table, the five-step procedure, the pitfalls (interop, process lifecycle, encoding, path) and the verification. |
| `references/diagnostic-commands.md` | The command reference the skill names, at the path it names: which layer, the orphan sweep, what reached the shell, the BOM check, the code page, the job-object state. |
| `test/cell-test.lisp` | Reads the folder as the index does: a knowledge cell with nothing to load, one prose skill with its sentence and its whole text, the index listing it, the reference beside it with its destructive lines still commented out. |

## License

MIT. The skill is Copyright (c) 2026 Yicheng-Mark: see `LICENSE` and `NOTICE`.
