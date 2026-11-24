# nodecode-neuralmind

NeuralMind Code Memory: the project's related files, symbols and recorded decisions added to every turn. A Nodecode rewrite of [dfrostar/neuralmind](https://github.com/dfrostar/neuralmind) by [dfrostar](https://github.com/dfrostar).

The memory is [NeuralMind](https://github.com/dfrostar/neuralmind) itself, the
program the Hermes plugin drove: it indexes a project once — files, the
symbols in them, how they call one another, and the decisions recorded in
commit trailers — and answers what a prompt is about from that index, locally.
The cell is the harness half: it asks the program at the moments the plugin
asked it, and puts the answer into the turn.

## Get it

Tell the agent "add the neuralmind cell", or run `nodecode add nc://neuralmind`.

## Set it up

The cell drives the `neuralmind` program, so that has to be there first, and
the project has to be built once:

```sh
pip install neuralmind        # or: pipx install neuralmind, or: uv tool install neuralmind
cd your-project && neuralmind build .
```

Then name the executable in `/settings` under `neuralmind.command` — `neuralmind`
when it is on `PATH`, else its path. A section that names no program, or one
that is not there, refuses at start and says so.

| Setting | Default | What it is |
| --- | --- | --- |
| `command` | — | The `neuralmind` executable: its name on `PATH`, or a path to it. Required. |
| `project` | unset | The project to serve, ahead of each session's own folder. It must have been built. |
| `timeout` | `8` | Seconds one call may take before it is stopped and its block left out. |
| `recall` | on | Add the files, symbols and decisions this turn's prompt matches. |
| `recap` | on | Start a session with the recap of the previous session in the project. |
| `record_edits` | on | Record the files a write in the turn changed, for the next session's recap. |

Left empty, `project` follows each session: the folder the session works in is
served when it has been built, and a session anywhere else is served nothing —
the plugin's rule.

## Use it

Nothing to call. At a session's first turn the request starts with a recap of
where the last session in that project left off, then the files and symbols
this turn's prompt matches; every later turn carries its own match. The block
is part of the turn's request, so the model reads it before it answers:

```text
NeuralMind session recap - the previous session in this project (last active 2h ago)...
Files edited (1, most recent first): shop/pricing.py

## NeuralMind associative recall
Code matching this prompt:
- shop/pricing.py: add_discount() L18, add_tax() L9
Connected to it in the synapse graph:
- shop/checkout.py: total() L30
```

Ask by hand when you want it:

```
/neuralmind                      the project, the program and the switches
/neuralmind recall discount code  what this project's index matches
/neuralmind recap                 the previous session's recap
```

and from eval, `(neuralmind:recall "where is the discount code applied")` and
`(neuralmind:recap)`. `(help :neuralmind)` has the same guidance the model is
given.

A write is recorded too: after an `eval` that ran `(write-file ...)` or
`(edit ...)`, each file it named is handed to NeuralMind's `edit-activity`, so
the next session's recap lists it — off the turn's path, as the plugin did.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The program runner, the project rule, the `:round` and `:tool` hooks, the two verbs, `/neuralmind` and the section. |
| `package.lisp` | The `neuralmind` package: two verbs, the words, one condition. |
| `test/` | Offline tests over a stubbed program: the lifecycle, the blocks a first turn carries and the payloads behind them, the failed and empty writes, the refusals, and `/neuralmind`. |

## Not ported

- **The `read_file` recording.** Nodecode has no read tool: a model reads
  through `sh` or `eval`, and guessing which paths a shell command touched
  would record reads the model never made.
- **The code an edit sends back for reuse feedback.** Nodecode's writes are
  Lisp forms inside `eval`, not a file's content argument.
- **The plugin's installer and its `config.json`.** The config section and
  `PATH` are that; nothing is copied into a harness home.

MIT, NeuralMind's. See NOTICE.
