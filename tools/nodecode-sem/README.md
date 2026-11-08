# nodecode-sem

Sem + Weave: entity-level code discovery and review with the local Sem CLI, plus branch merge previews with Weave. A Nodecode rewrite of [Ataraxy-Labs/sem](https://github.com/Ataraxy-Labs/sem) by [Ataraxy-Labs](https://github.com/Ataraxy-Labs).

The original is two skills that tell an agent to type `sem` and `weave` commands at a terminal. This cell does the typing: four tools, one command and one manual topic, with the CLIs' own answers, no shell in between.

## Get it

Tell the agent "add the sem cell", or run `nodecode add nc://sem`.

## Set it up

Install [Sem](https://github.com/Ataraxy-Labs/sem#install) yourself, and [Weave](https://github.com/Ataraxy-Labs/weave#install) if you want merge previews. Tested upstream with Sem 0.27.0 and Weave 0.5.4. Check `sem --version`: GNU Parallel also ships an unrelated executable named `sem`.

The cell needs no key and no section to run: it finds `sem` and `weave` on `PATH`. `/sem` reports which of the two this machine has. When one lives elsewhere, name it:

```jsonc
"sem": { "sem_path": "/opt/homebrew/bin/sem", "weave_path": "weave" }
```

| Setting | Default | What it is |
| --- | --- | --- |
| `sem_path` | `sem` | the Sem executable, on `PATH` |
| `weave_path` | `weave` | the Weave executable; only previews need it |
| `directory` | empty | the repository the CLIs run in; the asking session's directory when empty |
| `offline` | `true` | run both CLIs with `SEM_NO_NETWORK=1 SEM_NO_TELEMETRY=1` |
| `timeout` | `120` | seconds one call may take before its process tree is killed |
| `max_chars` | `30000` | how much of one answer the model is given |

A present section is an enabled one; `"enabled": false` vetoes it. `config.example.jsonc` carries all of it, commented.

## Use it

The model gets four tools, and asks for them by name:

| Tool | What it answers |
| --- | --- |
| `sem_find` | where an entity is defined, who calls it, or its body with the context around it (`names`, `callers`, `context`, `budget`, `in`) |
| `sem_diff` | changed entities, not changed lines, for a range or the working tree (`range`) |
| `sem_impact` | what depends on an entity, and which tests reach it (`name`, `tests`, `file`) |
| `weave_preview` | how a branch would merge into HEAD, without merging it (`branch`, `file`) |

So "find `parse_config` and its callers, then review the uncommitted entity changes" is one or two calls rather than a shell command per question.

`/sem` with no arguments lists both programs and their versions; `/sem find parse_config --json` runs Sem itself, the arguments split on whitespace and never through a shell. The manual topic `(help :sem)` carries the guidance the original skills gave: choose the smallest query, an empty caller or test list is not proof there is none, do not invent a base branch, and a clean merge preview is not compiler or behavioral verification.

Nothing here edits, stages, commits or merges, and `sem check` -- which runs the repository's own build, linter and tests -- is not one of the tools. Sem may keep a local index outside the repository; with `offline` on, neither CLI is given the network.

The tests stub every CLI call: `NODECODE_SRC=<nodecode checkout> .github/test-cell.sh tools/nodecode-sem` from the cells repository.

## License

MIT. The plugin it rewrites is Copyright (c) 2026 Ataraxy Labs: see `LICENSE` and `NOTICE`.
