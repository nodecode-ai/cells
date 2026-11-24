# nodecode-debug-desk

Debug Desk: today's git commits in the folder the session works in, plus the last shell command the agent ran that failed. A Nodecode rewrite of [aydnOktay/hermes-debug-desk](https://github.com/aydnOktay/hermes-debug-desk) by [aydnOktay](https://github.com/aydnOktay).

Two answers a developer asks for all day: what landed today, and what broke just now. Both come from this machine alone — git runs on the repository, the failure is the agent's own shell call, and the state file is under the Nodecode home.

## Get it

Tell the agent "add the debug-desk cell", or run `nodecode add nc://debug-desk`.

## Set it up

Nothing has to be set: `git` on `PATH` is the whole setup. `debug-desk.debounce_seconds` in the cell's `debug-desk` section (default 120) is how long a repeat of the same failure counts as the same failure.

## Use it

The model gets two tools:

| Tool | What it answers |
| --- | --- |
| `debug_daily_report` | Commits since local midnight in the folder it names (the session's own when empty): the count, the conventional-commit type counts (`feat 2, fix 1`), the files touched, the newest subjects. No model is called. |
| `debug_last_failure` | The last shell command the agent ran that failed: the command, its exit code, the folder, and the last eight lines. `summarize: true` adds three sentences from the session's own model. |

And a command of your own:

```
/debug-desk today             commits since local midnight
/debug-desk last              the last failed shell command
/debug-desk last summarize    the same, with a written summary
```

From eval, the same two answers:

```lisp
(debug-desk:today)                       ; the session's folder
(debug-desk:today "~/work/site")          ; another repository
(debug-desk:last-failure)
(debug-desk:last-failure :summarize t)
```

**What counts as a failure.** A shell tool call whose answer carries a nonzero `[exit N]` line, says `ERROR: ...`, or carries hints of a traceback, an `Error:`, `panic:` or `npm ERR!`. The answer's last eight lines are kept, each clipped to 240 characters, with common tokens, API keys, passwords and bearer headers replaced by `[REDACTED]` before anything is written. The same command with the same status and the same last line, twice inside `debounce_seconds`, is one failure counted, not a page of them.

**What it does not see.** A terminal outside this process: your own shell, your editor's, another app's. The desk is the agent's desk.

## Test

```sh
.github/test-cell.sh tools/nodecode-debug-desk
```

The tests need no network: the digest runs git on a throwaway repository the test makes, the failure is an answer the test hands the cell's own hook, and the summary asks a Lisp function instead of a model.

## What is different from the Hermes plugin

Hermes drew the desk in a desktop pane with a status chip that turned red for a fresh failure; Nodecode has no cell-owned pane, so the same data answers as text — the tools, `/debug-desk`, and `debug-desk:today` / `debug-desk:last-failure` from eval. The plugin watched Hermes's `terminal` and `process` tools; the cell watches the shell tool calls Nodecode's engine makes. Nothing else changed: the same two tools, the same redaction, the same eight lines, the same 120-second debounce, the same git log since local midnight.
