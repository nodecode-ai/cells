# nodecode-run-debugger

Agent Run Debugger: an imported agent run's timeline, diagnosis and inert replay, offline. A Nodecode rewrite of [Sahil-SS9/agent-run-debugger](https://github.com/Sahil-SS9/agent-run-debugger) by [Sahil-SS9](https://github.com/Sahil-SS9).

Hand it one trace file and it tells you what the evidence supports: the order of what happened (from the recorded causal parents, not from the clock), what the source itself reports as failed, the hypotheses over it — each with its alternatives and what would falsify it — the evidence that is missing, and a replay that walks the recorded timeline and executes nothing. A completed run is never reported as a passed task, and an unknown cost stays unknown.

## Get it

Tell the agent "add the run debugger cell", or run `nodecode add nc://run-debugger`.

## Set it up

No key and no address: this cell reads files on this machine and talks to nothing. Every member of the `run-debugger` section has the original plugin's default, so an absent section is a working cell.

| Member | What it is |
| --- | --- |
| `max_bytes` | Hard cap on bytes read from one selected trace (default 67108864). |
| `max_line_bytes` | Hard cap on one JSONL line (default 8388608), so an embedded prompt snapshot cannot swallow the file. |
| `max_events` | Hard cap on records accepted from one trace (default 200000). |
| `allow_partial` | Keep the valid prefix of a truncated final line, marked PARTIAL with an import gap (default off). |
| `retention_days` | How long an imported run is kept under the home (default 90); only files this cell wrote are ever deleted. |

## Use it

The tools the model calls, all local:

| Tool | What it does |
| --- | --- |
| `rundebug_import` | Import one or more trace files you name. A directory, an archive or a raw database is refused; nothing is extracted and nothing runs. |
| `rundebug_timeline` | The recorded order — causal parents first, source sequence as the tie-break — with gaps and concurrent pairs left visible. |
| `rundebug_explain` | What the source reports, the ranked hypotheses over it, the missing evidence, the read-only next checks, and the cost as reported. |
| `rundebug_replay` | The same order walked: imported command and error text is shown as data, never executed. |
| `rundebug_status` | The runs held, the lanes, the bounds, and where the runs are kept. |

The same verbs are one command, `/rundebug import FILE`, `/rundebug timeline RUN`, `/rundebug explain RUN`, `/rundebug replay RUN`, `/rundebug report RUN`, `/rundebug runs`, `/rundebug status`, and functions in the `rundebug` package for eval. `/rundebug report` writes one markdown file under the home and answers its path.

The trace is JSONL, one JSON object a line, in the original plugin's own format: a `trace` envelope (`"schema": "hermes-run-debugger/trace"`, `"version": 1`), then `attempt` and `event` records. An event names its kind (`tool_call`, `model_call`, `provider_error`, `interrupt`, …), what it reports about itself (`ok`, `failed`, `interrupted`, `skipped`, `unknown`), where that came from (`observed`, `reported`, `inferred`, `unknown`), its `source_sequence`, and its `causal_parents`; a parent the trace does not carry becomes a visible gap rather than an invented edge.
