# nodecode-deepsec

DeepSec Shield: a local security review of a tree, as tools. A Nodecode rewrite of [Unclecheng-li/DeepSec](https://github.com/Unclecheng-li/DeepSec) by [Unclecheng-li](https://github.com/Unclecheng-li).

Four tools the model calls, one command the operator types. A scan reads a file or a
whole tree and answers a summary line and its findings, worst first: the rule, the
target and line, the evidence, and a suggested fix.

| Tool | What it answers |
| --- | --- |
| `deepsec_scan` | hardcoded credentials, code evaluated from a value, shell and SQL built from a value, markup written from a value, TLS verification off, a download piped into a shell, a weak hash, a world-writable mode, an unpinned install |
| `deepsec_agent_audit` | the same, plus what an agent's own prompts and configuration say: an instruction to drop earlier ones, to hide an action from the user, to send data to an address, to read a credential store, to run a download |
| `deepsec_supply_chain` | `package.json` and `requirements.txt`: a name one edit away from a popular one, a name declared private that a public registry answers for, a fetch over plain HTTP |
| `deepsec_report` | a saved scan's JSON, rendered as markdown, json, sarif or text |

## Get it

Tell the agent "add the deepsec cell", or run `nodecode add nc://nodecode-deepsec`.

## Set it up

Nothing is required. The `deepsec` section tunes it:

- `max_findings` (50) caps how many findings one scan answers; the rest are counted in
  the summary line.
- `include_tests` (off) reads `test`, `fixtures` and `examples` folders too.
- `severity_floor` (`info`) drops the milder findings from an answer; a saved result
  keeps every one.

## Use it

Ask for it in words — "scan ./src for hardcoded secrets" — or type `/deepsec src` to
scan where you work. From eval the same verbs are `(deepsec:scan "src")`,
`(deepsec:agent-audit "skills/")`, `(deepsec:supply-chain "." :private-packages '("acme-core"))`
and `(deepsec:report "result.json" :format "markdown")`, each answering a string.
`(help :deepsec)` carries the whole manual while the cell runs.

A finding is a lead, not a proof: a scan reads code, it never runs it. Test folders,
`node_modules`, `.git`, build output and binary files are skipped.

The rewrite does what the original's pattern layer did, in Common Lisp, against the same
finding shape. It does not run the original's separate scanner: see NOTICE.
