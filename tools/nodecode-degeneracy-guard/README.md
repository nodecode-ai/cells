# nodecode-degeneracy-guard

Degeneracy Guard: asks before a tool call whose argument looks like a repetition loop runs. A Nodecode rewrite of [simin-yuan/hermes-degeneracy-guard](https://github.com/simin-yuan/hermes-degeneracy-guard) by [simin-yuan](https://github.com/simin-yuan).

A model in a repeat loop can say "I am repeating" and still cannot stop.
That is the premise, kept from the original: the breaker runs outside the
model. In Nodecode one tool, `eval`, writes the files and runs the programs,
so a loop lands in the arguments of a call -- and a call is the only part of a
turn that changes anything outside the process.

The guard watches every tool call's arguments. When one of them is a payload
of at least 200 characters in at least 12 non-blank lines, has no data shape,
and repeats itself -- 18 identical consecutive lines, or a block of at most 20
lines repeated 18 times over 60% or more of the payload -- it asks you:

| Answer | What happens |
| --- | --- |
| **Allow once** | the call runs as written |
| **Deny** | the call is refused, and the agent is told you chose it |
| **No answer** in `ask_seconds` | the call is refused |
| **Nobody to ask** (no page or app open) | the call is refused |

A refusal is the tool result, so the agent reads why and can shorten the
payload instead of working around the guard.

## What counts as a data shape

Repetition is normal in data, so the guard leaves it alone. A payload passes
untouched when it is: JSON or JSONL, a markdown table, a checklist, XML or
HTML rows, SQL statements, TOML or YAML, a timestamped log, a numeric matrix,
a separator block, delimited records (CSV/TSV) whose rows differ, or a list of
code elements ending in a comma. A table of forty empty rows, forty lines of
`0,0,0,0`, a SQL file of identical INSERTs and a log where one heartbeat line
appears forty times all pass.

What it cannot do, from the original's own measurements: it matches exact
repeated lines, so a loop that rewords itself each pass, or the same sentence
behind a rising counter, passes. That is why it asks a person instead of
scoring harder.

## Get it

Tell the agent "add the degeneracy-guard cell", or run:

```sh
nodecode add nc://degeneracy-guard
```

## Set it up

No key, no account, nothing leaves the machine. The section is optional; the
defaults ask, and the thresholds are the original's.

```jsonc
{
  "degeneracy-guard": {
    "mode": "ask",           // ask | refuse | warn
    "allow_tools": [],      // tool names never judged
    "ask_seconds": 120,      // how long the question waits
    "incidents": 200,        // incident files kept, newest first
    "max_rounds": 500        // model requests before the long-session notice
  }
}
```

- `mode`: `ask` is the original's behaviour. `refuse` stops such a call
  without asking, for a machine nobody is watching. `warn` only records it
  and lets it run.
- `allow_tools`: the original's `DEGENERACY_GUARD_ALLOW_TOOLS`, as a list.
- `max_rounds`: the long-session notice, one at the threshold and one every
  50 requests after it. `0` never says it.

## Use it

Nothing to type while it works. To look at it:

```
/degeneracy              the mode, the thresholds, how many incidents are kept
/degeneracy check TEXT   judge a text as a tool argument would be judged
/degeneracy last         the newest incident
/degeneracy clear        drop the incident files
```

The tool `degeneracy_check(text, kind)` answers the same judgement for a text
the model hands it, without touching a call: `kind` is `argument` (data shapes
exempt, the rule the hook applies) or `output` (model text, judged line by
line). The guard never judges its own check tool, since the text it is handed
is exactly the text a call would be refused for carrying.

An incident is one JSON file under `degeneracy-guard/incidents/` in the
Nodecode home: the time, the tool, the criterion, the measurements, and the
last 800 characters of the argument. It is a local record of what the guard
did, kept to `incidents` files; nothing is sent anywhere.

## Testing

From this repository, offline against a pinned Nodecode tree:

```sh
.github/test-cell.sh tools/nodecode-degeneracy-guard
```

The tests cover the three output criteria and their nearest false positives,
the data-shape exemptions, the hook's three modes, the Allow once / Deny
answers, the exemption list, the tool and the command, the contract's refusal
of a call with no `text`, and the de-bounce of the long-session notice.
