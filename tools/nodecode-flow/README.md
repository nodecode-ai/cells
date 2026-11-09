# nodecode-flow

Flow: recorded agent sessions as a tree of lineage, handoffs and tool use, and a trusted-paths baseline a later run is compared with. A Nodecode rewrite of [rewanthtammana/hermes-flow](https://github.com/rewanthtammana/hermes-flow) by [rewanthtammana](https://github.com/rewanthtammana).

A session is an agent. A session that was spawned by another hangs under it;
so does one forked from it. Every tool call the log recorded is a row under
its agent, with how often it ran, how many of those failed and how long they
took. `/flow` reads this machine's own event log, so it shows what already
happened: no new agent is run and nothing is changed. When a person wants to
know whether a later run went somewhere new -- a handoff nobody reviewed, a
browser tool an agent never had -- they save a baseline once, edit it if
needed, and ask `/flow compare` afterwards.

## Get it

Tell the agent "add the flow cell", or run `nodecode add nc://flow`.

## Set it up

Nothing is required: the log is already there. The `flow` section of
`~/.nodecode/config.jsonc` (or `/settings`) tunes it:

```jsonc
{
  "flow": {
    "max_sessions": 20,   // sessions a /flow listing shows
    "max_calls": 12,      // tool rows under one agent
    "baseline": ""       // trusted-paths JSON /flow compare uses when it names none;
                           // empty means flow/trusted-paths.json under this home
  }
}
```

## Use it

```text
/flow                      the newest sessions, one a line, with their call counts
/flow show SESSION         that session and its descendants as a tree
/flow mermaid [SESSION]    the same flow as a mermaid flowchart
/flow baseline [PATH]      write the trusted connections a later run is compared with
/flow compare [PATH]       the connections the flow has that the baseline does not, and the reverse
/flow report [PATH]        one self-contained HTML page of the flow
```

A tree reads like this, with a new connection marked when a baseline is given:

```text
Flow · 3 agents · 9 calls · 1 handoff
Recorded lineage and tool use; an edge does not prove data transfer.
├── review the parser [s-4KD2] · updated 2026-11-09 21:54
│   · sh x4 · 1 failed · 0.2s
│   · edit x2
└── audit the diff [s-9QM1] · handoff · updated 2026-11-09 22:10
    · read x3 · 0.1s
```

The model has two tools too: `flow_tree` (one session's flow) and
`flow_compare` (a flow against a baseline), so it can answer "what did the
agents do" or "did anything go somewhere new" without a slash command.

Nothing is dialled: no key, no network, no telemetry. A baseline is plain
JSON a person can read and edit, and the page is a single file with no
external resource.

## Credit

The original is MIT, Copyright (c) 2026 Hermes Flow contributors; see
[LICENSE](LICENSE) and [NOTICE](NOTICE).
