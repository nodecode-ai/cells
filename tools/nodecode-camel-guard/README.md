# nodecode-camel-guard

CaMeL Guard: gate a turn's changing tool calls on the operator's own words once
that turn has read tool output — the part of a prompt injection that decides what
happens next. A Nodecode rewrite of
[nativ3ai/hermes-camel-guard-plugin](https://github.com/nativ3ai/hermes-camel-guard-plugin)
by [nativ3ai](https://github.com/nativ3ai).

CaMeL's rule is one sentence: a value that came from data may not decide what to
do. So the guard splits a turn in two. Before any tool output, a call runs as it
always did. After it, every call that changes something is held against a single
source — the message the operator typed, classified into capabilities — and
nothing else: not the page the agent just read, not a file's contents, not the
agent's own earlier sentence. A prompt injection lives in exactly the place the
guard does not listen.

## Get it

Tell the agent "add the camel-guard cell", or run

```sh
nodecode add nc://nodecode-camel-guard
```

## Set it up

No key: the classification runs on the session's own auxiliary model.

The mode starts **off**, so installing it changes nothing. Pick one:

- `/camel monitor` — run everything, keep what enforce *would* have refused
  (`/camel trace` reads them back).
- `/camel enforce` — refuse them; the call fails, and the model is told which
  capability was missing and which tool had tainted the turn.

The command saves the pick to the config, and takes effect at the next call. The
same pick is in the `camel-guard` section, which `/settings` draws:

```jsonc
"camel-guard": {
  "enabled": true,
  "mode": "monitor",                       // off, monitor, enforce
  "read_only_tools": ["board"],           // audited names: never gated
  "tool_capabilities": { "deploy": "external_system" },  // pin a tool's capability
  "trace_size": 50
}
```

Start in monitor for a day, read `/camel trace`, then enforce.

## Use it

Nothing to type: it sits on the tool call.

What a call needs, it is read off the call. Nodecode has one tool, eval, so the
guard reads the *form*: `sh`, `run-program` and `launch-program` are command
execution; `write-file`, `edit` and `delete-file` are file mutation; `fork` is
delegation; `cron-add` a scheduled action; `http-fetch` external-system side
effects; `new-cell` workspace mutation. A comment naming `sh`, or a file body
containing one, is not a call and needs nothing. A tool the guard does not know
needs external-system authority — the original's conservative default — and a
name the operator listed in `read_only_tools` needs nothing at all.

Refusals name the capability, the tools whose output tainted the turn, and the
policy's reason (`trusted_intent_missing`, `explicitly_denied`,
`fallback_read_only` when the classifier could not be asked, `missing_trusted_turn`
when the log does not carry the message). In enforce, a classifier that fails
authorizes nothing — the original's fail-closed rule.

A speed bump, not a security boundary, as the original says of itself: a form
that builds its command out of a variable, or calls a verb the table does not
name, shows no effect and passes. It catches the ordinary shape of an injected
instruction — read a page, then act on it.

`/camel status` says what the guard is doing; `(help :camel-guard)` is the
manual; `(help :guard)` is the core's other, simpler refusal, which reads
commands rather than authority.
