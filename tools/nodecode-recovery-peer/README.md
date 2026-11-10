# nodecode-recovery-peer

Recovery Peer: one bounded second opinion from another model when a tool call keeps failing or stalls. A Nodecode rewrite of [rezaaibox-ui/nexus-recovery-peer](https://github.com/rezaaibox-ui/nexus-recovery-peer) by [rezaaibox-ui](https://github.com/rezaaibox-ui).

The agent that is stuck is the last thing that will notice. When the same call
fails the same way -- or returns the same output -- three times in a row, this
cell asks a model on a provider of your own for at most two *different* next
steps and how to check each, and appends them to the result the agent is about
to read. The agent still decides, and nothing the peer says counts as verified
until it is run.

| The stall | What the cell does |
| --- | --- |
| The same call fails the same way, 3 times | asks once, on the third result |
| The same read returns the same output, 3 times | asks once, on the third result |
| Anything else | passes the result through untouched |

It never replays the call, never substitutes a model for the session's own, and
never asks twice for one stall. A loop costs one small call, not one per step.

## Get it

Tell the agent "add the recovery-peer cell", or run:

```sh
nodecode add nc://recovery-peer
```

## Set it up

It needs one setting, `peer`: the provider and model of the second opinion, as
`provider/model` -- a model you also have, on a provider other than the one you
run on. In `/settings`, or in the `recovery-peer` section of
`~/.nodecode/config.jsonc` (`config.example.jsonc` shows the whole section):

```jsonc
"recovery-peer": { "peer": "some-lane/some-model" }
```

Without it the section refuses, and `/cells` and the notices block say which
key is missing. `other_provider_only` (on by default) keeps the promise that
it is really a second opinion: a peer on the provider you are already using is
not asked.

## Use it

Once it is on, there is nothing to call: every tool call of every session
passes through the cell's `:TOOL` hook.

| Verb | What it answers |
| --- | --- |
| `/recovery-peer` | on or off, the peer, this session's streak, the last word |
| `/recovery-peer on` / `off` | start or stop watching tool results |
| `/recovery-peer ask` | ask the peer now, about the last stall |
| `/recovery-peer reset` | forget every streak and the last word |
| `recovery_peer` tool | the model asks by hand: `task`, `failure`, `tool` |
| `(help :recovery-peer)` | the same, as prose |

The peer sees the task (the operator's newest ask, when the log can say), the
call as `eval(form)` and how it ended, each clipped to `clip` characters, with
the obvious credentials replaced by `[redacted]`. Redaction is best-effort,
not a security boundary: do not name a peer whose provider should not see this
conversation's error text.

## Test it

```sh
.github/test-cell.sh tools/nodecode-recovery-peer
```

The tests are offline: the peer is bound to scripted answers, so "the third
identical failure is the one that asks", "one stall asks once" and "a peer
that raises answers a refusal instead of failing the call" are facts a test
reads rather than a provider's mood.
