# nodecode-opencode-free

OpenCode Free: OpenCode's free models through the OpenCode CLI, with Nodecode keeping the tools. A Nodecode rewrite of [cryptobredda/hermes-opencode-free-acp](https://github.com/cryptobredda/hermes-opencode-free-acp) by [cryptobredda](https://github.com/cryptobredda).

Pick `opencode-free/<model>` in `/models` and each model round is one prompt
to your own signed-in `opencode` CLI, spoken over the [Agent Client
Protocol](https://agentclientprotocol.com) on stdio. The agent loop stays
Nodecode's: the cell hands the CLI the round's system prompt, its history,
its tool schemas and its previous tool results as text, and the call the
model asks for comes back in that text, is parsed against the schemas the
round advertised, and runs as a Nodecode tool, with Nodecode's approvals
and its usage line.

- The CLI's own native tools are off the table: the cell declares no file,
  terminal or permission capability, answers a permission ask "cancelled",
  and fails the round when the CLI reports a native tool call. What the
  model may do, it asks Nodecode for in words.
- Both the agent mode (`build`) and the model must be confirmed by the CLI
  before a prompt is sent, so a failed selection never falls back to a
  possibly paid default.
- Only the curated free ids can be selected. Free availability, limits and
  privacy terms are the vendor's and can change; a model priced "free"
  today is not a promise.

It is text-only and buffered: a round's answer arrives whole, not token by
token, the CLI is stopped at the end of every round, and token usage is not
reported, so the Dashboard's cost for a round is unknown rather than zero.

## Get it

Tell the agent "add the opencode-free cell", or run `nodecode add nc://opencode-free`.

## Set it up

There is no key here: the credential is the login your OpenCode CLI already
has. Install the CLI, run `opencode` once and sign in, then install the cell.
When no `opencode` is on PATH the cell says so at boot; point it at the
binary with the section's `command` and `(restart-cells)`.

This is optional; the section is on unless it says `"enabled": false`:

```jsonc
{
  "opencode-free": {
    "command": "opencode",
    "args": ["acp"],
    "models": ["glm-5.3-flash-free", "deepseek-v4.1-flash-free"],
    "timeout_seconds": 600
  }
}
```

See [`config.example.jsonc`](config.example.jsonc).

## Use it

Choose `opencode-free/glm-5.3-flash-free` (or `deepseek-v4.1-flash-free`,
`mimo-v2.6-flash-free`, `nemotron-3.5-super-vl-free`,
`nemotron-3.5-lightning-free`, `beam-free`, `big-pickle`) in `/models` and
work as usual. The CLI runs in the session's own folder, so what it is told
about the working directory is what Nodecode's prompt names. A round that
fails says why in the turn's error: a missing CLI, a model the CLI no
longer offers, or no answer at all.

## Before you use it

This is not an authorization to use OpenCode or any model: your own
account, the vendor's terms and any limits on your workload apply. Some free
tiers use what you send to improve their models, so treat a round as leaving
this machine. The CLI is a trusted executable running as you, and its own
configuration, plugins and login are its own; the cell's protocol refusals
are not an operating-system sandbox.

## Test

The tests need neither the CLI, a key, a model nor the network: a whole
round runs against `test/fake-acp.sh`, a stand-in ACP server under
`/bin/sh`.

```sh
sbcl --non-interactive --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(push #p"/path/to/cells/providers/nodecode-opencode-free/" asdf:*central-registry*)' \
  --eval '(asdf:test-system "nodecode-opencode-free")'
```

MIT licensed; see [LICENSE](LICENSE) and [NOTICE](NOTICE).
