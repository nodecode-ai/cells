# nodecode-kiro-acp

Kiro (AWS): the Kiro CLI's agent as a Nodecode provider, over the Agent Client Protocol. A Nodecode rewrite of [tobenwarrior/hermes-kiro-provider](https://github.com/tobenwarrior/hermes-kiro-provider) by [tobenwarrior](https://github.com/tobenwarrior).

A turn on a `kiro-acp/<model>` runs in the CLI. The cell spawns
`kiro-cli acp`, speaks the
[Agent Client Protocol](https://agentclientprotocol.com) down its pipes
(`initialize`, `session/new`, the model and the reasoning effort, then one
`session/prompt`), and folds the streamed answer back into the turn as its
text and its calls. No API key is stored: the login is the Kiro CLI's own,
made with `kiro-cli` on this machine.

## Get it

Tell the agent "add the kiro-acp cell", or run `nodecode add nc://kiro-acp`.

## Set it up

Install the [Kiro CLI](https://kiro.dev/docs/cli/) and sign it in once,
outside Nodecode (`kiro-cli`, then its login). Then name the CLI, which is
the section's one required setting:

```jsonc
{
  "kiro-acp": {
    "command": "kiro-cli"
  }
}
```

`command` is a name on PATH or a path. The rest is optional:

```jsonc
{
  "kiro-acp": {
    "command": "kiro-cli",
    "args": ["acp"],
    "models": ["default"],
    "effort": ""
  }
}
```

`args` is the whole argv after the CLI's name: `acp` is the ACP transport,
and the CLI's own flags ride here — `--agent NAME`, `--model NAME`,
`--effort high`, `-a` to trust its tools. `models` is what `/models`
offers under `kiro-acp/`: name the ids your `kiro-cli` reports for your
account; `default` (the one offered with no config) sends no model and
keeps the agent's own session model, which is what the original did when a
turn named the provider itself. `effort` is the session's reasoning effort
when the turn names none; empty leaves the CLI's own default.

## Use it

Pick a model under `kiro-acp/` in `/models` and talk. A round is one CLI
child: `session/new` in the session's folder, then one `session/prompt`
carrying the conversation and the tools as OpenAI function schemas. The
model answers with `<tool_call>{...}</tool_call>` blocks in its text, which
the cell parses back into the round's tool calls, so Nodecode runs the tools
as it does for every other provider.

The CLI's own file requests (`fs/read_text_file`, `fs/write_text_file`)
are served by this image, and a permission ask is answered with the first
allow the CLI offers, since a round is headless. The Kiro agent's built-in
tools run inside the CLI's own process, under the account that owns the
login — not through Nodecode's tool loop.

When no `kiro-cli` is on PATH the cell says so once and leaves the provider
out.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-kiro-acp
```

MIT licensed.
