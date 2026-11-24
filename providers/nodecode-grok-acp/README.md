# nodecode-grok-acp

Grok: your xAI Grok subscription as a Nodecode provider, through the `grok` CLI you are already logged in to. A Nodecode rewrite of [nicolasdmolina/hermes-grok-acp](https://github.com/nicolasdmolina/hermes-grok-acp) by [nicolasdmolina](https://github.com/nicolasdmolina).

A turn on a `grok-acp/<model>` runs in the CLI. The cell spawns
`grok agent --no-leader stdio`, speaks the
[Agent Client Protocol](https://agentclientprotocol.com) down its pipes
(`initialize`, `session/new`, the model and the reasoning effort, then one
`session/prompt`), and folds the streamed answer back into the turn as its
text and its calls. `/models` lists the subscription's models, and no API
key is stored: the login is the CLI's own, in `~/.grok` (or `GROK_HOME`).

## Get it

Tell the agent "add the grok-acp cell", or run `nodecode add nc://grok-acp`.

## Set it up

Install the `grok` CLI and sign it in once, outside Nodecode (`grok` then
`/login`; `grok models` says `You are logged in`). Then name the CLI, which
is the section's one required setting:

```jsonc
{
  "grok-acp": {
    "command": "grok"
  }
}
```

`command` is a name on PATH or a path. The rest is optional:

```jsonc
{
  "grok-acp": {
    "command": "grok",
    "args": ["agent", "--no-leader", "stdio"],
    "models": ["grok-4.7", "grok-4.6", "grok-4.5"],
    "effort": ""
  }
}
```

`args` is the whole argv after the CLI's name: `stdio` is the ACP
transport, and `--no-leader` keeps the request off a shared Grok leader.
`models` is what `/models` offers under `grok-acp/` — name what the CLI
lists for your account. `effort` is the session's reasoning effort when the
turn names none (`xhigh`, `high`, `medium`, `low`); empty leaves the CLI's
own default. The turn's own effort, when Nodecode sets one, is what a round
sends.

## Use it

Pick a model under `grok-acp/` in `/models` and talk. A round is one CLI
child: `session/new` in the session's folder, then one `session/prompt`
carrying the conversation and the tools as OpenAI function schemas. The
model answers with `<tool_call>{...}</tool_call>` blocks in its text, which
the cell parses back into the round's tool calls, so Nodecode runs the tools
as it does for every other provider.

The CLI's own file requests (`fs/read_text_file`, `fs/write_text_file`)
are served by this image, and a permission ask is answered with the first
allow the CLI offers, since a round is headless. The CLI's built-in shell
runs inside the CLI's own process, under the account that owns the login —
not through Nodecode's tool loop.

When no `grok` is on PATH the cell says so once and leaves the provider out.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-grok-acp
```

MIT licensed.
