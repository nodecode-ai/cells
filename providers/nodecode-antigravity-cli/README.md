# nodecode-antigravity-cli

Antigravity CLI: Gemini and Claude models through the `agy` CLI on an Antigravity subscription. A Nodecode rewrite of [soyelmismo/hermes-antigravity-subscription](https://github.com/soyelmismo/hermes-antigravity-subscription) by [soyelmismo](https://github.com/soyelmismo).

A turn on an `antigravity-cli/<model>` goes to the Antigravity CLI the user
already has, signed in on this machine, so the models ride the subscription
the account already pays for: `/models` lists them under
`antigravity-cli/`, and a round spawns one `agy` child with the prompt on its
standard input and `--output-format stream-json` on its output, folding the
events as they come.

## Get it

Tell the agent "add the antigravity-cli cell", or run `nodecode add nc://antigravity-cli`.

## Set it up

Install the Antigravity CLI (`agy`) and sign it in once, outside Nodecode.
Nothing else is required; the section is optional:

```jsonc
{
  "antigravity-cli": {
    "command": "agy",
    "models": ["gemini-3.8-flash", "gemini-3.1-pro", "gpt-oss-120b",
               "claude-sonnet-4-6", "claude-opus-4-6-thinking"],
    "effort": ""
  }
}
```

`command` is the CLI, a name on PATH or a path. `models` is what `/models`
offers under `antigravity-cli/` — name what `agy models` shows for your
account. `effort` is handed to the CLI as `--effort` when it is set
(`low`, `medium`, `high`), which is how the same model is asked to think
harder. No key is stored: the login is the CLI's own, and no request leaves
this image.

## Use it

Pick a model under `antigravity-cli/` in `/models` and talk. Tool calls come
back as `<tool_call>` blocks the cell parses into the round's calls, so
Nodecode runs the tools; a round in which the CLI takes a tool into its own
hands ends there rather than trusting it. When no `agy` is on PATH the cell
says so once and leaves the provider out.

## Test

From this repository's root:

```sh
tools/test-cell.sh providers/nodecode-antigravity-cli
```

MIT licensed.
