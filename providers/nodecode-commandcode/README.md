# nodecode-commandcode

Command Code: the models of a Command Code account as a Nodecode provider, over the same `/alpha/generate` wire the Command Code CLI speaks. A Nodecode rewrite of [NanamiMio/hermes-commandcode-oauth](https://github.com/NanamiMio/hermes-commandcode-oauth) by [NanamiMio](https://github.com/NanamiMio).

With it, `/connect` offers Command Code, `/models` lists its models, and a
turn on `commandcode/<model>` goes to `https://api.commandcode.ai/alpha/generate`
the way the vendor's own CLI sends it: the CLI's client headers, the key the
CLI already stored, and the CLI's six-digit-version wire.

## Get it

Tell the agent "add the commandcode cell", or run `nodecode add nc://commandcode`.

## Set it up

Sign in with the Command Code CLI once (`~/.commandcode/auth.json` then holds
the key), or save a key from <https://commandcode.ai> with `/connect`, or set
`COMMANDCODE_CLI_TOKEN`. A key saved with `/connect` wins over both; the CLI's
file is only ever read, never written.

The section is optional and on unless it says `"enabled": false`:

```jsonc
{
  "commandcode": {
    // where Command Code is served; the lane appends /alpha/generate
    "base_url": "https://api.commandcode.ai",
    // the file the CLI keeps its sign-in in
    "cli_auth_file": "~/.commandcode/auth.json",
    // import the CLI's stored key when nothing else answered
    "read_cli_login": true
  }
}
```

## Use it

Pick `commandcode` in `/models`, then a model: the CLI's snapshot lists
DeepSeek V4/V4.1, Kimi K3, GLM 5.3 Flash, MiniMax M3/M3.1, Qwen3.7 Max and
Qwen3.8 Omni Flash, the Muse Spark pair, and the zero-cost Laguna S 2.1. A
turn carries the conversation, the tool schemas, the working directory and the
OS/architecture to Command Code. Images ride to the models that take them.

`/models` asks the account's own catalog (`GET /provider/v1/models?view=client`)
first and falls back to the snapshot when the account cannot be asked.

## What the wire is

The request is the CLI's own body: a workspace `config` block, an `agent`
mode, and `params` carrying the model, the messages as typed parts (text,
reasoning, `tool-call`, `tool-result`, `image`), one `system` string, the
tools as `input_schema` declarations, the output budget and `stream: true`.
The answer is NDJSON: `text-delta`, `reasoning-delta`, a whole `tool-call`,
`finish` with `totalUsage` (the cached part of the prompt apart from the
rest). An in-stream `{"type": "error"}` on a 200 is a failure carrying its
status, and a stream that ends without `finish` fails closed rather than
handing a turn an empty answer.

## Not ported

See [NOTICE](NOTICE).

MIT licensed.
