# nodecode-impossibl

Impossibl: one OpenAI-compatible key for 100+ models from OpenAI, Anthropic, Google, xAI, DeepSeek, Moonshot, Z.ai, Qwen, MiniMax and more. A Nodecode rewrite of [impossiblco/hermes-impossibl](https://github.com/impossiblco/hermes-impossibl) by [impossiblco](https://github.com/impossiblco).

With it, `/connect` offers Impossibl, `/models` lists its models, and a turn
on `impossibl/<creator>/<model>` goes to `https://api.impossibl.com/v1` over
the OpenAI chat wire, which is what the gateway speaks.

Impossibl's `/v1/models` answer is no plain listing: every model names the
endpoints it serves in `endpoints`, and whether it takes tool calls in
`supports_tools`. A Nodecode turn always offers tools, so only the models
serving `/v1/chat/completions` that do not declare `supports_tools: false`
are listed; image, speech, transcription, embedding and rerank models are
left out. A catalog that cannot be read, or that names no such model, leaves
the core's own listing standing, so a degraded picker is never an empty one.
Set `chat_only` to `false` to list every model the core reads.

Ported from the Hermes provider plugin of the same name. See [NOTICE](NOTICE).

## Get it

Tell the agent "add the impossibl cell", or run `nodecode add nc://impossibl`.

## Set it up

Make a key at <https://impossibl.com>. Save it with `/connect`, or set
`IMPOSSIBL_API_KEY`; a saved key wins over the variable, and no other
variable's key is ever sent to Impossibl.

The section is on unless it says `"enabled": false`:

```jsonc
{
  "impossibl": {
    // where Impossibl is served; a relay's address goes here
    "base_url": "https://api.impossibl.com/v1",
    // list only the models routed through chat completions that take tools
    "chat_only": true,
    // one catalog read's ceiling, in seconds
    "timeout_seconds": 8
  }
}
```

The plugin's `IMPOSSIBL_BASE_URL` variable is this section's `base_url`: a
base the section does not serve is a relay's catalog, not Impossibl's, and
its models are then listed as the core reads them.

## Use it

`/models`, pick Impossibl, pick a model. Without a key the fallback roster is
still listed — eleven flagships across the labs Impossibl fronts, the plugin's
own `fallback_models` — so the picker has something to offer before the live
catalog answers.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-impossibl
```

MIT licensed.
