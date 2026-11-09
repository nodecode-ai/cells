# nodecode-chutes

Chutes: its open models on a pay-as-you-go key, and a model list cut down to
the ones that take tool calls, as a Nodecode provider. A Nodecode rewrite of
[TheStreamCode/hermes-chutes-provider](https://github.com/TheStreamCode/hermes-chutes-provider) by [TheStreamCode](https://github.com/TheStreamCode).

With it, `/connect` offers Chutes, `/models` lists its models, and a turn on
`chutes/<model>` goes to `https://llm.chutes.ai/v1` over the OpenAI chat
lane. What the original adds, and this cell keeps:

- the model list is Chutes' own catalog: `/chutes` reads `GET /v1/models`
  and puts the models whose `supported_features` name `tools` in `/models`,
  so a chute that takes no tool call is never offered to an agent; the
  original's two meta-models (`default:latency`, `default:throughput`) stand
  in until the command runs
- the window a model publishes (`max_model_len`) rides in the catalog entry,
  so `/models` shows it
- the key is `CHUTES_API_KEY`, and no other provider's variable is ever sent
  to Chutes; the base is the section's `base_url`, else `CHUTES_BASE_URL`,
  else `https://llm.chutes.ai/v1`

Ported from [TheStreamCode/hermes-chutes-provider](https://github.com/TheStreamCode/hermes-chutes-provider).
See [NOTICE](NOTICE).

## Get it

Tell the agent "add the chutes cell", or run `nodecode add nc://chutes`.

## Set it up

Make a key at <https://chutes.ai/app/api> and save it with `/connect`, or set
`CHUTES_API_KEY`. A saved key wins over the variable.

The section is on unless it says `"enabled": false`; its one member is
optional:

```jsonc
{
  "chutes": {
    // where Chutes is served; the chat lane appends /chat/completions
    "base_url": "https://llm.chutes.ai/v1"
  }
}
```

## Use it

```
/chutes            # read Chutes' catalog and list the models that take tool calls
/chutes deepseek   # only the models whose id carries "deepseek"
```

Then pick a model in `/models` and talk to it as usual. `default:latency`
and `default:throughput` are Chutes' own meta-models: the service picks the
chute, by the answer's latency or its throughput. A model the catalog did not
mark as tool-capable is still selectable in `providers.chutes.models` if you
know it takes tools.

MIT licensed.
