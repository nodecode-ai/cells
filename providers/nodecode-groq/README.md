# nodecode-groq

Groq: fast inference for open models, each model sent only the reasoning
effort it accepts. A Nodecode rewrite of
[AhmetArif0/hermes-groq-provider](https://github.com/AhmetArif0/hermes-groq-provider)
by [AhmetArif0](https://github.com/AhmetArif0).

Groq speaks the OpenAI chat wire at `https://api.groq.com/openai/v1`. With
this cell, `/connect` offers Groq, `/models` lists the models that can hold a
conversation, and a turn on one of them goes out in the form Groq takes:

- `reasoning_effort` is lowered to the nearest level the model accepts, never
  raised: gpt-oss takes `low`, `medium` and `high`, so `minimal`, `xhigh` and
  `max` become the closest of those, and `qwen3.8` also takes `none`
- turning reasoning off sends `none` where the model has it and its weakest
  level otherwise (gpt-oss always reasons, so it gets `low`)
- a model without reasoning is sent no `reasoning_effort` field at all, which
  Groq would otherwise refuse
- whisper, orpheus, prompt-guard and allam are left out of the picker: speech
  models, a 4K-window model and a classifier cannot run a conversation

The model list is Groq's own `/v1/models`, read once at start and refreshed
hourly, with models.dev's rows for context, prices and rungs where it
publishes them. Before either has been read, the list is the three chat models
the original named.

## Get it

Tell the agent "add the groq cell", or run `nodecode add nc://groq`.

## Set it up

Make an API key at [console.groq.com/keys](https://console.groq.com/keys),
then pick Groq in `/models` and paste the key when the picker asks, or set
`GROQ_API_KEY` in the environment. The address is optional, for a relay or a
proxy:

```jsonc
{
  "groq": {
    "base_url": "https://api.groq.com/openai/v1"
  }
}
```

## Use it

Ask for a model — `/models` lists what Groq currently serves — then talk as
usual. `/think` offers the rungs the chosen model takes; a pick it does not
take is lowered to one it does before the request leaves.

Groq's free plan allows 8,000 tokens a minute per model, and a turn with
tools is usually larger than that, so agent work wants the paid Developer
plan. That is Groq's limit; the cell does not change a request's size.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-groq
```

MIT licensed. See [NOTICE](NOTICE).
