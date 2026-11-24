# nodecode-telnyx

Telnyx: hosted open-weight models and proxied frontier routes, on an OpenAI-compatible API. A Nodecode rewrite of [team-telnyx/hermes-inference-provider](https://github.com/team-telnyx/hermes-inference-provider) by [team-telnyx](https://github.com/team-telnyx).

Telnyx serves inference at `https://api.telnyx.com/v2/ai/openai` (chat
completions: streaming, tool calling, usage on the final chunk) plus a
standard authenticated `/models` catalog that mixes models on Telnyx's own GPU
infrastructure (Kimi, GLM, MiniMax) with proxied frontier routes
(`openai/...`, `anthropic/...`). With this cell, `/connect` offers Telnyx,
`/models` lists what your key can see, and a turn on `moonshotai/Kimi-K3`
(or any other catalog id) goes to that base:

- the live catalog is the core's own listing (`GET /v2/ai/openai/models`
  with your key as its Bearer); the models in this folder are the fallback
  the picker shows before that answers, and the ones the original ships
- no output cap is volunteered. A cap combined with tool calls trips
  Telnyx's error 10015 on hosted models, and no catalog row predicts which
  models reject it, so a Telnyx round carries `max_tokens` only when you
  configured a ceiling yourself

## Get it

Tell the agent "add the telnyx cell", or run `nodecode add nc://telnyx`.

## Set it up

Make a key in the Telnyx portal at
<https://portal.telnyx.com/#/app/api-keys>, then save it with `/connect`, or
set `TELNYX_API_KEY`. A saved key wins over the variable. The key is required
for the catalog too: an anonymous `/models` request answers 401, and the
picker then shows only the fallback rows.

The address is optional, for a relay that speaks the same wire:

```jsonc
{
  "telnyx": {
    "base_url": "https://api.telnyx.com/v2/ai/openai"
  }
}
```

## Use it

Pick Telnyx in `/models` and a model from the list, then talk as usual.
Models are named by their catalog id, e.g. `moonshotai/Kimi-K3`,
`zai-org/GLM-5.2` or `MiniMaxAI/MiniMax-M3-MXFP8`. Pricing is per 1M tokens
on Telnyx's [model docs](https://developers.telnyx.com/docs/inference/models).

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-telnyx
```

MIT licensed. See [NOTICE](NOTICE).
