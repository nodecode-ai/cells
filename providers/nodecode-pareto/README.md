# nodecode-pareto

Pareto Inference: GLM 5.3 Flash, DeepSeek V4.1 Flash and MiMo V2.6 Flash,
pay per token. A Nodecode rewrite of
[pareto-inference/hermes-pareto-provider](https://github.com/pareto-inference/hermes-pareto-provider)
by [pareto-inference](https://github.com/pareto-inference).

Pareto speaks the OpenAI chat wire at `https://api.paretoinference.com/v1`.
With this cell, `/connect` offers Pareto Inference, `/models` lists its three
models, and a turn goes out in the form Pareto takes:

- the model is named `vendor/model` on the wire, so the picker's bare
  `glm-5.3-flash` is sent as `z-ai/glm-5.3-flash`
- `reasoning_effort` is sent top-level in Pareto's spelling: the picker's
  rungs go through as they are, `ultra` becomes `max`, and turning reasoning
  off sends `none` (Nodecode's own `off` rung is the wire's `none`)
- every request streams, which is what Pareto requires: a plain call is
  answered HTTP 400 `streaming_only`, and Nodecode's chat lane only streams

Pareto bills per token; its pricing is at
[docs.paretoinference.com/pricing](https://docs.paretoinference.com/pricing).

## Get it

Tell the agent "add the pareto cell", or run `nodecode add nc://pareto`.

## Set it up

Make an API key at
[paretoinference.com/dashboard](https://paretoinference.com/dashboard), then
pick Pareto Inference in `/models` and paste the key when the picker asks, or
set `PARETO_API_KEY` in the environment. The address is optional, for a relay
or a proxy:

```jsonc
{
  "pareto": {
    "base_url": "https://api.paretoinference.com/v1"
  }
}
```

## Use it

Ask for one of the models — `/models` lists what Pareto Inference serves —
then talk as usual. `/think` offers the rungs Pareto takes (`off` through
`max`); one it does not take is left out of the request, so Pareto's own
per-model default applies.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-pareto
```

MIT licensed. See [NOTICE](NOTICE).
