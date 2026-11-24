# nodecode-coreweave

CoreWeave Inference: CoreWeave Serverless Inference, its endpoint and its live model catalog, as a Nodecode provider. A Nodecode rewrite of [coreweave/serverless-inference-for-hermes](https://github.com/coreweave/serverless-inference-for-hermes) by [coreweave](https://github.com/coreweave).

With it, `/connect` offers CoreWeave Inference, `/models` lists the models
your key can use, and a turn on a CoreWeave model goes to
`https://api.inference.wandb.ai/v1` over the OpenAI chat lane, as the
original sends it:

- the provider's name, endpoint and key variable are the cell's catalog row,
  so nothing has to be typed in as a custom endpoint
- the model list is CoreWeave's own: `/coreweave` reads `GET /models` with
  your key and puts what it answers in `/models`. The original's provider
  ships no fallback list, and neither does this one
- the key is `COREWEAVE_API_KEY`, the variable the original reads; a key
  `/connect` saved answers first, and no other provider's key is ever sent to
  CoreWeave

See [NOTICE](NOTICE).

## Get it

Tell the agent "add the coreweave cell", or run `nodecode add nc://coreweave`.

## Set it up

Make an inference-enabled W&B API key at <https://wandb.ai/authorize> and
save it with `/connect`, or set `COREWEAVE_API_KEY` in the environment
Nodecode runs in. A saved key wins over the variable.

The section is on unless it says `"enabled": false`; its one member is
optional:

```jsonc
{
  "coreweave": {
    // where CoreWeave Serverless Inference is served; the chat lane
    // appends /chat/completions
    "base_url": "https://api.inference.wandb.ai/v1"
  }
}
```

## Use it

```
/coreweave            # read CoreWeave's model list and put it in /models
/coreweave kimi       # only the models whose id carries "kimi"
```

Then pick a model in `/models` and talk to it as usual. An empty or failed
list is not proof that a model is unavailable: adding one by hand under
`providers.coreweave.models` works the same way it does for any provider.

MIT licensed.
