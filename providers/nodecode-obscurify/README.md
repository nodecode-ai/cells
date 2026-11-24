# nodecode-obscurify

Obscurify.ai: a privacy-focused, OpenAI-compatible API to 200+ models, listed
by the models a turn can run. A Nodecode rewrite of
[Obscurify-ai/hermes-obscurify-provider](https://github.com/Obscurify-ai/hermes-obscurify-provider)
by [Obscurify-ai](https://github.com/Obscurify-ai).

Obscurify.ai (https://obscurify.ai) serves the OpenAI chat wire at
`https://obscurify.ai/v1` with a bearer key. Accounts are a random account
number -- no email, no password -- and text requests use zero-data-retention
routes. With this cell, `/models` offers Obscurify.ai, `/models` lists its
models, and a turn on one of them goes out the way the API wants it:

- the model list is the API's own public `/v1/models` catalog, minus the
  decision models (they answer structured questions only, and refuse a chat
  request without their `response_format`) and minus the models whose context
  window is under the floor Hermes required, 64,000 tokens; the floor is the
  section's `min_context_tokens` here, so it can be lowered
- context windows come from that same catalog, so a turn's budget is the
  API's own number rather than a guess
- reasoning effort goes as the top-level `reasoning_effort`, the API's own
  spelling of the rungs `off` through `max`; a turn that turned reasoning
  off sends `none`
- before the catalog has answered, the picker lists the five featured models
  the plugin named, with the windows it checked for them

The catalog is public, so it is read without the key, once at start and
hourly after that, and kept in memory: a box with no network keeps the
featured list, and a failed refresh keeps the list already read. The key
rides only on chat turns.

## Get it

Tell the agent "add the obscurify cell", or run `nodecode add nc://obscurify`.

## Set it up

Make an account at [obscurify.ai](https://obscurify.ai) and add a balance:
the API works on every plan with a positive balance, frontier models want
the Casual plan or higher. On [obscurify.ai/account](https://obscurify.ai/account),
click **Show API Key**, then pick Obscurify.ai in `/models` and paste the key
when the picker asks, or set `OBSCURIFY_API_KEY` in the environment. A key
saved with `/connect` answers first, and no other provider's key is ever sent
to Obscurify.ai.

The address and the floor are optional, for a relay or a smaller context
budget:

```jsonc
{
  "obscurify": {
    "base_url": "https://obscurify.ai/v1",
    "min_context_tokens": 64000,
    "timeout_seconds": 8
  }
}
```

## Use it

Ask for a model -- `/models` lists what the catalog currently serves -- then
talk as usual. `/think` offers the rungs; a model the catalog says does not
reason is listed without them. Good first picks are
`deepseek/deepseek-v4-flash`, `z-ai/glm-5.3`, `moonshotai/kimi-k2-0905` and
`qwen/qwen3-coder`. A turn costs money from the Obscurify.ai balance at that
model's price.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-obscurify
```

MIT licensed. See [NOTICE](NOTICE).
