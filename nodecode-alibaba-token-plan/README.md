# nodecode-alibaba-token-plan

A Nodecode cell for the [QwenCloud Token Plan](https://home.qwencloud.com/billing/subscription/token-plan-individual),
Alibaba's token subscription (Qwen3.8, Qwen3.7, GLM-5.2 and DeepSeek V4 Pro).
With it, `/connect` offers QwenCloud Token Plan, `/models` lists the plan's
models, and a turn on `alibaba-token-plan/<model>` goes to the plan's
endpoint in your region over the OpenAI chat wire, asking a reasoning model
to think in its own dialect.

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`alibaba-token-plan` provider. See [NOTICE](NOTICE).

## What omp's login does, and how this cell carries it

omp's login for this provider is a custom one, but it signs nothing in. The
Token Plan is sold as two regional products whose keys do not cross, so the
login asks:

1. Which region: 1 International, Singapore (the default,
   `https://token-plan.ap-southeast-1.maas.aliyuncs.com/compatible-mode/v1`),
   2 China, Beijing
   (`https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1`), or
   3 a custom base URL.
2. The key (`sk-sp-...`), from that region's console. It checks the key with
   `GET <base>/models`.
3. Optionally, the Cookie header of the console's usage request, copied from
   the browser's DevTools, for quota reporting.

It keeps a region other than the default inside the credential, so both
inference and model discovery go there. Here:

- **Region.** The section's `"region"`: `"international"` (the default) or
  `"china"`.
- **Custom base.** `"base_url"`, when set, wins over the region, as omp's
  option 3 does.
- **Key.** Save it with `/connect`, or set `ALIBABA_TOKEN_PLAN_API_KEY` (or
  `BAILIAN_TOKEN_PLAN_API_KEY`). A saved key wins over the variables.
  `/connect` checks a key against the region's `/models` listing before it
  saves it, as omp's login does. `OPENAI_API_KEY` is never sent: omp refuses
  that fallback for this provider by name.

Where to subscribe and copy the key:
<https://home.qwencloud.com/billing/subscription/token-plan-individual> for
the international plan, <https://www.aliyun.com/benefit/scene/tokenplan> for
the China one.

## Models

The bundled rows are the provider rule's seed: the plan's documented
Individual text models. They outrank a same-id row from models.dev, as omp's
seed precedence has it. The core also lists the region's `/models` once a
key is saved, which is omp's credentialed discovery.

## Thinking

omp's rule for this provider is the Qwen dialect: for a reasoning model, an
effort sends `enable_thinking: true` and no `reasoning_effort`, and `off`
sends `enable_thinking: false`. Qwen3.8 Max and Qwen3.8 Flash take both
while they think: `enable_thinking: true` and `reasoning_effort` for the
depth (Max's rungs are low, medium, xhigh). Not thinking, they send
`enable_thinking: false` like the rest. Qwen3.8 Max Preview stays on the
switch alone. With no effort set, nothing is sent, so the plan's default
applies, as the chat lane does for every provider.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-alibaba-token-plan/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "alibaba-token-plan": {
    // where the plan was bought: international (Singapore) or china (Beijing)
    "region": "international"
    // another regional endpoint, or a proxy; set, it wins over region
    // "base_url": "https://token-plan.<region>.maas.aliyuncs.com/compatible-mode/v1"
  }
}
```

## Not ported

- **Quota reporting.** omp reads the plan's usage from the console's private
  usage API, with a browser session cookie the operator copies out of
  DevTools at login. The cell neither asks for nor keeps that cookie.
- **omp's structured credential** (`{"token", "cookie", "baseUrl"}` as one
  JSON string). The region lives in the section instead, so a plain key is
  all `/connect` needs.
- The token grammar check (`sk-...`) omp applies before a request.
- omp's discovery filter and per-model limits for listed models the seed
  does not carry.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-alibaba-token-plan
```

MIT licensed.
