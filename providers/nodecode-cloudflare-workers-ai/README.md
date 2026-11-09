# nodecode-cloudflare-workers-ai

Cloudflare Workers AI: its models, their context windows and the reasoning
effort each takes, as a Nodecode provider. A Nodecode rewrite of
[AhmetArif0/hermes-cloudflare-workers-ai-provider](https://github.com/AhmetArif0/hermes-cloudflare-workers-ai-provider)
by [AhmetArif0](https://github.com/AhmetArif0).

## Get it

Tell the agent "add the cloudflare-workers-ai cell", or run `nodecode add nc://cloudflare-workers-ai`.

## Set it up

You need a Cloudflare account and a Workers AI API token. In the Cloudflare
dashboard open **AI → Workers AI → Use REST API**: it shows the **Account ID**
and a button to make a **Workers AI API token**.

- **Account.** Set `account_id` in the section, or `CLOUDFLARE_ACCOUNT_ID` in
  the environment nodecode runs in. The address it builds is
  `https://api.cloudflare.com/client/v4/accounts/<account>/ai/v1`;
  `base_url` (or `CLOUDFLARE_WORKERS_AI_BASE_URL`) overrides it whole.
- **Token.** Save it with `/connect`, or set
  `CLOUDFLARE_WORKERS_AI_API_TOKEN`. A token saved with `/connect` wins.

The token's own variable, and not Cloudflare's `CLOUDFLARE_API_TOKEN`: a
Pages or DNS token in that one must never ride a Workers AI turn.

```jsonc
{
  "cloudflare-workers-ai": {
    "account_id": "your account id"
  }
}
```

## Use it

`/connect` offers **Cloudflare Workers AI**, `/models` lists its models, and a
turn on `cloudflare-workers-ai/<model>` goes over the OpenAI-compatible chat
wire to the account. Pick the model in `/setup` or `/models` — `@cf/openai/gpt-oss-120b`
is a good first one on the free plan — and its efforts are the rungs that
model takes.

The Workers Free plan includes 10,000 Neurons a day and serves the free
models below; the last seven need Workers Paid, and their names say
`(paid plan)` in the picker.

## What the plugin did, and what this cell does with it

Hermes cannot select Workers AI as a provider, and a custom endpoint pointed
at it lists no models — Cloudflare's OpenAI-compatible `/models` answers HTTP
405 — and assumes a 256K window for every model, the 128K gpt-oss ones
included. This cell is the same three answers in Nodecode:

- **The listing is built in.** Workers AI's roster here is the 16 models with
  function calling and at least 64K of context, with Cloudflare's own context
  windows (128K to 1.3M), vision flags and reasoning flags from the plugin's
  table of 2026-09-29. The picker is answered from the roster without a
  request, so the 405 never reaches the operator.
- **Reasoning effort takes the model's form.** Cloudflare refuses a rung a
  model does not take — qwen3.8 answers HTTP 400 to `high` — so a pick is
  mapped the way the plugin maps it: a level the model takes goes as it is,
  one it does not becomes the nearest weaker level it does, a level below its
  lowest becomes that lowest, and turning reasoning off sends `none` where the
  model takes it and its lowest level otherwise (gpt-oss always reasons; the
  catalog says glm-5.3 turns `none` into `max`). A model whose catalog entry
  lists no levels reasons without being asked and gets no `reasoning_effort`
  field at all. With no effort picked, nothing is sent.
- **The context window is Cloudflare's.** Each catalog row carries the
  window, so compression starts where the model actually ends.

| Model | Context | Vision | Reasoning effort sent | Plan |
|---|---|---|---|---|
| `@cf/openai/gpt-oss-120b`, `@cf/openai/gpt-oss-20b` | 128K | | low, medium, high | Free |
| `@cf/qwen/qwen3.8-27b` | 262K | ✓ | low, medium, xhigh | Free |
| `@cf/zai-org/glm-4.7-flash` | 131K | | — | Free |
| `@cf/google/gemma-4-26b-a4b-it` | 256K | ✓ | — | Free |
| `@cf/nvidia/nemotron-3-120b-a12b` | 256K | | — | Free |
| `@cf/meta/llama-4-scout-17b-16e-instruct` | 131K | ✓ | — | Free |
| `@cf/mistralai/mistral-small-3.1-24b-instruct` | 128K | | — | Free |
| `@cf/ibm-granite/granite-4.0-h-micro` | 131K | | — | Free |
| `@cf/deepseek-ai/deepseek-v4-flash-0731` | 1.3M | | none, low, high, max | Paid |
| `@cf/deepseek-ai/deepseek-v4-pro-0813` | 1M | | none, low, high, max | Paid |
| `@cf/moonshotai/kimi-k2.6` | 262K | ✓ | none, high | Paid |
| `@cf/moonshotai/kimi-k2.7-code` | 262K | ✓ | — | Paid |
| `@cf/zai-org/glm-5.2` | 262K | | none, high, max | Paid |
| `@cf/zai-org/glm-5.3`, `@cf/zai-org/glm-5.3-flash` | 1.3M | flash only | low, high, max | Paid |

## Not ported

- The plugin's clear message when a Workers Free account picks a paid-plan
  model (`is not available on the workers free plan` as `model_entitlement`,
  no retry, fall back). Nodecode has no per-provider error classifier a cell
  can hook, so the 403 arrives as Cloudflare wrote it; the `(paid plan)`
  marker in the picker is what keeps it from being a surprise.
- The `workers-ai` provider id: this cell is `cloudflare-workers-ai`, the id
  Nodecode's own privacy list already carries for the endpoint, so a turn's
  privacy reading stays right.
- The plugin's own two environment variable names as the only source of the
  address: `account_id` and `base_url` are section settings here, and the
  variables still answer when a section says nothing.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-cloudflare-workers-ai
```

Every round is a stubbed `dex:post`, every listing a stubbed
`nle::http-fetch`, every key variable a stubbed `nle::credential-env`:
nothing reaches Cloudflare.

MIT licensed, as the original.
