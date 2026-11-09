# nodecode-scaleway

Scaleway Generative APIs: Scaleway's European, OpenAI-compatible inference as a Nodecode provider, on a curated tool-calling model list with each model's real context window and price. A Nodecode rewrite of [scaleway/hermes-scaleway-provider](https://github.com/scaleway/hermes-scaleway-provider) by [scaleway](https://github.com/scaleway).

## Get it

Tell the agent "add the scaleway cell", or run `nodecode add nc://scaleway`.

## Set it up

Make an IAM API key at <https://console.scaleway.com/iam/api-keys>; the IAM
principal needs the `GenerativeApisModelAccess` permission set. Its secret is
the key. Save it with `/connect` (pick Scaleway), or leave `SCW_SECRET_KEY`
in the environment — the same variable the Scaleway CLI and SDKs export, so an
existing CLI setup works as it is. A key `/connect` saved answers first; the
variable `SCALEWAY_API_KEY` is read too, when `SCW_SECRET_KEY` is unset.

Optional, and rarely needed — the section is on unless it says
`"enabled": false`:

```jsonc
{
  "scaleway": {
    // where the API is served, appended /chat/completions. A relay's
    // address goes here, or SCALEWAY_BASE_URL in the environment, which
    // sets the default.
    "base_url": "https://api.scaleway.ai/v1"
  }
}
```

## Use it

`/models` lists Scaleway's models, and a turn on one goes to
`https://api.scaleway.ai/v1/chat/completions` with the key as its bearer.

The picker is the curated list, not the raw `/v1/models` catalog: the
serverless models that drive an agent over chat-completions, each with the
context window, output limit, price and image modality models.dev publishes
for it, so compression runs when the model's own window fills rather than at
one window assumed for the whole endpoint. `deepseek-v4.1-flash-1017` is a
good default. `gpt-oss-120b` is deliberately absent — Scaleway serves its
tool-calling on the beta Responses API — as are the embedding, speech and
transcription models.

For the plugin's cheap auxiliary tier, point the auxiliary model at
`mistral-small-3.2-24b-instruct-2506`, the cheapest tool-calling
vision-capable model on the platform.

## What this rewrite carries, and what it leaves to Nodecode

The plugin contributes a provider profile: a base, an alias, a key
variable, a curated `fallback_models` list, a `models_dev_id`, and an
auxiliary model. Nodecode's catalog already speaks models.dev, so the cell
adds only what a profile says that a catalog cannot: the curated list
itself, the recorded numbers under it for a home that has never fetched
models.dev, and the ambient `SCW_SECRET_KEY`.

- **The alias.** Hermes answers to `--provider scw`; Nodecode names a
  provider once, so a turn names `scaleway/<model>`.
- **Model metadata.** Nodecode reads models.dev at runtime, so a model the
  core has fetched keeps models.dev's own numbers; `models.json` here is the
  row the plugin records, for a curated id the live catalog does not carry
  yet.
- **The setup default.** The plugin's setup picks `deepseek-v4.1-flash-1017`
  first. Pick it once with `/model`; there is no per-provider default to
  set from a cell.
- **Never auto-detected.** Hermes refuses to infer the provider from the
  ambient key. Nodecode may infer it when no provider is configured and
  `SCW_SECRET_KEY` is the only key in the environment; naming the provider
  in `providers.scaleway` or picking a model settles it.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-scaleway
```

Every store is a temp `auth.json`, every key variable a stubbed
`nle::credential-env`: nothing reaches Scaleway.

MIT licensed.
