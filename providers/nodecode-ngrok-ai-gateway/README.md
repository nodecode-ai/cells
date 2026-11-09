# nodecode-ngrok-ai-gateway

The ngrok AI Gateway: one access key for every model provider you wired up
at ngrok, with failover. A Nodecode rewrite of
[ngrok/hermes-ngrok](https://github.com/ngrok/hermes-ngrok) by
[ngrok](https://github.com/ngrok).

## Get it

Tell the agent "add the ngrok-ai-gateway cell", or run
`nodecode add nc://ngrok-ai-gateway`.

## Set it up

Make an access key (`ng-…`) at <https://app.ngrok.ai/keys>, then save it
with `/connect`, or set `NGROK_AI_GATEWAY_KEY`. A saved key wins over the
variable. Nothing else is needed: the models are whatever your gateway
routes, so the picker asks the gateway itself.

## Use it

`/connect` offers **ngrok AI Gateway**; pick a model from `/models` and the
turn goes to `https://gateway.ngrok.ai/v1/chat/completions`, or a turn names
it as `ngrok/<model>`.

- **Model ids are the gateway's own**: `openai:gpt-4o`,
  `anthropic:claude-sonnet-4-5` — the provider, a colon, the model. They go
  out exactly as written, and the gateway routes each one to the account
  credential configured behind it.
- **The model list is live**: the gateway's `/models` answer, with your key,
  is what `/models` lists. Nothing is bundled, because the list is
  per-account gateway configuration — a key that routes nothing lists
  nothing.
- **A failure says what the gateway said**: an unrouted model reads as the
  model's fault, an access policy or a rejected upstream key as `forbidden`
  rather than "sign in again", and an account out of credit as `billing`.

Set a gateway of your own with the section, or with
`NGROK_AI_GATEWAY_BASE_URL` (the section wins):

```jsonc
{
  "ngrok-ai-gateway": {
    "base_url": "https://gateway.ngrok.ai/v1"
  }
}
```

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-ngrok-ai-gateway
```

MIT licensed. The original's licence and what was left out are in
[NOTICE](NOTICE).
