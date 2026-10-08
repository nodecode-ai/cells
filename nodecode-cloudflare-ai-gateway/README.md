# nodecode-cloudflare-ai-gateway

A Nodecode cell for [Cloudflare AI Gateway](https://developers.cloudflare.com/ai-gateway/).
With it, `/connect` offers Cloudflare AI Gateway, `/models` lists the models
oh-my-pi bundles for it (Claude, GPT, Grok, Qwen, DeepSeek, Kimi and the
Workers AI models), and a turn on `cloudflare-ai-gateway/<model>` goes to
`https://gateway.ai.cloudflare.com/v1/<account>/<gateway>` the way oh-my-pi
sends it:

- the model id's namespace picks the route and the wire:
  - `anthropic/<id>` goes to `/anthropic/v1/messages` on the Messages wire,
    named `<id>` with its dots turned to dashes (`anthropic/claude-sonnet-4.5`
    is `claude-sonnet-4-5`)
  - `openai/<id>` goes to `/openai/chat/completions` on the chat wire, named
    `<id>`
  - `workers-ai/<id>` goes to `/compat/chat/completions` on the chat wire,
    named whole
  - any other id rides the wire its bundled row names (the Messages route
    for all but the Workers AI rows), named whole
- every route authenticates to the gateway alone, with
  `cf-aig-authorization: Bearer <token>`: no `Authorization` and no
  `x-api-key` leaves for the upstream
- a chat round names its cap `max_completion_tokens` and sends no
  `prompt_cache_key`, as omp's chat compat does for a host that is not
  OpenAI's own

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s
`cloudflare-ai-gateway` provider. See [NOTICE](NOTICE).

## Token, account and gateway

omp's login is `custom`: it opens
<https://developers.cloudflare.com/ai-gateway/configuration/authentication/>,
asks for an AI Gateway token with Run permission, the account id and the
gateway id, and stores the three as one credential. This cell splits them by
what they are:

- the token is the secret: save it with `/connect`, or set
  `CLOUDFLARE_AI_GATEWAY_API_KEY`. A saved token wins over the variable.
- the account id and the gateway id are not secret: they are the section's
  `account_id` and `gateway_id`, else `CLOUDFLARE_ACCOUNT_ID` and
  `CLOUDFLARE_GATEWAY_ID`, as omp reads them.

A credential saved as omp's own JSON, `{"token": "...", "accountId": "...",
"gatewayId": "..."}`, is read too, and its ids come first. A round with no
account or no gateway is refused before anything is sent, in omp's words.

`/connect` does not check the token: the gateway's model listing sits behind
the account and the gateway, so the picker is answered from the bundled
roster without a request.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-cloudflare-ai-gateway/` and
restart Nodecode, or run `(restart-cells)`.

## Configure

```jsonc
{
  "cloudflare-ai-gateway": {
    "account_id": "0123456789abcdef0123456789abcdef",
    "gateway_id": "default",
    // the gateway's root, before /<account>/<gateway>
    "base_url": "https://gateway.ai.cloudflare.com/v1"
  }
}
```

An operator's `providers.cloudflare-ai-gateway.sdk` (or a model's `sdk`) pins
a wire, as for any provider.

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-cloudflare-ai-gateway
```

## Not ported

- Live discovery. omp asks the gateway's Anthropic listing and Workers AI's
  model list; this cell serves the bundled rows.
- omp's per-model Messages compat beyond the route: its Claude Code identity
  headers and system line, strict-tool and context-management toggles. The
  core's Messages lane builds the body.

MIT licensed.
