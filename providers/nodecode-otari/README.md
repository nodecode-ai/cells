# nodecode-otari

Otari: one key for every model an LLM gateway serves, with budgets, routing
and usage tracking on each call. A Nodecode rewrite of
[mozilla-ai/hermes-provider-otari](https://github.com/mozilla-ai/hermes-provider-otari)
by [mozilla-ai](https://github.com/mozilla-ai).

With it, `/connect` offers Otari, `/models` lists the models your key may use
— read live from the gateway's `GET /api/v1/models` — and a turn on any of
them goes to `https://api.otari.ai/api/v1` over the OpenAI chat wire.

## Get it

Tell the agent "add the otari cell", or run `nodecode add nc://otari`.

## Set it up

Make a key at <https://otari.ai>. Save it with `/connect`, or set
`OTARI_API_KEY`. A saved key wins over the variable.

The model id is whatever the gateway's catalog lists: a routed id such as
`openai/gpt-4o`, a pinned `provider:model` selector such as
`anthropic:claude-sonnet-4-6`, or the name of a routing policy.

A gateway other than the hosted one is the section's `base_url`, `/api/v1`
included:

| Gateway | `base_url` |
| --- | --- |
| otari.ai | not set (defaults to `https://api.otari.ai/api/v1`) |
| otari.ai, EU region | `https://eu.api.otari.ai/api/v1` |
| your own Otari | your gateway's address plus `/api/v1` |

An EU key sent to the default host is answered with `421 Misdirected
Request`: give the EU host in `base_url` instead.

## Use it

```jsonc
{
  "otari": {
    // where the gateway is; the EU host or a self-hosted Otari goes here
    "base_url": "https://api.otari.ai/api/v1"
  }
}
```

Then pick a model the gateway serves (`/models` lists them, as your key sees
them) and turn. The section is on unless it says `"enabled": false`.

Reasoning effort goes out as `reasoning_effort`. Otari takes `minimal`,
`low`, `medium`, `high` and `xhigh`, so the strongest rung Nodecode offers,
`max`, is sent as `xhigh`. Otari has no `none`: an effort `off`, or none
picked at all, leaves the field out and the route's own default applies.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-otari
```

Apache-2.0, the same licence as the original. See [NOTICE](NOTICE).
