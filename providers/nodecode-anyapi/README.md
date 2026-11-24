# nodecode-anyapi

AnyAPI.ai: one key for many vendors' models, listing only the routes that take tool calls. A Nodecode rewrite of [es697/hermes-provider-anyapi](https://github.com/es697/hermes-provider-anyapi) by [es697](https://github.com/es697).

With it, `/connect` offers AnyAPI, `/models` lists its models, and a turn on
`anyapi/<model>` goes to `https://api.anyapi.ai/v1` over the OpenAI chat
wire, which is what the gateway speaks.

AnyAPI's `/v1/models` answer is no plain listing: every route carries the
request surfaces it accepts in `tags`. A Nodecode turn always offers tools,
so only the routes tagged `function_calling` (or
`chat_completions:function_calling`) are listed; a catalog that cannot be
read, or that names none, leaves the whole listing standing, so a degraded
picker is never an empty one. Set `tool_capable` to `false` to list every
route.

Ported from the Hermes provider plugin of the same name. See [NOTICE](NOTICE).

## Get it

Tell the agent "add the anyapi cell", or run `nodecode add nc://anyapi`.

## Set it up

Make a key at <https://anyapi.ai>. Save it with `/connect`, or set
`ANYAPI_API_KEY`; a saved key wins over the variable, and no other
variable's key is ever sent to AnyAPI.

The section is on unless it says `"enabled": false`:

```jsonc
{
  "anyapi": {
    // where AnyAPI is served; a relay's address goes here
    "base_url": "https://api.anyapi.ai/v1",
    // list only the routes AnyAPI tags as taking tool calls
    "tool_capable": true,
    // one catalog read's ceiling, in seconds
    "timeout_seconds": 8
  }
}
```

The plugin's `ANYAPI_BASE_URL` variable is this section's `base_url`: a base
the section does not serve is a relay's catalog, not AnyAPI's, and its routes
are then listed as the core reads them.

## Use it

`/models`, pick AnyAPI, pick a model. Without a key the fallback roster is
still listed — a handful of flagships across the vendors AnyAPI fronts — so
the picker has something to offer before the live catalog answers.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-anyapi
```

MIT licensed.
