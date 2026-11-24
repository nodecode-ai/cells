# nodecode-anyrouter

AnyRouter: one key for many vendors' models, with routing and fallback, as a
Nodecode provider. A Nodecode rewrite of [AnyRouterHQ/hermes-provider-anyrouter](https://github.com/AnyRouterHQ/hermes-provider-anyrouter) by [AnyRouterHQ](https://github.com/AnyRouterHQ).

With it, `/connect` offers AnyRouter, `/models` lists its routes, and a turn
on `anyrouter/<route>` goes to `https://anyrouter.dev/api/v1` over the chat
lane, as the original sends it:

- the model list is AnyRouter's own catalog: `/anyrouter` reads
  `GET /v1/models` and puts the routes that take tool calls in `/models`,
  and the original's fallback routes stand in until it does
- the catalog's rates (`input_per_1m`, `output_per_1m`) ride in the catalog
  entry, so `/models` and the usage surfaces cost a round on a priced route
  from AnyRouter's own figures
- the session id rides as `session_id`, so related requests group in
  AnyRouter's Request Logs dashboard
- routing preferences (which upstreams to use, avoid or try first, how to
  sort them, whether to fall back, a price ceiling) ride as the body's
  `provider`
- reasoning rides as AnyRouter's own `reasoning` object, which it translates
  to each upstream's thinking control; an off selection is
  `{"enabled": false}`
- the app-attribution headers are off until the section asks for them, and
  then they name Nodecode

See [NOTICE](NOTICE).

## Get it

Tell the agent "add the anyrouter cell", or run `nodecode add nc://anyrouter`.

## Set it up

Make a key at <https://anyrouter.dev> and save it with `/connect`, or set
`ANYROUTER_API_KEY`. A saved key wins over the variable. No other provider's
key is ever sent to AnyRouter.

The section is on unless it says `"enabled": false`; every member is
optional:

```jsonc
{
  "anyrouter": {
    // where AnyRouter is served; the chat lane appends /chat/completions
    "base_url": "https://anyrouter.dev/api/v1",
    // credit Nodecode's traffic in AnyRouter's public app rankings
    "attribution": false,
    // upstream routing: only / ignore / order, and how to sort them
    "only": ["anthropic"],
    "ignore": ["deepinfra"],
    "order": ["anthropic"],
    "sort": "price",           // default, price, throughput or latency
    "allow_fallbacks": "off",  // default, on or off
    "max_price": 5              // whole US dollars per million tokens
  }
}
```

## Use it

```
/anyrouter            # read AnyRouter's catalog and list the tool-capable routes
/anyrouter grok       # only the routes whose id carries "grok"
```

Then pick a model in `/models` and talk to it as usual. `anyrouter/hermes`
and `anyrouter/auto` are first-party meta-models that route across the
upstreams with fallback; the rest are direct `vendor/model` routes. A route
AnyRouter serves but the catalog did not mark as tool-capable is still
selectable in `providers.anyrouter.models` if you know it takes tools.

MIT licensed.
