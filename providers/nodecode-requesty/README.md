# nodecode-requesty

Requesty: an LLM gateway with managed routing policies, as a Nodecode
provider. A Nodecode rewrite of [requestyai/hermes-provider-requesty](https://github.com/requestyai/hermes-provider-requesty) by [requestyai](https://github.com/requestyai).

With it, `/connect` offers Requesty, `/models` lists its routes, and a turn
on `requesty/<policy>` goes to `https://router.requesty.ai/v1` over the chat
lane, as the original sends it:

- a model is one of Requesty's managed routing policies (a curated route
  with failover): `/requesty` reads `GET /v1/models/managed` and puts the
  ones that take tool calls in `/models`, and a bundled list of the
  original's fallback policies stands in until it does
- reasoning rides as the top-level `reasoning_effort` Requesty documents,
  and only to a model the catalog marks reasoning; Nodecode's `minimal` goes
  out as `min`, `xhigh` as `high`, and an off selection as `none`
- the session id rides as `requesty.trace_id`, so one conversation groups
  in the Requesty dashboard

See [NOTICE](NOTICE).

## Get it

Tell the agent "add the requesty cell", or run `nodecode add nc://requesty`.

## Set it up

Make a key at <https://app.requesty.ai/api-keys> and save it with `/connect`,
or set `REQUESTY_API_KEY`. A saved key wins over the variable.

The section is on unless it says `"enabled": false`:

```jsonc
{
  "requesty": {
    // where Requesty is served; the chat lane appends /chat/completions
    "base_url": "https://router.requesty.ai/v1"
  }
}
```

## Use it

```
/requesty            # read the managed routing policies and list them
/requesty claude     # only the routes whose id carries "claude"
```

Then pick a model in `/models` and talk to it as usual. Point `base_url` at
`https://router.eu.requesty.ai/v1` to keep every request on Requesty's EU
endpoint.

MIT licensed.
