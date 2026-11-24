# nodecode-markifact

Markifact: run Google, Meta, TikTok and other ad operations through Markifact's hosted MCP server. A Nodecode rewrite of [markifact/markifact-mcp](https://github.com/markifact/markifact-mcp) by [markifact](https://github.com/markifact).

The model gets two tools, `markifact_tools` and `markifact_call`, which reach
Markifact's eight MCP tools over the same endpoint the plugin names:
`find_operations`, `get_operation_inputs`, `run_operation`,
`run_write_operation`, `list_connections`, `get_file_url`, `read_file` and
`upload_media`. Behind them are some 1000 operations over Google Ads, Meta,
TikTok, LinkedIn, Pinterest, Snapchat, Microsoft, Amazon and Reddit Ads, GA4,
Shopify, Klaviyo and HubSpot. `/markifact` signs in, lists the tools, or calls
one, and `(help :markifact)` carries the plugin's operating protocol.

## Get it

Tell the agent "add the markifact cell", or run `nodecode add nc://markifact`.

## Set it up

The cell needs a Markifact account and the endpoint in the `markifact` section
of `config.jsonc` (or `/settings`):

```jsonc
{
  "markifact": {
    "endpoint": "https://api.markifact.com/mcp",
    "timeout": 30
  }
}
```

Then sign in:

```
/markifact login
```

Markifact has no API keys: its server is OAuth 2.1 with PKCE and dynamic client
registration, so the command registers this client, opens the sign-in page in
your browser, and keeps the tokens in `auth.json` under `oauth_tokens.markifact`
(refreshed while they last). A browser on another machine cannot reach this
one's loopback listener: paste the address it landed on under **Paste where the
browser landed** in `/markifact`, or `/markifact code <address>`. On first use
sign in to Markifact itself and connect your ad platforms at
<https://www.markifact.com/app/connections>.

A token can also be supplied without a sign-in: `access_token` in the section,
or `token_env` naming a variable that holds one.

## Use it

```lisp
(markifact:tools)                       ; what the server exposes, with schemas
(markifact:call-tool "find_operations" (nlk:json-object "query" "list campaigns"))
```

or, from a conversation, `markifact_call` with `tool` and `arguments`. The
`(help :markifact)` topic tells the model the order to work in: discover with
`find_operations`, inspect with `get_operation_inputs`, resolve the account
with the platform's `*_select_accounts`, run, then verify. Writes go through
`run_write_operation` and are only run after an explicit yes, because they
spend real budgets.

`/markifact workflow NAME` prints the steps of one of the plugin's skills:
`launch-google-search`, `launch-pmax`, `launch-meta-campaign`,
`edit-meta-creative`, `rotate-creative`, `negative-keyword-sweep` and
`diagnose-underperformer`.

See [NOTICE](NOTICE) for what the rewrite keeps of the original and what it
leaves out. The licence is the original's, MIT.
