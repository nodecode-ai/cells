# nodecode-bulkpublish

BulkPublish: publish, schedule and measure posts on fifteen social networks. A Nodecode rewrite of [bulkpublish/hermes-bulkpublish](https://github.com/bulkpublish/hermes-bulkpublish) by [bulkpublish](https://github.com/bulkpublish).

## Get it

Tell the agent "add the bulkpublish cell", or run `nodecode add nc://bulkpublish`.

## Set it up

A key is made at <https://app.bulkpublish.com/developer>. Give it to the cell
either way:

- name the environment variable that holds it, `bulkpublish.api_key_env`
  (`BULKPUBLISH_API_KEY` by default), or
- paste it into `bulkpublish.api_key`, where it is kept in a file only you read.

`bulkpublish.endpoint` is the server's MCP URL,
`https://mcp.bulkpublish.com/mcp/full` unless a proxy of your own stands in
front of it. `bulkpublish.approve_writes` (on by default) answers what a
publishing call would send until the call carries `"confirm": true`.

```jsonc
{
  "bulkpublish": {
    "endpoint": "https://mcp.bulkpublish.com/mcp/full",
    "api_key_env": "BULKPUBLISH_API_KEY"
  }
}
```

## Use it

The server's own tools are reachable by name: fifteen networks — X, Instagram,
Facebook, LinkedIn, Threads, Bluesky, Mastodon, TikTok, YouTube, Pinterest,
Google Business, Discord, Telegram, Tumblr, Snapchat — with drafts, scheduling,
approvals, media, an RSS-to-social feed and analytics.

Ask in the conversation, or call the two tools:

- `bulkpublish_tools` lists what the server offers, all of it or the tools a
  word matches, one a line.
- `bulkpublish_call` runs one by name with its arguments object:
  `{"tool": "list_channels"}`, then `{"tool": "create_post", "arguments":
  {"channelId": "...", "content": "..."}}`, then `{"tool": "publish_post",
  "arguments": {"postId": "..."}, "confirm": true}`.

From eval, the same three verbs:

```lisp
(bulkpublish:channels)                              ; the accounts this key can post to
(bulkpublish:tools :query "analytics")              ; what the server can do
(bulkpublish:call "get_post" "postId" "abc123")     ; one tool, its arguments as pairs
```

`/bulkpublish` reads the account by hand: `channels`, `tools [word]`,
`quota`, and `call get_post {"postId": "abc123"}`. `(help :bulkpublish)`
carries the workflow the original's skills taught: a draft before a publish,
the three calls a local media file takes, the platform type overrides, and
the approval flow.

Every refusal is `ERROR: BULKPUBLISH-ERROR` and names what the server said.
