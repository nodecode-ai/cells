# nodecode-aixy

Aixy: an inference gateway as a Nodecode provider, with its live model catalog. A Nodecode rewrite of [aixy-tech/aixy-hermes-plugin](https://github.com/aixy-tech/aixy-hermes-plugin) by [aixy-tech](https://github.com/aixy-tech).

Aixy serves models over the OpenAI chat wire at one base, with a project key.
With this cell, `/connect` offers Aixy for that key, `/models` lists the
models the gateway itself advertises (asked live, with the key, never a list
baked into this folder), a turn on `aixy/<model>` goes to
`https://api.aixy-gateway.com/v1/chat/completions`, and `/aixy` answers what
the gateway serves and where its key comes from.

## Get it

Tell the agent "add the aixy cell", or run `nodecode add nc://aixy`.

## Set it up

Make a project key in Aixy, then either save it with `/connect` (pick Aixy) or
set `AIXY_API_KEY`. The key is a secret in either place and this folder stores
nothing.

Nothing else is needed: the section defaults to the public gateway, and
`base_url` is where a gateway or relay of your own is served:

```jsonc
{
  "aixy": {
    "base_url": "https://api.aixy-gateway.com/v1"
  }
}
```

The base must be HTTPS, or HTTP on this machine's loopback; a base carrying
credentials, a query or a fragment is refused at start, because the project
key is what the cell sends to it. See [`config.example.jsonc`](config.example.jsonc).

## Use it

```
/aixy          what the gateway serves: each model id and its context window
/aixy key      where the project key comes from, and where to make one
/models        the picker, where one of those ids becomes this session's model
```

A conversation's requests carry its scope in the `X-Aixy-Session-ID` header,
which is how Aixy Activity groups them. `"session_header"` names that header
when your gateway spells it differently; `"none"` sends none.

Two things the rewrite leaves to Nodecode rather than carrying over: the
plugin's interactive wizard (its work is `/connect`, `/models` and `/setup`
here) and its optional MCP OAuth step (Nodecode's own `mcp` cell adds an MCP
server). See [NOTICE](NOTICE).

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-aixy
```

MIT licensed, as the original is.
