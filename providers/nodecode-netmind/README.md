# nodecode-netmind

NetMind: its OpenAI-compatible inference API and its bundled roster, as a Nodecode provider. A Nodecode rewrite of [NetMind-AI/netmind-hermes-provider](https://github.com/NetMind-AI/netmind-hermes-provider) by [NetMind-AI](https://github.com/NetMind-AI).

NetMind serves models over the OpenAI chat wire at one base, with a key. With
this cell, `/connect` offers NetMind for that key, `/models` lists the 31
models the plugin bundles with the window and the answer cap each one has, a
turn on `netmind/<model>` goes to
`https://api.netmind.ai/inference-api/v1/chat/completions`, and `/netmind`
prints the roster and where the key comes from.

## Get it

Tell the agent "add the netmind cell", or run `nodecode add nc://netmind`.

## Set it up

Make a key at <https://www.netmind.ai/>, then either save it with `/connect`
(pick NetMind) or set `NETMIND_API_KEY`. The key is a secret in either place
and this folder stores nothing.

Nothing else is needed: the section defaults to the public service, and
`base_url` is where a relay or a service of your own is:

```jsonc
{
  "netmind": {
    "base_url": "https://api.netmind.ai/inference-api/v1"
  }
}
```

The base must be HTTPS, or HTTP on this machine's loopback; one carrying
credentials, a query or a fragment is refused at start, because the key is
what the cell sends to it. See [`config.example.jsonc`](config.example.jsonc).

## Use it

```
/netmind        the roster: each model, its window, its answer cap
/netmind key    where the key comes from, and where to make one
/models         the picker, where one of those ids becomes this session's model
```

The roster is the one the original bundles, not a listing asked of the
service, because the plugin declares no model listing. Four of the 31 models
(`openai/gpt-6-astra`, `openai/gpt-6-sol`, `openai/gpt-6-luna`,
`openai/gpt-6.1-sol`) are declared without tool calls, and a turn always
offers tools, so `/models` leaves those four out; `/netmind` still names
them, and one can be pinned in `providers.netmind.models`.

What the rewrite leaves to Nodecode rather than carrying over: the plugin's
provider alias, its auxiliary-model default and its health-check and
listing flags are Hermes-side, so nothing here names them; the key is saved
by `/connect` and the model picked in `/models`. See [NOTICE](NOTICE).

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-netmind
```

MIT licensed, as the original is.
