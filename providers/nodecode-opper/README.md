# nodecode-opper

Opper: an EU-hosted AI gateway, 700+ models from 30+ providers behind one
OpenAI-compatible API. A Nodecode rewrite of
[opper-ai/hermes-opper](https://github.com/opper-ai/hermes-opper) by
[opper-ai](https://github.com/opper-ai).

With it, `/connect` offers Opper, `/models` lists what the gateway publishes,
and a turn on `opper/<model>` goes to `https://api.opper.ai/v3/compat` over the
chat-completions wire, the way the plugin's profile sent it.

## Get it

Tell the agent "add the opper cell", or run `nodecode add nc://opper`.

## Set it up

Make a key at <https://platform.opper.ai>, then save it with `/connect` or set
`OPPER_API_KEY`. A saved key wins over the variable.

Optional, in `~/.nodecode/config.jsonc`:

```jsonc
{
  "opper": {
    // where the gateway is served; the chat lane appends /chat/completions
    "base_url": "https://api.opper.ai/v3/compat"
  }
}
```

`OPPER_BASE_URL` in the environment stands in for that setting, the way the
plugin's own `env_vars` allowed.

## Use it

Model ids are the gateway's **pool** names: `claude-sonnet-4-6`,
`claude-opus-5`, `gpt-5.5`, `gpt-5.4-mini`, `gemini-3.8-flash`,
`deepseek-v4-pro`, `kimi-k3`, `mistral-large-2512`. A pool is every provider
serving that model, and the gateway picks the route per request. Prefix a
vendor to pin one: `anthropic/claude-sonnet-4-6`, `azure/gpt-5.5`. The
catalogue at `https://api.opper.ai/v3/compat/models` is what `/models` reads,
so the ids on offer are the live ones; the eight above are the roster shown
when that address cannot be reached.

Switch mid-session with `/model opper:gpt-5.4-mini`.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-opper
```

MIT licensed. The original plugin and its licence are named in [NOTICE](NOTICE).