# nodecode-gondola

Gondola: a USDC-settled marketplace for Venice AI inference, as a Nodecode
provider whose model picker carries only the chat routes. A Nodecode rewrite of
[Gondola-Market/hermes-gondola-provider](https://github.com/Gondola-Market/hermes-gondola-provider)
by [Gondola-Market](https://github.com/Gondola-Market).

## Get it

Tell the agent "add the gondola cell", or run `nodecode add nc://gondola`.

## Set it up

Mint a key at <https://gondola-ai.com/wallet?tab=keys> (a Gondola key starts
with `gnd_`) and top the wallet up with USDC on Base once at
<https://gondola-ai.com/wallet>. Save the key with `/connect` (pick Gondola),
or set `GONDOLA_API_KEY` in the environment; a saved key answers first. Each
request is metered against the prepaid balance.

The section is optional — the base defaults to Gondola's own:

```jsonc
{
  "gondola": {
    // where the OpenAI-compatible API is served; the chat lane appends
    // /chat/completions, and /models is read here
    "base_url": "https://api.gondola-ai.com/v1"
  }
}
```

## Use it

`/models` lists Gondola's chat models, and a turn on one goes to
`<base_url>/chat/completions`.

Gondola serves one OpenAI-compatible endpoint in front of Venice's models and
routes each request to the cheapest supplier. Its `GET /v1/models` is a mixed
catalog — text, image and video models together, the majority not chat — so the
cell keeps only what a turn can run on: an entry whose `endpoint` is
`/v1/chat/completions`, else one whose `modality` is `text`, else one that
names neither, which is kept the way the stock picker would keep it, so a
catalog shape the cell does not know degrades to "list everything" rather than
to an empty picker.

When the catalog is unreachable, the picker falls back to the five tool-calling
models the original plugin curates — `deepseek-v4-flash`, `claude-sonnet-5`,
`kimi-k2-7-code`, `qwen3-coder-480b-a35b-instruct-turbo`,
`venice-uncensored-1-2` — so an offline start still fills it.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-gondola
```

MIT licensed — see [LICENSE](LICENSE). Ported from the original plugin; see
[NOTICE](NOTICE).
