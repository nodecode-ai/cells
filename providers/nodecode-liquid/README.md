# nodecode-liquid

Liquid Inference: an auction exchange for LLM inference. A Nodecode rewrite of [architect-xyz/hermes-liquid-inference](https://github.com/architect-xyz/hermes-liquid-inference) by [architect-xyz](https://github.com/architect-xyz).

Each request goes to the eligible offer with the lowest estimated charge,
and carries a cap on what that request may be charged. This cell puts the
exchange in Nodecode as the provider `liquid`:

- `/connect` offers Liquid Inference and `/models` lists its model,
  `liquid.auto`
- every request carries `x-liquid-cap-usd`, the most that request may be
  charged: 1.00 USD unless you say otherwise
- every request carries `X-Title`, naming the client it comes from
- a reasoning level you pick rides as a top-level `reasoning_effort`; a turn
  with none sends none

The exchange speaks the OpenAI chat wire, so a turn on `liquid.auto` goes to
`https://router.inference.ai.exchange/v1/chat/completions` like any
catalog provider's.

## Get it

Tell the agent "add the liquid cell", or run `nodecode add nc://liquid`.

## Set it up

Make a buyer key with the `spend` scope at
<https://inference.ai.exchange>. Save it with `/connect liquid`, or set
`LIQUID_API_KEY` in the environment. A saved key wins over the variable.

Then the section, a top-level key of `~/.nodecode/config.jsonc`:

```jsonc
{
  "liquid": {
    // where the exchange is served; a relay's address goes here
    "base_url": "https://router.inference.ai.exchange/v1",
    // the most one request may be charged, in USD
    "cap_usd": "1.00"
  }
}
```

The section is on unless it says `"enabled": false`. With it off, a turn still
reaches the exchange on a key, but the request carries neither the cap nor the
client name.

## Use it

```sh
nodecode config set model.provider liquid
nodecode config set model.default liquid.auto
```

`/liquid` says how the exchange stands: its base, its cap, its model, and
whether a key is there. A cap that is not a positive decimal (`"abc"`,
`"0.00"`) refuses at start, because a malformed cap is the uncapped bill the
header exists to prevent.

## Test

From the cells repository's root:

```sh
.github/test-cell.sh providers/nodecode-liquid
```

Offline: every wire is stubbed.

MIT licensed; see [LICENSE](LICENSE) and [NOTICE](NOTICE).
