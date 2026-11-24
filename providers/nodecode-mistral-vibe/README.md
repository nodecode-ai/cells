# nodecode-mistral-vibe

Mistral Vibe: the browser sign-in and the model lane of an unofficial Mistral
Vibe client. A Nodecode rewrite of [nebneo/hermes-mistral-vibe](https://github.com/nebneo/hermes-mistral-vibe) by [nebneo](https://github.com/nebneo).

The relay at `https://api.mistral.ai/v1` answers the OpenAI chat wire. With
this cell `/models` offers `mistral-vibe`, a turn on it sends the account's
key as a bearer, and `/models` lists the account's own models: the listing
is the relay's `GET /models`, asked with the key, the way Nodecode asks any
provider's. The fallback model `mistral-vibe-cli-latest` and the relay's GLM
rows are published by the cell, so the provider is pickable before the first
listing answers.

Two things it keeps from the original:

- **its own key.** `MISTRAL_VIBE_API_KEY` (or a key the sign-in saved), and
  never a plain `MISTRAL_API_KEY` or the chat family's default variable, so a
  key made for another purpose cannot cross into a Vibe round.
- **the relay's reasoning vocabulary.** `reasoning_effort` is validated per
  model on that relay: GLM-5.3, GLM-5 and GLM-latest take
  `low`/`high`/`max`, GLM-5.2 adds `medium`, and the relay cannot switch
  thinking off at all. A configured effort is clamped **down** to the highest
  rung the model takes (never up; the vendor's top tier, `xhigh`, reads
  `max`), and a model whose set was not verified is sent no effort at all and
  keeps the relay's own default.

## Get it

Tell the agent "add the mistral-vibe cell", or run `nodecode add nc://mistral-vibe`.

## Set it up

Sign in from the browser:

```
/mistral-vibe login
```

The console's own PKCE flow opens: the cell shows the address to open, the
browser completes it there, and the key it mints is saved into `auth.json`
the way `/connect` saves one. `/mistral-vibe status` says whether a key is
set; `/mistral-vibe logout` takes the saved one out again (the console's key
stays live until it is deleted there).

Without the sign-in, save a key with `/connect` or set
`MISTRAL_VIBE_API_KEY`. The environment variable outranks a saved key.

The section is optional and on by default:

```jsonc
{
  "mistral-vibe": {
    // where the relay is served; the chat lane appends /chat/completions
    "base_url": "https://api.mistral.ai/v1"
  }
}
```

`"enabled": false` turns the cell off.

## Use it

Pick a model in `/models` under **Mistral Vibe**, and set its reasoning
effort there. A GLM row lists exactly the rungs the relay takes for it.

**Billing.** This is an unofficial client of the Vibe console: a model
visible in `GET /models` proves only that the account can see it, not that a
Vibe plan covers it. The original's README says so, and Mistral recommends
pay-as-you-go for API automation. Check your plan before running turns.

## Test

From the cells repository:

```sh
.github/test-cell.sh providers/nodecode-mistral-vibe
```

Nothing in the slice touches the network: the console and the wire are
stubbed.

MIT licensed. See [NOTICE](NOTICE).
