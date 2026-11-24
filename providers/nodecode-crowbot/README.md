# nodecode-crowbot

Crowbot: crowbot.sh's OpenAI-compatible relay as a Nodecode provider, with its whole reasoning ladder kept. A Nodecode rewrite of [AgileIndustrialComplex/hermes-crowbot](https://github.com/AgileIndustrialComplex/hermes-crowbot) by [AgileIndustrialComplex](https://github.com/AgileIndustrialComplex).

With it, `/connect` offers Crowbot, `/models` lists its models, and a turn on
`crowbot/<model>` goes to `https://api.crowbot.sh/v1` over the chat lane:

- the models the original names (`kimi-k3`) are listed from the start, and
  `/crowbot` reads the relay's own `GET /models` and puts everything the
  account can reach in `/models`
- every model is offered the relay's whole reasoning ladder - minimal, low,
  medium, high, xhigh, max, and off - so a pick of `max` is sent as `max`
  rather than clamped to what a chat-completions ladder happens to hold
- a round the operator gives no effort to is sent the section's
  `default_effort` (medium), because the relay reads no effort as its own
  ceiling; a pick, `off` included, is left alone
- the key is the account's 16-digit number, read from `CROWBOT_API_KEY`, from
  the variable an existing Hermes install keeps, or from `/connect`

See [NOTICE](NOTICE).

## Get it

Tell the agent "add the crowbot cell", or run `nodecode add nc://crowbot`.

## Set it up

Make an account at <https://crowbot.sh>, find its 16-digit account number, and
save it with `/connect`, or set `CROWBOT_API_KEY`. A saved key wins over the
variable. No other provider's key is ever sent to Crowbot.

The section is on unless it says `"enabled": false`; every member is
optional:

```jsonc
{
  "crowbot": {
    // where Crowbot is served; the chat lane appends /chat/completions
    "base_url": "https://api.crowbot.sh/v1",
    // the window /models reports, since the relay's list names none
    "context_tokens": 262144,
    // what a round is sent when no effort is picked;
    // "endpoint" leaves it to the relay, whose default is its ceiling
    "default_effort": "medium"
  }
}
```

## Use it

```
/crowbot            # read the relay's model list and offer it in /models
/crowbot kimi       # only the ids that carry "kimi"
```

Then pick a model in `/models`, and its reasoning effort in `/think`, and talk
to it as usual. A model the relay serves but its list did not name can still be
picked through `providers.crowbot.models` in the config.

MIT licensed.
