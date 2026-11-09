# nodecode-bharatcode

BharatCode: an OpenAI-compatible model provider with a free student tier. A Nodecode rewrite of [thenabbu/bharatcode-hermes](https://github.com/thenabbu/bharatcode-hermes) by [thenabbu](https://github.com/thenabbu).

BharatCode serves the OpenAI chat wire at `https://bharatcode.ai/api/model/v1`
with a bearer key, and its free tier is for students with an `.ac.in` or
`.edu.in` address. With this cell, `/connect` offers BharatCode, `/models`
lists its models, and a turn on `bharatcode/<model>` goes to that base:

- the picker is the provider's own `GET /models`, read once at start and
  again every ten minutes; the two models the original names
  (`deepseek-v4.1-flash`, `qwen-3.8-27b`) are what it offers until the
  provider has answered
- every row is marked as taking images, because the original declares the
  provider vision-capable for all of its models

## Get it

Tell the agent "add the bharatcode cell", or run `nodecode add nc://bharatcode`.

## Set it up

Make a key at <https://bharatcode.ai> (free for students with an `.ac.in` or
`.edu.in` address) and save it with `/connect`, or set `BHARATCODE_API_KEY`.
A key saved by `/connect` wins over the variable.

Then pick BharatCode in `/models`. Its address is optional, for a relay:

```jsonc
{
  "bharatcode": {
    "base_url": "https://bharatcode.ai/api/model/v1"
  }
}
```

## Use it

Ask for a model and talk as usual. `/models` lists what the account may use;
the list is what the provider last named, so a model that appears after a
restart is picked up on the next refresh.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-bharatcode
```

MIT licensed. See [NOTICE](NOTICE).
