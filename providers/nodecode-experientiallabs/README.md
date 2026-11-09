# nodecode-experientiallabs

Experiential Labs: its OpenAI-compatible multi-model gateway as a Nodecode
provider, with the wallet and daily-cap view from its Cost API. A Nodecode
rewrite of [kyoto-aircon/hermes-experientiallabs](https://github.com/kyoto-aircon/hermes-experientiallabs) by [kyoto-aircon](https://github.com/kyoto-aircon).

With it, `/connect` offers Experiential Labs, `/models` lists its models, and
a turn on `experientiallabs/<model>` goes to
`https://api.experientiallabs.ai/v1` over the OpenAI chat-completions wire.
`/explabs` answers what the account has left.

## Get it

Tell the agent "add the experientiallabs cell", or run `nodecode add nc://experientiallabs`.

## Set it up

Make a key at <https://platform.experientiallabs.ai/settings/api-keys>, then
save it with `/connect experientiallabs`, or set `EXPLABS_API_KEY` (the
longer `EXPERIENTIALLABS_API_KEY` is read too, after it). A saved key wins
over the variable.

The section is optional; a relay's address goes in `base_url`:

```jsonc
{
  "experientiallabs": {
    "base_url": "https://api.experientiallabs.ai/v1"
  }
}
```

## Use it

- `/models` lists the curated tier the original ships as its picker fallback:
  `qwen3.8-27b`, `deepseek-v4-flash`, `gpt-6-luna`, `claude-haiku-4.5`,
  `gpt-6-astra`, and the cheap vision tier `glm-5.3-flash`. The gateway lists
  several hundred more at its own `GET /v1/models`; any id it serves can be
  named directly.
- `/explabs` reads the Cost API (`GET /credits` and `GET /key`) and answers
  the wallet balance and, when the key carries one, the daily cap and what is
  left of it. A missing key, or an unreachable Cost API, is a sentence — the
  command never fails the shell.

MIT licensed.
