# nodecode-llmapi

LLM API: Claude, GPT, Gemini, Kimi and more on one key, each model sent on the
wire it works best on. A Nodecode rewrite of
[llmapi-ai/hermes-provider-llmapi](https://github.com/llmapi-ai/hermes-provider-llmapi)
by [llmapi-ai](https://github.com/llmapi-ai).

[LLM API](https://llmapi.ai) is one gateway with one key, served at
`https://api.llmapi.ai/v1`. With this cell, `/connect` offers LLM API,
`/models` lists the chat models the gateway currently serves, and a turn on one
of them goes out the way the gateway wants it:

- a `claude-*` model rides the Anthropic Messages API (`/v1/messages`), where
  the lane adds its prompt-cache breakpoints, so a tool loop reads its prompt
  from the cache on the second round
- a GPT-5-or-later model (`gpt-5*`, `gpt-6*`, ... — not `gpt-4.x`, not
  `gpt-oss`) rides the Responses API (`/v1/responses`), where reasoning items
  and tool calls round-trip natively
- everything else rides Chat Completions (`/v1/chat/completions`)

The picker, its context windows, image input, reasoning rungs and list prices
come from the gateway's own public catalog, `GET /v1/models`, which needs no
key. It is read once at start and hourly after that, and kept in memory: a
gateway that does not answer leaves the last list standing, and a box that has
never read one shows the nine featured models the original named.

`/llmapi usage` reads `GET /v1/credits` and prints the account's balance, its
billing mode and the monthly cap, if one applies.

## Get it

Tell the agent "add the llmapi cell", or run `nodecode add nc://llmapi`.

## Set it up

Make a key at
[app.llmapi.ai/dashboard/api-keys](https://app.llmapi.ai/dashboard/api-keys),
then pick LLM API in `/models` and paste the key when the picker asks, or set
`LLMAPI_API_KEY` in the environment. A key saved with `/connect` wins over the
variable, and no other provider's key is ever sent to this gateway: with
neither, a round goes out keyless and the gateway says why.

The address is optional, for a regional gateway or a relay:

```jsonc
{
  "llmapi": {
    "base_url": "https://api.llmapi.ai/v1"
  }
}
```

The catalog, `/llmapi usage` and every model request follow it.

## Use it

Ask for a model — `/models` lists what the gateway serves right now — then talk
as usual: the wire is picked from the model's name, so mixing a Claude model
and a GPT-5 model in one session costs nothing but a different lane.

```
/llmapi            the address, whether a key is set, and what the catalog holds
/llmapi usage      the credit balance, billing mode and monthly cap
/llmapi models     how many models the picker offers, and when they were read
/llmapi refresh    read the catalog again now
```

`(help :llmapi)` answers the same from the model's side.

A model with a window under 64K is not in the picker but still runs when asked
for by name: the floor is the one the original set, not a limit of the gateway.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-llmapi
```

MIT licensed, Copyright (c) 2026 LLM API Inc. See [NOTICE](NOTICE).
