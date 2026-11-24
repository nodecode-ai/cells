# nodecode-apinex

APInex: Claude over the Messages wire and the OpenAI-compatible vendors and free tier on one key. A Nodecode rewrite of [Apinex-AI/hermes-provider-apinex](https://github.com/Apinex-AI/hermes-provider-apinex) by [Apinex-AI](https://github.com/Apinex-AI).

APInex is a gateway with two wires at one host, and one key for both. With
this cell, `/models` offers it twice: `apinex` carries the Claude family
(including its free Claude models) over the Anthropic Messages API, and
`apinex-openai` carries GPT, Gemini, DeepSeek, GLM, Qwen, Kimi, Grok and the
Free tier over the OpenAI-compatible chat API. Both answer at
`https://api.apinex.bond/v1`, the first at `/messages` and the second at
`/chat/completions`.

A turn on either goes out with the model's own id, the context window and
vision flag the original's capability matrix gave it, and the key from
`APINEX_API_KEY`. The model list is the one the plugin bundled, split by the
same rule its `fetch_models()` applied: a picker never offers a media,
embedding or speech model, nor a roster entry that takes no tools.

## Get it

Tell the agent "add the apinex cell", or run `nodecode add nc://apinex`.

## Set it up

Make a key at <https://apinex.bond/register>, then save it with `/connect`
(pick APInex) or set it in the environment:

```sh
export APINEX_API_KEY=sk-apx-...
```

A key saved with `/connect` wins over the variable. The variable is the same
one the original plugin read, so a Hermes setup carries over unchanged.

Optional: the section takes the gateway's base, for a relay or a mirror.

```jsonc
{
  "apinex": {
    "base_url": "https://api.apinex.bond/v1"
  }
}
```

## Use it

Pick APInex in `/models`, or name a model directly:

- `apinex/claude-sonnet-5.5`, `apinex/claude-opus-5.5`, `apinex/claude-fable-5.1`
- `apinex/free/claude-sonnet-4.6`
- `apinex-openai/free/glm-5.3-flash`, `apinex-openai/free/gemini-3.8-flash`,
  `apinex-openai/gpt-6.1-sol`, `apinex-openai/grok-4.7`

Both providers read `APINEX_API_KEY` and nothing else: a turn on either
never picks up `ANTHROPIC_API_KEY` or `OPENAI_API_KEY` from the environment.

MIT licensed, like the plugin it is rewritten from. See [NOTICE](NOTICE).
