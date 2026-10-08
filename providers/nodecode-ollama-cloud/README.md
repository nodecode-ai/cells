# nodecode-ollama-cloud

A Nodecode cell for [Ollama Cloud](https://ollama.com), Ollama's hosted
runtime. With it, `/connect` offers Ollama Cloud, `/models` lists its models
(asked of `GET /api/tags`), and a turn on `ollama-cloud/<model>` goes to
`https://ollama.com/api/chat` over Ollama's own wire, the way oh-my-pi sends
it.

## Why a lane of its own

models.dev lists `ollama-cloud` on the OpenAI-compatible chat wire
(`https://ollama.com/v1`), which the core's chat lane already speaks. omp does
not use it: its catalog maps that very models.dev row to its `ollama-chat` api
(`provider-models/openai-compat.ts`,
`simpleModelsDevDescriptor("ollama-cloud", "ollama-cloud", "ollama-chat",
"https://ollama.com")`) and serves all 52 bundled models over `POST
/api/chat`. omp's source states no single reason; what the native wire carries
that `/v1` does not is:

- the `think` value, with Ollama's own rungs `low`, `medium`, `high` and
  `max`; the DeepSeek V4 and GLM 5.2 ladders end at `max`, which no
  `reasoning_effort` spelling reaches, and `think: false` turns thinking off
- the prompt's cache split (`prompt_eval_cached_count`), so cache reads are
  counted apart from the rest of the prompt
- `done_reason: "load"`, Ollama's answer to a request with no user turn,
  which omp turns into an error instead of an empty answer
- a tool result named by its tool (`tool_name`), images as base64 arrays

The cell ports omp's choice: it registers a lane, `ollama-cloud`, whose
stream (`wire.lisp`) speaks `/api/chat`:

- the request is `{"model", "messages", "tools"?, "think"?,
  "tool_choice"?, "options"?: {temperature, top_p}, "stream": true}`; the
  system prompt leads as a `system` message, a history system message (an
  eviction stub) stays `system`, a tool result is `{"role": "tool",
  "tool_name"}`, an assistant's calls go back with their arguments as
  objects, and its thinking is never sent back (Ollama Cloud answers 400 to
  history that carries `thinking`)
- a request with no user turn gets one: the last system turn past the
  prompt's own becomes `user`
- an image rides as base64 in `images` for a model that takes images; for one
  that does not, `[image omitted: model does not support vision]` stands in
  its place
- the effort maps to `think` as omp maps it: `off` is `false` for a thinking
  model, `minimal` and `low` are `low`, `xhigh` is `high`, `max` is `max`;
  no effort leaves the key off
- `tool_choice` is `none` or `required` (`auto` is left off); tool schemas
  pass omp's Ollama sanitizer: a boolean subschema widens to a union of every
  primitive type, a boolean `additionalProperties` goes, a type list folds
  to one type or an `anyOf` under `allOf`
- `num_predict` is never sent: omp marks every Ollama Cloud model
  `omitMaxOutputTokens`, so the output ceiling is the endpoint's
- the answer is NDJSON, one chunk per line: `message.thinking` is reasoning,
  `message.content` is text, `message.tool_calls` arrive whole (each named
  `ollama:<block>:<name>`, omp's id), and the `done` chunk carries the
  finish and the usage (`prompt_eval_count` less its cached part as input,
  the cached part as cache reads, `eval_count` as output)
- a round with calls is a tool round whatever `done_reason` says; `length`
  with nothing in it is the context overflow the core evicts on; `load` is an
  error; an `{"error"}` line on a 200 is said as the provider's error; a
  stream that ends without `done` is a truncated one

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `ollama-cloud`
provider. See [NOTICE](NOTICE).

## Key

Make a key at <https://ollama.com/settings/keys>. Save it with `/connect`, or
set `OLLAMA_CLOUD_API_KEY`. A saved key wins over the variable. `/connect`
checks a typed key by asking `GET /api/tags` with it: a 401 is a refused key,
any listing an accepted one. No other
provider's key (`OPENAI_API_KEY` among them) is ever sent to Ollama.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-ollama-cloud/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "ollama-cloud": {
    // where Ollama's native API is served; the lane appends /api/chat
    "base_url": "https://ollama.com"
  }
}
```

To keep the OpenAI-compatible wire instead, pin it the core's way, which the
cell leaves alone (the key still comes from this cell):

```jsonc
{
  "providers": {
    "ollama-cloud": {"sdk": "openai-completions", "base_url": "https://ollama.com/v1"}
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-ollama-cloud
```

## Not ported

- omp's stream markup healing (`utils/stream-markup-healing.ts` and the
  `dialect/` scanners behind it): Kimi's `<|tool_calls_section_begin|>`
  tokens and DeepSeek's DSML envelope leaked into `content` are not turned
  back into tool calls, and leaked `<think>` idioms are not lifted out of the
  text. The core's own route still takes a leading `<thinking>` block as
  reasoning.
- omp's message normalization (`transform-messages.ts`, 1,300 lines:
  cross-model thinking rewrites, orphan repair); the core's history fold
  already pairs every call with a result.
- omp's own retry of a 5xx (2 s, 5 s, 10 s, except llama.cpp's tool-call
  parse failure) and its replay of an empty answer: the core's retry policy
  and its PROVIDER-EMPTY-RESPONSE retry stand in for both.
- the per-model `effortMap` (no bundled row carries one) and the first-event
  watchdog (the core's idle deadline covers the wait for headers and every
  line after).
- Discovery asks `/api/show` only for a model the bundled rows do not carry,
  for its window; omp asks it for every model, for its window and its
  `thinking` and `vision` capabilities. A listed model the rows do not carry
  is taken to think (an effort reaches it as `think`) and to take no images.

MIT licensed.
