# nodecode-empiriolabs

EmpirioLabs AI: its live chat catalog as a Nodecode provider, with tool calling, image input and reasoning declared per model. A Nodecode rewrite of [EmpirioLabs-AI/hermes-empiriolabs](https://github.com/EmpirioLabs-AI/hermes-empiriolabs) by [EmpirioLabs-AI](https://github.com/EmpirioLabs-AI).

EmpirioLabs AI serves open models on infrastructure of its own behind the
OpenAI chat wire at `https://api.empiriolabs.ai/v1`, and publishes its catalog
at `GET /v1/models`. With this cell, `/connect` offers EmpirioLabs AI,
`/models` lists its chat models, and a turn on `empiriolabs/<model>` goes to
that service with your key.

The catalog is where the details come from, as the original had it:

- **The model list is the catalog's chat models that take tool calls.** A turn
  always offers tools and reads text back, so a row without
  `POST /v1/chat/completions` in `supported_endpoints`, without
  `function_calling` in `features`, or marked `deprecated` is left out. A base
  that answers a plain OpenAI-style list (an `EMPIRIOLABS_BASE_URL` of your
  own) has none of those fields, and is taken whole.
- **Each model's details are declared, not guessed.** The context window
  (`context_length` or `context_window`), image input (`input_modalities` or
  an `image`/`vision` feature), reasoning (`capabilities.reasoning` or a
  `reasoning` feature), the `reasoning_effort` levels the model lists under
  `supported_parameters`, and its input/output price per token all reach
  `/models`, the reasoning picker and the turn.
- **`reasoning_effort` is sent only when the model takes it.** With a level
  chosen and the model declaring levels, the round asks for the chosen one, or
  the nearest level below it — never a dearer one — and the weakest when the
  chosen one is below all of them. A model that declares none is sent nothing,
  so the service's own default applies.
- **The two ids the original falls back to** (`glm-5-3`, `qwen3-7-flash`) are
  listed before the catalog has answered, so the picker is never empty.

## Get it

Tell the agent "add the empiriolabs cell", or run `nodecode add nc://empiriolabs`.

## Set it up

Make a key at <https://platform.empiriolabs.ai/dashboard/api-keys>. The
picker asks for it when you choose EmpirioLabs AI; a key saved with
`/connect` wins over the environment. `EMPIRIOLABS_API_KEY` works the same
way, as the original read it.

The base is optional, for a relay in front of the service:

```jsonc
{
  "empiriolabs": {
    // where the service is served; the chat lane appends /chat/completions
    "base_url": "https://api.empiriolabs.ai/v1"
  }
}
```

## Use it

Run `/models`, choose **EmpirioLabs AI** and pick a model from the catalog.
`/think` offers the levels that model declares. Then talk as usual: prompts,
tool calls and tool results go to `api.empiriolabs.ai` (or your `base_url`)
with your key, and are billed to the account that owns it.

## Not ported

- The original's one-hour in-memory catalog cache with its back-off after a
  failed fetch. The picker's own listing cache already holds a fetched
  listing for ten minutes and a failure for one, and this cell adds no second
  copy; its catalog is refreshed by the listing the picker and `/connect`
  already make.
- The original's auxiliary-model pick (the lowest-priced featured chat model
  with tool calling and a 200K context). Nodecode keeps one auxiliary target
  for the whole image — `/models aux` — rather than one per provider, so the
  choice is yours there, with the price of every model in the catalog row.
- The original's gzip handling of the catalog body: `nlk:http` negotiates
  and decodes the response itself.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-empiriolabs
```

Every catalog GET is a stubbed `nlk:http`, every key variable a stubbed
`nle::credential-env`, every round a stubbed `dex:post`: nothing reaches the
service.

MIT licensed.
