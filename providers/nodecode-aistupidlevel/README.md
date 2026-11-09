# nodecode-aistupidlevel

AI Stupid Level: the Smart Router, where each request goes to the model measuring best right now, as a Nodecode provider. A Nodecode rewrite of [StudioPlatforms/aistupidlevel-sdk](https://github.com/StudioPlatforms/aistupidlevel-sdk) by [StudioPlatforms](https://github.com/StudioPlatforms).

With it, `/connect` offers AI Stupid Level, `/models` lists its routing
modes, and a turn on `aistupidlevel/auto-tooling` goes to
`https://aistupidlevel.info/v1` over the chat lane. A model id here is not
one model but a routing rule:

- `auto-tooling` sends each request to the model scoring best on the
  tool-use benchmark (the mode to pick for an agent)
- `auto-coding`, `auto-reasoning`, `auto-best` and `auto-value` route on the
  coding, multi-turn reasoning, combined and score-per-dollar benchmarks
- `auto-task` picks one of those from the request itself
- an id such as `claude-opus-5-5` pins that one model

The router calls the model with the provider keys saved on the account
(OpenAI, Anthropic, Google, DeepSeek, Kimi, GLM), so billing stays on those
accounts. It takes text and tool calls only and refuses image input.

The live catalog is the router's own `GET /v1/models`, which Nodecode's
listing reads with the key once it is saved: the pinned models and any mode
added later appear in `/models` without a change here. The seven rows above
(the original's fallback list and the fast mode it names for titles and
compression) stand in until that listing answers.

See [NOTICE](NOTICE).

## Get it

Tell the agent "add the aistupidlevel cell", or run `nodecode add nc://aistupidlevel`.

## Set it up

Make a router key at <https://aistupidlevel.info/router/keys> (it starts
`aism_`), and add the provider keys the router should call with in the
router settings on the same site. Save the router key with `/connect`, or set
`ASL_ROUTER_KEY`; a saved key wins over the variable.

The section is on unless it says `"enabled": false`:

```jsonc
{
  "aistupidlevel": {
    // where the router is served; the chat lane appends /chat/completions
    "base_url": "https://aistupidlevel.info/v1"
  }
}
```

The original's profile also names an `ASL_BASE_URL` variable for the same
address; here that setting is the section's `base_url`.

## Use it

Pick a routing mode in `/models` and talk to it as usual. `auto-tooling` is
the original's own recommendation for an agent loop: the router keeps a
tool-calling conversation on one model, so a loop does not switch models
mid-task.

MIT licensed.
