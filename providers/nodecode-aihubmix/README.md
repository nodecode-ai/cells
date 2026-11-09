# nodecode-aihubmix

AIHubMix: 400+ OpenAI-compatible models as a Nodecode provider, with the model picker filtered to the routes that can call tools. A Nodecode rewrite of [AIhubmix/hermes-provider-aihubmix](https://github.com/AIhubmix/hermes-provider-aihubmix) by [AIhubmix](https://github.com/AIhubmix).

## Get it

Tell the agent "add the aihubmix cell", or run `nodecode add nc://aihubmix`.

## Set it up

Make a key at <https://aihubmix.com/token>. Save it with `/connect` (pick
AIHubMix), or set `AIHUBMIX_API_KEY` in the environment; a saved key answers
first.

Optional, and rarely needed — the section is on unless it says
`"enabled": false`:

```jsonc
{
  "aihubmix": {
    // where the OpenAI-compatible API is served; a relay's address goes here
    "base_url": "https://aihubmix.com/v1"
  }
}
```

## Use it

`/models` lists AIHubMix's models, and a turn on one goes to
`https://aihubmix.com/v1/chat/completions`.

AIHubMix fronts many upstream backends and publishes no capabilities on its
plain model list, so the cell reads the capability-annotated catalog at
`https://aihubmix.com/api/v1/models?types=llm` and keeps only the entries
advertising `tools` or `function_calling` — either marker alone qualifies,
because requiring both would hide about a third of the agentic catalog. A
model that cannot call tools is dead weight in an agent's picker.

When that catalog is unreachable, the picker falls back to a curated list of
tool-capable flagships, so an offline start still fills it. A `base_url`
other than AIHubMix's own means a proxy or a self-hosted relay, whose catalog
is not AIHubMix's to interpret, so the curated list answers there too.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-aihubmix
```

MIT licensed — see [LICENSE](LICENSE). Ported from the original plugin; see
[NOTICE](NOTICE).
