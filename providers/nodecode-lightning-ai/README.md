# nodecode-lightning-ai

Lightning AI: its OpenAI-compatible gateway as a Nodecode provider, with the model list it publishes. A Nodecode rewrite of [dinosauria123/hermes-lightningai-provider](https://github.com/dinosauria123/hermes-lightningai-provider) by [dinosauria123](https://github.com/dinosauria123).

## Get it

Tell the agent "add the lightning-ai cell", or run `nodecode add nc://lightning-ai`.

## Set it up

Make a key at <https://lightning.ai>. Save it with `/connect` (pick Lightning
AI), or set `LIGHTNING_API_KEY` in the environment; a saved key answers first.
The original also reads `LIGHTNING_AI_API_KEY`, and this cell reads it second.

Optional, and rarely needed — the section is on unless it says
`"enabled": false`:

```jsonc
{
  "lightning-ai": {
    // where the OpenAI-compatible gateway is served; a relay's address goes here
    "base_url": "https://platform.lightning.ai/api/v1"
  }
}
```

The original sends chat and its model probe to `https://lightning.ai/api/v1`.
That host now answers `{"error": "moved"}` and names
`https://platform.lightning.ai/api/v1`, which is the address this cell defaults
to; set `base_url` (or `LIGHTNING_BASE_URL`, which wins over the section) back
to the old address if you are pinned to it.

## Use it

`/models` lists Lightning AI's models, and a turn on one goes to
`<base>/chat/completions` with `Authorization: Bearer`.

The models are the ones the gateway publishes: the original fetched
`GET <base>/models` and held them, and this cell reads the same list — once at
start, when a key is set, and again whenever `/lightning-ai` runs, which puts
the rows in the picker for the rest of the session. `/lightning-ai TEXT`
narrows the listing to the ids containing TEXT. With no key, start makes no
request at all; when a read fails the command says why and a keyless box is
told where a key is made.

The list is read from the same address a turn goes to, so a key that routes to
another deployment lists that deployment's models.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-lightning-ai
```

MIT licensed — see [LICENSE](LICENSE). Ported from the original plugin; see
[NOTICE](NOTICE).
