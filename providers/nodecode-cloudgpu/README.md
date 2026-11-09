# nodecode-cloudgpu

CloudGPU: DeepSeek, Kimi, GLM and MiniMax models from cloudgpu.app on one OpenAI-compatible key. A Nodecode rewrite of [mrhy7176/hermes-cloudgpu](https://github.com/mrhy7176/hermes-cloudgpu) by [mrhy7176](https://github.com/mrhy7176).

## Get it

Tell the agent "add the cloudgpu cell", or run `nodecode add nc://cloudgpu`.

## Set it up

Make a key at <https://cloudgpu.app/api>. Save it with `/connect` (pick
CloudGPU), or set `CLOUDGPU_API_KEY` in the environment; a saved key answers
first.

Optional, and rarely needed — the section is on unless it says
`"enabled": false`:

```jsonc
{
  "cloudgpu": {
    // where the OpenAI-compatible API is served; a relay's address goes here
    "base_url": "https://cloudgpu.app/v1"
  }
}
```

The original also lets `CLOUDGPU_BASE_URL` stand in for the address; this cell
reads it the same way, and the variable wins over the section.

## Use it

`/models` lists CloudGPU's models, and a turn on one goes to
`https://cloudgpu.app/v1/chat/completions` with `Authorization: Bearer`.

Until the live list is read, the picker holds the six models the original's
profile bundles, `deepseek-v4.1-flash` among them. `/cloudgpu` reads the list
the API publishes at `GET https://cloudgpu.app/v1/models` — public, no key
needed — and puts it in the picker for the rest of the session, so the models
a deployment actually serves are the ones offered. When that list cannot be
read, the bundled six stand and the command says why; `/cloudgpu TEXT` narrows
the listing to the ids containing TEXT.

The list is read from the same address a turn goes to, so a key that routes to
another deployment lists that deployment's models.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-cloudgpu
```

MIT licensed — see [LICENSE](LICENSE). Ported from the original plugin; see
[NOTICE](NOTICE).
