# nodecode-openrouter

A Nodecode cell for [OpenRouter](https://openrouter.ai). models.dev already
makes OpenRouter usable on the chat lane; this cell keeps that and adds what
oh-my-pi does:

- `/openrouter login` signs in from the browser and saves the key OpenRouter
  hands back where `/connect` saves keys (`api_keys.openrouter`)
- `/models` lists omp's bundled OpenRouter rows beside models.dev's
- every request names the app it comes from (`HTTP-Referer`,
  `X-OpenRouter-Title`, `X-OpenRouter-Categories: cli-agent`) and asks
  OpenRouter's cache to keep it an hour (`X-OpenRouter-Cache`,
  `X-OpenRouter-Cache-TTL: 3600`); the attribution names Nodecode
- a routing variant, when one is chosen, rides the model id
  (`openai/gpt-5.5:nitro`), unless the id names one already
- reasoning goes as OpenRouter's own `reasoning: {"effort": ...}`, and off
  as `{"enabled": false}`, never as `reasoning_effort`
- no output cap the operator did not set: a catalog cap above an upstream's
  own makes OpenRouter skip that upstream
- routing preferences (`only`, `order`) ride as the body's `provider`

Ported from [oh-my-pi](https://github.com/can1357/oh-my-pi)'s `openrouter`
provider. See [NOTICE](NOTICE).

## Sign in

```
/openrouter login
```

answers at once with the address to open, and listens on
`http://localhost:54549/callback` (another port when that one is taken;
OpenRouter accepts any loopback callback). The sign-in is OpenRouter's PKCE
flow: no client registration, the S256 verifier is the proof. When the
browser comes back, the code is exchanged for a key, the key is saved, and a
notice says so. `http://localhost:<port>/launch` redirects to the sign-in
page, for a terminal that cut the long address.

On another machine, paste the address the browser ended on, the code in it,
or a key you made by hand:

```
/openrouter code <address, code or sk-or-... key>
```

A pasted key is checked against `/api/v1/auth/key` before it is saved. The
key does not expire. `/openrouter status` says whether a key is saved,
`/openrouter logout` forgets it.

Without the sign-in: make a key at <https://openrouter.ai/settings/keys>
and save it with `/connect`, or set `OPENROUTER_API_KEY`. A saved key wins
over the variable. No other provider's key is ever sent to OpenRouter: with
neither, the round goes keyless and OpenRouter says why.

## Install

Copy this folder into `~/.nodecode/cells/nodecode-openrouter/` and restart
Nodecode, or run `(restart-cells)`.

## Configure

Optional. The section is on unless it says `"enabled": false`:

```jsonc
{
  "openrouter": {
    "base_url": "https://openrouter.ai/api/v1",
    // default, nitro, floor, online or exacto
    "variant": "nitro",
    "only": ["anthropic", "openai"],
    "order": ["anthropic"]
  }
}
```

## Test

From this repository's root:

```sh
tools/test-cell.sh nodecode-openrouter
```

## Not ported

- omp's per-model compat (strict-tool retry on OpenRouter's Anthropic
  upstreams, reasoning-history filtering for Muse Spark, the cost OpenRouter
  reports per request): the core's chat lane has no seam for a retry without
  strict tools, and reads usage, not OpenRouter's billed cost.
- omp's OpenRouter Responses, image, video, rerank and decisions transports:
  this cell serves the chat wire only.

MIT licensed.
