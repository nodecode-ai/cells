# nodecode-kame-rotate

KAME API Rotation: one chat completion through a pool of API keys — the healthiest key per call, the next key instead of a failed one. A Nodecode rewrite of [Kame696/kame-api-rotation-for-hermes](https://github.com/Kame696/kame-api-rotation-for-hermes) by [Kame696](https://github.com/Kame696).

## Get it

Tell the agent "add the kame-rotate cell", or run `nodecode add nc://kame-rotate`.

## Set it up

The `kame-rotate` section of the config names the endpoint and, by environment
variable, the keys:

```jsonc
{
  "kame-rotate": {
    "base_url": "https://api.example.com/v1",
    "key_envs": ["EXAMPLE_KEY_1", "EXAMPLE_KEY_2"],
    "model": "gpt-4o-mini",
    "cooldown_seconds": 60
  }
}
```

The keys themselves stay in the environment variables `key_envs` names; the
section never holds one. `base_url` is any OpenAI-compatible endpoint —
`/chat/completions` is appended unless the address already names it. Set it
from `/settings` or the page's Settings tab like any other cell.

## Use it

Two tools, and one command:

- `kame_chat` `{prompt, system?, model?}` — one chat completion through the
  pool. The call starts at the first ready key, and a key that answers 429
  (honouring its `Retry-After`), 401, 403 or a 5xx sits out for
  `cooldown_seconds` before it is asked again; the next key serves the call
  instead of the turn failing. The answer says which key served it. A 400 and
  its siblings are the call's own fault, so they come back as one failure
  carrying the endpoint's words, without spending another key.
- `kame_keys` — every key's state: ready or cooling and for how long, calls
  served, calls failed, the status it last answered. Keys are never printed.
- `/kame status` shows the same; `/kame reset` forgets every key's record.

The pool is state in the cell, not on disk: a restart starts every key ready.

## What of the original it keeps, and what it drops

The original is the provider half of KAME: it hands every one of Hermes' API-key
provider profiles a client that rotates that provider's keys, invisibly. This
rewrite keeps the promise — healthiest key per call, a refusal read for what it
says, the next key instead of a failed turn — as verbs, because Nodecode's
provider lanes build their own clients and a cell cannot stand inside one.
One pool per section, not every provider at once; the secrets live in the
environment rather than in Hermes' key store.

MIT licence, the original's, in LICENSE. See NOTICE.
