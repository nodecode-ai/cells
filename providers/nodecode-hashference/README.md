# nodecode-hashference

Hashference: open and frontier models over one OpenAI-compatible API, each row sized by the input and output limits Hashference's own catalog states. A Nodecode rewrite of [0xalank/hermes-hashference-provider](https://github.com/0xalank/hermes-hashference-provider) by [0xalank](https://github.com/0xalank).

## Get it

Tell the agent "add the hashference cell", or run `nodecode add nc://hashference`.

## Set it up

Make a key at <https://hashference.com/keys>. Save it with `/connect` (pick
Hashference), or set `HASHFERENCE_API_KEY` in the environment; a saved key
answers first.

Optional, and rarely needed — the section is on unless it says
`"enabled": false`:

```jsonc
{
  "hashference": {
    // where the OpenAI-compatible API is served; a relay's address goes here
    "base_url": "https://hashference.com/v1"
  }
}
```

## Use it

`/models` lists Hashference's models, and a turn on one goes to
`https://hashference.com/v1/chat/completions`.

Two things the catalog is read for, both of which a plain custom endpoint gets
wrong:

- **The input limit, not the window.** The hosted models advertise a
  163,840-token window but accept 98,304 input tokens, so a session sized from
  the window comes back refused as oversized input. The catalog states the
  input limit per model, and that is what the row carries.
- **The output cap.** Where the catalog states the model's default output
  cap (8,192 on the hosted models), a turn that sets none asks for that rather
  than a number the API would lower anyway.

The picker holds only the models an agent can run on: Anthropic Messages
aliases (the same models for another wire) are left out, as is anything the
catalog marks unable to call tools, and anything taking less than 64K tokens
of input. When the catalog is unreachable, a curated list of tool-capable
models fills the picker instead, so an offline start still works.

The catalog is read from `/models` of whatever `base_url` names, without
credentials, once at start and again every ten minutes while it stays
unreachable. Reading the catalog is the cell's only request of its own; chat
traffic goes through Nodecode's own client with your key.

## Test

From this repository's root:

```sh
.github/test-cell.sh providers/nodecode-hashference
```

MIT licensed — see [LICENSE](LICENSE). Ported from the original plugin; see
[NOTICE](NOTICE).
