# nodecode-modular

Modular Cloud: MiniMax, DeepSeek and Gemma models, each sent the reasoning
effort its API takes. A Nodecode rewrite of
[tommyjtl/hermes-modular-provider](https://github.com/tommyjtl/hermes-modular-provider)
by [tommyjtl](https://github.com/tommyjtl).

[Modular Cloud](https://docs.modular.com/models) serves open models over an
OpenAI-compatible API at `https://api.modular.com/v1`. With this cell,
`/connect` offers Modular, `/models` lists the four models it describes, and a
turn on `modular/<model>` goes out the way Modular takes it:

- the reasoning control is a nested `reasoning` object, not the chat lane's
  top-level `reasoning_effort`: `{"effort": LEVEL}` to think at a depth,
  `{"enabled": false}` to stop thinking, `{"enabled": true}` to just think;
- a level the model does not offer is lowered to the nearest one it does,
  never raised: DeepSeek V4.1 Flash takes `low`, `high` and `max`, so
  `minimal` becomes `low` and `xhigh` becomes `high`; MiniMax-M3.1-Flash
  takes five levels; minimax-m3 and gemma-4-31b-it take a toggle and no
  level at all, so any pick there means "think";
- turning reasoning off sends `reasoning.enabled = false` where the model has
  that switch, and its lightest level otherwise (MiniMax-M3.1-Flash always
  reasons, so it gets `low`);
- with no level picked, a model with a switch is told to think and a model
  without one is sent no reasoning field, so its own default stands -- the
  original's rule.

## Get it

Tell the agent "add the modular cell", or run `nodecode add nc://modular`.

## Set it up

Make a key at <https://docs.modular.com/models>, save it with `/connect`, or
set `MODULAR_API_KEY`. The key rides to Modular and nowhere else.

The section is on unless it says `"enabled": false`:

```jsonc
{
  "modular": {
    // a proxy, or another base speaking Modular's wire
    "base_url": "https://api.modular.com/v1"
  }
}
```

## Use it

Pick Modular in `/models`, then a model, and pick a thinking level with
`/think` if you want one. A model the table does not describe still works if
you name it: it is dialled at the same base and sent no reasoning object.

## Not ported

- The `GET /v1/models` read that narrowed the live list to the described
  models. Nodecode reads this cell's catalog row instead, which carries the
  same four models; a model Modular adds is one line in `provider.lisp`.
- Prices, which the original left to Modular's public catalog.
- The profile's default auxiliary model (`MiniMaxAI/MiniMax-M3.1-Flash`):
  Nodecode has no per-provider auxiliary model to set.

Licensed under the Apache License 2.0; see [LICENSE](LICENSE) and
[NOTICE](NOTICE).
