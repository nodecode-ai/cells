# nodecode-model-presets

Model Presets: named model, provider and reasoning-effort picks, landed with one `/preset` or one tool call. A Nodecode rewrite of [will-lynas/hermes-model-presets](https://github.com/will-lynas/hermes-model-presets) by [will-lynas](https://github.com/will-lynas).

A name you keep saying — the small fast one, the one that thinks hard, the one on a
subscription rather than a key — is a preset. Write it once in the config, and moving a
session onto it is one short line instead of a model id, a provider and a reasoning rung.
The original plugin did this by rewriting the `/model` line at the harness gateway; this
cell hands the pick to the gateway's own `SELECT-MODEL` and `SELECT-EFFORT`, so a preset
switch pins the session, checks the rung against that model's ladder, makes the pair the
default a new session starts on, and every attached shell's footer follows — the same
thing a `/models` pick does.

## Get it

Tell the agent "add the model-presets cell", or run `nodecode add nc://model-presets`.

## Set it up

No key, no network, nothing required: presets are names you choose. Name them under
`model-presets` in `~/.nodecode/config.jsonc` (or in `/settings`, as the same object), one
object a preset:

```
"model-presets": {
  "presets": {
    "fast": { "model": "claude-haiku-4.5", "provider": "anthropic" },
    "deep": { "model": "gpt-5.4", "provider": "openai", "effort": "high" }
  }
}
```

`model` is the id a `/models` pick would take and `provider` the lane that serves it — both
required. `effort` is optional: one of `off`, `minimal`, `low`, `medium`, `high`, `xhigh`,
`max`, and left out means the model's own reasoning. The original plugin's words are read
too: `aliases` instead of `presets`, `reasoning_effort` instead of `effort`. A preset of
the wrong shape refuses at start and says which member is wrong rather than being dropped
silently.

`config.example.jsonc` beside this file is the whole section, commented.

## Use it

```
/preset                     the presets, as a picker; a row switches this session
/preset list                names, models, providers and efforts, one a line
/preset status              what this session is pinned to now
/preset fast                switch this session to `fast`
/preset fast --effort high --provider openrouter    one pick's overrides
```

The agent can do the same from a turn, which is what makes this worth having in a
conversation: "put this session on my deep preset" is one call of the `model_presets` tool
(`action: "switch", preset: "deep"`), and "which presets do I have" is `action: "list"`.
Overrides take precedence over the preset's own values, exactly as the original's
`--provider` and `--reasoning` flags did.

A switch applies from the next turn, and a session that has already begun keeps the model
it began on — so if a different model is wanted in the middle of long work, say so before
the next ask. A rung the model does not admit is refused with the rungs it does, the model
having already moved; the answer says so in as many words. `/preset status` reads what is
pinned without moving anything.

`(help :model-presets)` answers the manual while the cell runs. The picker, the lines and
the refusals all come from the one table the section declares, so a preset that reads
right is a preset that works.

## Credits and licence

MIT, the original's licence, kept byte for byte in `LICENSE`; `NOTICE` says what was
rewritten and what was left out. Tests: `.github/test-cell.sh tools/nodecode-model-presets`
from the cells repository — the pick is stubbed, so they need no provider, no key and no
network.
