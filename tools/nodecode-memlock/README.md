# nodecode-memlock

MemLock: pinned standing instructions the model keeps following after the conversation is compacted. A Nodecode rewrite of [Sahil-SS9/hermes-memlock](https://github.com/Sahil-SS9/hermes-memlock) by [Sahil-SS9](https://github.com/Sahil-SS9).

## Get it

Tell the agent "add the memlock cell", or run `nodecode add nc://memlock`.

## Set it up

Set `memlock.pins_file`, the JSON file the pins are kept in, in `/settings` (or the `memlock` section of `~/.nodecode/config.jsonc`): `memlock/pins.json` under the home is the usual answer. Two optional knobs sit beside it: `max_pins` (16) caps how many exist, `reminder_chars` (600) caps how much one turn repeats. Until `pins_file` is set, the pin tool and `/pins` refuse and say so.

## Use it

Tell the agent a rule that must keep applying -- "always answer in bullet points, and pin that", "never touch the main branch -- pin it" -- and it calls the `pin` tool. From the next turn, every turn's first request opens with the pins:

```
[Standing instructions — still active]
  - Reply in bullet points
```

so a rule stated once does not fade when the conversation is folded, thinned or edited away. The model can also `update` a pin, `unpin` one, or `list` them; the operator types `/pins` for the list, `/pins pin <text>` to pin one, `/pins unpin <pin_id>` to drop one. `(help :memlock)` has the same instructions for the model.

A pin is the organism's: every session reads the same set, and it lives in one JSON file under the home, so a stop and start (or a restart) keeps it. Priority orders the list when `reminder_chars` runs out; the pins that fit come first.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: the pins file, the `pin` tool, `/pins`, the `:round` hook that opens a turn with them, the memlock section. |
| `test/cell-test.lisp` | The tests: lifecycle, the tool and the command, the pin cap, the refusal without `pins_file`, delivery into a stubbed turn's request, and a pin surviving a restart. |
| `LICENSE` | The original's MIT licence. |

## Credits

Sahil-SS9's [hermes-memlock](https://github.com/Sahil-SS9/hermes-memlock) at `123b9fd`, MIT, copyright (c) 2026 Sahil Saghir. See [NOTICE](NOTICE) for what the rewrite keeps and what it leaves out.
