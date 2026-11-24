# nodecode-switchyard

Switchyard: asks a Jev decision which skill fits the task, and how much effort to spend. A Nodecode rewrite of [bgrablin/hermes-switchyard](https://github.com/bgrablin/hermes-switchyard) by [bgrablin](https://github.com/bgrablin).

The original plugin sits in the agent's loop: on every eligible turn it offers
its skill catalog to a hosted decision model, adopts the winner when it is sure
enough, and lowers the reasoning effort of routine requests. This cell keeps
the decision and hands it to the model:

- `switchyard_route` — a tool: "which of these skills fits this task, and how
  much reasoning does it need?"
- `(switchyard:route task &key skills effort)` — the same through eval.
- `(switchyard:skills)` — the prose skills this home keeps, as candidates.
- `/switchyard` — status, `check`, `skills`.

One request, one answer, nothing loaded, nothing changed behind your back: the
cell is advisory, and the model decides what to do with the answer.

## Get it

Tell the agent "add the switchyard cell", or run `nodecode add nc://nodecode-switchyard`.

## Set it up

A key, from either provider:

- TypeSafe — make one at <https://console.typesafe.ai/keys>, then
  `TYPESAFE_API_KEY`, or save it in `auth.json` under `api_keys.typesafe`.
- OpenRouter — make one at <https://openrouter.ai/keys>, then
  `OPENROUTER_API_KEY`, or `api_keys.openrouter` in `auth.json`.

`provider` is `auto` (TypeSafe when its key is there, else OpenRouter), or one
of them by name. `effort_cap` is the operator's level — the answer never goes
above it. `endpoint` and `model` are the escapes for a compatible endpoint of
your own. `/switchyard check` proves it answers.

```jsonc
"switchyard": {"provider": "auto", "effort_cap": "high", "timeout_seconds": 10}
```

## Use it

```
(switchyard:route "the release is red: the parser test fails on windows"
                  :skills '("writing-tests" ("release" "cut a release, changelog")))
=> (:NEEDS-SKILL 0.91 :SKILL "writing-tests" :EFFORT "low" ...)
```

```
model: switchyard_route {"task": "cut 1.4", "skills": ["release", "writing-tests"]}
       => skill: release (P 0.88, confidence 0.71); effort: minimal (cap high); needs_skill 0.93
```

The answer's thresholds are the original plugin's, uncalibrated policy and
labelled as such: a skill is adopted only when `needs_skill` is at least 0.80,
the winning option's probability at least 0.80 and the choice's own confidence
at least 0.80. Below any of them the decision is "none fits", with the reason.

What leaves this machine: the task text (at most 4,000 characters), the
candidate names (and their descriptions when you pass pairs), the cap, and the
provider's key. Nothing else — no history, no tool output, no files.

## What it is not

Not the plugin's hooks: it does not run on every turn, does not adopt a skill
by itself, and does not touch the request's effort — the model asks, and acts.
The computer-use and browser tools, the session-search rerank, the approval
provider and the legacy-cleanup command live in the original and not here (see
[NOTICE](NOTICE)).

Licence: MIT, the original's. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
