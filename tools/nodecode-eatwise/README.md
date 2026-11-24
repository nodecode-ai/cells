# nodecode-eatwise

Eatwise: a local-first Chinese meal nutrition review, daily targets and a consented on-device meal history. A Nodecode rewrite of [harrylabsj/hermes-plugin-eatwise](https://github.com/harrylabsj/hermes-plugin-eatwise) by [harrylabsj](https://github.com/harrylabsj).

The original plugin was a manifest around [eatwise-mcp](https://github.com/harrylabsj/eatwise-mcp), a zero-dependency Node server reached over MCP. This cell carries the engine's arithmetic and food table itself: 467 Chinese and Taiwanese dishes, staples and cooked-dish recipes, per 100 g, with their default servings. Nothing leaves the machine -- no request, no subprocess, no Node.

## Get it

Tell the agent "add the eatwise cell", or run:

```sh
nodecode add nc://eatwise
```

## Set it up

Nothing is required. In `/settings`, the section `eatwise` carries:

| Setting | Default | What it does |
|---|---|---|
| `history_file` | (empty) | where the profile and the meals live; empty is `eatwise/store.json` under the Nodecode home, a relative path is read under it |
| `meal_share_percent` | 33 | the share of the day's targets one meal is compared with when its type is not named |
| `retention_days` | 180 | how many days a recorded meal is kept |

Consent is not a setting but a pair of switches the person turns on, through the `eatwise_history` tool's `configure` action: **profile memory**, then **meal recording**. Nothing is written before they agree, and turning a switch off keeps what is stored -- `delete` removes it.

## Use it

- `eatwise_foods` -- "红烧肉", "白饭", "粥": the entries a meal is likely to name, with the id, one serving in grams and the numbers per 100 g.
- `eatwise_targets` -- BMR, TDEE, target calories, protein, fat, carbohydrate, fibre, sodium and water, from the profile on the machine or from fields you pass. A minor, a pregnancy or a breastfeeding stage answers a referral instead of numbers.
- `eatwise_review_meal` -- one meal against the table and the day's targets: every food with its grams and numbers, the meal total, the comparison, one action and the assumptions (a serving nobody wrote is the default serving, and the answer says so). `confirmed=true` is required because the foods and portions are the person's to confirm; `today_before` judges the day, `day_complete` additionally allows an under-limit verdict; `save=true` writes the meal once meal recording is on.
- `eatwise_history` -- `status`, `configure`, `profile`, `save_profile`, `list`, `day`, `delete`.
- `/eatwise` shows the store's state, `/eatwise 2026-11-08` one recorded day, `/eatwise 2碗白饭 红烧肉` a review of what you type.
- `(help :eatwise)` carries the guidance the model reads.

A profile holds gender, age, height, weight, activity level, goal, health risks, target weight and life stage -- never a name, phone number, id or medical record, which the original's schema rejected by design and this one refuses too.

## Notes

- The numbers are the engine's: TFND (Taiwan Food Nutrient Database) and USDA FNDDS entries, plus derived cooked-dish recipes whose values are recomputed from their components. Every answer names its source, and a derived recipe says it is an editable default.
- A meal's numbers are deterministic: the same items always give the same answer, which is why the model must not estimate calories itself.
- The store is one JSON file, written atomically, under the home; a meal is dropped once it is older than the retention.

## Tests

```sh
.github/test-cell.sh tools/nodecode-eatwise
```

Offline: the section lifecycle, the table lookup, the target arithmetic and its refusal, a safety referral, a review from items and from text, the consent gates and a saved meal read back by day.

## Credits

A rewrite of harrylabsj's Hermes Agent plugin, MIT, at commit `d482a1230703e3714abf187754608fd0704000d5`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
