# nodecode-costguard

CostGuard: meters token spend per session and day, warns near a budget, and refuses tool calls past it. A Nodecode rewrite of [Whydiriki700/costguard](https://github.com/Whydiriki700/costguard) by [Whydiriki700](https://github.com/Whydiriki700).

## Get it

Tell the agent "add the costguard cell", or run `nodecode add nc://costguard`.

## Set it up

Set a budget and it is enforced; with none set, CostGuard only meters and
reports, and never blocks.

```jsonc
{
  "costguard": {
    "session_token_budget": 200000,     // billable tokens one session may spend
    "daily_token_budget": 1000000,      // ... or one local day, all sessions
    "session_cost_budget_usd": "2.00",  // dollars, written as a string
    "daily_cost_budget_usd": "10.00",
    "warn_percent": 80                  // where the model is told to finish
  }
}
```

- A budget of `0`, or absent, is off. The two cost budgets are strings
  because a section has no decimal member: `"2.50"` is two dollars fifty.
- `warn_percent` (80) is the share of a budget at which one line rides the
  turn: prefer finishing over starting new sub-tasks.

## Use it

Ask the model what this run has cost, and it calls `costguard_status`:

```
CostGuard usage:
- this session: 41,238 tokens in 12 calls, est. cost $0.0871
- 2026-11-08 (today): 61,004 tokens in 18 calls, est. cost $0.1290
- 2026-11-06: 12,900 tokens in 4 calls, est. cost $0.0240 (1 unpriced call)
Budgets: session tokens 200000, daily tokens 1000000, session cost $2.00
```

`/cost` prints the same table for this session, `/cost budgets` names the
limits, and `(costguard:report :session-id "s" :days t)` answers it from
eval. `(costguard:spend :session-id "s")` answers the same numbers as data.

The numbers are the ones Nodecode already records: every provider round
leaves a `turn.usage` fact with the counts the provider reported and the cost
the catalog's price made of them when the round was recorded. Tokens counted
are the billable ones — input, output and reasoning; cache reads are free,
as providers bill them. A model the catalog does not price, or a round whose
provider reported no usage, meters in tokens with its cost honestly left
unknown rather than invented.

Once a budget is exceeded, every tool call is refused with a budget message
(CostGuard's own `costguard_status` stays callable), so the loop that spends
— each tool call is another provider round — is the loop the brake stops.
One honest limit, as the original has it: the brake cannot cancel the reply
already being generated when a budget trips; it stops the work that reply
asks for next, and the model is told to summarise.

Nothing leaves the machine: the cell reads the store and nothing else — no
network, no keys, no files of its own.
