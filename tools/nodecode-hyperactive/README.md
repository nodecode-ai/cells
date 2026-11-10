# nodecode-hyperactive

Hyperactive: goals, promises and tap cards that speak first. A Nodecode rewrite of [justin06lee/hyperactive](https://github.com/justin06lee/hyperactive) by [justin06lee](https://github.com/justin06lee).

The operator states a goal, a promise, a decision or a quick call; the cell files it, and
then it does the asking. Every pulse it looks for what is due -- a check-in, a promise
that has come due, a decision to revisit, a goal that has stood still, a card waiting on an
answer -- and says it, as one message, each item naming the goal it serves.

## Get it

Tell the agent "add the hyperactive cell", or run `nodecode add nc://hyperactive`.

## Set it up

Nothing is required; the defaults are a pulse every five minutes, at most five items in one
message, a card re-asked after an hour, and a nudge after three still days. They live in the
`hyperactive` section of `~/.nodecode/config.jsonc` (or `/settings`):

```jsonc
"hyperactive": {
  "pulse_minutes": 5,          // how often it looks for what is due
  "max_items": 5,              // most items one message carries; the rest wait
  "card_minutes": 60,          // before a card is asked again, and again before a low-stakes one is settled
  "stale_days": 3,             // a week goal standing still this long gets one nudge (quarter goals: four times)
  "quiet_hours": "22:00-07:00" // local; empty is never quiet. Deferred, never dropped.
}
```

What it keeps is one file, `~/.nodecode/hyperactive/state.json`, under the home.

## Use it

Ask for what you want in the conversation -- "I want the draft out by Tuesday", "I owe Sam
the deck Friday", "we're going with SQLite, let's revisit in a month" -- and the model files
it through the `ha_*` tools. Or drive it yourself:

```
/hyperactive                          goals, cards, promises, the schedule
/hyperactive due                      run the pulse now
/hyperactive answer CARD OPTION       answer a tap card
/hyperactive why                      why it last spoke
/hyperactive done COMMITMENT          close a promise
/hyperactive goal HORIZON TITLE [--parent N] [--follow-up WHEN] [--why TEXT]
/hyperactive followup GOAL WHEN TEXT
/hyperactive decision ID keep|revise [new choice]
```

Two rules keep it safe to leave running. Every message names the goal it serves, so
`/hyperactive why` can explain any of them. And it never settles money, public or
irreversible calls: a low-stakes card whose recommendation goes unanswered is asked again
after `card_minutes`, settled through its typed action as long again after that, and
anything else waits for the operator however long it takes.

A due item arrives as the cell's standing notice: the operator sees it at once, and the
model reads it at the tail of its next request, so it brings it up on its own. The pulse
makes no model call.

## Licence

MIT. A rewrite of justin06lee/hyperactive; see `NOTICE` and `LICENSE`.
