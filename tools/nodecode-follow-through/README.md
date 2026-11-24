# nodecode-follow-through

Follow-Through: an agent never ends a turn on a promise. A Nodecode rewrite of [Agenziamaia/follow-through](https://github.com/Agenziamaia/follow-through) by [Agenziamaia](https://github.com/Agenziamaia).

When a model's final answer says work has started or will happen -- "I've started the deep dive and will send you the report shortly" -- and nothing is running, the answer is held and turned into a nudge: do that work now, in this turn, or actually launch it and say exactly what is running. The turn that promised ends and the next one opens on the nudge, so the work really does continue.

## Get it

Tell the agent "add the follow-through cell", or run `nodecode add nc://follow-through`.

## Set it up

Nothing is required. The `follow-through` section has one member, `max_nudges`: how many times one prompt may be held before a promising answer is let through (2 by default, 0 to keep only the tool).

```jsonc
"follow_through": { "max_nudges": 2 }
```

## Use it

Nothing to do: the cell is on as soon as it starts, and every session gets it.

- `/follow-through` (or `/follow-through status`) says whether it is on, the cap, and how many nudges this session has had since your last prompt. `/follow-through off` and `/follow-through on` are the switch.
- The `follow_through` tool takes a `quote` and answers the same rule, so an agent that wants to check its own draft before sending it can ask.
- `(nodecode-follow-through:promise-sentence TEXT)` is the reader itself, for a script or a test: the promise sentence in TEXT, or NIL.

What is left alone: offers and questions ("If you want, I'll...", "Should I...?"), negations ("I'm not going to..."), honest status ("Done: ..."), and any sentence shorter than twelve characters. The patterns are English and Italian, as the original's were.

## How it works

One hook reads each model round as it is recorded (`NLK:RECORD-ASSISTANT-MESSAGE`); a message with text and no tool call is a round that can end the turn, and its first promise sentence is the one quoted. The nudge goes into the session through `NLE:SUBMIT`, the one in-process ingress, so the agent's next turn starts with it in front of it. A second hook, around `NLE:SUBMIT`, starts the count over at every prompt that is not the cell's own -- a chain of promises cannot outrun `max_nudges`, and the next thing the person says resets it.

Nothing is written to disk, no network is used and no model call is made: the detector is the original's regexes, the count is a hash table.

The original could rewrite the reply inside the same turn, because Hermes lets a plugin do that; a cell cannot, so the nudge arrives as a prompt of its own. `NOTICE` says what that cost.

## License

MIT, Agenziamaia's (see `LICENSE` and `NOTICE`).
