# nodecode-interim-ledger

Interim Ledger: which models write messages mid-turn, how many per turn and how long. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

Many models say something before they call a tool: "Let me check the config
first", "Now I'll run the tests". In Nodecode that text scrolls past with the
tool output and is gone. This cell keeps one row for each of them, and
`/interim-log` shows which models write them, how many a turn gets and how
long they are - so you can tell that one model narrates three times a turn
and another says nothing at all.

## Get it

Tell the agent "add the interim-ledger cell", or run `nodecode add nc://interim-ledger`.

## Set it up

Nothing to set: no key, no endpoint, no dependency. One setting is optional,
and `/settings` holds it:

```jsonc
{
  "interim-ledger": {
    "rows": 5000        // how many messages the ledger keeps, newest first
  }
}
```

## Use it

```
/interim-log             last 24 hours, by model
/interim-log session     the most recent session
/interim-log all         everything in the ledger
/interim-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
/interim-log providers   add to any of the above to group by provider (or days)
/interim-log recent      add to any of the above to list the newest 10 messages instead
/interim-log recent 25   the newest 25 instead (1 to 100)
/interim-log clear       delete the ledger
```

`/interim-log` alone answers something like:

```
Mid-turn messages, last 24 hours:

model       messages  turns  per turn  median chars   last
a-model            3      2       1.5            57  3m ago

Counted for every turn, whether or not the text was drawn on a surface.
```

- **messages** counts mid-turn messages, and **turns** the turns that had at least one.
- **per turn** is messages divided by turns: how many a model writes when it writes any.
- **median chars** is the middle length of those messages, in characters.
- **last** is how long ago the newest one was. The model with the most messages comes first.

`/interim-log recent` lists the newest ten of the chosen window, newest first
(`recent 25` lists 25, up to 100), with the step of the turn each was written
at and how many tool calls rode it. `/interim-log days` puts one day on each
line; days are the machine's own calendar days. `session` and `all` take
precedence over a time window.

The same reports are on the manual for eval, when a report is to be pasted
somewhere:

```lisp
(interim-ledger:summary :window "all" :by "provider")
(interim-ledger:recent :window "7d" :limit 25)
(interim-ledger:clear)
```

## What it records

One JSON line per mid-turn message in `~/.nodecode/interim-ledger/interim.jsonl`:

`ts`, `session`, `turn`, `model`, `provider`, `step`, `chars`, `calls`.

The text of a message is never written, only its length; `session` and `turn`
are the first 12 hex digits of a SHA-256 of the session and turn ids, enough
to tell them apart. The file is bounded at the section's `rows`, newest kept.

## What it does not see

- A round where the model calls tools without writing anything first: there
  is no message to count.
- The final answer of a turn: only text beside a tool call is counted.

## How it works

It advises `nlk:record-assistant-message`, the one function every completed
provider round of every turn passes through, and records a row when the
message carried text beside at least one tool call. It is an observer: an
error of its own is swallowed, so a turn records itself exactly as it would
without the cell.

## Licence

MIT. Rewritten from olric3015's Hermes plugin; see NOTICE.
