# nodecode-advisor

Advisor: a second model reviews each turn's work and posts tagged advice. A Nodecode rewrite of [ljubomirj/hermes-advisor-plugin](https://github.com/ljubomirj/hermes-advisor-plugin) by [ljubomirj](https://github.com/ljubomirj).

The primary agent does the work; the advisor checks it. After a completed
turn the transcript goes to a second model, which answers in one of three
tags:

| Tag | Meaning | Delivery |
| --- | --- | --- |
| `[NIT]` | Non-urgent cleanup, a missed opportunity | Posted at once |
| `[CONCERN]` | Wrong direction, fragile approach, missing constraint | Held until a later review raises it again |
| `[BLOCKER]` | Fundamentally unsound path | Held until a later review raises it again |

Holding is the point: a one-off remark never interrupts the work. A concern
is posted only when a second review reaches for the same point again, and a
held note the next review is silent about is dropped as resolved. A review
with nothing to say says `nothing to flag`, and a review that leaked machine
output (a tool call, a bare JSON blob) is dropped rather than delivered.

The advisor reviews; it never acts. It has no tools, edits no files and
changes no session state. It reads the turn's transcript and the project's
`WATCHDOG.md`, if there is one, and answers prose.

## Get it

Tell the agent "add the advisor cell", or run:

```sh
nodecode add nc://advisor
```

## Set it up

The section is off until `auto` says otherwise; one review is one model
call, so the switch is the operator's.

```jsonc
{
  "advisor": {
    "auto": true,          // review every completed turn
    "model": "",           // empty: the auxiliary model (/models picks it)
    "provider": "",        // with model, a reviewer on a provider of its own
    "clip": 500,           // characters kept per tool call or result
    "max_note_chars": 1200,
    "watchdog": ""         // a file of project review guidance
  }
}
```

No key of its own: the review is one side call on the model Nodecode already
has configured, so it is billed to that provider and nothing else leaves the
machine it would not otherwise. Set `model` and `provider` to send the
review to a second model — the point of the cell is a different angle, and a
different model is the cheapest way to get one. (In a session, `/models`
picks the auxiliary target for every side call; `advisor.model` overrides it
for this cell alone.)

## Use it

```
/advisor                       on or off, the model, how many notes are held
/advisor on                     review every completed turn
/advisor off                    stop; /advisor review and the tool still work
/advisor review [focus]         review the session's newest turn now
/advisor model NAME            the reviewer's model, this run
/advisor provider NAME         the reviewer's provider, this run
/advisor clear                  drop the held notes and start over
/advisor test SEVERITY NOTE    deliver a test note: nit, concern or blocker
```

Advice that is ready stands as a notice under the cell's key: every shell
shows it, and the agent reads it in the notices block of its next request.
`/advisor clear` is the reset when a held note has gone stale — the
`off` then `on` of the original plugin.

The model has one verb of its own, `advisor_review`: pass it a transcript
(the ask, the steps, the tool calls and results, the answer) and it answers
the same tagged advice. Use it before committing to a large refactor, or
when the answer is not obviously the one that was asked for. Notes are held
per session, so a concern raised twice is delivered the second time.

`(help :advisor)` carries the same manual into the conversation.

## What changed from the plugin

The Hermes hooks are gone and the mechanism is a cell's: a `:FRAME` observer
sees a turn reach a terminal fact, one worker thread reviews it, and the
advice is a standing notice instead of an injected user message. The full
list is in [NOTICE](NOTICE). `/advisor`'s verbs, the severity model, the
hold-and-reconfirm rule and the reviewer's instruction are the original's.

## Tests

```sh
.github/test-cell.sh tools/nodecode-advisor
```

Offline: the reviewer is a scripted answer and the transcript a literal, so
the held-and-reconfirmed rule, the tool, the command, the queued turn and
the clipping are all asserted without a provider.
