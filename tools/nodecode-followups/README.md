# nodecode-followups

Follow-ups: the agent writes down a moment worth returning to, and it comes back at the right moment. A Nodecode rewrite of [xiaok/hermes-autonomous](https://github.com/xiaok/hermes-autonomous) by [xiaok](https://github.com/xiaok).

Most assistants wait. You ask, it answers, the thread closes — and the thing
you mentioned on Tuesday (you were sick, you had a meeting, you bought
something) never comes back. This cell keeps that thread.

A follow-up is not an unfinished task. It is a moment worth returning to, in
one of four origins: **care** (they mention a sore throat, an interview,
waiting on results), **utility** (going out tomorrow — check the weather),
**risk** (about to spend money or sign something), **curiosity** (something
minted, a company they talked about). "Did your throat get better?" is worth
saying with no action value at all.

## Get it

Tell the agent "add the followups cell", or run:

```sh
nodecode add nc://nodecode-followups
```

## Set it up

Nothing is required: the section runs on its defaults, and the agent fills the
board as the conversation goes. What you may want to set, in
`~/.nodecode/config.jsonc` or on the Settings page:

```jsonc
"followups": {
  "quiet_start": 23,        // no reminder from this hour...
  "quiet_end": 8,           // ...to this one; equal hours mean no window
  "cooldown_minutes": 90,   // the least time between two reminders
  "max_items": 3,           // how many one reminder may carry
  "max_lines": 6,           // and how many lines it may take
  "mute_days": 30           // how long a pushed-back category stays muted
}
```

## Use it

From a session, through eval:

```lisp
(followups:add :subject "the sore throat" :why "care" :at "+18h"
               :quote "my throat is killing me" :hint "ask if it is better")
(followups:due)                 ; what has come due, and whether the gate holds it
(followups:board)               ; the whole board
(followups:checked 3)           ; looked at it: back in a day, then two, then four
(followups:retire 3 :note "it healed")
(followups:outcome 3 "annoyed") ; three ignored or one annoyed mutes that category
```

The model has the same four verbs as tools: `followup_add`, `followup_list`,
`followup_checked`, `followup_close`. `/followups` in a shell shows the board
and its panel, and `(help :followups)` carries the manual.

**How a follow-up comes back.** At each model request the cell looks at the
board: the follow-ups whose time has arrived and which the gate lets through
arrive as one reminder in the round about to be asked — a running turn reads
it at its next round, an idle session at its next turn. Nothing runs in the
background and nothing polls.

**The gate is code, not judgement**, so it cannot be talked out of: the quiet
window (23:00–08:00 by default), a 90-minute cooldown between two reminders,
the categories you pushed back on, at most three follow-ups and six lines in
one reminder. A quiet refusal is final for that request; the item stays due
for the next one.

## Where it lives

One JSON file, `followups/board.json` under the home, mode 0600. `id`,
`key`, `subject`, `why`, `quote`, `hint`, `at`, `status`, `checks`, `sends`,
`outcome`, `note`: the board is that file, so it survives a restart and can be
read by anything.

The same words with the same key update one item rather than adding a second,
so a follow-up with new information (the flight is now at 16:10) is one line
in the board, not two.

## Credit

Rewritten from xiaok's Hermes Agent plugin `hermes-autonomous`, MIT. See
[NOTICE](NOTICE): what was kept, and what was left out.

Run `.github/test-cell.sh tools/nodecode-followups` from the cells repository.
