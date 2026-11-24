# nodecode-approval-ledger

Approval Ledger: which approval questions were asked, how they ended and how long people took. A Nodecode rewrite of [olric3015/hermes-plugins](https://github.com/olric3015/hermes-plugins) by [olric3015](https://github.com/olric3015).

Nodecode asks its operator before it lets a risky shell command run: the guard, a
block pattern of your own, and the phone approval all put one question to the
person watching. Each question is answered, denied or times out, and then it is
gone. This cell advises the one seam those gates use and keeps one row per
question, so you can see afterwards that one rule asked thirty times this week,
that most of your answers come late at night, or that a rule you thought was
gentle is the one stopping you most.

## Get it

Tell the agent "add the approval-ledger cell", or run `nodecode add nc://approval-ledger`.

## Set it up

Nothing to set: every question is recorded from the first start. The section
only tunes it, in `~/.nodecode/config.jsonc` under `approval-ledger`:

```jsonc
{
  "cells": ["nodecode-approval-ledger"],
  "approval-ledger": {
    "keep_rows": 5000,          // what a trim keeps
    "max_bytes": 2000000,       // the size past which the ledger is trimmed
    "default_hours": 24,        // the window /approval-log reads with no argument
    "record": true,
    "surface": "operator"       // what the report calls where the question asked
  }
}
```

`file` names another ledger file; empty is `approval-ledger.jsonl` in the
Nodecode home.

## Use it

```
/approval-log             the last 24 hours, by the rule that asked
/approval-log session     the most recent session that asked
/approval-log all         everything in the ledger
/approval-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
/approval-log surfaces    add to any of the above to group by where it asked
/approval-log days        add to any of the above to group by day, newest first
/approval-log recent      the newest 10 decisions instead
/approval-log recent 25   the newest 25 (1 to 100)
/approval-log clear       delete the ledger
```

```
Approvals, all recorded:

rule              asked  approved  denied  unanswered  median wait
recursive delete      5         2       3           0         0.6s
```

- **asked** counts questions. A question a person answered counts once.
- **approved** is an answer that lets the call run (Allow once, Approve);
  **denied** is one that stops it (Deny, Reject).
- **unanswered** is a prompt nobody answered: it timed out, or no shell was
  there to ask. The call did not run.
- **median wait** is how long a person took to answer, from the question to the
  answer; silences are left out of it.

From eval, the same over the verbs:

```lisp
(approval-ledger:report :window "7d" :by "day")
(approval-ledger:recent :window "all" :limit 5)
(approval-ledger:record "deploy" "Deny" :wait 1.2)   ; a decision taken elsewhere
(approval-ledger:rows)                                ; the whole ledger, as plists
(approval-ledger:clear)
```

## What it records, and what it does not

One JSON line per decision in `approval-ledger.jsonl`: `ts`, `rule`, `surface`,
`session_id`, `answer`, `outcome`, `wait_s`. The question's own detail — the
command text — is **not** kept, only the rule's title. Past `max_bytes` the file
is rewritten to its newest `keep_rows` rows. The cell is an observer: it cannot
answer a question or change one, it never raises, and it has no network.

Every gate answers the model itself; this cell only reads the same question the
operator saw. A rule that denied a call without asking (a deny pattern) never
asked, and so is not a row.

## Files

`ledger.lisp` is the file, the row and the report, network-free. `cell.lisp` is
the manual, the question advice, the verbs and `/approval-log`.
