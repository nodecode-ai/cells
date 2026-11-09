# nodecode-epistemic

Epistemic Harness: a case that records what the agent predicted before a check, then what the check showed. A Nodecode rewrite of [quangphucqp/epistemic-harness](https://github.com/quangphucqp/epistemic-harness) by [quangphucqp](https://github.com/quangphucqp).

An agent investigating a failure can build an explanation around whatever it
finds. This cell gives the observation something to disagree with: before a
selected check, the model writes down what it expects and what would change its
belief; after the call, it records what came back, whether that matched, and
whether the belief moved.

A **case** is one bounded inquiry: a decision, the one unknown most likely to
change it, and a stopping condition. A **check** is committed against the case
— the tool it will call, the arguments, the predicted outcomes, and the
observation that would count against it. Committing runs nothing: the ordinary
tool call is made separately, and its own result is kept, so the comparison is
against what really came back.

| The model calls | For |
| --- | --- |
| `epistemic_case` `open` | the decision, the unknown, the stopping condition, the claims, the plan |
| `epistemic_probe` `commit` | the prediction, before the check runs; one pending check per case |
| `epistemic_probe` `compare` | what the check showed: `match`, `mismatch` or `unresolved`, and whether the belief changed |
| `epistemic_case` `revise` | the claim a mismatch contradicted, with the reason |
| `epistemic_case` `close` | the outcome and a summary, refused while a check is pending |
| `epistemic_case` `show` / `list` | the receipt, or every case |

While a case is running, its decision, its stopping condition and its pending
prediction ride each model request as one short reminder, so the agent does not
have to remember what it said it expected. `/case` shows the same thing to the
operator, `/case list` every case, `/case ID` one of them.

A mismatch marks the claim stale until a revision addresses it — a justified
downgrade (`grounded` to `hypothesis`, or to `unresolved`) repairs it, and a
no-op rewrite does not. A close is refused while a check is pending, and the
outcome cannot claim more than the checks showed.

Everything is local: one `cases.sexp` under the Nodecode home, no key, no
network, no telemetry.

## Get it

Tell the agent "add the epistemic cell", or run `nodecode add nc://epistemic`.

## Set it up

Nothing needs setting. The section in `~/.nodecode/config.jsonc` (or
`/settings`) moves the store or quiets the reminder:

```jsonc
{
  "epistemic": {
    "dir": "epistemic",     // where cases.sexp lives, under the home
    "remind": true,          // the running case in front of the model at each request
    "remind_chars": 900,     // how much of it one request carries
    "capture": true,         // keep the committed check's own tool result
    "observe_chars": 600,    // how much of that result is kept
    "case_chars": 1200,      // the longest single piece of case text
    "keep": 50               // closed cases kept; an active one always stays
  }
}
```

## Use it

Ask for a case when a question could change after a check — a failing service,
a disputed cause, which of two explanations to test next:

> Open a case on whether the config reload caused the failure, try one check,
> and close it with whatever the log supports.

The model opens the case, commits the prediction, makes the read, compares,
revises if the log says otherwise, and closes. `epistemic_case list` returns
every case it ever opened, with the outcome each one reached.
