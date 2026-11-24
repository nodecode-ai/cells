# nodecode-witness

Witness: grades each skill the agent keeps against what its tools actually did, and quarantines the ones the trail contradicts. A Nodecode rewrite of [BenCoy09/hermes-witness](https://github.com/BenCoy09/hermes-witness) by [BenCoy09](https://github.com/BenCoy09).

An agent that writes its own skills grades its own homework. Witness checks the receipts. It asks no model anything: the evidence is this session's tool trail — the results the store already journalled — and the rules are deterministic.

## Get it

Tell the agent "add the witness cell", or run `nodecode add nc://witness`.

## Set it up

Nothing to set up, and no key. The section, in `/settings` or the Settings tab:

```jsonc
{
  "witness": {
    "mode": "gate",              // gate quarantines a grade-D skill; observe only labels it
    "dir": "",                   // where the grades, the ledger and the quarantine live; empty is witness/ under this home
    "min_uses": 3,               // uses a skill needs before it can be called degraded
    "degraded_percent": 60        // the failed share of those uses, in percent
  }
}
```

## Use it

When the agent keeps a prose skill — `(define-skill ship-it "# Ship it ...")` — the cell grades it on the spot, against the tool results of the session that wrote it:

| What it finds | Where it comes from |
| --- | --- |
| the last tool call before the save failed | the session's tool results |
| the same error twice | the errors, folded to their signature |
| a verify script written and never run | the skill's own file names vs the commands that ran |
| "verified" with no passing test | the skill's words vs the trail |
| a push/deploy/delete that drops one of your rules | your own `define-memory` rules (`only <branch>`, `never <env>`) |
| placeholders, unsupported confidence | the skill's words |

Grades: **A** a passing verify command stands in the trail · **B** no negative evidence · **C** weak · **D** contradicted by the trail.

In mode `gate` a grade-D skill's file is moved to `witness-quarantine/` beside its cell, out of the index, and the model is told in its prompt not to claim the skill is verified. `/witness release NAME` puts it back and trusts it.

The model's tool:

```
witness(action, skill, text)
  action=grade    grade skill (a name the index lists) or text (prose given here)
  action=report   every grade, worst first
  action=show     one skill's findings
  action=verify   check the chained ledger
  action=trace    the last tool names and argument keys the cell saw
```

The operator's command:

```
/witness                 every grade, worst first
/witness show NAME       the evidence behind one grade
/witness grade NAME      grade it again now
/witness trust NAME      vouch for it; the gate stops flagging it
/witness release NAME    restore a quarantined skill to where it was
/witness mode [gate|observe]
/witness verify          the ledger's sha256 chain
/witness trace           what the hooks last received
```

Every grade and every human action is a line of a sha256-chained ledger under `<dir>/ledger.jsonl`: a line edited or removed in the middle shows up in `/witness verify`.

## Honest limits

- The rules read tool *results*, never the agent's summary. An unknown result shape counts as a success, so the gate errs toward missing a problem, not toward crying wolf.
- A skill is graded when it is written through the eval seam (`nlk:record-definitions`) or on demand. A skill edited in a file by hand is not graded until `/witness grade NAME`.
- Only the prose shape (`(define-skill name "one sentence" ...)`) is graded; a procedure (`(define-skill name (args) ...)`) is a function in the layer and is left alone.
- The constraint rules understand `only <env>` and `never <env>` phrasing, not arbitrary prose.
- It cannot prove a skill correct. It can show when the evidence contradicts it.

MIT, like the original. The credit and what the rewrite left out are in [NOTICE](NOTICE).
