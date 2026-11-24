# nodecode-ux3

Product judgment across user, evidence and business flows: the decision gate before a direction is built, and at its next iteration. A Nodecode rewrite of [cis2042/ux3-product-design-harness](https://github.com/cis2042/ux3-product-design-harness) by [cis2042](https://github.com/cis2042).

## Get it

Tell the agent "add the ux3 cell", or run `nodecode add nc://ux3`.

## Use it

Ask the model to judge a direction before it builds it, or call the gate yourself:

```lisp
(ux3:route "Add a migration for the billing table")
;; => (:ROUTE "commitment" :BYPASS NIL :REVIEW-REQUIRED T :MODE "standard"
;;     :WHY "the prompt names \"migration\", \"billing\": high risk, so it commits")

(ux3:evaluate "Let teams share one board" :evidence-tier "t2")
;; => (:ROUTE "discovery" :MODE "standard" :EVIDENCE-TIER "t2" :LANES
;;     (("user" "continue" "t2") ("evidence" "continue" "t2")
;;      ("business" "continue" "t2"))
;;     :COMBINED-VERDICT "continue" :WEAKEST-FLOW "user_flow"
;;     :NEXT-STEP "write a context pack inside the execution boundary: ..." ...)

(ux3:decide '((:user "verify" "t2")
               (:evidence "stop_reframe" "t1")
               (:business "continue" "t3"))
             :may-do '("Edit the settings page")
             :must-not-do '("Edit the settings page"))
;; => (:COMBINED-VERDICT "stop_reframe" :WEAKEST-FLOW "evidence_flow"
;;     :EVIDENCE-TIER "t1" :AUTHORIZED NIL :PROBLEMS ("the execution boundary ...") ...)
```

`(ux3:gate-text ...)` and `(ux3:review-text ...)` render either answer as prose.
The model can also call the `ux3_gate` tool with a prompt (and an optional
evidence tier), and `/ux3 one line to judge` runs the same gate from the shell.

While the cell runs, `(help :ux3)` answers the method: the three flows, the
evidence tiers, the one-verdict rule, and the operating rules the original's
skill states (review before implementing, list disconfirming evidence first,
name may_do / must_not_do / must_ask, keep a human-owned call human-owned).

## The three flows

| Flow | The question |
| --- | --- |
| user_flow | Who is it for, who else is affected, what are they trying to accomplish, and what does it cost them or take from their control now? |
| evidence_flow | What was observed, where did it come from, what interpretation is being made, which counter-signal weakens it, and what changes if it is wrong? |
| business_flow | Who creates value, who receives it, who pays or decides, who carries the cost and the risk, and does the exchange stay viable and legitimate? |

Evidence tiers bound the action, and confidence never upgrades them: t0 forces
`stop_reframe`, t1 can never return `continue`, and a review's headline tier is
the weakest lane's. Verdicts are never averaged; the weakest flow is chosen
deterministically (severity, then the lowest tier, then evidence, user,
business), so two people reading one review reach the same next step.

## Configure

Nothing is required. The section, on unless it says `"enabled": false`:

```jsonc
{
  "ux3": {
    "evidence_tier": "t0",      // the tier a gate that names none is read at
    "working_language": "en"    // the prose's language; flow, verdict and tier names stay canonical
  }
}
```

A tier or a language of the wrong shape is a `REFUSED` line on the folder's
record, and `(restart-cells "nodecode-ux3")` tries again once it is fixed.

## What is in it

| File | What it is |
| --- | --- |
| `kernel.lisp` | The decision kernel: route, the three-lane baseline, the reduction, the tier floor, the boundary check, and the prose renderers. No I/O, no network, no clock. |
| `cell.lisp` | The method as the help topic, the `ux3_gate` tool, `/ux3`, and the `ux3` section. |
| `README.md`, `NOTICE`, `LICENSE` | What this is, where it came from, and the original's licence. |

## License

MIT, the original's: Copyright (c) 2026 Product Design Harness contributors. See
`LICENSE` and `NOTICE`.
