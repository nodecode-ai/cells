# nodecode-gonogo

Go/No-Go: turns a pilot eval score into a ship-or-not verdict. A Nodecode
rewrite of [keppy/hermes-plugin-gonogo](https://github.com/keppy/hermes-plugin-gonogo)
by [keppy](https://github.com/keppy).

## Get it

Tell the agent "add the gonogo cell", or run `nodecode add nc://gonogo`.

## Set it up

Nothing to answer. The cell reads one optional member of its own section in
`~/.nodecode/config.jsonc`:

```jsonc
"gonogo": { "default_target_percent": 95 }
```

That is the accuracy a call that names no target is measured against, in
percent; a call may always pass its own. No key, no network, no dependency:
the statistics run in Lisp, in the session.

## Use it

A pass rate is not a decision. 47 of 50 reads as 94%, but the 95% interval
on that is [83.8%, 97.9%] — so against a 90% bar you cannot yet say you
cleared it, and against 95% the honest answer is that 50 cases cannot settle
it. The cell answers with the interval, the verdict, and the sample the
claim would need:

```lisp
(gonogo:decide :passed 47 :total 50)
;; => (:VERDICT "ASSIST ONLY" :REASON "pass rate 94.0% [83.8%, 97.9%] is below the 95% target ..."
;;     :PASS-RATE (:POINT 0.94d0 :LOW 0.838d0 :HIGH 0.979d0 :TEXT "94.0% [83.8%, 97.9%]")
;;     :NEEDED-N NIL :OPERATING-POINT NIL :MARKDOWN "**ASSIST ONLY** ...")

(gonogo:decide :passed 47 :total 50 :target 0.9)
;; => :VERDICT "INSUFFICIENT EVIDENCE" :NEEDED-N 204
```

Three verbs, and `(help :gonogo)` has their shapes:

| Verb | The question it answers |
| --- | --- |
| `(gonogo:decide :passed N :total M)` or `(gonogo:decide :results-path "run.jsonl")` | "Is this good enough to ship?" |
| `(gonogo:report :results-path "run.jsonl")` | "Give me the report artifact" — for a doc, a PR, a pilot review |
| `(gonogo:compare :report-a "a.jsonl" :report-b "b.jsonl")` | "Is B actually better than A?" — McNemar on the cases they share |

### The five verdicts

| Verdict | What it means |
| --- | --- |
| `AUTOMATE` | The pass rate's **lower bound** clears the target. Ship it. |
| `AUTOMATE WITH REVIEW` | A confidence threshold reaches the target on this selection set; do not ship yet, re-measure it on a fresh holdout. |
| `ASSIST ONLY` | A draft generator, not an unattended step. |
| `DO NOT AUTOMATE` | Not a fit for this workflow as scoped. |
| `INSUFFICIENT EVIDENCE` | The estimate looks fine but the sample cannot support the claim; the answer carries how many cases would. |

`INSUFFICIENT EVIDENCE` is the reason the cell exists: it is the verdict a
careful consultant gives and a dashboard never does.

### The results file

One JSON object per line; only `passed` is required, so a domain expert can
build one in a spreadsheet.

```jsonl
{"id": "inv-001", "passed": true,  "confidence": 0.94, "expected": "Acme", "output": "Acme"}
{"id": "inv-006", "passed": false, "confidence": 0.42, "expected": "Acme", "output": "Cinder Works"}
{"id": "inv-007", "passed": false, "error": "TimeoutError: 30s"}
{"id": "msg-014", "passed": true,  "expected_action": "no_reply", "action": "no_reply"}
```

- `confidence` (0–1) is optional for a whole file, but mixed missing and
  present values are refused; it is what unlocks the review threshold, since a
  threshold can only be chosen from a per-case score.
- `error` is a crashed case — a failure, not an aborted run. `abstained` is an
  explicit abstention, which lands at zero confidence so a threshold defers it
  first.
- `expected_action` (absent means answer) and `action` grade decisions not to
  act; a hold-back case passes exactly when they match.
- `group` on every case folds correlated checks into one trial per group,
  passing only if every case in it passed — several fields from one document
  are one draw of the system under test.
- `(gonogo:report ... :save-report "report.json")` writes the report JSON,
  and `gonogo:compare` pairs two of those by case id (or by group id with
  matching case membership), so a run measured today can be checked against
  one measured last week.

### What it touches

The paths you name: a results file read, a report JSON or HTML page written
when you ask for one. No key, no network, no environment variable, no other
cell's state.

`/gonogo` prints the target in force and the three verbs; `(help :gonogo)`
answers the manual.

## Credits

The statistics and the verdict rules are [gonogo](https://github.com/keppy/gonogo)'s,
by the same author; this folder carries them in Common Lisp. See [NOTICE](NOTICE).
MIT — see [LICENSE](LICENSE).
