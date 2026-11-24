# nodecode-ordaprompt

OrdaPrompt: routes an ask to a kept topic or a live session, and abstains when it cannot tell. A Nodecode rewrite of [aska-digital/protean-ordaprompt](https://github.com/aska-digital/protean-ordaprompt) by [aska-digital](https://github.com/aska-digital).

The original is a fail-closed comparative classifier for request routing. Given
a request and a candidate set, it scores **every** candidate in one batch --
the kept topics, a `novel` sentinel, an `ambiguous` sentinel, the live
sessions and a `new_session` sentinel -- and answers one of three bands:

| Band | What it claims |
| --- | --- |
| `automatic` | a calibrated score above the threshold, and a margin over the runner-up: route without asking |
| `fallback_escalate` | in between: the ranked candidates are returned and the caller asks |
| `abstain_or_new_session` | nothing fits, the session is contaminated, or the top two are tied: another session opens |

Abstention is a first-class answer, not a failure. Session reuse is stricter
than topic assignment: a session that wins on similarity but carries a
contamination flag, or is near-tied with starting fresh, never routes
automatically. Every decision is one call, and a hashes-only receipt can be
kept of it.

In Hermes the whole surface was a command line, and its `register()` declared
no tools, hooks or settings. Here it is a tool the model calls and a command
the operator types; the band table above is the original's, line for line.

## Get it

Tell the agent "add the ordaprompt cell", or run:

```sh
nodecode add nc://ordaprompt
```

## Set it up

Nothing is required. The section's defaults are the original's thresholds,
and the scoring backend is offline.

```jsonc
{
  "ordaprompt": {
    "calibration_active": true,
    "receipts_file": "~/.nodecode/ordaprompt/receipts.jsonl"
  }
}
```

`calibration_active` is the one switch worth knowing. The original shipped the
automatic band **locked off** (`calibration_model_id: "none"`) because its
synthetic calibration was above the error gate it set for itself, so every
answer was `confirm` or `abstain`. A locked band here is the same: with
`calibration_active` false nothing routes on its own, every answer
escalates or abstains, and the code says `calibration_missing`. The default is
`true`, because this cell's backend has no calibration file to validate
against; turn it off to get the release's posture back.

Thresholds are integers in hundredths -- the section has no float type --
so `85` is the original's `tau_topic = 0.85`. A member is set in `/settings`
or in `~/.nodecode/config.jsonc`, then `(restart-cells "nodecode-ordaprompt")`.

| Member | Default | Notes |
| --- | --- | --- |
| `calibration_active` | `true` | unlocks the automatic band |
| `tau_topic` | `85` | a topic scores this much to route on its own |
| `mu_topic` | `20` | and beats the second candidate by this much |
| `tau_novel` | `85` | a novel-topic proposal needs this much residual |
| `mu_novel` | `25` | and this margin over the best kept topic |
| `tau_sess_sim` | `90` | a session is this similar to be reused |
| `tau_sess_util` | `55` | and carries at least this much utility |
| `mu_sess` | `25` | and beats starting fresh by this much |
| `tau_low` | `60` | under this the router abstains rather than guessing |
| `k_block` | `50` | contamination at or above this blocks reuse |
| `w_cost` | `30` | what a session's context costs its utility |
| `w_contam` | `50` | what a session's contamination risks |
| `new_session_utility` | `35` | the fixed utility of starting fresh |
| `context_cost_norm` | `8000` | tokens at which a session's cost counts whole |
| `receipts_file` | unset | a JSONL file of hashes-only receipts |

## Use it

The tool, from the model's side:

```jsonc
{
  "prompt": "the billing export writes an empty file on staging",
  "topics": ["billing", "deploy"],
  "sessions": [
    {"session_id": "s-3", "summary": "billing export rewrite", "tokens": 12000,
     "flags": []}
  ]
}
```

and the answer it reads back:

```
decision topic_assign, band automatic
topics: billing 1.00, ambiguous 0.00, deploy 0.00, novel 0.00
sessions: s-3 0.37, new_session 0.35
receipt: ~/.nodecode/ordaprompt/receipts.jsonl
bands: topic automatic; session abstain_or_new_session (below_margin)
```

The bands line is the decision's whole reason: the effective band is the more
conservative of the two surfaces, and the code names which rule fired
(`calibration_missing`, `ambiguous_won`, `below_margin`, `contaminated_high`,
`new_session_argmax`, `below_min_utility`, `below_min_score`).

From `eval`, or from a layer:

```lisp
(ordaprompt:route "the billing export is empty" '("billing" "deploy") nil)
;; => (:BAND "automatic" :DECISION "topic_assign" :CODE NIL ...)
```

The command:

```
/ordaprompt                                how it is configured, and how to call it
/ordaprompt billing,deploy :: the ask      one decision over two topic labels
```

`(help :ordaprompt)` carries the same in one paragraph.

### The backend, and what changed hands

The original scored a prompt by hashing it into a 16-dimensional vector and
comparing that with a hash of each candidate id: deterministic, offline, and
with no relation to what the prompt actually said -- it was the fixture
backend, waiting for a real NLI model behind the same interface. This cell
scores by words instead: a candidate's score is the fraction of its own words
the prompt carries (`deploy` meets `deployment`, but `bill` does not meet
`billing`), `novel` is the residual, and `ambiguous` is how tied the top two
are once something scores. Everything above the score -- both bands, the
margins, the session utility, the sentinels, the effective band, the codes --
is the original's.

### The receipt

With `receipts_file` set, every decision appends one JSON line: the prompt's
`sha256:`, the candidate labels, the scores, the utilities, the band, the code
and the router version. Never the prompt, never a file, never a path -- the
original's receipt writer refused free text, and this one has nowhere to put
any. A receipt that cannot be written says so in the answer rather than being
lost.

## Tests

```sh
.github/test-cell.sh tools/nodecode-ordaprompt
```

13 tests, offline: the cell's start and stop, a named topic routing, an
unmatched ask proposing a new topic, a locked band escalating, a call with no
candidate refusing, a clean session reused, a contaminated one not, a receipt
holding hashes and not words, and both command verbs.

## Gaps

- The original's OpenRouter adapter (provisional taxonomy proposals, hashes
  only, disabled by default) is not carried: there is no key to set and no
  proposal to make, so a novel ask answers `novel_propose` and leaves the
  naming to the caller.
- `confirm` is not a decision of its own in the original either -- it is the
  `escalate` band -- and here the ranked list is the confirmation.
- The calibration document (`--calibration`, Platt scaling per surface) and the
  eval harness (`eval/`) are not carried: both existed to prove the automatic
  band's error gate on synthetic fixtures. `calibration_active` is the whole of
  what the gate decided.
- `classify` and `route-session` are one tool here: sessions are an argument,
  and leaving them out is the topic-only call.

## Credit

The router, its bands, its thresholds, its sentinels, the contamination
weights and the receipt shape are aska-digital's, from
[aska-digital/protean-ordaprompt](https://github.com/aska-digital/protean-ordaprompt)
at commit `60e89af`, MIT. See [NOTICE](NOTICE).
