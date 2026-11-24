# nodecode-jev

Jev: fast advisory judgments -- triage a request, pick a next step, rate relevance -- answered as probabilities. A Nodecode rewrite of [ourines/hermes-jev](https://github.com/ourines/hermes-jev) by [ourines](https://github.com/ourines).

Ask one bounded question and get an answer you can act on without a second model call:

```lisp
(jev:triage "The export button writes an empty CSV on the staging build; no logs yet.")
;; => (:CATEGORY (:TYPE :CHOICE :CHOICE "bug" :PROBABILITIES (("bug" . 0.6) ...) :CONFIDENCE 0.55)
;;     :MISSING-CONTEXT (:TYPE :BOOL :BOOL 0.2)
;;     :REQUIRES-APPROVAL (:TYPE :BOOL :BOOL 0.8)),
;;    (:ENABLED NIL :THRESHOLD NIL :REVIEW-QUESTIONS () ...),
;;    "typesafe/jev-latest"
```

Jev answers typed questions -- a choice among labelled options, a yes/no, a rubric score -- about one
state, and nothing else: no conversation, no tool call, no action. The model decides; Jev only says
how likely. Its answer is explicitly advisory: it grants no approval and executes nothing.

| Verb | What it answers |
| --- | --- |
| `(jev:triage state)` | what kind of work this is (bug, feature, research, docs, operations, unknown), whether context is missing, whether it needs authorization |
| `(jev:next-step state)` | continue, retry, change_approach, ask_user, escalate or stop -- and whether the attempts are stuck |
| `(jev:relevance state)` | how directly an item helps a goal, 0 to 2, and whether the goal or the item is missing |
| `(jev:ask state :preset :relevance)` | the same three presets, by name |
| `(jev:ask state :questions ...)` | your own questions, in one call |
| `(jev:presets)` | the three rubrics, offline |
| `(jev:status)` | backend, model, where the key comes from; no network |

Every judgment also answers a **review** value: with a threshold set in the section, the answers whose
probability sits near the middle, whose confidence is low, that come back `unknown`, `ask_user` or
`escalate`, or that say approval is likely, are named as worth a human look. It is a heuristic, not a
calibration, and it never replaces a safety check.

## Get it

Tell the agent "add the jev cell", or run `nodecode add nc://jev`.

## Set it up

Pick one backend, as the original insists -- there is no fallback from one to the other:

- **typesafe** (default model `jev-latest`). Make a key at <https://console.typesafe.ai/>, then set
  `TYPESAFE_API_KEY`, or save it in `~/.nodecode/auth.json` under `api_keys.typesafe` the way
  `/connect` keeps a key. The nodecode-typesafe cell reads the same entry, so one key serves both.
- **cloudflare** (default model `typesafe/jev`, the same model through AI Gateway). Set
  `CLOUDFLARE_JEV_API_TOKEN` -- a Workers AI token -- or save `api_keys.cloudflare-jev`, and put the
  32-hex account id in the section. An authenticated gateway with Unified Billing credits is what makes
  the route run; requests ask the gateway to keep no log, use no cache and allow one attempt.

```jsonc
{
  "jev": {
    "backend": "typesafe",          // or "cloudflare"
    "account_id": "",              // cloudflare: 32 hex digits
    "gateway_id": "",              // cloudflare, optional
    "model": "",                   // empty: jev-latest / typesafe/jev
    "review_threshold": ""         // empty: heuristic off; else 0.5 < n < 1
  }
}
```

`(jev:status)` and `/jev` say what is configured without asking anyone. A request that fails says why
in one line -- no key, an HTTP status, or Cloudflare's numeric provider code -- and never echoes the
key.

## Use it

In a turn, put the observations in `state` -- the goal, the item, the last attempts, what you know --
and read the answer with `getf`:

```lisp
(multiple-value-bind (answers review backend)
    (jev:next-step '(:goal "ship the migration guide"
                     :attempts ("draft written" "linter keeps failing on headings") 
                     :permissions "edit docs, no deploy"))
  (getf (getf answers :next-step) :choice)   ; => "retry"
  (getf review :review-questions))           ; => () with no threshold set
```

A question of your own, with three primitives in one paid call:

```lisp
(jev:ask "The patch renames the public function parse-config."
         :questions '(:risky (:type "bool" :instructions "Does this change risk breaking callers?"
                              :criteria (:true "an API changes" :false "internal only"))
                      :kind  (:type "choice" :instructions "What kind of change is this?"
                              :criteria (:fix nil :feature "adds" :refactor "restructures"))
                      :size  (:type "score" :instructions "How large is the change?"
                              :criteria ("trivial" "small" "medium" "large"))))
```

A choice needs two or more options, each a rubric or `nil`; a bool's criteria are optional; a score
needs two or more levels, lowest first. Give exactly one of `:preset` and `:questions`. An answer
that is missing, of the wrong type, an option that was never offered or a level outside the rubric is
a refusal naming the question -- never a guess.

`(help :jev)` carries the same manual while the cell runs.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-jev
```

## Not ported

The Hermes plugin surfaces: the interactive `hermes jev setup` wizard, its `status`/`test`/`guide`/
`evaluate --file` subcommands and its JSON request files (a call through `eval` is the request), and the
two bundled skills (the manual the model reads replaces them). No retries and no cross-backend
fallback, as the original. See [NOTICE](NOTICE).

MIT licensed.
