# nodecode-storm

Storm Panel Research: one question fanned out across a panel of model seats, then a judge
synthesizes the seat reports. A Nodecode rewrite of
[bmtrnavsky/storm-fusion-research-pipeline](https://github.com/bmtrnavsky/storm-fusion-research-pipeline)
by [bmtrnavsky](https://github.com/bmtrnavsky).

The method is an adaptation of Stanford's STORM (Shao et al., NAACL 2024): discover the
viewpoints a question deserves, answer it from each one, ground the claims, synthesize.
The panel adds the part a single answer cannot have: the judge *recommends*, and the
human decides.

## Get it

Tell the agent "add the storm cell", or run `nodecode add nc://storm`.

## Set it up

Nothing is required: with no section at all every seat, and the judge, run on the session's
own model. To make a lineup the default, put the section in `~/.nodecode/config.jsonc`:

```jsonc
"storm": {
  "lineup": ["alpha/model-a", "beta/model-b"],  // models every seat answers on; empty = the session's own
  "judge": "gamma/model-c",                      // the judge's model; empty = the session's own
  "max_tokens": 4000                             // one side call's ceiling, at least 256
}
```

`/settings` and the page's Settings tab write the same section. A model is
`provider/model` as `/models` lists the lane, or a bare model on the session's own
provider.

## Use it

The model calls the tool, or you ask in words — "run a panel on whether we ship Friday
with an operations seat and a support seat":

```json
{"topic": "Should we ship on Friday?",
 "seats": [{"name": "operations", "brief": "the deploy window, the rollback",
            "questions": ["Is the window open?", "Who is awake at 18:00?"]},
           {"name": "support", "brief": "the on-call queue"}],
 "lineup": ["alpha/model-a", "beta/model-b"],
 "judge": "gamma/model-c"}
```

Every seat answers on every model in the lineup — two seats on two models is four reports —
and one judge call reads all of them. The answer is one JSON object:

```json
{"success": true,
 "panel": {"seat_reports": [{"seat": "operations", "model": "alpha/model-a",
                             "findings": "..."}],
           "synthesis": "consensus, then the contradictions and their weights...",
           "contradictions": [{"claim": "...", "seats_for": ["operations"],
                               "seats_against": ["support"], "weight": "..."}],
           "unresolved": ["..."]},
 "calls": [{"seat": "operations", "model": "alpha/model-a"}]}
```

Through `eval` the same round is
`(storm:panel "topic" '((:name "operations" :brief "the deploy window")))`, and
`(help :storm)` answers the method. A run that fails answers `{"success": false, "error",
"hint"}`: the hint names what to check, and nothing was swapped in for the model that
failed — the panel never silently substitutes a brain.

`/storm` shows the lineup the next round runs on; the row runs `/storm status`, which asks
no model. Caps: 12 seats, 4 lineup slots, as the panel spec sets them.

## What it does not do

Nested fanout through a mixture-of-agents preset, a per-call temperature and a token-usage
report: the host's side call takes a completion ceiling and no temperature, and answers
text with no usage, so the rewrite drops both. See [NOTICE](NOTICE).

Run `.github/test-cell.sh tools/nodecode-storm` from the cells repository.
