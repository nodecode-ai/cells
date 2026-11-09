# nodecode-skill-court

Skill Court: paired with/without evidence for a written skill, a 95% verdict and a quarantine list. A Nodecode rewrite of [saatvik333/hermes-skill-court](https://github.com/saatvik333/hermes-skill-court) by [saatvik333](https://github.com/saatvik333).

A skill the agent wrote for itself is graded by its author: it reads fine, so it
stays. Skill Court asks the only question that matters — did the runs get
better? — and answers it with numbers. Each case is one task run twice, once
with the skill and once without it:

```
skill_court_case      add a case: the task, and how a run passes
skill_court_record    record what that case did in both arms, with the tokens each spent
skill_court_report    helps, hurts, neutral or inconclusive, with its 95% interval
skill_court_quarantine  put a skill out of service, take it back, or list the ones out
```

```
queue-records: hurts -- success fell from 67% without the skill to 33% with it
(3 cases, 3 pairs; 95% CI -67%..-0%; no token counts) [quarantined]
```

A verdict needs three cases judged in both arms and its whole interval on one
side, so it is never made of one lucky run. A skill that costs a quarter more
in tokens without lifting success hurts; one that lifts success while costing
a fifth less helps.

## Get it

Tell the agent "add the skill-court cell", or run `nodecode add nc://skill-court`.

## Set it up

Nothing is required, and there is no key: the court runs nothing itself and
keeps everything on this machine. The section lives in
`~/.nodecode/config.jsonc`; every member is optional and the defaults are the
original's:

```jsonc
{
  "skill-court": {
    "min_tasks": 3,        // cases judged in both arms before a verdict
    "margin": "0.25",      // relative token cost that counts as a difference
    "resamples": 2000,     // bootstrap resamples behind every interval
    "seed": 1,             // so a report reads the same twice
    "notify": true         // stand a notice naming each quarantined skill
  }
}
```

The record is one file under the home, `skill-court/state.sexp`, written
atomically and holding the cases, the pairs and the quarantine list — no word
of a conversation.

## Use it

Ask the model to try a skill, and it works the four tools: add three cases
that a user would actually ask, run each one with the skill and again without
it, record both outcomes, and read the report. The numbers decide, not the
reading of the prose.

From the operator's side, `/skill-court` answers the court itself:

```
/skill-court                        what the court holds and how it decides
/skill-court list                   every skill's verdict, best first
/skill-court report queue-records   one skill, its interval, its cases, its quarantine
/skill-court cases queue-records    the cases it is tried on
/skill-court task queue-records ... add a case by hand
/skill-court quarantine NAME [NOTE] one skill out of service
/skill-court release NAME           and back
/skill-court reset NAME             forget a skill's evidence
```

Quarantine never edits the skill: it stands a warning notice naming it, under
`nodecode-skill-court/<name>`, so the model reads it in every turn's notices
until it is released. `(help :skill-court)` carries the same manual into the
conversation.

## How it works

Repeated pairs of one case are not independent evidence, so the pairs are
averaged per case first and every interval is a bootstrap over cases with a
fixed seed: a report reads the same twice, on any machine. The verdict rules
are the original's — `hurts` when the whole success interval is below zero,
`helps` when it is above, the token rules when success did not move, and
`inconclusive` under `min_tasks` cases.

## Test

From the repository root:

```sh
.github/test-cell.sh tools/nodecode-skill-court
```

The slice is offline and doesn't need the network: the scores, the four
tools' bodies, the command, the refused config, the start and the stop, and
the record's second reading from disk are all exercised against a scratch file
under the test home.

## What the rewrite does not carry

The Docker replay itself — a cloned profile per arm, a container per run — the
task mining from session history, the blind LLM judge, the appeal that drafts
and proves a fix, and the automatic trial queue with its daily budget. A cell
runs no agent turn of its own, so the runs are the caller's and the court
keeps the evidence, decides the verdict and holds the quarantine. See
[NOTICE](NOTICE).

MIT — see [LICENSE](LICENSE).
