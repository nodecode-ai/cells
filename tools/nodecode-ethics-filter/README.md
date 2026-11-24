# nodecode-ethics-filter

Ethics Filter: any decision comes back GREEN, AMBER or RED with the reasoning. A Nodecode rewrite of [RegeneratusLabs/ethics-filter](https://github.com/RegeneratusLabs/ethics-filter) by [RegeneratusLabs](https://github.com/RegeneratusLabs).

## Get it

Tell the agent "add the ethics filter cell", or run `nodecode add nc://ethics-filter`.

## Set it up

Nothing is required. The section `ethics-filter` holds three optional settings:

| Setting | What it does |
| --- | --- |
| `default_constitution` | which preset a call uses when it names none: `small-business-ethical` (the default), `personal-reflection`, `corporate-governance`, `startup-quick`, `maximalist`, `minimal-safe` |
| `strictness` | `constitution` (the default: keep the preset's own), or `relaxed`, `moderate`, `strict` |
| `audit_path` | where scored verdicts are kept, one JSON line each, under this home unless absolute |

## Use it

Ask the agent to check a decision before it acts on it, or type:

```
/ethics Move the warehouse to a cheaper site outside town
/ethics Move the warehouse --context "18 staff, the lease ends in March" --scores fairness=70,compliance=85
/ethics --audit
```

Two calls, one decision, the same through the `ethics_evaluate` tool:

1. **The brief.** With only the decision, the tool answers which of the six modules apply to it (environmental, fairness, transparency, conscious-leadership, ethical-framework, compliance), the rubric each one scores against, and the thresholds in force. Modules a decision does not touch are left out.
2. **The verdict.** Score every enabled module 0-100 and call again with the scores. The overall score is their mean; the verdict comes from the strictness thresholds, and **any single module below the red threshold blocks the whole decision** whatever the mean says. A spread of 40 points or more between modules is reported as a tension for a person to resolve.

Every scored verdict is appended to the audit trail as one JSON line: the decision, the context, the constitution and strictness, the thresholds, every module's score, the flags, the tensions and the reasoning.

The reasoning is the point: the scores are structured thinking aids, and the answer should say which rubric band each score sits in and why.

## What is in it

| File | What it is |
| --- | --- |
| `package.lisp` | the package, the six modules with their rubric bands, the six presets and the three strictness levels, as data |
| `engine.lisp` | relevance, thresholds, the verdict, the brief, the prompt and the audit trail |
| `cell.lisp` | the `ethics_evaluate` contract and tool, `/ethics`, the help topic and the section |
| `test/cell-test.lisp` | offline tests: the brief, the verdict, the veto, tensions, the trail and every refusal |

## License

MIT, the original's. The engine and its rubrics are Copyright (c) 2026 Ethics Filter Contributors: see `LICENSE` and `NOTICE`.
