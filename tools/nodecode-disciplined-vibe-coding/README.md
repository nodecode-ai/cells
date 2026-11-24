# nodecode-disciplined-vibe-coding

Disciplined Vibe Coding: eleven prose skills that run one coding task through intake, define, plan, teardown, build, verify and commit, with a gate at every stage. A Nodecode rewrite of [bmtrnavsky/disciplined-vibe-coding](https://github.com/bmtrnavsky/disciplined-vibe-coding) by [bmtrnavsky](https://github.com/bmtrnavsky).

## Get it

Tell the agent "add the disciplined vibe coding cell", or run `nodecode add nc://disciplined-vibe-coding`.

## Use it

The cell is a knowledge cell: nothing runs, and the eleven skills sit in the index every conversation is pinned with. Ask for one by name, or ask the index for the rest.

```lisp
(help 'intake)                   ; the whole procedure, as the original wrote it
(index "test first")             ; which of them answers this
(help 'disciplined-vibe-coding)  ; the pipeline that strings them together
```

| Skill | When it is for |
| --- | --- |
| `intake` | The start of any task: what success looks like, the stakes tier, the direction |
| `brainstorm` | Approach forks, batched into one stop |
| `research` | Docs and prior art before building on an API, logged in `.pipeline/read.log` |
| `plan` | One milestone as atomic steps: paths, checks, rollback, budget lines |
| `execute` | Build to an approved plan; workers own assigned files, one hand owns git |
| `test-first` | A failing test before the code, declared exemptions otherwise |
| `debug` | Docs before theory, cause before fix, two failures and stop |
| `verify` | Fresh evidence before any done-claim, classified and counted |
| `version` | Narrative commits, semver, a changelog people can read |
| `write-docs` | Pragmatic Simplified Technical English for docs, notes and answers |
| `disciplined-vibe-coding` | The whole pipeline, stage by stage, with the gates in order |

Every skill is scaled by one declared rung, in `guides/economy-ladder.md`: a one-liner, a micro-spec, a full spec, or a project. The rung sets the ceremony and never steps down mid-build.

## What is in it

| File | What it is |
| --- | --- |
| `intake.lisp` ... `write-docs.lisp` | Ten of the original `skills/*/SKILL.md` files, each as one prose `define-skill`. |
| `disciplined-vibe-coding.lisp` | The original's pipeline `SKILL.md`, whole, as one prose `define-skill`. |
| `guides/` | The markdown those skills name: the economy ladder, and the to-spec, writing-plan and wayfinder templates. |
| `test/` | The test slice: the folder reads as a knowledge cell and each skill answers its own prose. |

## License

MIT. The skills are Copyright (c) 2026 Brad Trnavsky: see `LICENSE` and `NOTICE`. The pipeline prose adapts Google's Elephant-Goldfish Model (Apache 2.0), and several skills name in their own `Lineage:` line the projects they were learned from.
