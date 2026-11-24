# nodecode-arena

Arena: a knockout tournament of sub-agents over one task — attack, defend and be judged until one answer wins. A Nodecode rewrite of [J182Razor/hermes-arena-skill](https://github.com/J182Razor/hermes-arena-skill) by [J182Razor](https://github.com/J182Razor).

## Get it

Tell the agent "add the arena cell", or run `nodecode add nc://arena`.

## Use it

When an answer is not good enough, do not ask again: make it a tournament.

- `plan` says what a run costs and writes nothing. `--quick` is 16 competitors, about 91 sub-agent calls in 16 waves; the full arena is 100, about 595 calls. Just `arena command=plan` with no size uses the setting.
- `init` starts a run: it deals every competitor a different strategy card — a reasoning mode, a workflow and a strategy — and pairs round one.
- `next` says what to do now. For each phase it names, `prompts` writes the briefs and lists the jobs in waves; each job is one sub-agent call told to read its brief and follow it exactly. `collect` then reads the judges' verdicts, `advance` closes the round and pairs the survivors.
- Every round is attack, defend, judge. A judge scores both revised answers against the shipped rubric, and the cell does the arithmetic: the weighted total, the fatal rule, the tie-break. When one solution is left, `winner` names it, the card it won with, the attacks it survived and the matches it took — and, if there was an answer the user had already rejected, how the winner did against it, blind.
- The state is one file, `arena.json`, in the run folder under `.arena/`. A compacted conversation loses nothing: `status` and `next` resume the run.

The task file is what decides the result: sub-agents cannot see the conversation, so `init`'s `task` must stand alone — the request in the user's own words, every constraint, the context a stranger needs, and what "done" looks like.

## Set it up

Nothing is required. The `arena` section of the config refines it: `agents` (the default size, 100) and `wave` (sub-agent jobs launched at once, 10).

## What is in it

| File | What it is |
| --- | --- |
| `bracket.lisp` | The tournament state machine: the deal, the bracket, the judges' arithmetic, the briefs. |
| `cell.lisp` | The manual, the `arena` tool and `/arena`. |
| `skill/` | The original's `skills/arena/` data, unchanged: `SKILL.md` (whose prompt templates every brief is rendered from), `rubric.md` and `strategies.json`. |

## License

MIT. The original is Copyright (c) 2026 Jake Schincariol: see `LICENSE` and `NOTICE`.
