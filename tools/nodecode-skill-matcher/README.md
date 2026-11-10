# nodecode-skill-matcher

Skill Matcher: routes a task to the few installed cells and skills that fit it, with the evidence for each match. A Nodecode rewrite of [xXLODXx/hermes-skill-router](https://github.com/xXLODXx/hermes-skill-router) by [xXLODXx](https://github.com/xXLODXx).

## Get it

Tell the agent "add the skill-matcher cell", or run `nodecode add nc://skill-matcher`.

## Set it up

Nothing is required. The cell reads the cell folders under this home and any
`SKILL.md` beneath them, and works on what is there.

In `/settings`, the `skill-matcher` section takes:

| Setting | Default | What it does |
| --- | --- | --- |
| `dirs` | empty | extra folders to scan for cell `.asd` files and `SKILL.md` files; empty reads this home's `cells/` folder |
| `max_matches` | 3 | how many matches one routing names; the rest are reported as rejected |
| `min_score` | 3 | evidence a match needs: a tag alone is 3, a description alone is not enough |
| `log` | true | append each routing to `skill-matcher/decisions.jsonl` under this home, never the task's words |

## Use it

Ask something like "which of your skills should handle this invoice-OCR job?", or say `/skill-matcher extract the tables from these pdfs`. The model can call the
tool itself:

```
match_skills {"task": "extract the tables from these pdfs", "max_matches": 3}
```

and reads back a short block:

```
## Skill routing - 2 matches for this task
- ocr-kit (required; confidence 0.99; evidence: description, name)
- ocr-kit/pdf-extraction (required; confidence 0.99; evidence: description, name, subword, tag)

Load one with (help 'name).
```

With no match it answers `No high-confidence match. Continue with the normal index.`
and nothing changes.

### How it ranks

The task's words against each entry's name (5), tags (3) and description (1).
A stem or compound overlap -- `excerpt` against `excerpts` -- is supporting
evidence and never carries a match on its own, which is what keeps a coincidence
out of the prompt. Confidence is the score over ten, plus 0.12 when more than
one kind of evidence agrees, capped at 0.99. A match needs exact evidence and
`min_score`; the entries over `max_matches` come back as rejections rather than
crowding the answer. Words that describe everything here (`nodecode`, `cell`,
`skill`, `task`, `the`, `and`, ...) route to nothing.

A task routed twice in one session says `unchanged since your last routing`
rather than repeating the block -- the original's topic-change check.

### The log

`skill-matcher/decisions.jsonl` under this home gets one line per routing: the
names matched, how many were rejected, the highest confidence and whether it
fell back. No word of the operator's task is in it. Delete the file to clear it.

## What of the original it keeps, and what it leaves out

The scoring, the evidence gate, the three-candidate budget, the fallback line,
the per-session topic check and the local decision log are the original's. The
catalog is this organism's own instead of Hermes's skill folder.

Left out: the automatic per-turn injection (the original ran on four Hermes hooks
and pushed its block into the first turn of every session; here the model calls
the tool, or the operator types `/skill-matcher`), the keyword-learning store
with its lift gate, and the dashboard page.

## Test it

Run `.github/test-cell.sh tools/nodecode-skill-matcher` from the cells repository.
The tests write a fixture inventory into the run's own scratch home, so they
read no skill of the machine and dial nothing.

## License

MIT, the original's. See `LICENSE` and `NOTICE`.
