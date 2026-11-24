# nodecode-memory-shaper

Memory Shaper: keeps a flat keyword index in front of the model and the detail in topic files on disk. A Nodecode rewrite of [ockentap/hermes-dynamic-memory](https://github.com/ockentap/hermes-dynamic-memory) by [ockentap](https://github.com/ockentap).

One line per subject -- five to twenty comma-separated keywords, a Unicode arrow, a bare `.md` name -- is what the model carries and reads first: `python,packaging,poetry,ruff,pytest,typing,imports → python.md`. The sentences are in `python.md`, opened only when a keyword matches. Nothing is summarised, so nothing is lost to a summary, and the index costs a few hundred characters a turn instead of a few thousand.

## Get it

Tell the agent "add the memory-shaper cell", or run `nodecode add nc://memory-shaper`.

## Set it up

Set `memory-shaper.index_file`, the index the model reads every turn, in `/settings` (or the `memory-shaper` section of `~/.nodecode/config.jsonc`): `memory/index.md` under the home is the usual answer. Until it is set, the `memory_index` tool and `/memory-shaper` refuse and say so.

Beside it, all optional: `topics_dir` (where the detail files go; empty means the index's own folder), `min_keywords` (5) and `max_keywords` (20), `verbose_chars` (140, the longest an index line may be before the detail belongs in its topic file), `index_chars` (800, how much of the index rides one turn's first request) and `allow_rewrite` (off; a whole-file rewrite needs it).

## Use it

Tell the agent what it should remember -- "remember that deploys go out from the release branch, canary first for ten minutes" -- and it calls the `memory_index` tool: `route` writes the paragraph to `deploy.md` and answers the pointer line to add, `add` validates that line and appends it to the index. Every turn from then on opens with the index:

```
[Keyword index — read the topic file when a line matches]
  canary,deploy,pipeline,release,rollback,staging → deploy.md
```

so the model knows the detail exists and reads the file when the conversation touches it. `lint` names every entry that is malformed and what to fix; `show` reads the index; `rewrite` replaces the whole file, one entry a line, when `allow_rewrite` is on.

A refusal is the fix, in the original's words: an ASCII `->` gets "use the Unicode arrow →", three keywords get "an entry carries 5 to 20", a path gets "a bare .md name inside the topics folder", and a sentence gets the comma-per-word count that gave it away. The operator types `/memory-shaper` to lint the index, `/memory-shaper add <line>` to add one, or `/memory-shaper show`.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: the entry format and its refusals, the index and topic files, the `memory_index` tool, `/memory-shaper`, the `:round` hook that opens a turn with the index, and the memory-shaper section. |
| `nodecode-memory-shaper.asd` | The system and its `/test` system. |
| `test/cell-test.lisp` | The offline tests: lifecycle, an entry added and linted, four refusals, a routed paragraph, an approved rewrite, a missing `index_file`, and the index reaching a turn's first request. |

## What it does not do

The original hooked another harness's memory tool and its file writes, so a write aiming at the index from any other tool was refused. Here the index is this cell's own file, written by this cell's tool, so there is no other path to lock: the shape is checked where it is written, and `lint` reports what a hand edit left behind. The original's `~/.hermes/memories/` layout, its `§` separator reasoning and its template-line auto-cleanup go with that harness.
