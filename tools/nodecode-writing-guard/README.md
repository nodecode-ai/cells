# nodecode-writing-guard

Writing Guard: dashes, stock AI phrases and mixed du/Sie, reported under every answer or fixed by rule. A Nodecode rewrite of [MadeByBrain/hermes-extensions](https://github.com/MadeByBrain/hermes-extensions) by [MadeByBrain](https://github.com/MadeByBrain).

## Get it

Tell the agent "add the writing-guard cell", or run `nodecode add nc://writing-guard`.

## Set it up

Nothing is required: with an empty section the cell checks every answer, changes nothing but one italic line under it, and is on as soon as its folder is.

- `mode` — `report` (the line only), `fix` (also replace the dashes as they stream), `off`.
- `footer` — whether the line goes under the answer at all; off leaves the answer byte for byte as the model said it.
- `dash_style` — `comma` (default) makes a dash between two words a comma and a space, `hyphen` a spaced hyphen. A range (10-20) and a list bullet always keep a hyphen.
- `address` — `auto` reports a paragraph that mixes du and Sie; `du` or `sie` also reports every use of the other form.
- `language` — the line's language: `auto` follows the answer.
- `check_dashes`, `check_phrases`, `check_address`, `skip_quotes`, `primer` — what to run, and whether the model is told to check a draft before delivering it.
- `extra_phrases`, `ignore_phrases` — the operator's own stock phrases, and the rule ids (`en.seamless`) or exact phrases to leave out.

Set them in `/settings` or in `config.jsonc`; `config.example.jsonc` is the section written out.

## Use it

Ask for anything and read the answer: where it carries an em or en dash, a stock phrase ("it is important to note", "Es ist wichtig zu beachten", "delve", "seamless"), or a paragraph that mixes du and Sie, one line says so:

> *writing-guard: 2 dashes; 1 AI phrase ("seamless").*

In `fix` mode the dashes are already gone from what you read — the answer is fixed as it streams, so a shell, a channel, a transcript and the history the next round reads back all see the fixed text. Nothing else is ever rewritten: the phrases and the form of address are reported, because a rule cannot know what the sentence meant.

- `/writing-guard` — the settings, what this process has checked, and the last answer's line; `/writing-guard TEXT` checks a text now; `/writing-guard off` pauses it, `on` resumes, `reset` clears the counters.
- `writing_guard_check` — the tool: a whole draft in, the line and (with `fix`) the draft with its dashes replaced out. It is what the standing primer sends the model to before it delivers a mail, a post or a page.
- `(help :writing-guard)` — the manual, for the model.

Code fences, inline code, URLs, markdown link targets, `MEDIA:` lines and quoted lines are masked out first, so nothing in them is counted or changed.

## What is in it

| File | What it is |
| --- | --- |
| `guard.lisp` | The rules: masking, language, dashes, the phrase table, du/Sie, the report and its line. |
| `cell.lisp` | The advice on the stream, the standing primer, the tool, `/writing-guard`, the section. |
| `nodecode-writing-guard.asd` | The system and its test system. |
| `test/` | The offline tests: the rules, the lifecycle, the stream, the tool, the command. |
| `config.example.jsonc` | The section, written out as documentation. |

## Tests

```sh
.github/test-cell.sh tools/nodecode-writing-guard
```

Offline, in a scratch home: the dash count and the rules that survive the fix, masking, the phrases in both languages, du/Sie, the lifecycle, report and fix mode on the stream, off and paused, the tool and the command.

## License

MIT. The original is Copyright (c) 2026 MadeByBrain: see `LICENSE` and `NOTICE`.
