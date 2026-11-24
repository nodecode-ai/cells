# nodecode-enchant

Enchant: rewrite a draft prompt, review the changes group by group, and undo or redo. A Nodecode rewrite of [noxius-studium/enchanted-composer](https://github.com/noxius-studium/enchanted-composer) by [noxius-studium](https://github.com/noxius-studium).

A draft in the composer is rarely the prompt you meant: it hedges, it buries the ask, it leaves out what the model cannot guess. Enchant rewrites it with one of five enhancers - clarify, tighten, grammar, expand, summarize - and then shows you the change instead of sending it. The rewrite arrives as numbered groups, each the text taken out of your draft and the text put in its place, and you take the whole rewrite or only the groups you pick. What you take is a revision: `/enchant undo` and `/enchant redo` move through the drafts that chat has been, and a redo spends no model call.

## Get it

Tell the agent "add the enchant cell", or run `nodecode add nc://enchant`.

## Set it up

Nothing to obtain: the rewrite runs on your own model, through the same side call a session title or a recap uses, so it spends one model turn per rewrite on the auxiliary target (`general.auxiliary_model`), or on the session's own model where none is configured. Three settings tune it, in `/settings` or in the `enchant` section of `config.jsonc`:

```jsonc
"enchant": {
  "enhancer": "clarify",  // what a bare /enchant uses: clarify | tighten | grammar | expand | summarize
  "history": 30,           // how many revisions one draft keeps for undo and redo
  "max_tokens": 4096       // one rewrite's completion ceiling
}
```

The default enhancer is `clarify`. Nothing is sent anywhere else, and no key is needed.

## Use it

| | |
| --- | --- |
| `/enchant` | Rewrites the draft in the composer with the default enhancer, and leaves the draft where it is while you look. |
| `/enchant tighten` | The same with a named enhancer. Text after it is the draft, if the composer is empty. |
| `/enchant apply all` | Takes the standing proposal whole and puts it in the composer, ready to send. |
| `/enchant apply 1,3` | Takes only `change-1` and `change-3`, and keeps the rest of your draft as you wrote it. |
| `/enchant instant expand` | Proposes and takes it in one step. |
| `/enchant undo`, `/enchant redo` | Move the draft one revision back or forward. |
| `/enchant discard`, `/enchant list` | Drop the proposal; or see the five enhancers as rows that run them. |

The model reaches the same review with three tools: `enchant_propose` returns the groups, `enchant_apply` takes the ones it names (or all of them), and `enchant_history` undoes, redoes or reports where the draft stands. From eval, the same verbs are `(enchant:propose "...")`, `(enchant:accept key)` and `(enchant:revise key -1)`.

Two behaviours worth knowing. A review that is past 512 tokens on either side, or past 32 groups, is offered whole rather than in groups, because the word diff would stop being cheap. And a move that would overwrite what you typed refuses: undo only goes back when the composer still holds the draft the history left there, so an edit made after a review is never lost.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the standing proposals, the revisions, the three tools and `/enchant`. |
| `enhancers.lisp` | The five enhancers and the instruction a rewrite runs with. |
| `review.lisp` | The word diff: tokens, the common subsequence, the change groups, and composing only the chosen ones. |
| `test/cell-test.lisp` | Offline tests: the diff, the lifecycle, the three tools over a stubbed side call, and the command's draft effect. |

## What the original has that this does not

The original is a Hermes Desktop composer plugin: a sparkle beside Send, a settings page, a composer bar whose icons you show or hide, and a prompt library of folders, favourites and tags. A cell draws none of that, so the composer is reached through `/enchant` and the groups are picked by name rather than by ticking boxes in a review sheet. Its live voice half - WebRTC and WebSocket sessions with a realtime voice provider - and its Codex subscription lane are left out whole: a cell has no microphone lease and no audio pipeline, and the rewrite already runs on the model the session has.

## Credits

Enchant is [noxius-studium](https://github.com/noxius-studium)'s plugin, from
[noxius-studium/enchanted-composer](https://github.com/noxius-studium/enchanted-composer)
at commit b91e684c78d8a43a5346339a5091a1109424a28c, under the MIT License,
Copyright (c) 2026 Composer Enhancements contributors. Its enhancer texts, its
word-diff review with selectable change groups, and its undo and redo were
reimplemented in Common Lisp; `NOTICE` says what was rewritten and what was left
out, and `LICENSE` is the original repository's licence file unchanged.
