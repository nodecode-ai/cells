# nodecode-enhance-prompt

Enhance Prompt: rewrites a rough draft into a structured, numbered agent brief, and puts it back in the composer without sending it. A Nodecode rewrite of [tuancookiez-hub/hermes-enhance-prompt](https://github.com/tuancookiez-hub/hermes-enhance-prompt) by [tuancookiez-hub](https://github.com/tuancookiez-hub).

The original is a sparkle beside Send in Hermes Desktop, plus an `enhance_prompt` tool and an `/enhance` command in the agent. A cell has no button to draw, so the rewrite is one verb here, reached three ways: the tool, the command, and eval.

| | |
| --- | --- |
| `/enhance <draft>` | Rewrites a draft into a brief. Where the shell hands a command its composer text, the brief replaces the draft in place. |
| `/enhance 2`, `/enhance 3` | Deepens it: each requirement broken into action, inputs and deliverable, then the work as phases with checkpoints. |
| `/enhance undo` | Puts back the draft the first rewrite started from. |
| `enhance_prompt` | The same rewrite as a tool the model calls, with `input` and `stage`. |

## Get it

Tell the agent "add the enhance-prompt cell", or run `nodecode add nc://enhance-prompt`.

## Set it up

Nothing to obtain: the rewrite runs on your own model, through the same side call a session title or a recap uses. Two optional settings, in `/settings` or in the `enhance-prompt` section of `config.jsonc`:

```jsonc
"enhance-prompt": {
  "default_stage": "1",   // what a fresh draft starts at: 1, 2 or 3
  "max_tokens": 16384      // one rewrite's completion ceiling
}
```

The call is a stateless side call, not an agent turn: no tools run, nothing is written to a session, and nothing is sent to a third party. It spends one model turn per rewrite — your credits, on the model the auxiliary lane names (`general.auxiliary_model`), or the session's own where none is configured.

## Use it

Type a rough draft in the composer and run `/enhance`:

```
make the exporter stop eating memory, it dies on big files
```

and the composer is replaced, unsent, with a brief:

```
Goal: the exporter completes on large files without exhausting memory.
1. Reproduce the failure on the largest fixture and measure peak RSS.
2. Find where the document is held whole, and stream it instead.
3. Verify: the fixture exports, and peak RSS stays under the budget.
```

Send it when it says what you meant. Running `/enhance` again on the brief deepens it one stage, and `/enhance undo` puts the original draft back. A leading 1, 2 or 3 picks the stage: `/enhance 2 make the exporter stop eating memory` starts that draft at stage 2.

A shell that can hand a command the composer's text — Nodecode releases whose command catalog carries `reads: ["draft"]` — gives `/enhance` the draft you are writing, and the brief goes back into the box, unsent, where `/enhance undo` still finds the original. Everywhere else the draft is the command's argument, and the brief is the answer it shows.

From eval, the same rewrite is `(enhance-prompt:enhance "make the exporter stop eating memory")`, and `(help :enhance-prompt)` is the model's manual for the tool.

### What the rewrite does and does not do

It does not answer the request and does not add goals the draft did not have: it restates the draft as a task an agent can execute on the first attempt, in the draft's own language, with only the sections that apply. The three stages are the original's own hints — a clarification, a task list, a phased plan — not three lengths of the same text.

### What the rewrite leaves out

The original's Desktop half is the sparkle's chrome: the score printed on the button (a second model call, before → after), the click-while-spinning cancel, the backup that lives in the button, the keybinding. The rewrite's default stance is the command's: one call, the brief in the composer, `/enhance undo` to discard it. It also sends nothing to a third party and stores nothing durable — a chain of drafts lives in memory, per chat, and is dropped when the cell stops.

## License

MIT. The prompts and the wrapper stripping are tuancookiez-hub's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
