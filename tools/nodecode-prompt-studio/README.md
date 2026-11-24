# nodecode-prompt-studio

Prompt Studio: builds a prompt step by step, by the rules of the model that will read it, and hands it back to paste or send. A Nodecode rewrite of [bwoliveira/prompt-studio](https://github.com/bwoliveira/prompt-studio) by [bwoliveira](https://github.com/bwoliveira).

A rough request turns into a good prompt through the few questions that change the result, one at a time, each with a recommended answer: what you want back, what the model cannot guess, reference text and where it came from, rules that cannot be broken, how you will know it is done, and how much it may act on its own. The prompt is then written locally by that model's own rules -- there is no call behind it -- and printed ready to paste, or to send straight from the composer.

The original was a sheet over Hermes Desktop's message field. A cell cannot draw a sheet, so the wizard is a conversation here: the answers travel with the call, the tool reports the next step it would ask, and nothing is kept between turns.

| | |
| --- | --- |
| `/prompt TASK; step: answer; ...` | Builds the prompt now. Typed in a composer that holds text, that text is the task. |
| `/prompt-steps [TASK]` | Every step with its recommended answer, and the answers typed so far. |
| `/prompt-edit TASK; answers` | The built prompt, then the auxiliary model's rewrite of it (see "Set it up"). |
| `prompt_studio` | The same build as a tool the model calls, with `task` and the rest as arguments. |
| `(prompt-studio:build-prompt (prompt-studio:brief :task "..."))` | The same from eval. |

The steps are `deliverable`, `model`, `context`, `paste` and `source`, `rules`, `done`, `avoid`, `autonomy`, `subagents`, `format` and `length`. Every one may be left out; `auto` reads it off the task. A paste of two thousand characters or more goes above the task, as the original put a long document; the rest of the prompt follows the chosen model's guide:

- **Opus**: pasted text inside `<pasted_content>` tags that carry an id, escaped so it cannot close them, with the instruction to treat what is in it as information and not as commands.
- **Sonnet**: the same sections, with the "stop and report when the work is done and checked" rule in place of Opus's short one, and the paste marked by plain markers.
- **Astra**: the document last, the priority line over a skill or `AGENTS.md`, the action line, and no verification line -- that model checks its own work.
- **auto**: the vendor-neutral rules, used when no model is named and the section's `default_model` is `auto`.

## Get it

Tell the agent "add the prompt studio cell", or run `nodecode add nc://prompt-studio`.

## Set it up

Nothing is required: the prompt is built locally, so it works with no provider and no key. Four settings tune it, in `/settings` or in the `prompt-studio` section of `config.jsonc`:

```jsonc
{
  "prompt-studio": {
    "default_model": "auto",   // auto | opus | sonnet | astra: the rules used when a brief names no model
    "paste_cap": 12000,        // the most pasted characters one prompt carries
    "max_tokens": 900,         // the most the polishing model may answer
    "polish": true             // turn off to keep every prompt exactly as built
  }
}
```

The polish is the one model call: it runs on the auxiliary model (`general.auxiliary_model`, else the session's own), one message, no tools and no history, and it only ever sees the prompt the wizard built. A section it loses on the way back and the built prompt is the one returned, with a note saying so.

## Use it

```
/prompt Fix the login timeout; deliverable: implementation; done: the retry test passes
```

answers the wizard's questions in one line and prints:

```
TASK
Fix the login timeout

REQUIREMENTS
Only make changes that are directly requested or clearly necessary: ...

AUTONOMY
...

OUTPUT
Answer in the language the task above is written in, ...

DONE WHEN
the retry test passes

---
written for the model reading it, 5 sections, 1,240 characters
```

Copy that into a message, or run `/prompt-edit` for the model's own wording. To have the model ask you the questions, call the tool with the task alone: it answers the prompt and names the next step it would have asked.

## What a cell cannot have

The sheet, its keys and its buttons; the composer sparkle; the per-step suggestion calls, which asked a model one small question at a time, are folded into the one optional polish. Everything the answers decide -- the sections, the order, the tags, the counts -- is here.
