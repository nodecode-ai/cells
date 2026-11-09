# nodecode-prompt-enhance

Prompt Enhance: rewrite a composer draft into the prompt you meant with a saved enhancer, and keep a library of the prompts you reuse. A Nodecode rewrite of [apoapostolov/hermes-agent-awesome-plugins](https://github.com/apoapostolov/hermes-agent-awesome-plugins) by [apoapostolov](https://github.com/apoapostolov).

## Get it

Tell the agent "add the prompt enhance cell", or run `nodecode add nc://prompt-enhance`.

## Set it up

The rewrite is one chat-completions call, so the cell needs an endpoint, a model and a key. Add a section to `~/.nodecode/config.jsonc` (the setup panel asks for the same fields):

```jsonc
"prompt-enhance": {
  "base_url": "https://api.openai.com/v1",   // any OpenAI-compatible endpoint
  "model": "gpt-5",                            // required: the model that rewrites
  "api_key_env": "OPENAI_API_KEY"             // the variable the key is read from
}
```

`base_url` and `api_key_env` have defaults; `model` does not, and until it is set the folder reports itself as needing setup. An endpoint of your own that takes no key — a local server, a gateway — still wants the variable set, to anything. The library tools need none of this: they are a JSON file under the home.

## Use it

- "Tighten this prompt: <draft>" — the model calls `prompt_enhance` with the draft and an enhancer, and the rewritten prompt comes back on its own, ready to send.
- The seeded enhancers are the original's five: `clarify`, `tighten`, `grammar`, `expand`, `summarize`. `prompt_list` shows them, `prompt_show` reads one whole, and `prompt_save` keeps your own in the `enhancers` folder.
- "Save that as a prompt called Review a diff" — `prompt_save`, then `prompt_compose` puts a saved prompt and the current draft together as one message.
- `/prompt` lists the library from the composer; `/prompt tighten` shows one.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The section, the six tools, the `/prompt` command and the manual. |
| `library.lisp` | The prompt library: one JSON file, folders, names, bodies, and the five seeded enhancers. |
| `test/cell-test.lisp` | Offline tests: lifecycle, the library round trip, and the one model call over a scripted endpoint. |

## What the original has that this does not

The composer buttons, the spinning icon, the glyphs and the drag reorder are the Hermes desktop's; a cell offers tools and a command instead. So is the one-call model override that never writes the session — the section's model is the rewrite's model here. Everything a user kept — the enhancers, the library, the payload with the draft under `Context:`, and one call outside the chat — is here.

## Credits

Prompt Enhance is [apoapostolov](https://github.com/apoapostolov)'s plugin, from
[apoapostolov/hermes-agent-awesome-plugins](https://github.com/apoapostolov/hermes-agent-awesome-plugins)
at commit 953732a9fb52b6fd89e73fc96d910aab63163e07, under the MIT License,
Copyright (c) 2026 Apostol Apostolov. The plugin's enhancer texts, prompt
library and one-shot rewrite were reimplemented in Common Lisp; `NOTICE` says
what was rewritten and what was left out, and `LICENSE` is the original
repository's licence file unchanged.
