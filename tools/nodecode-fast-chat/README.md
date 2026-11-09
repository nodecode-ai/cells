# nodecode-fast-chat

Fast Chat: open a session on the model you name, leaving your defaults alone. A Nodecode rewrite of [iyoda/hermes-fast-chat](https://github.com/iyoda/hermes-fast-chat) by [iyoda](https://github.com/iyoda).

## Get it

Tell the agent "add the fast-chat cell", or run `nodecode add nc://fast-chat`.

## Set it up

`/fast-chat` needs one thing to know: the model a bare `/fast-chat` opens on. In `/settings` (or the page's Settings tab), under `fast-chat`, answer `model` with an id as `/models` lists it, behind `PROVIDER/` for another provider and before `:EFFORT` for a rung:

```jsonc
{
  "fast-chat": {
    "model": "gpt-6.1-sol",
    "provider": "openai-codex",
    "effort": "low"
  }
}
```

`provider` may be blank (the gateway's default provider serves the model) and `effort` is one of `off`, `minimal`, `low`, `medium`, `high`, `xhigh`, `max`. That is the folder's whole section: `config.example.jsonc` is the same thing, commented.

## Use it

```
/fast-chat gpt-6.1-sol
/fast-chat openai-codex/gpt-6.1-sol:high
/fast-chat canary-9 --provider acme --effort minimal --in ~/work --prompt sort the inbox
```

Each call opens a **new session of this organism** on exactly the provider, model and reasoning rung the line names, and records that pick on that session alone — the fact `/models` writes. Your default model does not move, and the session you typed the command in keeps what it runs on.

- `PROVIDER/MODEL:EFFORT` is one word: the prefix is a provider only when the spec holds a single slash, and the suffix an effort only when it names a rung, so `openrouter/deepseek/deepseek-r1:free` stays whole. `--provider` and `--effort` say either part outright and win over the word.
- `--in DIR` is the new session's working directory; without it the new session works where this one does.
- `--prompt TEXT` starts the new session's first turn at once, with the rest of the line as its prompt — no quotes needed. Without it the session waits, and the answer names its id: open it from `/sessions`.
- `/fast-chat --help` answers the usage line.

The command is for an interactive shell. The manual is `(help :fast-chat)`.

## What it is

| File | What it is |
| --- | --- |
| `cell.lisp` | The command, its argument line, and the section. |
| `package.lisp` | The package: `launch`, the readers of a line, the rungs, the condition. |
| `test/` | The slice: the lifecycle, the argument line, a real session pinned to a target. |

One `nle:define-cell` declaration: a section, `/fast-chat`, and the manual topic. No worker, no hook, no tool, no thread — an invocation creates a session and returns. The cell writes no file of its own and reaches no host: the session it opens runs on whatever provider the named model belongs to, and the first turn, if any, is an ordinary turn of this organism.

## Credit

MIT, Copyright (c) 2026 iyoda and Copyright (c) 2025 Nous Research: the original repository's `LICENSE`, unchanged, is here. `NOTICE` says what was rewritten and what was left out.
