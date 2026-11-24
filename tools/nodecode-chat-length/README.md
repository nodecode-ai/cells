# nodecode-chat-length

Chat Length: know when a chat is getting too long, and get an opener for a fresh one. A Nodecode rewrite of [MadeByBrain/hermes-chat-length](https://github.com/MadeByBrain/hermes-chat-length) by [MadeByBrain](https://github.com/MadeByBrain).

A long chat gets slow, expensive and vague, and once Nodecode starts dropping
older turns from the window, details are gone for good. This cell reads how
full the window is from the numbers the provider actually billed, and says so
in the reply in which a threshold is first passed:

> **Heads-up: this chat is getting long** (about 72 % of the context window).
> Replies get slower and less precise from here on. Best to open a new chat and
> paste this opener:
>
> ```text
> Continue our chat "Summer party in Cologne" (session s-K3F9Q2ZA). The earlier
> chat is still saved: read it back with `nodecode resume s-K3F9Q2ZA` if you need
> what we said there.
> Status: ...
> Open: ...
> ```

The urgent wording comes at the second threshold, and each level speaks once
per chat. Once the fill drops well below the first threshold (an eviction, a
cleared chat, a smaller model) both arm again.

The numbers are the provider's, not a guess: the cell reads the session's
newest `turn.usage` fact in the log — prompt tokens including cache reads and
writes, plus the reply — against the model's context window. It is a `:round`
hook, so the notice rides the request in which the threshold is crossed,
whether you are in the shell, the web page or a chat channel. Nothing is added
to the system prompt.

## Get it

Tell the agent "add the chat-length cell", or run `nodecode add nc://chat-length`.

## Set it up

Nothing is required and no key is needed. Its section in `config.jsonc` (or
`/settings`) is optional:

| Setting | Default | What it is |
| --- | --- | --- |
| `notice_percent` | 70 | fill level, in percent of the context window, for the first notice |
| `urgent_percent` | 90 | fill level, in percent, for the urgent notice |
| `language` | `auto` | `auto` follows the conversation (English template, translated by the model); `en` or `de` fixes the wording |

## Use it

- The notice arrives on its own, once per level, in the reply.
- `/chat-length` answers the numbers on demand: `chat-length: 87.6k of 200k
  tokens, 43 % of the context window (first notice at 70 %, urgent at 90 %).`
  Past the first threshold it adds the opener, ready to paste.
- The `chat_length` tool answers the same for the model, so it can tell you
  its own fill instead of guessing.
- `(help :chat-length)` has the manual; `(chat-length:chat-fill "s-K3F9Q2ZA")`
  answers `(values TOKENS WINDOW RATIO)` from eval.
