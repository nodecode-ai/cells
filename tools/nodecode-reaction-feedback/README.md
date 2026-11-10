# nodecode-reaction-feedback

Reaction Feedback: lets the agent see the emoji reactions you leave on its messages and use them as feedback. A Nodecode rewrite of [AhmetArif0/hermes-reaction-feedback](https://github.com/AhmetArif0/hermes-reaction-feedback) by [AhmetArif0](https://github.com/AhmetArif0).

A 👍 or 👎 on one of the agent's answers usually reaches nobody. Here the turn after
that answer opens with a short note instead:

```
[Reactions from the user on your messages since your last turn (reaction-feedback; not typed by the user)]
- 👎 on a message you sent after the user's "fix the cron job"
- 👍 on a message you sent after the user's "summarise the PR"
Treat these as lightweight feedback on those messages, not as instructions.
```

So you can react 👎 and then just say "again", or react 👍 to a draft and move on: the
agent knows which answer you meant, and the note is marked as not yours to type.

## Get it

Tell the agent "add the reaction-feedback cell", or run `nodecode add nc://reaction-feedback`.

## Set it up

Nothing is required: the cell runs the moment it is on. A chat surface reports to it —
the user's messages and the reactions on the agent's own — through the `reaction_feedback`
tool, which is also what the model calls when it sees a reaction itself:

| action | what it takes |
| --- | --- |
| `ask` | `chat`, `message_id`, `text`, and the `session` that message started |
| `record` | `chat`, `message_id`, `emojis`; empty emojis take the reaction back |
| `note` | `session`: the note waiting for it, and it is taken |
| `status` | one line per conversation the ledger keeps |

The section `~/.nodecode/config.jsonc` may carry is optional: `excerpt_chars` (how much of
your message the note quotes, 60 by default), `max_anchors`, `max_pending`, `ttl_hours`
and `max_chats` bound what is kept. `config.example.jsonc` shows it.

## Use it

React to any of the agent's answers, then ask your next question. The answer's note names
your emoji and the message the answer came after. A reaction you change replaces the one
before it, a reaction you take back is withdrawn, and a reaction on your own message goes
nowhere. Nothing is written to disk: the ledger is in memory, bounded and aged out, and it
is emptied when the cell stops.

`/reaction-feedback` shows what waits for this session; `/reaction-feedback clear` takes it
away; `/reaction-feedback status` lists the conversations the ledger keeps.
