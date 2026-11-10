# nodecode-kanban-stream

Kanban Stream: a running turn in one Telegram message, edited in place. A
Nodecode rewrite of [mbgulden/hermes-kanban-stream](https://github.com/mbgulden/hermes-kanban-stream)
by [mbgulden](https://github.com/mbgulden).

The original streamed a kanban worker's activity to an ops chat: one message
per task, rewritten as the work went on, holding the tool calls, an elapsed
timer and a tokens-a-second rate, and finished with one rewrite that was
pinned. Nodecode has no board, so the unit is a turn of a session: each turn
owns one message, and a session's folder names it in the head.

## Get it

Tell the agent "add the kanban-stream cell", or run `nodecode add nc://kanban-stream`.

## Set it up

You need a Telegram bot of your own — two fields in the `kanban-stream`
section of `~/.nodecode/config.jsonc`:

```jsonc
{
  "kanban-stream": {
    "bot_token_env": "KANBAN_STREAM_BOT_TOKEN",  // the variable holding the bot token
    "chat_id": "111111111"                        // where the stream is posted
  }
}
```

- **The bot.** Message BotFather, `/newbot`, and copy the token into that
  environment variable; open a chat with the bot and send it anything, so it
  may write to you.
- **The chat.** Your numeric Telegram user id for a private stream, or a
  group's id (negative) for a team one; the bot has to be in the group.
- **The token inline.** `"bot_token": "123456:AA..."` in the section works
  too; `bot_token_env` is read only when it is empty.

Then `/kanban-stream test` posts one line: if it lands, the token and the
chat are right. The section refuses at start without a chat or a token, and
`/cells` shows the folder `REFUSED` with the reason until both are set.

| Field | Meaning |
| --- | --- |
| `chat_id` | the Telegram chat the stream is posted to |
| `bot_token` | the bot token, written here |
| `bot_token_env` | the variable holding the token, when it is not written above |
| `sessions` | the session ids to stream; empty streams every session |
| `history` | tool lines the message repeats above the one running (default 5) |
| `throttle_seconds` | seconds between one message's rewrites (default 5) |
| `timeout_seconds` | one Bot API call's ceiling (default 5) |
| `pin_final` | pin a turn's finished message (default on) |

## Use it

Work as usual. A turn that calls a tool opens one message and rewrites it as
the turn goes on; a turn's end rewrites it once with `✅ Done.` (or
`✗ Blocked.` and the failure's own words) and pins it. A turn that only
talks sends one finished message.

- `/kanban-stream` — where it posts, what is streaming now, the last error.
- `/kanban-stream test` — post one test line.
- `/kanban-stream off` / `on` — mute it without stopping the cell.
- The `kanban_stream` tool does the same from eval: a line to post, or the
  state when it is empty.

What leaves the machine is the message: the session's folder name and id,
the names of the tools it calls with the first 80 characters of their
arguments, the elapsed time, the token rate, and the first 200 characters
of a turn's ending. Nothing else is read: this cell dials
`api.telegram.org` and nothing else, keeps no store, and swallows every
failure so a Telegram outage can never stall a turn.

## What the rewrite does not do

The original's `/steer` companion daemon — a long-poll that turned a
Telegram message into a `[steer]` comment on a kanban task — has no
counterpart here: Nodecode's `steer_turn` is the harness's own, and a chat
that steers is the `telegram` channel cell's job. The original's
`[no-stream]` task marker is the `sessions` list here, and its per-task
state (one worker per process) is per session, keyed by the session id.
