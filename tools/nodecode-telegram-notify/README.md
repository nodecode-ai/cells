# nodecode-telegram-notify

Telegram Notify: a Telegram message when a turn ends, fails or waits for an answer, from a bot of your own. A Nodecode rewrite of [madpower2000/hermes-telegram-notify](https://github.com/madpower2000/hermes-telegram-notify) by [madpower2000](https://github.com/madpower2000).

The model gets one tool, and a person gets the same verb from eval:

| What | Does |
| --- | --- |
| `telegram_notify` | One message to your Telegram chat: the body, another chat the bot can see, silent or not. |
| `/telegram-notify` | What a message would use right now, one test message, or the chat ids the bot has heard from. |
| a hook on every live frame | With `notify_on` set, a message when a turn starts, when it ends (with its final answer), when it fails, and when a question is waiting for an answer. |

Ask for "message me when the migration is done" and the model calls `telegram_notify`; a long turn that finishes while you are away tells you so.

## Get it

Tell the agent "add the telegram-notify cell", or run `nodecode add nc://telegram-notify`.

## Set it up

You need a Telegram bot of your own, and one chat to deliver to:

1. In Telegram, talk to **@BotFather** → `/newbot` → name it → copy the token it answers with.
2. Send your new bot any message (a bot cannot start a conversation, so the chat has to speak first).
3. Export the token in the shell that starts Nodecode, and ask the cell which chat ids the bot can see:

```sh
export TELEGRAM_BOT_TOKEN=...   # the token @BotFather gave you
```

Then, in `/settings` (or in the `telegram-notify` section of `config.jsonc`):

```jsonc
"telegram-notify": {
  "bot_token_env": "TELEGRAM_BOT_TOKEN",
  "chat_id": "123456789",
  "notify_on": ["turn-end", "turn-failed"],
  "include_answer": true
}
```

`/telegram-notify chats` prints the id and the name of every chat the bot has heard from — put the one you want in `chat_id`. `/telegram-notify test` then sends one message, so you know the pair works before you rely on it.

Optional, and off by default unless it says otherwise:

| Setting | What it does |
| --- | --- |
| `notify_on` | The events that message by themselves: `turn-start`, `turn-end`, `turn-failed`, `questions`. Empty, nothing messages on its own — only the tool and `/telegram-notify test` send. |
| `include_answer` | Put a turn's final answer in the message its end sends (on by default); `answer_max_chars` bounds it, 3200 by default. |
| `title` | The name a message's headline carries, `Nodecode` by default. |
| `max_chars` | The ceiling on one message, 3900 by default, under Telegram's own 4096. |
| `timeout` | How long one Bot API request may take, in seconds. |

## Use it

`/telegram-notify` says what a message would use; `/telegram-notify test` sends one; `/telegram-notify chats` lists the chats the bot can see. The model calls the tool when something is worth an interruption, and the manual it reads is `(help :telegram-notify)`.

From eval, in the `telegram-notify` package:

```lisp
(telegram-notify:notify "the build is green" :silent t)
```

`notify` is thin sugar over the tool's own body: it decodes the same contract and answers the same JSON. `(telegram-notify:status)` is what the command answers.

### What it does not do

The original also reuses the bot of a Hermes Telegram *gateway* when one is configured, and suppresses the copies its gateway already delivers. Here the bot is the section's own token and chat: one pair, one destination, and the cell never reads another app's login. It also does not talk back: this is an outbound lane, so messages arrive and the conversation stays where you are — to write to Nodecode in Telegram, there is the `channel-telegram` room.
