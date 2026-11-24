# nodecode-telegram-stickers

Telegram Stickers: three tools that let the agent answer with a sticker from your own Telegram packs, as your chat bot. A Nodecode rewrite of [churnast/hermes-telegram-stickers](https://github.com/churnast/hermes-telegram-stickers) by [churnast](https://github.com/churnast).

| Tool | What it does |
| --- | --- |
| `telegram_sticker_find` | What the packs hold, and the stickers that fit a few words ("facepalm", "eye roll", "sleepy") or an emoji. |
| `telegram_sticker_send` | One sticker, as a reply and in the right forum topic, paced per chat. |
| `telegram_sticker_settings` | What is set; switch stickers off or back on in a chat; change a chat's cooldown. |

A `/stickers` command in the chat shows the same facts and takes `refresh`, `mute <chat id>` and `unmute <chat id>`. Ask "send a sticker for this", "no more stickers here", or just let the model pick the moment.

## Get it

Tell the agent "add the telegram-stickers cell", or run `nodecode add nc://telegram-stickers`.

## Set it up

The cell sends through the Telegram Bot API, as the bot you already talk to, so it needs that bot's token and nothing else. In `/settings` (or in the `telegramstickers` section of `config.jsonc`):

```jsonc
"telegramstickers": {
  "token_env": "TELEGRAM_BOT_TOKEN",   // or a file: "token_file": "~/.config/telegram-token"
  "packs": ["MyFavouritePack", "AnotherPack"],
  "default_chat_id": "1234"
}
```

The token is named, never written into the config: `token_env` holds the name of the environment variable that carries it (the same variable the Telegram chat bot uses), `token_file` a file only you can read. Set exactly one.

`packs` are sticker set short names — the part after `t.me/addstickers/` in a pack's link, or the whole link. The bot must be able to add the pack; almost every public pack works. With no packs set, four packs made by Telegram are used, as in the original plugin.

| Setting | What it does |
| --- | --- |
| `default_chat_id` | The chat the tools send to when a call names none; groups and channels are negative numbers. |
| `cooldown_seconds` | The least seconds between two stickers in one chat, 20 by default; `0` turns pacing off. |
| `api_base` | The Bot API base URL, for a Bot API server of your own. |
| `timeout` | How long one Bot API call may take, in seconds. |

## Use it

You do not call the tools; the model does, when a reply is a reaction rather than an answer. A person can call the same functions from eval, in the `telegram-stickers` package:

```lisp
(telegram-stickers:find-stickers (nlk:make-json-object "query" "facepalm"))
(telegram-stickers:send-sticker (nlk:make-json-object "sticker" "cats:1" "chat_id" "1234"))
```

`(help :telegram-stickers)` is the model's manual for the three verbs, kept while the cell runs.

### How it picks, and how it paces

Word-based picking is honest about what it has: a sticker carries the emoji its author tagged it with, so a query is matched against those emojis (and the feelings they stand for — a facepalm can be answered by an eye roll), and against pack names. An emoji no pack has falls back to the nearest emoji of the same feeling. When nothing fits, the answer lists the emojis the packs do have, so the model can send the closest one once or answer in words.

Pacing is per chat, in memory: a second sticker inside the chat's cooldown is refused with the wait, and a chat switched off with `telegram_sticker_settings` stays off until it is switched back on or the cell restarts.

### What the rewrite leaves out

The original plugin is a Hermes gateway plugin, so it knows which chat and which message a turn belongs to, learns packs from stickers you send the bot in a direct chat, and appends a sticker hint to Telegram turns. A cell has no gateway of its own: `chat_id` (or `default_chat_id`) names the chat, `reply_to_message_id` the message, the packs are the ones the section names, and the model decides the moment without a hint. Also left out: the vision model that described each sticker picture (`/stickers describe`), so picking by "what the picture shows" is limited to the emoji tags and pack names, and the on-disk catalog and pacing state, which live in memory here.

## License

MIT. The tools, their reaction table and the pacing are churnast's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
