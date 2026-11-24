# nodecode-zalo

Zalo: talk to Nodecode through a Zalo bot. A Nodecode rewrite of [moken627-hub/hermes-plugin-zalo](https://github.com/moken627-hub/hermes-plugin-zalo) by [moken627-hub](https://github.com/moken627-hub).

People you allow message the bot in a private chat or a group, and it answers
there with the same agent you run in the terminal: same tools, same models,
same store. Each chat is a session of its own, so a conversation there is a
conversation in the agent's history. The bot also answers six commands
itself — `/menu`, `/rules`, `/huong-dan`, and for a group's admins `/kick`,
`/pin` and `/poll` — without spending a turn.

## Get it

Tell the agent "add the zalo cell", or run `nodecode add nc://zalo`.

## Set it up

Create a bot at [bot.zaloplatforms.com](https://bot.zaloplatforms.com), and
keep the token it gives you in a file only you can read, or in a variable.
Then turn the cell on and give it a `channels.zalo` section (or use
`/settings`):

```jsonc
{
  "channels": {
    "zalo": {
      "bot_token_file": "~/.nodecode/secrets/zalo.token",  // or bot_token_env, or bot_token
      "allowed_users": ["1234567890123456789"]             // the Zalo user ids who may talk
    }
  }
}
```

The bot refuses to start until it has a token and knows who may talk to it.
`allow_all: true` lets any Zalo user in — they run with your machine's
authority, so name people when you can.

| Member | Default | What it does |
| --- | --- | --- |
| `bot_token` | unset | the bot token, written here |
| `bot_token_file` | unset | a file holding the token, one line |
| `bot_token_env` | unset | the variable holding the token |
| `allowed_users` | unset | the Zalo user ids who may talk |
| `allow_all` | `false` | let anyone through |
| `api_base` | `https://bot-api.zaloplatforms.com` | the Bot API root |
| `poll_seconds` | `2` | how long one lap waits before the next |
| `poll_timeout` | `25` | how long the API holds a `getUpdates` call open |
| `max_length` | `2000` | the platform's per-message limit |
| `timeout_seconds` | `60` | one request's ceiling |

Exactly one of `bot_token`, `bot_token_file` and `bot_token_env` is set.

## Use it

Message the bot. It keeps the conversation in the session `zalo-<chat id>`,
so the next message in that chat carries on where the last one stopped, and
the answer comes back as plain text — Zalo renders no markdown, so headings,
emphasis, code fences and table rows are flattened, and an answer past 2000
characters is split.

From the shell, `/zalo` shows the bot it is talking as, the address, the
people allowed and whatever asks are still owed an answer; `/zalo CHAT-ID
TEXT` sends a message to one chat. The model has `zalo_send {chat_id, text}`
for the same thing, so a scheduled turn can report back into a chat.

The commands the bot answers itself, without a turn:

- `/menu` — the list of commands
- `/rules` — the group's rules
- `/huong-dan` — how to use the bot
- `/kick @user` — remove a member
- `/pin <message_id>` — pin a message
- `/poll <question> | option, option` — a poll

## The wire

`POST {api_base}/bot{token}/<method>` with a JSON body: `getUpdates` (long
polled), `sendMessage`, `getMe`, `kickGroupMember`, `pinGroupMessage` and
`createPoll`, answered as `{"ok": true, "result": …}`. A failure from the
API is reported with the token scrubbed out of the text.

## Not ported

- **Webhook mode.** The original can receive updates on its own aiohttp
  listener; a cell long-polls `getUpdates`, which needs no open port.
- **The anti-spam, warn and report systems, the settings/history/CRM stores,
  the welcome messages and the name triggers.** They are JSON files and
  in-memory maps of the original's own; the admission rule (`allowed_users`
  or `allow_all`), the six commands and the plain-text shaping are carried.
- **`hermes gateway setup`.** The section, `/settings` and `/zalo` are the
  same surface.
