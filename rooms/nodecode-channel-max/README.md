# nodecode-channel-max

Max Gateway: talk to Nodecode through a MAX messenger bot. A Nodecode rewrite of [Realmagnum/hermes-max-integration](https://github.com/Realmagnum/hermes-max-integration) by [Realmagnum](https://github.com/Realmagnum).

The bot reads its chats by long polling, so nothing on your machine has to be reachable from outside; each ask runs as a session of its own, and the answer is posted back where the ask came from. A dialog is addressed to the bot by being one; in a group it answers the messages that name it.

## Get it

Tell the agent "add the max channel cell", or run `nodecode add nc://nodecode-channel-max`.

## Set it up

1. Make a bot in MAX (a token from MAX's own bot tooling) and copy its token. Put it in an environment variable or in a file of mode 600 — the config names the one, never the token itself.
2. List the MAX user ids the bot may answer. Without that list it refuses to start, so a stranger's message never opens a turn.
3. In `~/.nodecode/config.jsonc`:

```jsonc
"channels": {
  "max": {
    "bot_token_env": "MAX_BOT_TOKEN",
    "allowed_users": ["123456789"],
    "poll_seconds": 3
  }
}
```

`/channels` shows the lane's state and advice; `(channel-max:probe-channel)` (`nck:probe "max"` from the page) asks MAX who the token is before anything starts.

## Use it

Message the bot in MAX. Each message opens a session named `max-<chat>`, and follows on in that chat; the answer comes back as one or more messages, and a long answer is split. The operator's own user id in `owner` is the one whose word is standing policy, as in every channel cell.

## What it is

- `cell.lisp` — the plans that speak MAX (send, edit, delete, typing), the candidate a message update becomes, the section declaration, the poll lap, and `start-channel`, the entry the channel kit calls.
- `package.lisp`, `nodecode-channel-max.asd`, `test/`.

The room/lane topology, the admission gate, the card the turn's progress is drawn on, and the answer delivery are `nodecode-channel-kit`'s, the same machinery Telegram and Discord use.

Tests: `.github/test-cell.sh rooms/nodecode-channel-max` in the cells repository, offline.
