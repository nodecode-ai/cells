# nodecode-channel-chatwork

Chatwork: talk to Nodecode from a Chatwork room. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-chatwork](https://github.com/TakeshiTGAL/hermes-plugin-chatwork) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

Chatwork is the business chat most Japanese companies live in: rooms, no
threads, and a message body that is its own markup. This cell puts a bot
account in it — mention it with Chatwork's own To button, or reply to
something it wrote, and it answers under your message the way the Reply
button does, so you get the usual notification. A poll of Chatwork's own API
brings the messages in, so your machine needs no public address and no open
port.

## Get it

Tell the agent "add the chatwork cell", or run `nodecode add nc://channel-chatwork`.

The channel kit that ships with Nodecode comes with it, and does the rooms,
lanes, cards and delivery; this folder contributes Chatwork's API and
notation.

## Set it up

1. Use an account of its own for the bot — not yours. Log in to Chatwork as
   it and issue an API token from
   `https://www.chatwork.com/service/packages/chatwork/subpackages/api/token.php`.
2. Keep the token in a file (mode 600) or an environment variable, never in
   the config.
3. Invite that account to every room it should answer in, with permission to
   write.
4. Add a `channels.chatwork` section to `~/.nodecode/config.jsonc`;
   [`config.example.jsonc`](config.example.jsonc) has every key. The least
   that works:

```jsonc
"channels": {
  "chatwork": {
    "token_file": "~/.nodecode/secrets/chatwork-token",
    "allowed_rooms": ["123456789"],
    "allowed_users": ["987654"],
    "owner": ["987654"]
  }
}
```

At least one of `allowed_rooms` / `allowed_users` must be set: a message
runs with full access to your machine, so the bot never starts open to
everyone.

5. Say "set up chatwork" to Nodecode, or call `(nck:probe "chatwork")`
   yourself: it names the account the token belongs to and the rooms it can
   see, by id and name, so you can fill `allowed_rooms` from that instead of
   guessing ids. The room id is the number after `rid` in the room's URL.

## Use it

- **Group rooms:** press Chatwork's To button on the bot — `[To:the bot]` —
  or reply to one of its messages, and it answers with a reply of its own.
- **My Chat and direct chats:** every message is an ask, mention or not.
- **Free-response rooms:** list a room in `free_response_rooms` and every
  message there is answered; set `require_mention` false to do that
  everywhere.
- **Commands:** a line starting with `/` is a Nodecode command (`/help`,
  `/new`, `/model`), answered in the room; with `owner` set, the commands
  that act on your machine are the operator's alone.
- **Writing:** answers are plain text with Chatwork's own notation — a code
  block becomes a Chatwork code box, a heading becomes 【...】, and a
  notification tag the model writes by itself is defused so it cannot ping a
  room.

The lane checks Chatwork every few seconds (`poll_seconds`, default 5), and
only re-reads a room whose metadata moved, so it stays inside Chatwork's 300
calls per five minutes. Messages said before the bot started are marked, not
answered; a restart does not replay them. A token that belongs to another
account starts fresh in every room.

## What it does not do

Chatwork has no message deletion, so a status card stays where it was
written and the answer posts under it. There is no typing indicator (the beat
is one cheap call a day), no reaction, and no file upload. Threads do not
exist in Chatwork, so every answer lands in the room it was asked in.

## License

MIT, see [LICENSE](LICENSE). Rewritten from the plugin named in [NOTICE](NOTICE).
