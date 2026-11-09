# nodecode-chatto

Chatto: talk to Nodecode through a self-hosted team chat server. A Nodecode rewrite of [jrimmer/hermes-chatto-plugin](https://github.com/jrimmer/hermes-chatto-plugin) by [jrimmer](https://github.com/jrimmer).

Chatto is a self-hostable team chat server with rooms, threads and direct
messages. This cell puts a bot account in it: mention the bot in a room and it
answers in a thread under your message; write to it directly and every message
is an ask. A poll of the server's own API brings the messages in, so your
machine needs no public address and no open port.

## Get it

Tell the agent "add the chatto cell", or run `nodecode add nc://chatto`.

The channel kit that ships with Nodecode comes with it, and does the rooms,
lanes, threads, cards and delivery; this folder contributes Chatto's API.

## Set it up

1. Give the bot an account of its own on your Chatto server. Never use yours.
2. Keep its password in a file (mode 600) or an environment variable, never in
   the config.
3. Add a `channels.chatto` section to `~/.nodecode/config.jsonc`;
   [`config.example.jsonc`](config.example.jsonc) has every key. The least that
   works:

```jsonc
"channels": {
  "chatto": {
    "url": "https://chat.example.com",
    "login": "nodecode-bot",
    "password_file": "~/.nodecode/secrets/chatto-password",
    "allowed_users": ["you"],
    "owner": ["you"]
  }
}
```

At least one of `allowed_users` / `allowed_rooms` must be set: a message runs
with full access to your machine, so the bot never starts open to everyone.

4. Say "set up chatto" to Nodecode, or call `(nck:probe "chatto")` yourself:
   it logs in and names the rooms the account can see, by id and name, so you
   can fill `allowed_rooms` from that instead of guessing ids.

## Use it

- **Direct messages:** every message is an ask, answered under it.
- **Rooms:** mention the bot — `@nodecode-bot what broke the build?` — and it
  answers in a thread under your message. A line starting with `/` runs a
  command without a mention.
- **Threads:** keep talking inside the thread without mentioning the bot; each
  reply continues the same conversation.
- **Many rooms:** with `allowed_rooms` empty the bot serves every room it is
  in and picks up rooms it is added to on the next poll. Name ids to narrow it.
- **Replies:** an answer is written as Chatto Markdown, in the room, under the
  ask's thread.

The lane polls every few seconds (`poll_seconds`, default 5). Messages said
before the bot started are marked, not answered; a restart does not replay
them.

## What it does not do

The original plugin listens on Chatto's realtime WebSocket; this one polls the
ConnectRPC API, so a reply lands within a few seconds rather than at once. A
status card's edit arrives as a fresh line in the thread, because Chatto's
edit call is not carried. File uploads and reactions are the next additions.

## License

MIT, see [LICENSE](LICENSE). Rewritten from the plugin named in [NOTICE](NOTICE).
