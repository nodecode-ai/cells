# nodecode-channel-matrix

A Nodecode add-on that lets people talk to your Nodecode through a Matrix
account. It reads the homeserver's `/sync` long poll, so your machine needs no
public address and no open port.

- **Direct messages:** a room with just you and the bot. Every message is an
  ask, answered quoting it.
- **Rooms:** mention the bot and it answers in a thread under your message.
  Inside that thread, keep talking without mentioning it; each reply
  continues the same conversation.
- **Answers:** written in Markdown, sent as Matrix HTML that Element and other
  clients render.
- **While a turn runs:** a status line in the thread, edited as it goes. Its
  buttons are reactions the bot puts on it: react ⏹️ to stop the turn, ℹ️ for
  details.
- **Files:** Nodecode reads images, recordings and files people post, and can
  post files back.
- **Invites:** an invite from an allowed user is joined on its own; anyone
  else's is left alone.
- **Restarts:** the sync position is kept, so a restart neither replays old
  messages nor answers what was said before the bot first started.

**Not yet: end-to-end encrypted rooms.** This version carries no crypto. In an
encrypted room the bot says so once and answers nothing. Element encrypts
direct messages by default, so create a room with encryption turned off and
invite the bot instead.

## Install

In Nodecode, run `/setup` → **Choose** and pick **nodecode-channel-matrix**.
Or copy this folder into `~/.nodecode/addons/nodecode-channel-matrix/`.
Either way, the channel kit that ships with Nodecode comes with it.

## Give the bot an account

1. Register an account for the bot on your homeserver. Never use your own.
2. Get it an access token:
   - On a server running Matrix Authentication Service (matrix.org, Element
     Server Suite): create a personal access token in the account settings.
   - Otherwise, log in once and keep the `access_token` it answers:
     ```
     curl -s https://<homeserver>/_matrix/client/v3/login -d \
       '{"type":"m.login.password","identifier":{"type":"m.id.user","user":"<bot>"},"password":"<password>"}'
     ```
3. Keep the token in a file (mode 600) or an environment variable, never in
   the config.
4. From an allowed account, invite the bot into a room (encryption off). It
   joins on its own.

## Configure

Add a `channels.matrix` section to `~/.nodecode/config.jsonc`;
[`config.example.jsonc`](config.example.jsonc) has every key. The least that
works:

```jsonc
"channels": {
  "matrix": {
    "homeserver": "https://matrix.example.org",
    "token_file": "~/.nodecode/secrets/matrix-token",
    "allowed_users": ["@you:example.org"],
    "owner": ["@you:example.org"]
  }
}
```

Or say "set up matrix" to Nodecode: it writes the section and checks the
token with `(nck:probe "matrix")`, which names the account, its rooms and
people.

At least one of `allowed_rooms` / `allowed_users` must be set: a message runs
with full access to your machine, so the bot never starts open to everyone.

## Tested

The tests run the lane against a fake homeserver with the real Nodecode
gateway behind it (`.github/test-cell.sh rooms/nodecode-channel-matrix`).
It has also been run against Synapse 1.163.0: invites, a thread in a room,
a direct message and an encrypted room.

## License

MIT, see [LICENSE](LICENSE).
