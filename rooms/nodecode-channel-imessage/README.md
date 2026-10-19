# nodecode-channel-imessage

A Nodecode add-on that lets people talk to your Nodecode over iMessage. iMessage
has no bot API, so the bot is an Apple ID of its own, signed into Messages on a
Mac that runs the [BlueBubbles server](https://github.com/BlueBubblesApp/bluebubbles-server)
(Apache-2.0, self-hosted). Nodecode polls that server's REST API, so this
machine needs no public address and the Mac needs no route back to it.

- **Direct messages:** every message is an ask, answered in the chat.
- **Group chats:** call the bot by one of its `mention_names` and it answers
  as an inline reply under your message. Inside that thread, keep talking
  without the name; each reply continues the same conversation.
- **Answers:** plain text. Messages draws no Markdown, so headings, emphasis
  and links are spelled the way a person would type them.
- **While a turn runs:** the typing bubble, and (with `reactions`) a ‼️
  tapback on your message that turns 👍 when the turn is done, 👎 when it
  failed. A turn that fails says why in a message. There is no status line:
  Messages can edit a message five times and marks it Edited, so a card
  ticking in place is not something it can carry. Type `/stop` to stop a turn.
- **Choices:** what Discord shows as buttons comes as numbered lines under the
  message; reply with the number (or the choice's words) to pick one.
  Commands work typed: `/help`, `/models`, `/status`, ...
- **Tapbacks:** a tapback on one of the bot's messages reaches the
  conversation as a line of context, the way a reaction does on Discord.
- **Files:** Nodecode reads photos (HEIC arrives as JPEG), voice notes (as
  MP3, transcribed), and files people send, and can send files and voice
  messages back (voice needs `ffmpeg` on this machine).
- **Restarts:** the read position is kept, so a restart neither replays old
  messages nor answers what was said before the bot first started.

## Private API

BlueBubbles sends in two ways. AppleScript works on any Mac: words, files and
choices. Its **Private API** (a helper inside Messages; needs SIP partly
disabled, see the BlueBubbles docs) adds inline replies, tapbacks, the typing
bubble and unsend. The adapter asks the server which it has at start and
serves accordingly: without the Private API, group answers go into the chat
instead of inline, and there are no tapbacks or typing bubble.

## Install

In Nodecode, run `/setup` → **Choose** and pick **nodecode-channel-imessage**.
Or copy this folder into `~/.nodecode/addons/nodecode-channel-imessage/`.
Either way, the channel kit that ships with Nodecode comes with it.

## Set up the Mac

1. Make the bot an Apple ID of its own. Never use your own.
2. On a Mac that stays on, sign that Apple ID into Messages.
3. Install the BlueBubbles server from [bluebubbles.app](https://bluebubbles.app),
   grant it Full Disk Access and Accessibility, and set a server password.
   For inline replies, tapbacks and typing, turn on its Private API.
4. Note the address this machine reaches it at: the Mac's LAN address and
   port (`http://192.168.1.20:1234`), its Tailscale address, or the server's
   own Cloudflare or ngrok tunnel.
5. Keep the server password in a file (mode 600) or an environment variable,
   never in the config.

## Configure

Add a `channels.imessage` section to `~/.nodecode/config.jsonc`;
[`config.example.jsonc`](config.example.jsonc) has every key. The least that
works:

```jsonc
"channels": {
  "imessage": {
    "url": "http://192.168.1.20:1234",
    "password_file": "~/.nodecode/secrets/bluebubbles-password",
    "allowed_users": ["+15551234567"],
    "owner": ["+15551234567"]
  }
}
```

People are their handles: a phone number with its country code (written any
way, `+1 (555) 123-4567` included) or an email address. Chats are their
BlueBubbles guids: `iMessage;-;+15551234567` for a direct message,
`iMessage;+;chat...` for a group. Say "set up imessage" to Nodecode, or run
`(nck:probe "imessage")`: the probe checks the server and names its chats and
the people in them, so you can pick ids by name.

## Talk to it from Nodecode

The model reaches the server as the bot through
`(nci:request METHOD PATH &key body)`, the password added for it, for anything
the adapter does not do itself: read a chat's history, send to another chat,
rename a group.

## Tests

`.github/test-cell.sh rooms/nodecode-channel-imessage` runs the lane end to end
against a fake BlueBubbles server on 127.0.0.1 (`test/fake-bluebubbles.lisp`).
