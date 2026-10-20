# nodecode-channel-whatsapp

A Nodecode add-on that lets people talk to your Nodecode on WhatsApp. The bot
is a **linked device** of a WhatsApp account, the way WhatsApp Web is: the
phone stays the account's primary, your machine needs no public address, no
open port and no Meta business account.

- **Direct chats:** every message is an ask, answered quoting it.
- **Groups:** @mention the bot, or quote (reply to) one of its messages, and
  it answers quoting you. WhatsApp has no threads, so a quote is how a
  conversation continues: quote the bot's answer and the next ask joins the
  same lane.
- **Answers:** written in Markdown, respelled into WhatsApp's own `*bold*`,
  `_italic_`, `~strike~` and code blocks; a long answer comes in parts.
- **While a turn runs:** a status card edited as it goes, with the typing
  indicator on. WhatsApp bots get no buttons, so the card says `/stop ends
  it`; `/stop` in the chat stops the turn.
- **Choices:** a question with options (`/models`, `/voice`, any answer with
  choices) comes as a native WhatsApp poll. A vote is the press.
- **Files:** Nodecode reads images, voice messages, videos and documents
  people send, and sends them back as WhatsApp media. A spoken reply arrives
  as a real voice message, waveform included (`voice_replies`).
- **Strangers:** a direct message from a number not allowed is answered with
  a pairing code and you are told; `/channels pair CODE` lets them in.
- **Group roles:** `allowed_roles` lets a group's admins (or only its
  creator) drive the bot as listed users do.
- **Restarts:** after the device was away, asks WhatsApp kept from the last
  `catch_up_minutes` are answered, each marked as late; older ones and ones
  already answered are skipped.
- **Calls:** a linked device cannot answer a call. The bot declines it and
  tells an allowed caller to send a voice message instead.
- **Read receipts:** an ask the bot takes is marked read (blue ticks).

**What WhatsApp does not allow:** threads, buttons, a `/` command menu, voice
calls, editing a message more than 20 minutes after it was sent (a longer
turn's card stops updating then; the answer still arrives), and reading a
message back, so a reaction, an edit or a deletion is reported as what was
sent, never as what is there.

## Install

In Nodecode, run `/setup` -> **Choose** and pick **nodecode-channel-whatsapp**.
Or copy this folder into `~/.nodecode/addons/nodecode-channel-whatsapp/`.
Either way, the channel kit that ships with Nodecode comes with it.

The first start builds the WhatsApp driver from [`driver/`](driver) (a small
Go program on [whatsmeow](https://github.com/tulir/whatsmeow), which speaks
WhatsApp's multi-device protocol). That needs Go 1.24 or newer. Built
elsewhere, point `driver_path` at it instead.

## Give the bot a number

Give it a number of its own: a second SIM, an eSIM, or WhatsApp Business on
the same phone. Linked to your own number, it speaks as you; with `self_chat`
on it then answers you in your chat with yourself (Message yourself).

WhatsApp does not allow automated accounts, and a number that misbehaves can
be banned. Keep the bot to people who expect it.

## Configure

Add a `channels.whatsapp` section to `~/.nodecode/config.jsonc`;
[`config.example.jsonc`](config.example.jsonc) has every key. The least that
works:

```jsonc
"channels": {
  "whatsapp": {
    "allowed_users": ["+15551234567"],
    "owner": ["+15551234567"]
  }
}
```

At least one of `allowed_chats` / `allowed_users` must be set: a message runs
with full access to your machine, so the bot never starts open to everyone.

## Link it

Restart the channels, then from Nodecode:

- `(ncw:link :phone "+<the bot's number>")` answers an 8-character code. On
  the bot's phone: WhatsApp -> Settings -> Linked devices -> Link a device ->
  **Link with phone number instead**, and type it.
- `(ncw:link)` answers a QR code to scan on that same screen.

Or say "set up whatsapp" to Nodecode: it writes the section, links the
device, and checks it with `(nck:probe "whatsapp")`, which names the account,
its groups and people. `(ncw:unlink)` logs the device out.

The device's keys are kept in `~/.nodecode/channels/whatsapp/<session>.db`;
keep that file private, since it is the account's login.

## Tested

The tests run the lane against a fake driver with the real Nodecode gateway
behind it (`.github/test-cell.sh rooms/nodecode-channel-whatsapp`): direct
chats, group mentions and quotes, edits, reactions and deletes, polls and
votes, files, calls, catch-up and linking. The driver itself has been run
against WhatsApp's servers up to the linking QR code.

## License

MIT, see [LICENSE](LICENSE). The driver links whatsmeow, MPL-2.0.
