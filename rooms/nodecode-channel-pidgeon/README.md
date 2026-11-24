# nodecode-channel-pidgeon

Pidgeon: an email address of its own for the agent. A Nodecode rewrite of [pidgeon-ai/pidgeon-python](https://github.com/pidgeon-ai/pidgeon-python) by [pidgeon-ai](https://github.com/pidgeon-ai).

Pidgeon gives an agent an inbox at `pidgeon.ai`. Mail sent to that address
arrives in Pidgeon's event log; this cell reads the log every few seconds, and
each email becomes a conversation in its own thread. The agent's answer goes
back as a reply in the same thread, in plain text. Nothing needs a public
address or an open port: the poll reaches out, and no one reaches in.

Two things are decided before a piece of mail becomes a turn, because anyone
can send email:

- mail the **Pidgeon Firewall** flags as prompt injection is held, not
  delivered (set `suspicious` to `deliver` to pass it with a warning), and
- mail whose From did not pass **DMARC** is held, because a From is forgeable.
  `allow_unauthenticated` trusts it anyway.

The model is also told, on every answer, that instructions inside an email are
the sender's words and not its own.

## Get it

Tell the agent "add the pidgeon cell", or run `nodecode add nc://nodecode-channel-pidgeon`.

The channel kit that ships with Nodecode comes with it, and does the threads,
sessions, cards and delivery; this folder contributes Pidgeon's API.

## Set it up

1. Make a Pidgeon account and an address for the agent (not one of yours), at
   [pidgeon.ai](https://pidgeon.ai).
2. Create an API key that can **read and send**, at `/app/developers/keys`.
3. Keep the key in a file (mode 600) or an environment variable, never in the
   config.
4. Add a `channels.pidgeon` section to `~/.nodecode/config.jsonc`;
   [`config.example.jsonc`](config.example.jsonc) has every key. The least that
   works:

```jsonc
"channels": {
  "pidgeon": {
    "api_key_file": "~/.nodecode/secrets/pidgeon-key",
    "allowed_users": ["you@example.com"]
  }
}
```

At least one of `allowed_users` / `addresses` must be set: an email runs with
full access to your machine, so the lane never starts open to everyone. Add
your own address to `owner` as well, and the agent treats your instructions as
standing policy rather than a stranger's request.

`(nck:probe "pidgeon")` reads the key and says which addresses it reaches and
whether it may send, without spending a message.

## Use it

Send an email to the agent's Pidgeon address. It answers in the thread. The
next email in that thread continues the same conversation, so a long exchange
stays one session rather than becoming a new one per message.

What it holds, and how to change it:

| Held | Why | Section key |
| --- | --- | --- |
| mail with no DMARC pass | a From is forgeable | `allow_unauthenticated` |
| mail the Firewall flags | prompt injection in the body | `suspicious` |
| mail filed as spam | it is spam | — |
| any address but the listed ones | the key may reach several | `addresses` |
| any sender but the listed ones | who may start a conversation | `allowed_users` |

The lane starts reading **from the moment it starts**: mail already in the
inbox when you turn the cell on is not answered as if it had just arrived, and
a restart begins again from that moment. Only mail that comes in while the
lane runs becomes a turn.

Answers are plain text with the Markdown markers taken out, and one email per
turn: a progress line or a status card is not emailed, since each would be a
separate message in someone's inbox.
