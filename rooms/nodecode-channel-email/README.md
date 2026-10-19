# nodecode-channel-email

A Nodecode add-on that lets people talk to your Nodecode by email. It reads a
mailbox over IMAP and answers over SMTP, so your machine needs no public
address and no mail server of its own.

- **An email is an ask.** The bot answers it with one reply email in the same
  thread: `Re:` subject, `In-Reply-To` and `References` set, so every mail
  client threads it.
- **A thread is a conversation.** Reply to the bot's answer and the next turn
  remembers the thread so far. A new email (not a reply) starts a new one.
- **Only what you wrote is read.** The earlier messages a reply quotes under
  itself are cut (Gmail, Outlook, Apple Mail and most languages' "On ... wrote:"
  lines); an HTML-only email is read as text.
- **Files** attached to an ask reach the model; files the model hands back ride
  the answer as attachments.
- **Answers read like mail.** The Markdown the model writes goes out twice in
  one email (`multipart/alternative`): as typed, for plain-text clients, and
  drawn as HTML -- headings, lists, code, tables, quotes, links -- for the
  rest.
- **Your Sent folder has the whole thread.** Every email the bot sends is
  filed, read, in the mailbox's Sent folder (the one the server marks `\Sent`,
  or `sent_mailbox`). Set `save_sent` to `false` for a provider that files
  SMTP mail itself, as Gmail does.
- **Read the moment it lands.** The inbox is watched with IMAP IDLE, so the
  server tells the bot about new mail at once; a server without IDLE is read
  every `poll_seconds`.
- **No noise.** Email has no live status card: a turn sends exactly its answer,
  never progress mail.

## Who gets an answer

Email's sender line is a claim anyone can type, so a message is answered only
when all of these hold:

1. It is addressed (To or Cc) to the bot's `address`.
2. A person wrote it: auto-replies (`Auto-Submitted`), lists (`List-Id`,
   `Precedence: bulk`), bounces, delivery reports, `no-reply@` senders and the
   bot's own mail are never answered, so two robots can't mail each other
   forever. The bot's own answers carry `Auto-Submitted: auto-replied` for the
   same reason.
3. The sender is on `allowed_senders` (an address, or `@domain` for everyone at
   exactly that domain).
4. The receiving server vouched for the sender's domain: the topmost
   `Authentication-Results` header (the one your provider stamps, above
   anything the sender wrote) shows DMARC, or DKIM or SPF for an aligned
   domain, passing. `auth_server` pins whose stamp counts. Mail sent between
   two accounts of the same provider often carries no stamp at all; set
   `require_auth` to `false` only when every allowed sender is on your own
   provider and you trust it to stop forged senders.

Everything else is read once, logged with the reason, and left alone.

## Install

In Nodecode, run `/setup` → **Choose** and pick **nodecode-channel-email**.
Or copy this folder into `~/.nodecode/addons/nodecode-channel-email/`. Either
way, the channel kit that ships with Nodecode comes with it.

## Give the bot an address

- **A mailbox of its own** is simplest: every message sent to it is a
  candidate.
- **A plus address of a mailbox you have** works too: `you+bot@example.com`.
  Only mail sent To or Cc that exact address is read. The inbox is opened
  read-only (IMAP `EXAMINE`, `BODY.PEEK`), so a shared inbox keeps every
  read/unread flag a person set, and nothing in it is moved or deleted; the
  bot's one write is the copy of each answer it files in Sent.

Create an **app password** for the mailbox (most providers require one for
IMAP/SMTP) and keep it in a file or an environment variable, never in the
config. Then add the section from [`config.example.jsonc`](config.example.jsonc)
to `~/.nodecode/config.jsonc` and restart the channels. `(nck:probe "email")`
logs in to both servers and reports what it reached -- whether the server
offers IDLE, and which folder sent copies go to -- sending and filing
nothing.

IMAP and SMTP default to `imap.<domain>:993` and `smtp.<domain>:465` (TLS from
the first byte), which is right for mailbox.org, Fastmail, Gmail and many
others; set `imap_host`/`smtp_host` when your provider names others. Port 143
or 587 uses STARTTLS, and a server that doesn't offer it never sees the
password.

## Restarts

The first start only notes where the mailbox stands: nothing already in it is
answered. After a restart, mail that arrived while the bot was down is still
answered when it is newer than `catch_up_minutes` (60 by default). The thread
book (who each thread is with, its subject and `References`, how far the
mailbox was read) lives in `channels/email-state.sexp` under the Nodecode home.

## From eval

`(nce:send-mail :to "kim@example.com" :subject "..." :text "..." :attachments
("/path/file.pdf"))` sends one new email from the bot's address. Answers to
asks are sent by the adapter; this is for mail the bot starts itself.

## Tests

`.github/test-cell.sh rooms/nodecode-channel-email` runs the slice offline:
the message reader, the gates, the composer and the HTML drawing as pure
tests; IDLE and the Sent copy against a scripted IMAP server on loopback; and
the lane end to end over the real gateway with a scripted inbox and a scripted
SMTP side.
