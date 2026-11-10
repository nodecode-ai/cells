# nodecode-channel-nextcloud-talk

Nextcloud Talk: talk to Nodecode through a Nextcloud Talk account. A Nodecode rewrite of [smilinTux/hermes-nextcloud-talk](https://github.com/smilinTux/hermes-nextcloud-talk) by [smilinTux](https://github.com/smilinTux).

The agent signs in as a genuine Nextcloud account of its own, with an app
password, and joins Talk rooms the way a person does: invited to a room it
is in it, addressed in a one-to-one room, or @mentioned in a group. Each
room is a session of its own, so a conversation there is a conversation in
the agent's history, with the same tools, models and store as the terminal.
Nothing is exposed to the network: the account reads its rooms by long
polling.

## Get it

Tell the agent "add the channel-nextcloud-talk cell", or run `nodecode add nc://channel-nextcloud-talk`.

## Set it up

On your Nextcloud server make a dedicated account for the agent — do not
reuse a person's — and give it an app password: **Settings → Security →
Devices & sessions → Create new app password**. Keep that password in a
file only you can read, or in a variable, and never type the account's real
login password here.

Then turn the cell on and give it a `channels.nextcloud-talk` section (or
use `/settings`):

```jsonc
{
  "channels": {
    "nextcloud-talk": {
      "server_url": "https://cloud.example.com",
      "username": "nodebot",
      "app_password_file": "~/.nodecode/secrets/nextcloud-talk.password",
      "allowed_users": ["kim"]                  // the Nextcloud user ids who may talk
    }
  }
}
```

The lane refuses to start until the server accepts the app password and
`allowed_users` names somebody — everyone who can reach the agent runs with
your machine's authority. Invite the account to the rooms you want it in,
or, in a one-to-one room, just message it.

| Member | Default | What it does |
| --- | --- | --- |
| `server_url` | — | the instance's address |
| `username` | — | the account's Nextcloud user id |
| `app_password_env` | unset | the variable holding the app password |
| `app_password_file` | unset | a file holding the app password, one line |
| `display_name` | unset | the name to match plain-text `@` mentions on, when the server's own answer is not it |
| `allowed_users` | — | the Nextcloud user ids the agent answers |
| `owner` | unset | the operator's user id, whose word is standing policy |
| `free_rooms` | unset | room tokens that answer every message, not only a mention |
| `poll_timeout_seconds` | `10` | how long one room's read is held open while it is quiet |
| `discovery_seconds` | `60` | how often the room list is read again |
| `max_rooms` | `20` | rooms polled at most, in turn |
| `reactions` | `false` | mark the message being worked on with a reaction |
| `soul_file` | unset | a `SOUL.md` that is the channel's standing persona |
| `turn_budget_minutes` | `0` | minutes an ask's turn may run; `0` caps nothing |

Exactly one of `app_password_env` and `app_password_file` is set.

## Use it

Message the account in a room, or @mention it in a group:

- **One-to-one room** — every message is an ask.
- **Group room** — only a message that names the account (`@nodebot`, or
  Talk's quoted `@"Node Bot"`) is one; everything else is room chatter.
- **A room in `free_rooms`** — every message is an ask, which is what a
  room dedicated to the agent wants.

The answer goes back into the room under the ask as Talk markdown, split
when it passes Talk's 32000 characters, and while a turn runs the account
shows Talk's typing signal. In a `mention` room the messages it did not
answer are kept as room chatter and handed over with the next one it does.
With `reactions: true` the message being worked on also carries an eyes
reaction.

From eval, the whole OCS surface is reachable as the account:

```lisp
(channel-nextcloud-talk:request "rooms")
(channel-nextcloud-talk:request "chat" :token "tok1" :limit 20)
(channel-nextcloud-talk:request "send" :token "tok1" :message "on my way")
```

## What it leaves out

The original's **bot mode** — a Nextcloud-side webhook bot with an inbound
listener and HMAC-signed requests — is not here: a cell reads Talk over the
account's own REST API and needs no port open. Federation invites are
accepted by messaging the account normally, so the inviter allowlist, the
`watch` room mode and the multi-agent chain limit are the original's runtime
bookkeeping rather than anything Talk asks for. `NOTICE` has the full list.
