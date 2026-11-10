# nodecode-iris-chat-channel

Iris Chat: one approved contact messaging the agent. A Nodecode rewrite of [irislib/iris-chat-channels](https://github.com/irislib/iris-chat-channels) by [irislib](https://github.com/irislib).

Iris is a private messenger with no phone number and no signup: messages are
end-to-end encrypted and carried over Nostr. This cell gives the agent an Iris
identity of its own through the `iris` CLI, answers the one person you approve,
and keeps the conversation in the session `iris-chat-channel` — the same agent, tools
and models you use in the terminal.

## Get it

Tell the agent "add the iris-chat-channel cell", or run `nodecode add nc://iris-chat-channel`.

## Set it up

Install the Iris CLI ([iris-chat-rs](https://github.com/irislib/iris-chat-rs)),
pair an agent identity in a private directory of its own, and accept the
invite your contact sends:

```sh
export IRIS_DATA_DIR=~/.nodecode/iris-channel/profile
iris --json account create --name "nodecode agent"
iris --json invite accept "<the private invite link>"
```

Then turn the cell on and give it a `channels.iris-chat-channel` section (or use
`/settings`):

```jsonc
{
  "channels": {
    "iris": {
      "state_dir": "~/.nodecode/iris-channel",  // the CLI's profile and pairing.json live here
      "owner": "npub1..."                       // the approved contact; omit when pairing.json names it
    }
  }
}
```

| Member | Default | What it does |
| --- | --- | --- |
| `state_dir` | unset | the private Iris channel directory; required |
| `owner` | unset | the approved contact's npub or hex id; `pairing.json` says it when this is absent |
| `program` | `iris` | the CLI to run |
| `poll_seconds` | `3` | how long one lap waits before the next |
| `timeout_seconds` | `45` | one CLI call's ceiling |
| `manage_service` | `true` | start the Iris service when it is down, and stop only the one this cell started |
| `max_length` | `12000` | the longest reply, in characters; a longer one is split |

The cell refuses to start until it has a state directory and one resolvable
contact, and it answers nobody else: messages are taken only from that
contact's direct chat.

## Use it

Write to the agent in Iris. Each new message starts a turn in the session
`iris-chat-channel`, and the finished answer goes back as text, split at
`max_length` when it is long. The ids of the messages already taken are kept
in `nodecode-seen.txt` in `state_dir`, so a restart does not answer anything
twice; the very first lap on a fresh directory marks what is already there as
seen instead of replaying it.

From the shell, `/iris-chat-channel` shows the contact, how many messages have been
seen and how many asks are still owed an answer; `/iris-chat-channel TEXT` sends
TEXT to the approved contact. The model has `iris_chat_send {text}` for the
same thing, so a scheduled turn can report back into the chat.

## The wire

The cell runs the `iris` CLI with an argument list and no shell, its
identity directory passed as `IRIS_DATA_DIR`:

- `iris --json --no-background-sync read <contact> --limit 1000` — the conversation
- `iris --json --no-background-sync send <contact> <text>` — one reply
- `iris --json --no-background-sync service status` / `service run` — the background service

Each answers `{"status": "ok", "data": …}` (or `{"ok": true, "result": …}`),
and an error is reported with what the CLI said.

## Not ported

- **The Hermes gateway adapter and its setup wizard.** The section, `/settings`
  and `/iris-chat-channel` take their place; pairing is the CLI's own `invite accept`.
- **The sqlite job queue and outbox.** One seen list and one session are the
  Nodecode form; a reply is not retried against the outbox.
- **The loopback bridge for managed containers** (`IRIS_CHANNEL_BRIDGE_URL`):
  a cell runs beside the CLI and needs no broker.
- **Typing and seen receipts**, and the wizard's first-contact introduction.

Nothing of the original's Python is bundled: the cell runs the same `iris`
CLI, and its own code is Common Lisp.
