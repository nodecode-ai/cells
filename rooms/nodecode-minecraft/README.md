# nodecode-minecraft

Minecraft: the agent in a server's in-game chat, as a bot that joins, listens
and answers. A Nodecode rewrite of
[kyssta-exe/hermes-minecraft-gateway](https://github.com/kyssta-exe/hermes-minecraft-gateway)
by [kyssta-exe](https://github.com/kyssta-exe).

Players talk to the bot in public chat — "what's north of spawn?", "write a
sign for me" — and it answers there, with the same agent, models and tools you
have in the terminal. Each player gets a session of their own, so a
conversation in the game is a conversation in the agent's history.

- **No Node, no bridge process, no port.** The cell speaks the game's protocol
  itself (Java protocol 763, Minecraft 1.20.1), so it joins directly.
- **One session per player**: `minecraft-<player>`.
- **Answers fit the game.** Chat lines hold 256 characters: a longer answer is
  split with `[1/N]` markers, and markdown and colour codes are flattened to
  plain text before it is said.
- **Whispers when you want them**: `private_replies` answers with `/tell`
  instead of speaking in public.
- **Who may talk**: `allowed_users`, or `allow_all` to let everyone in.
- **A trigger when chat is busy**: `prefix` (say `@bot`) is stripped before
  the agent sees the line; `require_prefix` makes it mandatory.
- **It looks alive**: the bot turns in place every few seconds, and rejoins
  when the server drops it.

## Get it

Tell the agent "add the minecraft cell", or run `nodecode add nc://minecraft`.

## Set it up

The bot joins in **offline mode** (any unused name), so it works on a server
with `online-mode=false` — that is what the original defaulted to. Give it a
`channels.minecraft` section (or use `/settings`):

```jsonc
{
  "channels": {
    "minecraft": {
      "server": "play.example.net",     // or an address on your network
      "port": 25565,
      "username": "NodecodeBot",
      "allowed_users": ["Steve", "Alex"] // or "allow_all": true
    }
  }
}
```

The cell refuses to start until it has a server and knows who may talk to it.
`allow_all: true` lets any player through — they run with your machine's
authority, so name people when you can.

| Member | Default | What it does |
| --- | --- | --- |
| `server` | — | the server's hostname or address |
| `port` | `25565` | the server's port |
| `username` | `NodecodeBot` | the name the bot joins under |
| `allowed_users` | unset | the player names who may talk |
| `allow_all` | `false` | let every player through |
| `prefix` | `` (none) | a trigger a line may open with, stripped before the agent sees it |
| `require_prefix` | `false` | only answer a line that opens with `prefix` |
| `private_replies` | `false` | whisper answers with `/tell` |
| `reply_length` | `256` | the chat line limit; a longer answer is split |
| `idle_seconds` | `8` | how often the bot turns in place; `0` stands still |
| `reconnect_seconds` | `10` | how long to wait before joining again |
| `timeout_seconds` | `10` | how long dialling may take |

## Use it

Say something in the server's chat. The bot answers in the chat; everyone
sees it unless `private_replies` is on.

| What you type | What happens |
| --- | --- |
| `hi` | the agent answers in public chat |
| `@bot what is this place?` | with `prefix` set to `@bot`, the same, triggered on purpose |
| `/help` | a line starting with a slash is passed to the server, not the agent |

In the terminal:

- `/minecraft` — where the bot is, whether it is connected, how many players
  it has seen, how many asks are owed an answer.
- `/minecraft say TEXT` — post a line in the game's chat yourself.

## What it does not do

- **Online mode.** A Microsoft-authenticated account needs a signed-in session
  this cell does not hold; offline-mode servers are what it joins.
- **Walk about.** The bot turns on the spot; the original's wandering and
  jumping need the world's physics.
- **Other game versions.** The packet ids are one table for protocol 763
  (Minecraft 1.20.1); a server of another version refuses the login, and the
  cell says so in its notice.
