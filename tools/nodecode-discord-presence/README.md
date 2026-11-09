# nodecode-discord-presence

Discord Rich Presence: the agent's session, the tool it is running, the model and the turn's tokens, kept on your Discord profile while it works. A Nodecode rewrite of [DarRahman/hermes-discord-presence](https://github.com/DarRahman/hermes-discord-presence) by [DarRahman](https://github.com/DarRahman).

The card is a Discord profile card: the line under your name in every server you share. While the cell runs it says what this agent is doing — `Active Session`, `[Reading File] Active Session`, `[Running Terminal Command]` — with the model and the turn's token total beneath it, and the elapsed clock beside it. It is sent to the Discord desktop client on this machine over the client's own local socket and goes no further than the client does; the client is what shows it to other Discord users, so there are switches for exactly how much it says.

| Tool | What it does |
| --- | --- |
| `presence_show` | A line of your own on the card: first line, second line. It replaces the automatic lines until cleared. |
| `presence_clear` | Takes the card down, and keeps it down until something asks for it again. |
| `presence_status` | What a card says now, which socket answered, and why the last send failed. |

`/presence` is the same three things for a person: `/presence` reads the card back, `/presence off` takes it down, `/presence on` puts it back, `/presence a line to show` sets one.

## Get it

Tell the agent "add the discord-presence cell", or run `nodecode add nc://discord-presence`.

## Set it up

The card is shown under a **Discord application of your own**, whose name and artwork are what other users see beside it:

1. Open <https://discord.com/developers/applications>, **New Application**, name it what you like, and copy its **Application ID** (a long number).
2. Keep the **Discord desktop app** running and signed in on the same machine. The browser and the web client have no local socket, so the card needs the desktop app.
3. Set the id in `/settings` (or in the `discord-presence` section of `config.jsonc`):

```jsonc
"discord-presence": {
  "client_id": "123456789012345678"
}
```

An id is not a secret — it names an application, it authorizes nothing — so it is written in the section as it is. Without one, the cell refuses at start and says so on `/cells`.

Optional, and the point of the section:

| Setting | What it does |
| --- | --- |
| `session_title_mode` | `generic` (the default) says `Active Session`, `full` says the workspace folder's name, `hidden` says neither. |
| `show_activity` | Names the tool the agent is on. Off, the card says nothing about what is running. |
| `show_model`, `show_tokens` | The model's name and the turn's token total. Both on by default. |
| `model_label` | The model name to show, when the organism's own is not the one to publish. |
| `show_git_branch` | The workspace repository's branch, read from `.git/HEAD`. Off by default. |
| `stealth_mode` | A card with no lines but the large text: none of the work is published. |
| `large_image`, `large_text` | An art asset key uploaded to your application, and the words on it. |
| `update_interval`, `hold_seconds` | How often a changed card is re-sent, and how long a tool's activity line stands. |
| `socket_path` | An explicit socket path, when the client's own is somewhere unusual. |

## Use it

You do not call the tools; the model does. The card updates itself: the agent names each tool it runs, and the tick re-sends the card when what it says has changed — a card that says the same thing is not sent again. On the way down the cell clears the card, so no frozen status is left on your profile.

A person can call the same functions from eval, in the `discord-presence` package:

```lisp
(discord-presence:show-presence "Reviewing the release" :state "tag v1.3.0")
(discord-presence:presence-status)
(discord-presence:clear-presence)
```

`(help :discord-presence)` is the model's manual for the three verbs, kept while the cell runs.

### What leaves this machine

One frame per change, to a Unix socket that the Discord desktop client of the same user listens on (`discord-ipc-0` upward under the runtime directory, or the `socket_path` you set). Nothing is posted to Discord by the cell itself, no HTTP request is made, and no history is read: the card's words come from the workspace's folder name, the model the organism is set to, the running turn's own token counts and, if you turn it on, `.git/HEAD`. The client publishes whatever the card says to everyone who can see your profile, which is why `stealth_mode` and the `hide` switches exist.

Linux and macOS only for now: the client's named pipe on Windows is not opened here.

## License

MIT. The card, its lines and its switches are DarRahman's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
