# nodecode-rocketchat

Rocket.Chat: ten tools over your own Rocket.Chat server's REST API — list rooms, read history and threads, search, post, upload files, DM and delegate to another bot. A Nodecode rewrite of [HalfbitStudio/hermes-plugin-rocketchat](https://github.com/HalfbitStudio/hermes-plugin-rocketchat) by [HalfbitStudio](https://github.com/HalfbitStudio).

The tools are the original plugin's, with the same names and arguments:

| Tool | What it does |
| --- | --- |
| `rocketchat_list_channels` | The channels and private groups the bot is a member of, with each room's id, name, type, topic and member count. |
| `rocketchat_get_history` | Recent messages of one room, newest first, pageable by offset or by an ISO-8601 window. |
| `rocketchat_search_messages` | A phrase search inside one room. |
| `rocketchat_get_thread` | One thread: its parent message and the replies, oldest first. |
| `rocketchat_get_permalink` | The web permalink of one message. |
| `rocketchat_create_channel` | Create a public channel or a private group (write). |
| `rocketchat_post` | Post a message to a room, by id or by name (write). |
| `rocketchat_send_file` | Upload a local file into a room, with a caption (write + upload). |
| `rocketchat_dm` | Open a direct message with a person, and optionally send them a message (write). |
| `rocketchat_delegate` | Hand one task to another Rocket.Chat bot in a marked envelope, so its reply ends there instead of starting a loop (write). |

Ask "what did the ops room say this morning?", "search general for the deploy notes", "post a summary to #general", "DM Ada the release link" — the model finds the room with `rocketchat_list_channels` and works from its id. Every message a read tool answers is marked `content_trust: untrusted`: it is what other people typed, so the model reads it as data and never as instructions.

## Get it

Tell the agent "add the rocketchat cell", or run `nodecode add nc://rocketchat`.

## Set it up

The cell talks to the server you already run, over its REST API v1. It needs three things:

1. **The server's base URL**, e.g. `https://chat.example.com`.
2. **A personal access token.** In Rocket.Chat: your **Profile** → **Personal Access Tokens** → **Create**. Create it with *Ignore Two Factor* checked, so unattended calls keep working on a workspace with 2FA.
3. **The token's own user id.** The same page shows the user `_id` next to the token; a bot account's is what belongs here.

Then, in `/settings` (or in the `rocketchat` section of `config.jsonc`):

```jsonc
"rocketchat": {
  "url": "https://chat.example.com",
  "token_env": "ROCKETCHAT_TOKEN",   // or a file: "token_file": "~/.config/rocketchat-token"
  "user_id": "the-bot-users-id"
}
```

A token is named, never written into the config: `token_env` holds the name of the environment variable that carries it (export `ROCKETCHAT_TOKEN` in the shell that starts Nodecode), and `token_file` a file only you can read. Set exactly one.

A plain `http://` URL is refused unless you set `allow_insecure_http` to true; use that only on a trusted local network.

Optional, and off by default:

```jsonc
"rocketchat": {
  "write_tools": true,                        // create, post, upload, DM, delegate
  "file_uploads": true,                       // send_file may read local files (write_tools too)
  "upload_roots": ["/srv/reports"],           // and only from these absolute directories
  "max_file_bytes": 104857600,                // 100 MiB
  "allowed_rooms": ["ROOMID1", "ROOMID2"],   // empty: every room the token can reach
  "timeout": 30
}
```

The read tools work as soon as the section has a url, a token and a user id; the five write tools answer a refusal naming `write_tools` until it is on. `allowed_rooms` is the one allowlist both halves respect: a room id outside it is refused before any request goes out.

## Use it

From the session, just ask. From eval, the same ten verbs are functions in the `rocketchat` package:

```lisp
(rocketchat:list-channels)                      ; every room the bot is in
(rocketchat:get-history "ROOMID")               ; its recent messages
(rocketchat:post-message "ROOMID" :message "on it")
```

`(help :rocketchat)` answers the same primer the model reads while the cell runs.
