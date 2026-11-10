# nodecode-fivedive

5dive App Chat: the web and mobile chat of a 5dive box, carried to the agent through a local spool folder. A Nodecode rewrite of [5dive-ai/5dive-plugins](https://github.com/5dive-ai/5dive-plugins) by [5dive-ai](https://github.com/5dive-ai).

On a 5dive box the box's bridge receives every message typed in the [5dive](https://5dive.ai) app and leaves one JSON file per message in a spool folder. This cell is the other end of that spool: it reads the folder, hands each message to the agent in a session of that chat's own, and writes the finished answer back where the bridge picks it up and sends it to the app. No listener and no token: the folder is private to the agent's own user, so only that user (or root) can write a message into it.

## Get it

Tell the agent "add the fivedive cell", or run `nodecode add nc://fivedive`.

## Set it up

Point the cell at the spool with a `channels.fivedive` section (or use `/settings`):

```jsonc
{
  "channels": {
    "fivedive": {
      "spool_dir": "~/.nodecode/channels/5dive",
      "allowed_senders": ["5dive-app"]
    }
  }
}
```

`spool_dir` is required: the bridge creates the folder, with `inbox/` and `outbox/<chat>/` inside it. The cell refuses to start without it.

| Member | Default | What it does |
| --- | --- | --- |
| `spool_dir` | none | the folder the bridge reads and writes |
| `allowed_senders` | `["5dive-app"]` | the senders who may talk; the bridge writes as `5dive-app` |
| `allow_all` | `false` | let every sender in the spool through |
| `reply_max_words` | `70` | word cap on one answer; a longer one goes back to the agent to shorten (`0` turns it off) |
| `poll_seconds` | `1` | how often the spool is read |
| `max_length` | `16000` | the chat's per-message character limit |

## Use it

Message the agent from the 5dive app. Each chat is a session of its own (`fivedive-dashboard` for the app chat), so the next message there carries on where the last one stopped. Markdown renders in the app; a message that arrives as a voice note is marked as spoken to the model and answered by voice; a file the bridge names in a message is named in the prompt, for the agent to read with its own tools.

An answer over `reply_max_words` words is held back and the agent is asked for a shorter one - up to two rewrites, after which the answer goes as written. Only the last version reaches the chat.

From the shell, `/fivedive` shows the spool, the chat, who may talk and what is owed an answer; `/fivedive TEXT` writes TEXT to the chat now. The model has `fivedive_send {text, files, chat_id}` for the same thing, files included, so a scheduled turn can report back into the app chat.

## The spool

```
<spool>/inbox/<id>.json              written by the bridge, read here
  {"id": "...", "text": "...",
   "route": {"source": "dashboard", "chat_id": "dashboard", "voice": true},
   "image_path": "/abs/path/on/this/box",
   "received_at": "2026-10-30T23:00:00Z"}

<spool>/outbox/dashboard/<id>.json   written here, sent by the bridge
  {"source": "dashboard", "chat_id": "dashboard", "text": "...",
   "files": ["/abs/path"], "voice": true}
```

Both sides write a temp name first and rename it to `*.json`, so a reader never sees half a file. A message is removed from `inbox/` before it is handed on, so a restart answers nothing twice; a file that is not a valid message is removed too and logged, because leaving it would make every lap trip on it. A message naming no sender is admitted: it can only come from the folder the bridge owns.

## Not ported

- **The Hermes gateway itself** - the platform registration, the adapter class, the asyncio loop - because a cell submits through the in-process ingress and reads the session's answer.
- **Cron delivery through `FIVEDIVE_HOME_CHANNEL`.** A scheduled turn reports into the chat with `fivedive_send` instead.
- **Images attached to the turn.** The bridge names a path on the box; the prompt names it and the agent reads it, rather than the cell base64-encoding it into the turn.
- **The 5dive bridge** (`plugins/dashboard/server.ts`), which stays on the 5dive side of the spool.

The licence is the original repository's (Apache-2.0); see [LICENSE](LICENSE) and [NOTICE](NOTICE).
