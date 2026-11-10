# nodecode-glasses

Smart glasses: a wearer's words and camera in, short replies out. A Nodecode rewrite of [100yenadmin/glasses-platform](https://github.com/100yenadmin/glasses-platform) by [100yenadmin](https://github.com/100yenadmin).

A pair of glasses carries a token. It says something, the agent answers on
the lens in one or two short lines, it asks "what is this" and the camera
takes a picture, it shares a picture it already took, and it receives what
the agent pushes without being asked: a reminder, a scheduled job's words.

## Get it

Tell the agent "add the glasses cell", or run `nodecode add nc://glasses`.

## Set it up

Give each pair a token in `channels.glasses` — one `token` for a single
wearer (`owner:default`), or `devices` for several, in the original's
`user:device=token` shape:

```jsonc
{ "channels": { "glasses": {
  "devices": "ada:frames-1=first-token, ada:frames-2=second-token",
  "session": "glasses",
  "auto_capture": "keywords" } } }
```

A token is a key: whoever holds it has a chat with your agent.

## Use it

The device talks to four verbs on the gateway's own port, under
`path_prefix` and its token:

| Verb | What it is |
| --- | --- |
| `GET /glasses/<token>/hello` | pairs the device and echoes the wearer it is |
| `POST /glasses/<token>/say` | `{"text": "..."}`, and `{"image": "<base64 jpeg>"}` when the glasses already took a picture; answers `{"id": ...}` |
| `GET /glasses/<token>/poll` | the frames the agent has said since the last poll: `response` for an answer, `push` for something unasked |
| `GET /glasses/<token>/photo` | a capture request the device has not answered, or `idle` |
| `POST /glasses/<token>/photo` | `{"request": "...", "data": "<base64>"}` answers it |

A visual question ("what is this", "read this", "the one on the left") makes
`/say` ask for a photo first while `auto_capture` is `keywords`; set it to
`off` to capture only through the `glasses_capture_photo` tool, which saves
the photo as a file the answer can name.

The gateway refuses any Host but loopback, so put an HTTPS reverse proxy in
front of it (or `nodecode link`) before a device off this machine reaches
it.

From the shell: `/glasses` shows the endpoint and the pairs, `/glasses say
TEXT` sends one query down the same path a wearer's words take, and
`/glasses photo` takes a picture from the last pair that spoke.
