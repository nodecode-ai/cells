# nodecode-talk

Talk: speak answers aloud and turn speech into text, through OpenAI's audio API. A Nodecode rewrite of [TheSmokeDev/hermes-talk](https://github.com/TheSmokeDev/hermes-talk) by [TheSmokeDev](https://github.com/TheSmokeDev).

The original plugin adds realtime, duplex voice to an agent: you speak, it answers out loud, and it runs its tools without leaving the conversation. A terminal agent has no microphone of its own, so this rewrite keeps the half of that plugin that works from any session — the two OpenAI audio lanes it carries — and exposes them as three tools:

| Tool | What it does |
| --- | --- |
| `talk_speak` | Say a text aloud: synthesized with OpenAI's speech model and played on this machine. |
| `talk_transcribe` | Turn an audio file already on this machine into text. |
| `talk_listen` | Record the microphone for a few seconds and turn what was said into text. |
| `talk_status` | This cell's own state: the models, the voice, the key's variable and the two audio CLIs. Read-only, no request. |

Ask "read me that summary out loud", "what does this recording say?", or "let me just say it" — the `/talk` command with no arguments records and transcribes, so a spoken question arrives as text the model can act on.

## Get it

Tell the agent "add the talk cell", or run `nodecode add nc://talk`.

## Set it up

An OpenAI API key, in the environment variable the section's `api_key_env` names (`OPENAI_API_KEY` unless you say otherwise). Nothing else is required: the cell finds a player and a recorder on its own.

```jsonc
"talk": {
  "api_key_env": "OPENAI_API_KEY",
  "voice": "cedar",
  "player": "",          // empty: afplay, paplay, aplay, ffplay or mpv, first found
  "record_command": ""   // empty: arecord, sox or ffmpeg, first found
}
```

The player and the recorder are CLIs on your machine. `record_command` is a command line where `%f` is the file to write and `%s` the seconds to record, for a recorder this cell does not know.

## Use it

- "say that out loud" — `talk_speak`.
- "transcribe notes.m4a" — `talk_transcribe`, on any audio file the API accepts.
- `/talk hello there` says it; `/talk` alone records and transcribes; `/talk status` reports the lane.
- `talk_speak` takes `speed` (0.25 to 4.0) and refuses a voice outside the ten the section offers, before it asks the API.

Audio and text go to OpenAI and nowhere else. Nothing is stored beyond the audio file each `talk_speak` writes under Nodecode's home (`talk/`), which the answer names.

## What the rewrite leaves out

The original's realtime call itself — one long-lived duplex session over WebRTC, with barge-in, endpointing, live tool calls and the Discord, dashboard and desktop surfaces — is a runtime of its own, not a tool. This cell speaks and transcribes one request at a time. See NOTICE.
