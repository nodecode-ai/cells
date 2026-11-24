# nodecode-live-voice

Live Voice: talk to your agent instead of typing, and hear its answers spoken aloud. A Nodecode rewrite of [Synero/hermes-live-voice](https://github.com/Synero/hermes-live-voice) by [Synero](https://github.com/Synero).

The original kept a full-duplex WebRTC call open between a desktop app and OpenAI's live model: a
mic button in the composer, an iMessage-style transcript, tool calls spoken into a chat. An image
has no WebRTC and no composer button, so the one call becomes two turns the model drives:
`voice_listen` records from this machine's microphone and answers the words, `voice_speak` writes
an answer as speech and plays it here, and `/voice` shows the transcript of both.

```text
> make the tests green, I'll say it
voice_listen(seconds: 10)            -> "make the tests green"
   ... the agent works ...
voice_speak(text: "All twelve tests pass now.")
saved ~/.nodecode/voice/said_20261108_101500_3f2a9c1b.mp3
4.1 KB, mp3, gpt-4o-mini-tts (alloy), 29 characters
played here: the operator hears it now
```

## Get it

Tell the agent "add the live-voice cell", or run:

```sh
nodecode add nc://live-voice
```

## Set it up

The cell needs an OpenAI API key and, on this machine, a recorder and a player:

- **key** — paste one into the cell's `api_key` in `/settings`, or export it:
  ```sh
  export OPENAI_API_KEY=...
  ```
- **a recorder**, one of `ffmpeg`, sox's `rec` or `arecord` (alsa-utils), found on `PATH`.
- **a player**, one of `afplay`, `mpv`, `ffplay`, sox's `play`, `paplay` or `aplay`, found on `PATH`.

`/voice` reports all four. When a device needs a flag of its own, name the whole command in
`recorder` or `player`: `%f` is the file and `%s` the seconds, so an interface other than the
default is one setting away — for example `recorder` = `ffmpeg -f avfoundation -i "2:0" -t %s %f`.

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the OpenAI key, written inline as a secret |
| `api_key_env` | `OPENAI_API_KEY` | the variable read when `api_key` is empty |
| `base_url` | `https://api.openai.com/v1` | change only for an OpenAI-compatible stand-in; `https` unless it is loopback |
| `transcribe_model` | `gpt-4o-mini-transcribe` | what `voice_listen` transcribes with (`gpt-4o-transcribe`, `whisper-1`) |
| `speech_model` | `gpt-4o-mini-tts` | what `voice_speak` speaks with (`tts-1`, `tts-1-hd`) |
| `voice` | `alloy` | the voice a call uses when it names none: alloy, ash, coral, echo, fable, nova, onyx, sage, shimmer, verse |
| `audio_format` | `mp3` | what a spoken answer is saved as: mp3, wav, opus, flac or aac (`aplay` and `paplay` want wav) |
| `max_seconds` | `60` | the longest one recording may be |
| `recorder` | (detected) | a recorder command of your own, `%f` the file, `%s` the seconds |
| `player` | (detected) | a player command of your own, `%f` the file |
| `directory` | `voice/` | where recordings, spoken files and `transcript.jsonl` are kept, under the home |

Without a key nothing is sent and nothing is billed: the tool answers the sentence that says
which setting to fill.

## Use it

| Tool | What it does |
|---|---|
| `voice_listen(seconds?, language?)` | records this machine's microphone (8 seconds unless set) and answers the words; `language` is an ISO-639-1 hint. One call per utterance |
| `voice_speak(text, voice?, instructions?, play?)` | has OpenAI say `text`, saves it in the folder, and plays it here; `instructions` sets tone, pace and accent for `gpt-4o-mini-tts`; `play` false saves only |

`/voice` reports the key, the microphone, the player, the folder and the last lines of the
transcript. `/voice say TEXT` speaks. `/voice hear [SECONDS]` records and answers the words.
`/voice transcript` lists what was heard and said. `(help :voice)` carries the primer the model
reads.

The call waits while the answer plays, so the agent is told to keep the spoken form to a sentence
or two and leave detail in the page: a hands-free reply is a summary, not the whole answer.

**Cost and privacy.** Every call is billed to your OpenAI account: transcription by the minute,
speech by the character. What leaves the machine is the recording itself for `voice_listen`, and
the spoken text and its instructions for `voice_speak`, sent to `base_url` with your key; nothing
else of the conversation does. The recordings, the spoken files and the transcript stay in the
folder, and a profile that loses the transcript loses a line of history, never the audio.

## Tests

```sh
.github/test-cell.sh tools/nodecode-live-voice
```

No device and no network: the endpoint answers handcrafted bytes, a stub writes the WAV a
recorder would write, and a machine with no player is a binding. The suite proves the request
shapes and their headers, the file that comes back and the player that runs on it, the multipart
transcription upload and the transcript line a recording leaves, and that a missing key, a missing
recorder and a length past `max_seconds` all refuse before anything is sent.

## Credits

A rewrite of Synero's Live Voice plugin for Hermes, MIT, at commit
`0578e0cc35022a9dcb469cb9fbfc4d8afc93fc65`: see [NOTICE](NOTICE) for what was rewritten and what
was left out, and [LICENSE](LICENSE) for the original licence.
