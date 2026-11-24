# nodecode-soniox

Soniox voice: text becomes a spoken audio file, a recording becomes text. A Nodecode rewrite of [XimilalaXiang/hermes-soniox](https://github.com/XimilalaXiang/hermes-soniox) by [XimilalaXiang](https://github.com/XimilalaXiang).

Two tools, `soniox_speak` and `soniox_transcribe`, plus `/soniox` to say one line out loud from the prompt. The plugin gave Hermes a Soniox text-to-speech provider and a Soniox speech-to-text provider; Nodecode has no provider slot for a voice, so this cell reaches the same two documented Soniox APIs directly: speak a narration into a file it can hand you, or read a voice note, a meeting or a video's audio back as words.

## Get it

Tell the agent "add the soniox cell", or run:

```sh
nodecode add nc://soniox
```

## Set it up

Create an API key at <https://console.soniox.com/>, then either paste it into the cell's `api_key` in `/settings`, or export it:

```sh
export SONIOX_API_KEY=...
```

The section (`soniox`), as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the key, written inline as a secret |
| `api_key_env` | `SONIOX_API_KEY` | the variable read when `api_key` is empty |
| `model` | `tts-rt-v2` | the text-to-speech model a speak call uses |
| `voice` | `Adrian` | the voice a speak call uses when it names none; any voice id the account's catalogue serves works |
| `language` | `en` | the language speaking uses when a call names none |
| `audio_format` | `mp3` | `mp3`, `wav`, `flac`, `opus` or `ogg` |
| `stt_model` | `stt-async-v6` | the asynchronous model a transcription uses |
| `stt_language` | (unset) | the language recordings are mostly in, or several comma separated; unset lets Soniox detect it |
| `poll_timeout_seconds` | `300` | how long a transcription may take before the call gives up |
| `api_base_url` | `https://api.soniox.com/v1` | Soniox's API root; change it only for a proxy |
| `tts_url` | `https://tts-rt.soniox.com/tts` | Soniox's text-to-speech endpoint; change it only for a proxy |
| `directory` | `soniox/` | where spoken files are kept, under the home |

Without a key the tools still answer, with the sentence that says which setting to fill, and nothing is billed. Both roots must be `https` (or `http` on localhost), so the key never travels in clear.

## Use it

Ask for speech: "read this summary out loud", "say 'we are live' in French with Camille". Ask for words: "transcribe this voice note", "what does this interview say". The model calls:

| Tool | What it does |
|---|---|
| `soniox_speak(text, voice?, language?, format?, speed?)` | one utterance, saved as an audio file in the soniox folder; the answer names the path |
| `soniox_transcribe(file, model?, language?, prompt?)` | one audio file on this machine, as the words it says |

`/soniox <text>` says one line out loud from the prompt; `/soniox` alone says what the cell is set to, and lists the voices it ships as a starting point. `(help :soniox)` is the manual topic the model reads.

What leaves the machine is the text to speak, or the recording's audio, sent to Soniox with the key -- nothing else from the conversation. A transcription uploads the recording, polls until Soniox finishes, reads the words and then deletes the transcription and the uploaded file from Soniox. Soniox's own retention and billing terms apply to anything it receives, and every call is billed to the account the key belongs to.

Speeds run 0.7 to 1.3; a pace outside that is clamped, as the plugin clamped it. A text of more than 5000 characters is refused before the call, with the sentence that says how to split it.

## Credit

This cell is a rewrite of XimilalaXiang's [hermes-soniox](https://github.com/XimilalaXiang/hermes-soniox); see `NOTICE`. MIT, see `LICENSE`.
