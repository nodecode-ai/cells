# nodecode-speech

AllModels Speech: one key for text-to-speech, speech-to-text and voice search -- the agent says a sentence into an audio file, reads an audio file back as text, and finds the voice before it speaks. A Nodecode rewrite of [allmodels-io/hermes-speech](https://github.com/allmodels-io/hermes-speech) by [allmodels-io](https://github.com/allmodels-io).

## Get it

Tell the agent "add the speech cell", or run

```sh
nodecode add nc://speech
```

## Set it up

Create an AllModels API key at <https://allmodels.io>, then either paste it into the cell's `api_key` in `/settings`, or export it:

```sh
export ALLMODELS_API_KEY=...
```

Pick a voice with `allmodels_speech_voices` (or `/speech voices British female narrator`) and keep its id in `tts_voice` and the provider that serves it in `tts_voice_provider` -- the two travel together. A call can name a voice of its own instead.

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the key, written inline as a secret |
| `api_key_env` | `ALLMODELS_API_KEY` | the variable read when `api_key` is empty |
| `tts_model` | `fish/s2-1-pro` | the text-to-speech model every call uses unless it names one |
| `tts_voice` | (unset) | the voice id every call speaks in, when it names none |
| `tts_voice_provider` | (unset) | the provider that serves `tts_voice` |
| `tts_speed` | (unset) | the pace, 0.25 to 4.0, as a number |
| `tts_format` | `mp3` | `mp3`, `opus`, `aac`, `flac`, `wav` or `pcm` |
| `stt_model` | `soniox/stt-async-v5` | the speech-to-text model every transcription uses unless it names one |
| `stt_language` | (unset) | the language recordings are mostly in; unset lets the model detect it |
| `stt_prompt` | (unset) | names and jargon the recordings are likely to contain |
| `api_base_url` | `https://api.allmodels.io` | AllModels' API root; the audio endpoints are its `/oai` path |
| `directory` | `speech/` | where the synthesized audio is kept, under the home |

Without a key the tools still answer, with the sentence that says which setting to fill, and nothing is billed. Voice search needs no key at all.

## Use it

Ask for speech: "say this out loud and give me the file", "read this voice note back to me", "find a warm British voice". The model calls:

| Tool | What it does |
|---|---|
| `allmodels_speak(text, voice?, voice_provider?, model?, speed?, format?)` | one utterance, saved as an audio file in the speech folder; the answer names the path |
| `allmodels_transcribe(path, model?, language?, prompt?)` | one audio file on this machine, as the words it says |
| `allmodels_speech_voices(query, model?, provider?, language?, limit?)` | the public AllModels voice catalogue, searched in words; no key, nothing billed |

The `/speech` command answers what the cell is set to (`/speech`), the account's credit (`/speech balance`), and the same voice search the tool runs (`/speech voices <words>`). `(help :speech)` is the manual topic the model reads.

The audio endpoints are AllModels' OpenAI-compatible surface -- `POST /oai/v1/audio/speech` and `POST /oai/v1/audio/transcriptions` -- so the same key that serves speech serves the models next door. Every call that reaches AllModels is billed to the account the key belongs to.

## Credit

This cell is a rewrite of allmodels-io's [hermes-speech](https://github.com/allmodels-io/hermes-speech); see `NOTICE`. MIT, see `LICENSE`.
