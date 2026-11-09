# nodecode-deepgram-voice

Deepgram voice: text becomes a spoken audio file, a recording becomes text. A Nodecode rewrite of [jaysonsantos/hermes-deepgram-voice-plugin](https://github.com/jaysonsantos/hermes-deepgram-voice-plugin) by [jaysonsantos](https://github.com/jaysonsantos).

Two tools, `deepgram_speak` and `deepgram_transcribe`, plus `/say` to say one line out loud from the prompt. The plugin gave Hermes an Aura 2 text-to-speech provider and a Nova speech-to-text provider; this cell reaches the same two documented Deepgram REST endpoints, so a model gets them directly: speak a narration into a file it can hand you, or read a voice note, a meeting or a video's audio back as words.

## Get it

Tell the agent "add the deepgram-voice cell", or run:

```sh
nodecode add nc://deepgram-voice
```

## Set it up

Create an API key at <https://console.deepgram.com/>, then either paste it into the cell's `api_key` in `/settings`, or export it:

```sh
export DEEPGRAM_API_KEY=...
```

The section (`deepgram-voice`), as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the key, written inline as a secret |
| `api_key_env` | `DEEPGRAM_API_KEY` | the variable read when `api_key` is empty |
| `voice` | `aura-2-thalia-en` | the Aura 2 voice a speak call uses when it names none |
| `audio_format` | `mp3` | `mp3`, `wav`, `ogg`, `opus` or `flac` |
| `stt_model` | `nova-3` | the Listen model a transcription uses when it names none (`nova-3`, `nova-2`, `enhanced`, `base`) |
| `api_base_url` | `https://api.deepgram.com/v1` | change only for a proxy; `https`, or `http` for a localhost stand-in |
| `directory` | `voice/` | where spoken files are kept, under the home |

Without a key the tools still answer -- with the sentence that says which setting to fill -- and nothing is billed.

## Use it

Ask for speech: "read this summary out loud", "say 'we are live' in a deep voice". Ask for words: "transcribe this voice note", "what does this interview say". The model calls:

| Tool | Arguments | Notes |
|---|---|---|
| `deepgram_speak` | `text` (required), `voice`, `format`, `speed` | at most 2000 characters per call; speed 0.7 to 1.5; the answer's first line is the saved file |
| `deepgram_transcribe` | `file` (required), `model`, `language`, `diarize`, `smart_format` | the file is an absolute path, a `~/` path or one under the home; a named language is forced, omitted it is detected and reported |

```
saved ~/.nodecode/voice/voice_the-tide-comes-in-at-four_20261109_101500_3f2a9c1b7d4e.mp3
said: The tide comes in at four.
aura-2-thalia-en, mp3, 41.2 KB
```

Give that path to the operator: it plays in any audio player and can be moved into a project, a video or a game. Text is refused over the API's per-call size rather than truncated, so a long narration is spoken in a few files at sentence ends.

`/say` with no argument lists the voices, the formats and the folder; `/say <text>` speaks the line and answers the file's path. `(help :voice)` carries the same guidance in the manual.

**Cost and privacy.** Every call is billed to your Deepgram account, by the second of audio. What leaves the machine is the text to speak or the recording's audio, sent to `api_base_url` with the key -- nothing else from the conversation, no conversation history, no files beyond the one named. Spoken files stay in `directory`; a transcription's answer is text and no copy of the recording is uploaded anywhere else.

## Tests

```sh
.github/test-cell.sh tools/nodecode-deepgram-voice
```

Every HTTP call is stubbed and the environment is a stub: the suite proves the request a speak call makes (endpoint, `model`, `encoding`, `container`, `speed`, the Token header, the JSON body), that the bytes that come back are saved and named for their container, that each container asks for its own parameters, that a 200 that is not audio is refused rather than saved; that a transcription posts the recording's own bytes with its content type and the right Listen parameters, reads the transcript, the detected language and the duration back; and that a missing key, an oversize text, a speed out of range, a missing or silent recording and a refused call all answer in words without a second request.

## Credits

A rewrite of jaysonsantos's Hermes Agent plugin, MIT, at commit `2d31b885ec873ed6693edd7ad1a80f7023439ac0`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
