# nodecode-fish-audio

Fish Audio: expressive text to speech, dictation, and a searchable voice library. A Nodecode rewrite of [100yenadmin/fish-audio-hermes](https://github.com/100yenadmin/fish-audio-hermes) by [100yenadmin](https://github.com/100yenadmin).

Three tools: `fish_speak` turns one text into an audio file (with [emotion cues], multiple speakers, and a speed), `fish_transcribe` turns a local audio or video file into text with speaker turns and an optional subtitle file, and `fish_voices` searches Fish Audio's voice library, shows one voice, lists your own, or clones one from samples you have.

## Get it

Tell the agent "add the fish-audio cell", or run:

```sh
nodecode add nc://fish-audio
```

## Set it up

Create an API key at <https://fish.audio/app/api-keys>, then either paste it into the cell's `api_key` in `/settings`, or export it:

```sh
export FISH_API_KEY=...
```

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the key, written inline as a secret |
| `api_key_env` | `FISH_API_KEY` | the variable read when `api_key` is empty |
| `api_base_url` | `https://api.fish.audio` | change only for an enterprise deployment; `https`, or `http` for a loopback stand-in |
| `tts_model` | `s2.1-pro` | `s2.1-pro`, `s2.1-pro-free`, `s2-pro` (Fish's mid tier), `drama-3` (expressive, multi-speaker) |
| `stt_model` | `transcribe-1-pro` | `transcribe-1-pro`, or the older `transcribe-1` |
| `voice` | (empty) | the voice every call uses unless it names one; empty is Fish Audio's default voice |
| `output_format` | `mp3` | `mp3` (plays anywhere), `ogg` (Ogg Opus, small enough for a voice note), `wav` |
| `directory` | `fish-audio/` | where the audio and subtitle files are kept, under the home |

Without a key every tool still answers -- with the sentence that says which setting to fill -- and nothing is billed.

## Use it

Ask for speech: "read this out loud, excited", "say this in a whisper", "two voices argue about the dishes". The model calls `fish_speak` with the text, and the emotion goes in square brackets: `[excited]`, `[whispering]`, `[laughing]`, `[sighing]`. The answer names the saved file; give that path to whoever asked: it plays in any audio player and can be attached to a message.

| `fish_speak` argument | Values | Notes |
|---|---|---|
| `text` | up to 3000 characters | what to say, in its own language |
| `voice` | a voice id | search one with `fish_voices` first; omitted, the section's voice, else Fish's default |
| `model` | `s2.1-pro`, `s2.1-pro-free`, `s2-pro`, `drama-3` | omit for the section's |
| `format` | `mp3`, `ogg`, `wav` | omit for the section's |
| `speed` | 0.5 to 2.0 | 1.0 unless sent |
| `language` | a BCP-47 tag | it fixes how numbers and dates are read, never the voice |
| `speakers` | 2 to 4 voice ids | a scene between voices; the S2 and drama models read it |

Ask for a transcript: "what does interview.mp3 say?", "transcribe this meeting and give me subtitles". `fish_transcribe` uploads the file (mp3, wav, ogg, webm, flac, m4a, aac; up to 50 MB), and with `srt` true it writes a `.srt` next to the audio, with Fish's own cue times.

Ask for a voice: "find a calm narrator that speaks Spanish", "which voices do I have?", "clone my voice from these three clips -- yes, I have permission". `fish_voices` answers a table of ids and titles; the id is what `fish_speak` takes.

The `/fish` command prints the settings and whether a key is set (which costs nothing), searches the library (`/fish voices rain`), or shows one voice (`/fish voice <id>`).

**Cost and privacy.** Every call is a paid Fish Audio API request on your account. What leaves the machine is the text or the audio file you named, the voice id, the model and the format, sent to `api_base_url` with your key -- nothing else from the conversation. A voice search sends only its words; a clone uploads only the sample files you named, and asks you to confirm the speaker gave permission first. The audio files and subtitles stay in the home.

## Tests

```sh
.github/test-cell.sh tools/nodecode-fish-audio
```

Every HTTP call is stubbed and the environment is a stub: the suite proves the request shape and its headers (the key in `authorization`, the model in `model`, the text, format and speed in the body), that the bytes that come back are saved and named in the answer, that a search lists ids and a `get` reads one voice, that a clone needs consent and uploads its samples as file parts, that a transcript writes an SRT with Fish's cue times, and that a missing key, a missing file, an unreadable kind and a 200 that is not audio all answer without a second billed call.

## Credits

A rewrite of 100yenadmin's Hermes Agent plugin, Apache-2.0, at commit `606afb54aa1a354cc8eac0b17f3da3a105ac55a7`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
