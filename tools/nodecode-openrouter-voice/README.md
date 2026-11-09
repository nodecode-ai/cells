# nodecode-openrouter-voice

OpenRouter voice: transcribe an audio file to text, and speak text as an mp3. A Nodecode rewrite of [esketids/hermes-openrouter-voice](https://github.com/esketids/hermes-openrouter-voice) by [esketids](https://github.com/esketids).

The model gets two tools. `transcribe_audio` posts an audio file to
OpenRouter's `/audio/transcriptions` and answers the words; `speak_text`
posts text to `/audio/speech` and writes the mp3 where you asked for it.
Both bill the same OpenRouter key your chat already uses, so there is no
second account.

## Get it

Tell the agent "add the openrouter-voice cell", or run

```sh
nodecode add nc://openrouter-voice
```

## Set it up

One key, from <https://openrouter.ai/keys>. Either put it in the
environment as `OPENROUTER_API_KEY`, or name it in the section:

```jsonc
{
  "openrouter-voice": {
    "api_key_env": "OPENROUTER_API_KEY"   // or "api_key": "sk-or-..." inline
  }
}
```

Everything else has a default that works: `stt_model`
`openai/whisper-large-v3`, `tts_model` `deepgram/aura-2`, `voice`
`aura-2-thalia-en`. `/settings` and the Settings tab both edit it, and a
start with no key refuses by name rather than failing on the first call.

## Use it

Ask for it in words — "transcribe this recording", "read this out loud" —
and the model calls the tool. By hand:

```
/voice                       where it points, and what it uses
/voice ~/notes/call.wav      transcribe a file, answered in the shell
/voice say good morning      speech into output_dir, answered with the path
/voice voices                the default model's voice ids
```

A `.wav` that is not 16 kHz mono is rewritten to that shape in Lisp before
it is sent, because one of the catalog's transcription models answers 400
otherwise; other containers (the desktop's webm/opus, mp3) go through
`ffmpeg` when it is installed, and are sent as they are when it is not. A
voice belongs to a model — `aura-2-*` for the default, `af_heart` for
kokoro — and the fish-audio models publish none, so they are sent without
one. Speed is 0.25 to 4.0: whatever a model accepts natively is sent, the
rest is time-stretched locally with `ffmpeg`.

## Test

```sh
.github/test-cell.sh tools/nodecode-openrouter-voice
```

Offline: every request is a stubbed `nlk:http`, and ffmpeg is stubbed
absent.

## Gaps

The original is a pair of Hermes voice providers, so Hermes' desktop Voice
tab could select OpenRouter for dictation and for spoken replies. Nodecode
has no seam a cell adds a voice provider to, so here the two directions are
two tools the model calls, plus `/voice`; a shell that wants to play the
mp3 itself still does that step. Audio is posted whole, as the original
does: no streaming transcription, and a long file is one long request.
