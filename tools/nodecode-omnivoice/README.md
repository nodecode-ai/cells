# nodecode-omnivoice

OmniVoice speech on this machine: a text becomes an audio file, by voice design or a reference clip. A Nodecode rewrite of [ThaungThanHan/hermes-omnivoice](https://github.com/ThaungThanHan/hermes-omnivoice) by [ThaungThanHan](https://github.com/ThaungThanHan).

One tool, `omnivoice_say`, speaks a sentence with [k2-fsa/OmniVoice](https://github.com/k2-fsa/OmniVoice) -- Xiaomi's open multilingual zero-shot TTS model -- and answers the file it wrote: voice design from a description, voice cloning from a short clip, a language, a pace, and the container you want. The speech is generated locally; nothing but the text leaves the machine.

## Get it

Tell the agent "add the omnivoice cell", or run:

```sh
nodecode add nc://omnivoice
```

## Set it up

This cell speaks through the runtime the original plugin installs, so install that once in its own environment:

```sh
pip install "hermes-omnivoice[omnivoice] @ git+https://github.com/ThaungThanHan/hermes-omnivoice.git"
```

It brings torch and the model loader; `omnivoice-hermes-tts` is the command the cell runs. The first call downloads the weights once and is slow.

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `command` | `omnivoice-hermes-tts` | the runtime's command, or the path to it |
| `model` | `k2-fsa/OmniVoice` | the model the runtime loads |
| `device` | `auto` | `auto`, `cpu`, `cuda` or `mps` |
| `dtype` | `float16` | `float16`, `float32` or `bfloat16` |
| `voice` | `auto` | the voice a call names none: `auto`, a described voice, or `ref:<a clip>` |
| `instruct` | (unset) | a voice description used when `voice` is empty |
| `ref_audio`, `ref_text` | (unset) | a clip to clone, and what it says |
| `language` | (unset) | a code such as `th` or `my`; empty, the text is read |
| `speed` | `1.0` | the pace |
| `num_step` | `32` | diffusion steps: more is slower and finer |
| `guidance_scale` | `2.0` | how closely the speech follows the text and the description |
| `format` | `wav` | `wav`, `mp3`, `flac` or `ogg`; MP3 needs ffmpeg |
| `directory` | `speech/` | where the files are kept, under the home |
| `player` | `auto` | `auto` picks `ffplay`, `afplay`, `paplay` or `aplay`; or name one with its flags |

These are the variables the runtime itself documents (`OMNIVOICE_MODEL`, `OMNIVOICE_LANGUAGE`, `OMNIVOICE_REF_AUDIO` and the rest), so anything the plugin's own config file accepted still works -- the cell hands them over as the section's values.

## Use it

Ask for it: "say this out loud", "read this paragraph as an audiobook clip", "speak this in Thai", "use a female British voice, low pitch", "clone this clip". The model calls the tool with:

| Argument | Values | Notes |
|---|---|---|
| `text` | up to 5000 characters | what to say, in any language OmniVoice knows |
| `voice` | `auto`, `instruct:<a described voice>`, `ref:<a clip>`, `<a described voice>` | omitted, the section's `voice` |
| `speed` | a number | 1.0 normal, 0.8 slower, 1.2 faster |
| `format` | `wav`, `mp3`, `flac`, `ogg` | MP3 falls back to WAV when ffmpeg is missing |
| `play` | true/false | hand the file to a player here as it is written; false for a file kept for later |

The answer names the file, the voice it resolved to, the model, its size and -- for a WAV -- its playing time, then whether it is playing:

```
wrote ~/.nodecode/speech/speech_hello-from-omnivoice_20261109-041500_3f2a9c1b7d4e.wav
said: "Hello from OmniVoice."
1.00 s, 86.1 KB, wav, voice designed: female, british accent, model k2-fsa/OmniVoice
playing with ffplay (it does not wait for the end)
```

Give that path to the operator: the file plays in any audio player and can be copied into a project. `/say <text>` speaks from the command line, and `/say` alone shows what the cell runs on and the files it has made; `(help :omnivoice)` carries the voice guidance.

**Voice cloning.** `ref:/a/clip.wav`, or any existing audio path, clones that voice. A clean clip of one speaker, without music, with its transcript in `ref_text`, gives the best result; without a transcript the runtime transcribes the clip first, which loads an ASR model and takes longer. Only clone voices you have the right to use.

**What leaves the machine.** The text and the settings go to a process on this machine. The only network traffic is the runtime's own first download of the model's weights (Hugging Face, `k2-fsa/OmniVoice`); the files stay in the speech folder.

## Tests

```sh
.github/test-cell.sh tools/nodecode-omnivoice
```

Nothing in the suite executes anything: the runtime, the player and the PATH are stubs. It proves the voice table the plugin's own code defines, the command line and the `OMNIVOICE_*` environment a call builds, the text file holding the words and gone afterwards, the WAV found when an MP3 was asked for and ffmpeg was missing, the length read out of the header, the player handed the file, `/say` speaking and listing, and four refusals: no runtime on the PATH, a failing runtime, one that wrote no file, and a section whose format, speed or guidance is wrong.

## Credits

A rewrite of ThaungThanHan's Hermes Agent plugin, Apache-2.0, at commit `21d2900c392247dc52d706fee956f08dbfc3c5ea`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
