# nodecode-sound-effects

ElevenLabs sound effects: a text description becomes an audio file the model saves and hands you. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

One tool, `elevenlabs_sound_effect`, turns "glass shattering on a concrete floor" or "90s hip-hop drum loop, 90 BPM, loopable" into a real audio file, up to 30 seconds, and keeps a library of every sound this profile has made.

## Get it

Tell the agent "add the sound-effects cell", or run:

```sh
nodecode add nc://sound-effects
```

## Set it up

Create an API key at <https://elevenlabs.io/app/settings/api-keys> with **Sound Effects** enabled (it is separate from Text to Speech), then either paste it into the cell's `api_key` in `/settings`, or export it:

```sh
export ELEVENLABS_API_KEY=...
```

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `api_key` | (unset) | the key, written inline as a secret |
| `api_key_env` | `ELEVENLABS_API_KEY` | the variable read when `api_key` is empty |
| `sound_model` | `eleven_text_to_sound_v2` | `eleven_text_to_sound_v2` or `eleven_text_to_sound_v3` |
| `output_format` | `mp3_44100_128` | `mp3_44100_128`, `mp3_44100_192`, `opus_48000_128`, `opus_48000_192`, `wav_44100`, `wav_48000` (192 kbps MP3 is Creator plans and up, 44.1 kHz WAV Pro and up) |
| `api_base_url` | `https://api.elevenlabs.io` | change only for a data-residency endpoint; `https` only, or `http` for a localhost stand-in |
| `directory` | `sounds/` | where the audio and the library are kept, under the home |

Without a key the tool still answers -- with the sentence that says which setting to fill -- and nothing is billed.

## Use it

Ask for a sound: "the sound of a heavy wooden door creaking open in a stone hall", "a short retro coin pickup", "rain on a tin roof, loopable". The model calls the tool with:

| Argument | Values | Notes |
|---|---|---|
| `prompt` | up to 450 characters | what the sound is: source, action, material, space |
| `duration_seconds` | 0.5 to 30 | pass it whenever a length is named; unset, ElevenLabs picks one at a flat, higher rate |
| `loop` | true/false | ambience, music beds, engine hums |
| `prompt_influence` | 0 to 1 | higher follows the prompt more literally (0.3 unless sent) |
| `model` | v2 or v3 | omit for the section's model |

The answer names the saved file, its real playing time (read out of the bytes, not from the request: a 1 s loop can come back 1.5 s long), the format, the size and the characters ElevenLabs billed. Give the path to the operator: the file plays in any audio player and can be copied into a project or a game engine.

```
saved ~/.nodecode/sounds/sfx_heavy-wooden-door_20261108_101500_3f2a9c1b7d4e.mp3
prompt: heavy wooden door creaking open in a stone hall
0.26 s long, loop, eleven_text_to_sound_v2, mp3_44100_128, 4.1 KB, 47 characters billed
id 3f2a9c1b7d4e -- /sfx lists this profile's sounds
```

`/sfx` lists the library (newest first, with per-container totals), and `/sfx <id or words>` shows one sound with its prompt, settings and file. `(help :sfx)` carries the prompt-writing guidance: one sound per call, concrete descriptions, sequences with "then", music with genre, tempo and key, and when to set `loop`.

Nothing is overwritten: every file is named with its description, a timestamp and its id, and every generation appends one line to `sounds/library.jsonl` beside the audio. Post-processing is local and free -- trim, normalise, convert or fix a loop seam with ffmpeg on the machine -- so a sound is generated once and edited after, not regenerated.

**Cost and privacy.** Every call is a paid ElevenLabs generation on your account (about 10 credits per second when a length is set, a flat amount per generation otherwise; loops can be billed on the request and returned longer). What leaves the machine is the description and the parameters above, sent to `api_base_url` with your key, and nothing else from the conversation. The library line never leaves the home.

## Tests

```sh
.github/test-cell.sh tools/nodecode-sound-effects
```

Every HTTP call is stubbed and the environment is a stub: the suite proves the request shape and its headers, that the bytes that come back are saved and their length read from the frames, header or granule, that `/sfx` lists and finds a sound, that the library survives a torn line, and that a missing key, a length in the prompt, an out-of-range value and a non-audio 200 all answer without a second billed call.

## Credits

A rewrite of NousResearch's Hermes Agent plugin, MIT, at commit `e7a218f26db88426ab3e7376f0e000b6e356f5e7`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
