# nodecode-hermes-band

Hermes Band: compose and render original music on your own machine — an arrangement is data (tempo, key, chords, parts) rendered to an exact-length wav and mp3. A Nodecode rewrite of [RegeneratusLabs/hermes-band](https://github.com/RegeneratusLabs/hermes-band) by [RegeneratusLabs](https://github.com/RegeneratusLabs).

Four tools, the plugin's own names and arguments: `band_render`, `band_validate_score`, `band_list_presets`, `band_doctor`. No music API, no model, no key, and no per-track cost: a score becomes MIDI, MIDI becomes stems through the fluidsynth you already have, the stems are mixed with per-part pan and level, a synthesised room is added, and ffmpeg masters the result to −14 LUFS and an mp3. The same score always renders the same audio.

## Get it

Tell the agent "add the hermes-band cell", or run:

```sh
nodecode add nc://hermes-band
```

## Set it up

You need three things on the machine, and `band_doctor` tells you which is missing:

```sh
apt install fluidsynth fluid-soundfont-gm ffmpeg     # Debian/Ubuntu
brew install fluid-synth ffmpeg                       # macOS, plus a GM .sf2
```

A General MIDI soundfont (`.sf2`) is what makes the instruments sound like instruments. `band_doctor` looks in the usual system locations; if yours lives elsewhere, set `soundfont`.

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `output_dir` | `hermes-band-out/` | where renders go, one subfolder per track |
| `soundfont` | (found) | a General MIDI `.sf2`; empty searches the system locations |
| `fluidsynth` | `fluidsynth` | the binary, as it is on PATH |
| `ffmpeg` | `ffmpeg` | the binary, as it is on PATH |
| `sample_rate` | `44100` | the render's sample rate |
| `lufs` | `-14` | the loudness target a render without its own uses |

## Use it

Ask for music, or write the arrangement yourself:

```python
{"name": "bed", "bpm": 90, "key": "D", "mode": "major", "bars": 12,
 "progression": ["I", "V", "vi", "IV"],
 "parts": [{"inst": "steel", "role": "fingerpick", "pattern": "folk",
            "register": [55, 79], "voices": 4},
           {"inst": "fingerbass", "role": "bass_root5", "start_bar": 2,
            "register": [40, 52], "gain": 0.9},
           {"inst": "drums", "role": "drums", "start_bar": 4, "pattern": "folk_soft"}]}
```

That is a twelve-bar folk bed: 32.0 seconds at 90 BPM, exact, one stem per part, mastered to −14 LUFS.

| Tool | What it answers |
|---|---|
| `band_doctor` | whether this host can render: fluidsynth, the soundfont, ffmpeg, the output folder |
| `band_list_presets` | the four bundled arrangements and the five rooms |
| `band_validate_score` | a one-line summary of a score, or every reason it cannot render |
| `band_render` | the wav and mp3 paths, the duration, the peak, and the stems |

Or pass a preset name instead of a score: `bed` (warm fingerpicked folk, room), `band` (melody, walking bass, kit, hall), `pulse` (minor electronic bed, close), `funk` (E dorian, close). `/hermes-band` lists them with their numbers, and `(help :hermes-band)` carries the whole vocabulary: roles, fingering patterns, drum patterns, rooms and roman-numeral chords.

What a render leaves on disk:

```
hermes-band-out/bed/
  bed.wav                 the mastered track
  bed.mp3                 192 kbps
  bed-00-steel.mid        the part, and the stem fluidsynth played
  bed-00-steel.wav
  ...
```

Nothing leaves the machine and nothing is billed: the score goes to fluidsynth and ffmpeg on this host, and the files stay in the output folder. Long renders run in the foreground of the tool call and take seconds to tens of seconds.

## Tests

```sh
.github/test-cell.sh tools/nodecode-hermes-band
```

No program runs: the runner seam writes the wav a real fluidsynth would and an empty file where ffmpeg's output goes, and `*FIND-PROGRAM*` answers a path that exists. The suite proves the four tools answer, that a bad part is refused by name, that a render writes a wav, an mp3 and one MIDI file per part with the length it really has, and that a host without a soundfont is told which setting to fill.

## Credits

A rewrite of RegeneratusLabs's Hermes Agent plugin, MIT, at commit `c9dec797d9bf29fd5e09be538952c7db9bace8c8`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
