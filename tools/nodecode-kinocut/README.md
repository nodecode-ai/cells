# nodecode-kinocut

Kinocut: local video editing on the ffmpeg you already have, with a receipt for every render. A Nodecode rewrite of [KyaniteLabs/kinocut](https://github.com/KyaniteLabs/kinocut) by [KyaniteLabs](https://github.com/KyaniteLabs).

An agent should never hand-write an ffmpeg command. Five tools cover the work most clips need -- inspect, cut, reframe, caption, check -- and each one validates its request against the file's own probe before anything runs, writes a **new** file into one render folder, and answers with the file's real length and the exact argv that made it. The source is never touched and no render is ever overwritten. Nothing leaves the machine.

| Tool | What it does |
|---|---|
| `kino_probe(path)` | duration, frame size and rate, codecs, audio streams, size; reads only |
| `kino_trim(path, start, end\|duration, mode)` | one clip; `accurate` cuts on the exact frame, `copy` keeps the source codecs and lands on a keyframe |
| `kino_reformat(path, aspect, fit)` | 9:16, 1:1, 4:5 or 16:9; `crop` fills the frame, `pad` adds bars |
| `kino_captions(path, subtitles)` | burn a real `.srt` or `.ass` into the picture |
| `kino_check(path, min_seconds?)` | a gate: duration, black frames, silence; a score out of 100, passing at the section's `pass_score` |

`/kino` is the bench report: which `ffmpeg` and `ffprobe` this profile runs, where renders land, and the last receipts. `/kino <id or words>` shows one receipt.

## Get it

Tell the agent "add the kinocut cell", or run:

```sh
nodecode add nc://kinocut
```

## Set it up

You need `ffmpeg` and `ffprobe` on your `PATH` -- the cell never installs them and never downloads anything. Then name the folder renders land in; it is the one setting without a default:

```jsonc
{
  "kinocut": {
    "output_dir": "~/Videos/cuts/"
  }
}
```

Until `output_dir` is set, every render refuses and says so; `kino_probe` works without it. The rest of the section, with its defaults:

| Setting | Default | What it does |
|---|---|---|
| `ffmpeg` | `ffmpeg` | the binary, or its full path |
| `ffprobe` | `ffprobe` | the binary, or its full path |
| `output_dir` | (required) | where renders and `receipts.jsonl` are kept; `~/` expanded |
| `crf` | `20` | x264 quality: lower is better and bigger |
| `preset` | `medium` | x264 speed against size |
| `font_name` | `DejaVu Sans` | the caption font, as fontconfig knows it |
| `font_size` | `20` | caption size in points |
| `caption_margin` | `40` | how far captions sit above the bottom edge |
| `max_black_percent` | `50` | a check fails when more of the clip than this is black |
| `pass_score` | `80` | the score `kino_check` must reach |
| `timeout_seconds` | `900` | one render's ceiling |

## Use it

Ask in plain words: "cut the first eight seconds", "make this a 9:16 short", "burn `talk.srt` into it", "is this ready to post?". The model calls the tools; for example:

```
kino_trim(path="/home/you/Videos/talk.mp4", start=0, duration=8)
  saved /home/you/Videos/cuts/kino_talk-trim_20261108-101500_3f2a9c1b7d.mp4
  trim: 8.00 s from 0.00 s of a 412.30 s source, accurate mode
  8.00 s, 1920x1080, 29.97 fps
  18.4 MB
  argv: ffmpeg -nostdin -y -hide_banner -loglevel error -ss 0 -i ... -t 8 ...
```

The order that works: probe, then one edit, then check, then **look at the frames yourself**. A receipt measures; it does not see. `(help :kino)` carries that guidance in the model's own words.

Renders re-encode to H.264/AAC MP4 (`-pix_fmt yuv420p`, `+faststart`, so they play anywhere and seek instantly), except a `copy`-mode trim, which keeps the source's codecs and container. Upscaling is allowed and never sharper than the source: a 720p clip reformatted to 1080x1920 is scaled up.

**Cost and privacy.** Every call is a local process on your own files; nothing is uploaded, no model is called, and the receipts stay in `output_dir`.

## Tests

```sh
.github/test-cell.sh tools/nodecode-kinocut
```

Both programs are stubbed, so the suite needs neither `ffmpeg` nor a network: it proves the argv each tool builds (the accurate and copy cuts, the centred 9:16 crop, the padded square, the libass burn), that the answer reports the length ffprobe reads back rather than the one requested, that a receipt is written with its source and argv, and that a missing render folder, a clip past the end of the source and a file that is not a subtitle all answer without a render.

## Credits

A rewrite of KyaniteLabs's Kinocut, Apache-2.0, at commit `31ddab08a99e09fb50e0855c61e1627d02050ac1`: see [NOTICE](NOTICE) for what was rewritten and what was left out, and [LICENSE](LICENSE) for the original licence.
