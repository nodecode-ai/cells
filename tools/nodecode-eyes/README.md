# nodecode-eyes

Eyes: a measured note on every image, video or sound a tool just wrote. A Nodecode rewrite of [Agenziamaia/eyes](https://github.com/Agenziamaia/eyes) by [Agenziamaia](https://github.com/Agenziamaia).

When a tool result names an image, video or sound file written in the last
few minutes, a measured note is appended to that result before the model reads
it: how much of the frame is filled, how much moves, how bright, how loud,
where the silences are, and for a video a contact sheet to look at. A black
frame, an empty canvas, a frozen clip and a silent voice-over each get a
WARNING naming what is wrong, so the agent knows before it says done. A prompt
cannot do this: the model never sees pixels unless something measures them.

```
wrote /home/you/work/promo.mp4

[eyes] promo.mp4: 3.0 s, 640x360; frame filled 0% on the median frame; something moves in 0% of half-seconds; brightness 1/255; no sound track. Contact sheet to look at: /home/you/Pictures/eyes/promo-9bad36.png. WARNING: almost black, nearly empty frame, barely moves.
```

Perception, never a block: the hook answers the tool's own text whatever
happens, a file is measured once, at most `max_notes` a result, and every
ffprobe and ffmpeg call has a timeout. It needs `ffmpeg` and `ffprobe` on the
machine, and it never calls a model or the network.

## Get it

Tell the agent "add the eyes cell", or run:

```bash
nodecode add nc://eyes
```

## Set it up

One setting is required: `sheet_dir`, the folder the contact-sheet PNGs are
written to (it is made if it is missing). Everything else has a default:

```jsonc
{
  "eyes": {
    "sheet_dir": "~/Pictures/eyes",
    "ffmpeg": "ffmpeg",         // the binary, or its full path
    "ffprobe": "",               // empty: ffprobe, or ffprobe beside ffmpeg
    "fresh_seconds": 300,        // how recently a file must have been written
    "max_notes": 3,              // how many files one result may be measured for
    "timeout_seconds": 25,       // one ffprobe or ffmpeg call's ceiling
    "tools": ["eval"]            // whose results are read; [] or ["*"] reads every tool's
  }
}
```

Set it in `/settings`, or tell the agent "set eyes up". Without `ffmpeg` and
`ffprobe` it says so and stays quiet.

## Use it

Nothing to call: a tool whose result names a fresh media file is measured on
its own. When you want more:

- `eyes_measure` — measure one file the model names, older than the window
  included: a path, and whether to write a contact sheet.
- `eyes_status` — what the cell watches, on which binaries, with which limits,
  and how many notes it has appended since it started.
- `/eyes` — the operator's: no argument answers the status, a path measures
  that file.
- `(eyes:measure "out.mp4")` — the same note from eval.

The measurements are the original's: the share of pixels that differ clearly
from the frame's own median brightness, the share of half-seconds whose frame
changed more than noise, the mean brightness, ebur128's integrated loudness
and silencedetect's silences, and an 8-frame contact sheet.

## Tests

```bash
.github/test-cell.sh tools/nodecode-eyes
```

The tests stub the one seam that runs a program, so nothing here needs ffmpeg.

## Licence

MIT, the original's. See [LICENSE](LICENSE) and [NOTICE](NOTICE).
