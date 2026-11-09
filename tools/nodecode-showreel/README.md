# nodecode-showreel

Showreel: a session cut into a captioned demo video under 90 seconds. A Nodecode rewrite of [Duffey234/hermes-showreel](https://github.com/Duffey234/hermes-showreel) by [Duffey234](https://github.com/Duffey234).

A session already says what it did: it asked, it ran commands, it read their
output, it wrote files, it answered. Showreel reads that history out of
Nodecode's own event log and cuts the interesting part into an MP4 with
burned-in captions, an `.srt` and a poster PNG -- fitted to the length you ask
for, never longer than 89 seconds, and rendered on your machine by the `ffmpeg`
you already have. Nothing is uploaded, nothing is synthesized, and no secret is
drawn: every string is scrubbed on the way out of the log and again on the way
into a frame.

A failing step and the step that fixed it are kept together, so the cut tells
the story the session actually told.

## Get it

Tell the agent "add the showreel cell", or run:

```sh
nodecode add nc://showreel
```

## Set it up

Two things: `ffmpeg` on your `PATH` (the cell never installs or downloads it),
and a folder renders land in -- the one setting without a default:

```jsonc
{
  "showreel": {
    "output_dir": "~/Videos/showreel",
    "default_length": 45,
    "default_aspect": "16:9"
  }
}
```

| Member | What it is |
| --- | --- |
| `output_dir` | the folder a cut, its `.srt` and its poster are written into (required) |
| `ffmpeg_path` | the `ffmpeg` to run; empty finds it on `PATH` |
| `font_file` | a TTF to draw with; empty looks for DejaVu, Liberation, Noto, Helvetica or Arial |
| `default_length` | the target length of a cut, in seconds (10 to 89, default 45) |
| `default_aspect` | `16:9`, `9:16` or `1:1` |
| `resolution` | `1080p` (1920x1080, upright 1080x1920) or `720p`, half that |

## Use it

```lisp
(showreel:render :session-id "..." :length 30 :aspect "9:16" :title "Snake")
```

or ask for it -- "make a 30 second vertical demo video of what we just did" --
and the agent calls `showreel_render`. It answers where the video, its
subtitles and its poster went.

| Command | What it does |
| --- | --- |
| `/showreel` | this session, the folder renders land in, the ffmpeg it would run |
| `/showreel cut [seconds] [16:9\|9:16\|1:1] [title...]` | render this session |
| `/showreel plan [seconds]` | the beats and captions, rendered nothing |
| `/showreel list` | recent sessions |
| `/showreel status` | ffmpeg, font and folder status |
| `/showreel demo [seconds] [aspect]` | a built-in sample session, no session needed |

Each render writes `<name>.mp4`, `<name>.srt` and `<name>.png`. The storyboard
is deterministic: the same session and the same settings always give the same
beats.
