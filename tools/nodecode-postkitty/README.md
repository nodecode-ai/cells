# nodecode-postkitty

Postkitty: one post in, every platform out. A Nodecode rewrite of [mertkayacs/postkitty](https://github.com/mertkayacs/postkitty) by [mertkayacs](https://github.com/mertkayacs).

Five tools the model calls. Give them a draft, some photos or a landscape
video, and a job folder comes back holding a version of each for Instagram,
TikTok, YouTube Shorts, X, LinkedIn, Threads, Bluesky and Facebook, checked
against that platform's own published limits. Nothing is posted or uploaded:
the files in `ready/` are the user's to upload.

| Tool | What it does |
| --- | --- |
| `postkitty_scan` | Opens a job, reads each file's own header, plans a framing per target, and reports each platform's limits beside what the draft counts |
| `postkitty_copy` | Counts every draft with that platform's rule (X weighted, Bluesky graphemes and bytes, TikTok UTF-16, Instagram hashtags and mentions), splits long X, Threads and Bluesky text into a numbered thread, writes `post.txt`, `thread.txt`, `alt.txt` and `advice.md` |
| `postkitty_make` | Renders each planned target with ffmpeg, checks the file against the target, puts only passing files in `ready/`, and writes `board.html` |
| `postkitty_redo` | Renders one target again with another layout, another part of the frame, a trim or another cover frame |
| `postkitty_check` | Holds a file the user brings from elsewhere against one target, without rendering |

The model reads the workflow with `(help :postkitty)`, and an operator can
type `/postkitty` for every platform target with its size and limits, or
`/postkitty tiktok` for one.

## Get it

Tell the agent "add the postkitty cell", or run `nodecode add nc://postkitty`.

## Set it up

Nothing is required; the tools answer as soon as the cell is on. In
`/settings`, under `postkitty`:

- `output_dir`: where job folders are written. Empty, the default, means
  `./postkitty` beside the working directory.
- `default_platforms`: the platforms a scan makes when the caller names
  none. Empty means every target that fits the inputs.
- `loudness_lufs`: the integrated loudness target of a rendered video's
  audio, `-14` by default, between `-40` and `-5`.

Rendering and video probing need `ffmpeg` and `ffprobe` on `PATH`. Without
video probing a clip's size and length are reported as unknown; without
`ffmpeg` a media target answers `skipped` and the copy still works.

## Use it

```
Give Hermes a draft, photos or a landscape video. postkitty fixes the wording and writes a
version for each platform, crops the photos and reframes the video, and checks the file
against the platform's limits. It never posts, uploads or schedules.

  Fix my wording and write this for X, LinkedIn and Threads: [your draft]
  Make team.jpg ready for Instagram and Bluesky.
  Turn talk.mp4 into vertical clips for TikTok, Reels and Shorts.
  Will this file upload to TikTok? [path to the file]
```

A target is uploadable only when its status is `ready`. `failed` carries
the checks that failed, and `skipped` means the renderer was missing. The
per-platform files are under `ready/<platform>/` in the job folder the tool
answers with, and `board.html` is the whole job on one page.

## How it differs from the plugin

The plugin shipped five tools and a skill, with OpenCV, Pillow, NumPy and
Python's regex library doing the work. This rewrite keeps the platform table
(the limits the plugin's author checked in 2026), the counting rules and the
five tool bodies, and drops the parts that needed OpenCV: face detection and
speaker tracking, so a video is framed by `crop` (fill the frame) or `pad`
(the whole frame, black bars) rather than by following a face, and there is
no `split`, `speaker` or `seam` layout. Rendering is `ffmpeg`, and the
review page is plain HTML with no contact sheet. See NOTICE.

## Test

```
NODECODE_SRC=~/nodecode/nodecode .github/test-cell.sh tools/nodecode-postkitty
```

Nothing in the tests reaches the network; a box without ffmpeg answers
`skipped`, and the tests allow it.
