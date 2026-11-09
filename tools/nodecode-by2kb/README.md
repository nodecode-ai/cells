# nodecode-by2kb

By2kb: finds learning videos by topic and turns a Bilibili or YouTube video into a transcript, an abstract and study notes in your own knowledge base. A Nodecode rewrite of [Charlesmpc/by2kb](https://github.com/Charlesmpc/by2kb) by [Charlesmpc](https://github.com/Charlesmpc).

The work is done by [by2kb](https://github.com/Charlesmpc/by2kb), a Python CLI: it fetches the media, transcribes it with a local Whisper model (or an optional cloud one), and writes three durable Markdown files. The cell runs that program and reads its JSON; the summaries are written by the session's own model, so the CLI never needs an API key.

## Get it

Tell the agent "add the by2kb cell", or run `nodecode add nc://by2kb`.

## Set it up

The cell drives the `by2kb` program, so that has to be there first:

```sh
pipx install "by2kb[asr-whisper,youtube]"     # or: uv tool install --python 3.12 "by2kb[asr-whisper,youtube]"
by2kb init --preset agent-local --library-root ~/knowledge
by2kb models install                          # the local Whisper model, once
```

It needs Python 3.12+, `ffmpeg` and `ffprobe` on `PATH`. The program is found on `PATH`; if it lives somewhere else, give its path in the cell's `by2kb` setting (`/settings`), and a cell that cannot find it refuses to start and says so. The knowledge base is the CLI's own: `~/.by2kb` holds its config, and the files land in the library root you chose.

## Use it

Ask for learning material on a topic, and send a video link when you have one. The model gets six tools:

| Tool | What it answers |
| --- | --- |
| `by2kb_search` | Numbered recommendations for a topic -- title, link and why it matched -- from Bilibili and YouTube. Metadata and cached captions only: nothing is downloaded yet. |
| `by2kb_select` | Takes the numbers the person replies with (`1`, `1,3`) from the list this conversation was last shown, and starts transcribing and filing those videos. |
| `by2kb_video` | One video: a Bilibili/YouTube URL, or the path to a local audio/video file. Answers a job. |
| `by2kb_next` | The job's next staged summary operation: the prompts to answer, its size and time limit, and the operation id. |
| `by2kb_submit` | Hands one operation its whole answer -- JSON when it asks for JSON, Markdown otherwise. |
| `by2kb_status` | Where a job stands, and the files it has written so far. |

The summaries arrive in stages: a video answers a job, `by2kb_next` asks for one bounded operation at a time, and `by2kb_submit` takes the answer, until the job reports the three files -- transcript, short abstract, long-form notes. The abstract is shown in the conversation; the paths are the knowledge base's.

You can look yourself:

```
/by2kb TiDB vector search      numbered recommendations
/by2kb 1,3                     file those two videos
/by2kb https://youtu.be/...   one video
/by2kb status JOB             where it stands
```

And from eval: `(by2kb:search "TiDB vector search")`, `(by2kb:video "https://...")`, `(by2kb:next "JOB")`, `(by2kb:submit "JOB" "OP" "...")`, `(by2kb:status "JOB")`. `(help :by2kb)` has the same guidance the model is given.

Discovery is deliberately conservative: a named subject has to appear in a candidate's title or description, and a thin result set is not padded. A recommendation is an excerpt, not the video -- the model is told not to claim it read a full video from one.

## Settings

| Setting | Default | What it is |
| --- | --- | --- |
| `by2kb` | `by2kb` | The program's name on `PATH`, or its path. |
| `provider` | `nodecode` | The provider name recorded in a summary job's provenance. |
| `model` | `nodecode` | The model name recorded in a summary job's provenance. |
| `scope` | empty | An opaque scope a conversation shares, so a number answers its own list. Empty derives one from the session. |
| `timeout_seconds` | 3600 | One run's ceiling; transcribing a long video takes minutes. |

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The six verbs, the argv each builds, the JSON each reads, `/by2kb`, and the standing paragraph. |
| `package.lisp` | The `by2kb` package: the verbs and the condition every refusal signals. |
| `test/` | Offline tests over a stubbed program: the argv each verb asks for, the recommendation list, the selection, one video's three files, a staged operation read page by page, the submitted answer and the refusals. |

The Hermes plugin intercepted a gateway message with a video URL, a `by2kb TOPIC` prefix or a bare number; Nodecode has no such seam, so here the model calls the tools and `/by2kb` gives the person the same routes. `NOTICE` says what else was left out.

## License

MIT. The original is Copyright (c) 2026 Charles: see `LICENSE` and `NOTICE`.
