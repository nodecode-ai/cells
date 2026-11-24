# nodecode-meeting

Meeting Overlay: the live meeting transcript and the records saved from it, as tools. A Nodecode rewrite of [taxbax/meeting-overlay](https://github.com/taxbax/meeting-overlay) by [taxbax](https://github.com/taxbax).

## Get it

Tell the agent "add the meeting cell", or run

```sh
nodecode add nc://meeting
```

## Set it up

No key, no account, no network. The cell reads a transcript directory and writes records; both have defaults, so it works the moment it is on.

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `dir` | `~/.nodecode/meeting` | where `rolling.txt` and `full.txt` are read from and appended to |
| `records` | (empty) | where a saved record goes; empty means `Records/` inside `dir` |
| `window_chars` | `20000` | how much of the newest transcript `rolling.txt` keeps |
| `stale_seconds` | `30` | how old the transcript may be before status calls the capture dead |

Point `dir` at whatever your transcriber already writes, or let the cell be the one that writes it. The line format is the original's: `[HH:MM:SS] [mic] what was said`.

## Use it

Ask the agent, or call the verbs from eval:

```lisp
(meeting:note-line "we should ship the patch on Friday" :source "audio")
(meeting:transcript :lines 30)
(meeting:status)
(meeting:save "Weekly sync" :notes "ship Friday; budget approved")
```

The four verbs are also the four tools the model calls — `meeting_append`, `meeting_transcript`, `meeting_status`, `meeting_save` — and `/meeting` prints where capture stands (`/meeting save TITLE` writes the record).

- **A transcript.** `meeting_append` takes one line at a time, tagged `mic`, `audio` (the other side of a call), `phone`, or a tag of your own. `rolling.txt` keeps the newest `window_chars`; `full.txt` keeps everything. Anything that can append a line can feed it — a transcriber, a script, or you, dictating.
- **Reading it.** `meeting_transcript` answers the newest lines with the transcript's age, and says `STALE` when it is older than `stale_seconds`, so an answer is never invented from words spoken an hour ago.
- **Records.** `meeting_save` writes one markdown record under `Records/YYYY/MM/week-YYYY-MM-DD/`: the title, the notes, and the transcript. The tree is the original's, so an Obsidian vault of meeting records reads the same.
- **An echo.** The same words arriving twice — your mic and the call's own audio — are one line, not two; the phone's mic is the preferred source and always wins.

## What is in it

| File | What it is |
|---|---|
| `cell.lisp` | the section, the four verbs, the four tools, `/meeting`, the primer |
| `package.lisp` | the `MEETING` package, so the verbs are callable from eval |
| `test/` | offline tests: a scratch transcript directory, the four tools through the real dispatch, the refusals |
| `config.example.jsonc` | the section, commented |

What the original had and this does not: the macOS whisper daemon and its BlackHole audio routing, the launchd agents, the phone PWA and the desktop pane. Transcript and records are what a cell can carry; on macOS the original's `listen.py` can still be the thing that appends the lines.
