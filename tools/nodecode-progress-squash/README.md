# nodecode-progress-squash

Progress Squash: a progress bar's redraws shown once, not once per frame. A Nodecode rewrite of [pierrenode/hermes-progress-squash](https://github.com/pierrenode/hermes-progress-squash) by [pierrenode](https://github.com/pierrenode).

A bar redraws its one terminal line by writing a carriage return and the next frame. Your terminal shows the last frame; a captured stream keeps every one of them, so a 300-step `tqdm` loop reaches the model as 300 copies of the bar. This cell advises `sh` — the function behind every `(sh "...")` call — and keeps only the frame a terminal would show:

```
Receiving objects:   0% (1/11886)\rReceiving objects:   1% (119/11886)\r … \rReceiving objects: 100% (11886/11886), 67.61 MiB, done.
```

becomes

```
Receiving objects: 100% (11886/11886), 67.61 MiB, done.
[progress-squash: 396 overwritten progress frames removed]
```

Two frames belong to the same progress line when the text before their first digit or bar character matches (`epoch:`, `Receiving objects:`). Anything else written between redraws, such as an error, keeps a line of its own. The footer tells the model how many frames were dropped.

## Get it

Tell the agent "add the progress squash cell", or run `nodecode add nc://progress-squash`.

## Use it

Nothing to configure: the folder is on as it stands, and every shell result goes through it.

- `/progress-squash` reports what this run has squashed — how many `sh` results it saw, how many it rewrote, how many frames it dropped. `/progress-squash reset` clears the counters.
- `(help 'progress-squash)` says the same in the manual, including what is left alone.

Settings, in the `progress-squash` section of the config (see `config.example.jsonc`):

| Setting | Default | What it does |
| --- | --- | --- |
| `footer` | `true` | end a rewritten output with the line saying how many frames were dropped |
| `min_frames` | `1` | how many overwritten frames of one line before that line is collapsed; `2` keeps a single overwrite |
| `keep` | `[]` | progress-line stems left as they are, such as `epoch:` |

Left as they are, byte for byte: output with no carriage return; Windows line endings (`\r\n`); a line with a single frame, or with no two consecutive frames of the same progress line.

The advice sits on `sh` itself, so the organism's own shell calls go through it too. Output with no progress frames is never touched, so nothing that parses a command's answer sees a difference.

## Licence

MIT. The original plugin is Copyright (c) 2026 pierrenode: see `LICENSE` and `NOTICE`.
