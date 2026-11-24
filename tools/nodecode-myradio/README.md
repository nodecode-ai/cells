# nodecode-myradio

Myradio: DJ and run your own local radio station from chat — what is on air, the queue, song requests and the DJ's lines. A Nodecode rewrite of [newjordan/myradio](https://github.com/newjordan/myradio) by [newjordan](https://github.com/newjordan).

## Get it

Tell the agent "add the myradio cell", or run `nodecode add nc://myradio`.

## Set it up

The cell drives a [myradio](https://github.com/newjordan/myradio) station that already runs on the same computer: start it with `myradio` (or `python3 -m myradio.server`), and the station serves its own page and its control API on port 8765. The cell reads the private `control.token` the station writes in its data folder — no key to paste, nothing to sign in to.

Two settings, both in the `myradio` section of `~/.nodecode/config.jsonc` (or `/settings`):

| Setting | Default | What it is |
| --- | --- | --- |
| `data_dir` | `~/.myradio` | The station's data folder, the one holding `control.token`. |
| `port` | `8765` | The port the station listens on. |
| `timeout_seconds` | `15` | One request's ceiling. |

If the station keeps its data elsewhere, point `data_dir` at it: the token in that folder is what tells the station this is its own agent.

## Use it

Ask for what you want: "what's on my radio station?", "queue some Big Earl", "write a DJ line for the next song", "make me a song about fixing the build". The model gets twelve tools:

| Tool | What it does |
| --- | --- |
| `myradio_status` | What is on air, the queue (with `queue_id`), the library, schedules, recent history, and whether the music and DJ voice makers are ready. |
| `myradio_library` | Search the library by words and kind — the way to find songs when the library is big. |
| `myradio_queue_add`, `myradio_queue_move`, `myradio_queue_remove` | Work the queue. |
| `myradio_start`, `myradio_skip`, `myradio_fade_next` | Playout on this computer. |
| `myradio_schedule_add` | Play a track once, at an exact time. |
| `myradio_make` | Ask the station's makers for new audio: a song, a DJ lead-in, a station ID, a spot. |
| `myradio_operations`, `myradio_run` | Everything else the running station offers, and one call to run any of it. |

The operator has `/radio`:

```
/radio                 what is on air, time left, up next and the stream
/radio play <words>    find a song in the library and play it next
/radio request <idea>  ask the station's music maker for a new song about anything
/radio skip            skip the current track (/radio fade fades it out instead)
/radio start           start playout on this computer
/radio stop            stop playout
/radio live            go live to your saved stream destination (public)
/radio off-air         disconnect the stream; playout continues
```

Going live, going off air and deleting audio are the owner's: the model is refused those, and `/radio live` is how a person makes the station public. `(help :myradio)` carries the DJ guidance, so the model knows to keep three to six carts queued, vary the energy, and never repeat an artist back to back.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The station client, the twelve tools, the card and `/radio`. |
| `package.lisp` | The package and the cell's one condition. |
| `test/cell-test.lisp` | Seventeen tests against a stubbed station: the wire, the arguments, the refusals and `/radio`. |

## License

Apache-2.0. The original plugin is Copyright (c) 2026 Omnispace Technologies and the myradio contributors: see `LICENSE` and `NOTICE`.
