# nodecode-music-jam

Music Jam: a four-piece synthesized band on a local page, conducted by the agent's tools. A Nodecode rewrite of [vdimarco/agent-music-jam](https://github.com/vdimarco/agent-music-jam) by [vdimarco](https://github.com/vdimarco).

Four musicians — drums, bass, keys and lead — each have six controls and a four-bar pattern. The cell opens a page on 127.0.0.1 with a token, the page plays the band through the browser's own Web Audio and keeps the audio clock, and the tools change the band in plain words or numbers. A change waits for the next bar, as the original's did: nothing jumps in mid-phrase.

## Get it

Tell the agent "add the music jam cell", or run `nodecode add nc://music-jam`.

## Set it up

Nothing is required. `music-jam.port` (default `0`) picks the loopback port the page is served on: 0 takes an available one on every jam, a fixed port is what a bookmark needs.

## Use it

Ask for a jam; the agent calls `jam_start` and hands you the page's address. Click **Play** there: no sound without it, because a browser will not start audio on its own.

- **The tools.** `jam_inspect` says what each musician is playing and what waits for a bar; `jam_direct` takes words ("keys less busy, darker", "a fill on drums", "space for a solo"); `jam_configure` and `jam_schedule` set the controls themselves, 0 to 1 except register at -12 to 12 semitones, with an optional delay in bars and a mute; `jam_undo` takes one musician back; `jam_stop` stops the transport and cancels what is queued.
- **The page.** Four cards, a transport and the clock. The keys A S D F G H J K play the lead voice, and the phrase is kept for the export. Every card shows its musician's intention and its density and energy.
- **Saving and exporting.** `jam_save` writes the whole session under the Nodecode home and `jam_restore` brings it back, stopped; `jam_export` writes the bars played so far as a standard MIDI file and answers where it is.
- **/music-jam.** Typed alone it answers the page's address and the state of the band; typed with a direction ("/music-jam keys darker") it conducts the band the way `jam_direct` does.

The page is served on loopback only, and every API call carries the token the address holds; the Host header must be the loopback address, so another site's page cannot post to it.

## What is in it

| File | What it is |
| --- | --- |
| `music.lisp` | The controls, the four local patterns, the session shape and the MIDI writer. |
| `engine.lisp` | The arrangement queue over the browser's audio clock: changes, undo, bars, save, restore. |
| `server.lisp` | The loopback listener, the page and its API. |
| `cell.lisp` | The ten tools, `/music-jam` and the cell's declaration. |
| `test/cell-test.lisp` | The declaration, the queue, the API and the listener, offline. |

## Licence

MIT. The original is Copyright (c) 2026 Agent Music Jam contributors: see `LICENSE` and `NOTICE`.
