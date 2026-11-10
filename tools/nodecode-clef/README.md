# nodecode-clef

Clef Music Tools: transpose chord symbols, spell piano chords, map ASCII guitar tab to piano notes and find music guides. A Nodecode rewrite of [MelMayssonOwen/hermes-plugins](https://github.com/MelMayssonOwen/hermes-plugins) by [MelMayssonOwen](https://github.com/MelMayssonOwen).

## Get it

Tell the agent "add the clef cell", or run `nodecode add nc://clef`.

## Set it up

No account and no key: the four tools ask Clef's public MCP server at
`https://clefdrills.com/mcp`, which is the endpoint the original plugin's
`mcp.json` named. Nothing is required in the `clef` section; two settings are
worth knowing:

| Setting | Default | What it does |
| --- | --- | --- |
| `endpoint` | `https://clefdrills.com/mcp` | the MCP server the three server tools and the guide search are asked |
| `timeout_seconds` | `30` | seconds one call may take |
| `offline_transpose` | `true` | answer `transpose_chords` in the cell, as the plugin's offline helper did, instead of asking the server |

What the tools send to `clefdrills.com` is the chords, tab or words you give
them, and nothing else. With `offline_transpose` on (the default),
transposition makes no request at all.

## Use it

Ask for the music and the model picks the tool:

- "transpose this to D" - `transpose_chords`, the chords moved by an interval, spelled with sharps or flats.
- "what notes are in a Cmaj7?" - `spell_piano_chord`, the pitches with their MIDI values.
- "turn this tab into piano notes" - `tab_to_piano_notes`, one line a column, lowest note first, with the server's caution that spacing is not rhythm.
- "find me a guide on capos" - `find_music_guides`, Clef's instructional guides with their urls.

From eval, and from `/clef`:

```lisp
(clef:transpose "C G Am F" 2)              ; => the chords, in the cell
(clef:spell-chord "Cmaj7" :octave 4)       ; => C4 (midi 60), E4 (midi 64), ...
(clef:tab-notes "e|-0-| ..." :capo 0)      ; => the columns as piano pitches
(clef:guides "guitar")                     ; => the guides, with urls
```

`/clef +2 C G Am F` transposes, `/clef chord Cmaj7` spells, and
`/clef guides guitar` finds; `/clef` alone says what the cell gives.
`(help :clef)` is the longer manual.

## What is in it

| File | What it is |
| --- | --- |
| `wire.lisp` | Clef's MCP server as JSON-RPC over the image's own HTTP call: `tools/call`, the structured answer, the refusal a refused call carries. |
| `chords.lisp` | The plugin's offline transposition helper, ported: chord roots moved, words left alone, no network. |
| `tools.lisp` | The four tools' contracts and the four verbs they share, plus `/clef`. |
| `cell.lisp` | The section, the tools, the command and the manual topic. |

## License

MIT. The original is Copyright (c) 2026 Opsibyte: see `LICENSE` and `NOTICE`.
