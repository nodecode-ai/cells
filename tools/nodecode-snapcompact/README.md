# nodecode-snapcompact

Snapcompact: renders a long transcript into dense bitmap PNG frames that a vision-capable model reads back for a fraction of the tokens the same words cost as text, and tells the model how to read them. A Nodecode rewrite of [vimona3ds/hermes-snapcompact](https://github.com/vimona3ds/hermes-snapcompact) by [vimona3ds](https://github.com/vimona3ds).

The original is a context engine for another agent: when the context fills, old turns are serialized and drawn into bitmap pages, and the newest turns stay verbatim. Nodecode evicts history rather than compacting it, so the same rendering arrives here as one tool and one command: hand it a text or a file, get back PNG pages and a reading guide, and look at the pages.

| Surface | What it does |
| --- | --- |
| `snapcompact` | Renders `text` (or a `file` holding it) into one or more PNG frames under `output_dir`, and answers each frame's path, its pixel size, the characters it carries, the tokens the words would have cost and the tokens the frames cost. The guide says to call `(look "PATH")` on every frame in order. |
| `/snapcompact` | The same from the operator's side: `/snapcompact ~/notes/transcript.txt`, or `/snapcompact` alone to be asked for a file. |

A page is a grid: `columns` characters wide and `rows` tall, no word wrap, a run of spaces is one space, and a line longer than the grid continues on the next row. Everything is local and deterministic — no model call, no key, no network. The frames are drawn with the 5x7 font in `font.lisp` and written as one-bit greyscale PNG by `png.lisp`.

The arithmetic behind it: a page of 100 by 60 characters at `scale` 1 is 616 by 496 pixels, which is about 400 tokens as a vision model bills it, against roughly 1,500 tokens for the same 6,000 characters as text. Pages past `max_frames` are dropped from the oldest end and counted in the answer.

## Get it

Tell the agent "add the snapcompact cell", or run `nodecode add nc://snapcompact`.

## Set it up

One setting is required — where the frames go:

```jsonc
"snapcompact": {
  "output_dir": "~/.nodecode/snapcompact",
  "columns": 100,
  "rows": 60,
  "scale": 1,
  "max_frames": 80
}
```

| Setting | What it does |
| --- | --- |
| `output_dir` | Where frames are written, one subfolder per call. Required. |
| `columns` | Characters across a page, 20 at the least. Wider pages mean fewer frames and more pixels in each. |
| `rows` | Rows down a page, 5 at the least. The last page is drawn as short as it is. |
| `scale` | Pixels a glyph pixel: 1 is the densest and cheapest, 2 or 3 for a model that misreads small text. |
| `max_frames` | The most frames one rendering writes; past it the oldest pages are dropped and counted. |

You need a vision-capable model to read the frames back. A model that only sees text still gets the answer's character and token counts, but not the words.

## Use it

Ask the model to archive the part of the conversation that no longer needs to be verbatim, or hand it a file:

```
snapcompact  {text: "…whole turns, tool calls with their output, the decisions…", label: "build-log"}
snapcompact  {file: "~/notes/transcript.txt"}
```

It answers with the frame paths and a guide; the model then calls `(look "PATH")` on each frame, oldest first, and continues from what it reads. From the keyboard, `/snapcompact PATH` does the same and prints the paths. From eval, `(snapcompact:frames-for "…")` answers the same object the tool does.

Write the text as something to be read back exactly: whole turns, tool calls with their output, the decisions and the paths a later turn depends on — and leave out what the workspace can re-derive, since re-reading a file is cheaper than a page of stale output.
