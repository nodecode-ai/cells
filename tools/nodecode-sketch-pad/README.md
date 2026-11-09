# nodecode-sketch-pad

Sketch Pad: draw a shape with the mouse in a browser tab, save it, and the model reads the PNG. A Nodecode rewrite of [aydnOktay/hermes-sketch-pad](https://github.com/aydnOktay/hermes-sketch-pad) by [aydnOktay](https://github.com/aydnOktay).

## Get it

Tell the agent "add the sketch-pad cell", or run `nodecode add nc://sketch-pad`.

## Set it up

Nothing is required. The cell serves the pad on `127.0.0.1:17319` and keeps
drawings in `sketches/` under this profile's home. In `/settings`, the
`sketch-pad` section changes three things:

- `port` (17319): where the pad answers; `0` lets the system pick, and a port
  someone else holds falls back to one it picks, with a notice.
- `max_bytes` (800000): the largest PNG a save takes.
- `directory` (`sketches/`): where the drawings go, under the home.

## Use it

1. `/sketch` prints the pad's address. Open it in a browser tab -- the same
   machine, nothing is exposed off it -- and draw with the mouse or a finger.
   Clear, pen width and ink colour are there; **Save** keeps the drawing.
2. Ask the agent about it: "what did I draw", "this shape", "the sketch". It
   calls `sketch_pad_last` for the file and `(look PATH)` to see it, then
   answers from the pixels.
3. Later in the session, the same path still works, so it can look again.

Each drawing is kept as `sketch-<stamp>.png`; `last.png` is always the newest
one, replaced atomically, for anything that wants one fixed path.

Two tools, both also callable from eval:

```lisp
;; the newest drawing: path, bytes, size, when it was saved
(nlk:json-value (sketch_pad_last) :string "path")
```

- `sketch_pad_last` -- the newest drawing (no arguments).
- `sketch_pad_save` -- keep a base64 PNG as a drawing: `{png_base64}`. A
  `data:image/png;base64,` prefix is accepted. Reaches the same folder the
  page saves into, so a screenshot or another tool's image can be talked about
  the same way.

A drawing is only what its pixels show: the model is told never to describe
one it has not looked at.

The pad is local. The page posts to this cell's own listener on loopback; no
third-party service, no key and no network are involved.
