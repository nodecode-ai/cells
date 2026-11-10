# nodecode-procgen-studio

Procgen Studio: image assets drawn as code. A Nodecode rewrite of [iamlukethedev/hermes-procgen-studio](https://github.com/iamlukethedev/hermes-procgen-studio) by [iamlukethedev](https://github.com/iamlukethedev).

Your own model writes the drawing -- an SVG document or a pixel-grid JSON object -- and this cell renders it locally to a PNG with the editable source and a reproducible seed beside it. No image API key, no image bill, no network: rendering is local.

## Get it

Tell the agent "add the procgen-studio cell", or run `nodecode add nc://procgen-studio`.

## Set it up

Nothing is required. The `procgen-studio` section in `/settings` holds:

| Setting | What it does |
| --- | --- |
| `preset` | The style a drawing runs on when no STYLE.md and no `style` argument applies: `flat-illustration`, `ember`, `frost`, `dark-fantasy-lowpoly`, `brand-minimal` or `pixel-arcade`. |
| `style_file` | A STYLE.md that applies everywhere, relative to the session's directory. |
| `output_dir` | A folder that also receives each render, its source and its metadata, for a project that wants the assets in its own tree. |
| `aspect` | The shape a render asked for without a size gets: `1:1` (1024x1024), `3:2`, `2:3`. |
| `render_timeout` | Seconds the SVG rasteriser may take before its process tree is killed. |

An SVG source needs one program on PATH: `rsvg-convert`, `resvg`, `magick`, `convert`, `cairosvg` or `inkscape`, the first found. A **pixel-grid** source needs none: it is drawn here, byte for byte. `/procgen-studio status` says which one was found.

## Use it

Ask for an image asset and the agent calls the tools:

- `procgen_draw` -- the prompt and the source; answers the render id, seed, image, source and metadata paths.
- `procgen_style` -- the contract to draw against: the style's prose rules and every palette role with its colour.
- `procgen_reroll` -- the same stored source with a new seed, or under another style, with no new drawing.
- `procgen_renders` -- what has been drawn, or one render's paths and source.

An SVG source is a document the model writes; a pixel grid is one JSON object whose letters map to palette roles and whose `.` cells are background:

```json
{"palette": {"K": "ink", "h": "base", "g": "accent"},
 "frames": [["..KKK..", ".KhhhK.", "KhKgKhK", "KhhhhhK", ".KhhhK.", "..KKK.."]]}
```

Two tokens make a source seedable: `{{seed}}`, `{{rand:MIN:MAX}}`, `{{pick:a,b,c}}` and any palette role (`{{gem}}`, `{{palette.gem}}`) are replaced before drawing, so `/procgen-studio reroll last --seed 7` is a real variation of the same program.

For the operator there is `/procgen-studio`:

```
/procgen-studio status                 what is in effect (style, set, rasteriser, renders)
/procgen-studio styles                 the six built-in presets
/procgen-studio style [name|show]      one preset's contract, or the one in effect
/procgen-studio init [preset]          write a STYLE.md into this directory
/procgen-studio set [name|off]         group the next draws into an asset set
/procgen-studio list [n]               recent renders
/procgen-studio show <id|last>         paths, seed and style of one render
/procgen-studio source <id|last>       print the drawing's own source
/procgen-studio reroll <id|last> [--seed n] [--style p]
/procgen-studio render <file> [--size WxH] [--style p]
```

### STYLE.md

`/procgen-studio init ember` writes a `STYLE.md` into the session's directory, and every render there is held to it:

```
preset: ember
palette: frost
palette.gem: #56e0ff
outline: 14px ink, round joins
Free prose here is passed to the model as it is.
```

A whole palette swap restyles every asset with no new drawing; a rule change needs a new one. `preset:` composes: `frost` is `ember`'s rules with a cold palette.

Renders live under the Nodecode home in `plugin-data/procgen-studio/renders/<id>/`: `image.png`, the source as it was written, the expanded source that was drawn, and `meta.json` with the prompt, seed, size and style.

## What is in it

| File | What it is |
| --- | --- |
| `presets.lisp` | The six style presets, as data: rules in the original's order, palettes role by role. |
| `style.lisp` | STYLE.md parsing, preset inheritance, palette finalising, and the contract the model reads. |
| `render.lisp` | PNG writing, seeded token expansion, the pixel-grid renderer, and the SVG rasteriser seam. |
| `store.lisp` | One folder a render: the id, `meta.json`, the output_dir copy, the active set. |
| `cell.lisp` | The four tools, `/procgen-studio`, the manual, and the section. |

## License

MIT. Procgen Studio is Copyright (c) 2026 Lucas (iamlukethedev): see `LICENSE` and `NOTICE`.
