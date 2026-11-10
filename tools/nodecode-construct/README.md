# nodecode-construct

Construct: one exact description of a thing becomes geometry you can look at, print and hand on -- a checked scene, a glTF, an OBJ, an STL and a page that turns. A Nodecode rewrite of [rtmandreyev/construct](https://github.com/rtmandreyev/construct) by [rtmandreyev](https://github.com/rtmandreyev).

A project is a folder with a `scene.json` in it: named parts, each of a
shape (box, cylinder, cone, sphere) at a full size in metres, a centre, a
rotation, and how it is known. The cell checks the scene, compiles it once
into triangles and lines, and every output comes from that one compile, so
the page, the model and the mesh cannot disagree. Nothing is downloaded,
nothing is fetched at run time, and no Python, Node or browser is needed: the
geometry is written here, and the page is one HTML file with its viewer
inlined.

## Get it

Tell the agent "add the construct cell", or run `nodecode add nc://construct`.

## Set it up

Nothing is required: the projects live under the home's `construct/` folder,
and a cube is a size and a place away from being a thing.

| Key | Default | What it does |
| --- | --- | --- |
| `projects_folder` | empty | Where the projects live: a full path, or empty for `<home>/construct/`. |
| `segments` | `24` | Sides on a curved shape. |

## Use it

Ask for a thing, and the model works in a project:

1. `construct_start` begins one -- `cube`, `shed`, or a whole scene you write
   as JSON.
2. `construct_edit` changes one part by its id: `move`, `size`, `rotate`,
   `certainty`, `role`, `color`, or `remove`. The whole scene is checked
   first, so a change that breaks it writes nothing and comes back with every
   problem; what did move comes back as the diff.
3. `construct_check` says what is wrong, each problem with its JSON path and
   the fix (`parts[2].size: size 0,0,0 is outside ... -- metres; a part is at
   least 0.1 mm`), and what the scene compiles to.
4. `construct_inspect` is what the record says is known: the parts by
   certainty, role and size, the overall size, and every fact with the source
   it came from.
5. `construct_build` writes `build/index.html`: the construct drawn in lines
   of light with pale faces, turning, drag to turn it, wheel to zoom, every
   part named and in its own colour. Open it in a browser.
6. `construct_export` writes `export/construct.gltf` with `construct.bin`,
   `export/construct.obj` and `export/construct.stl` (millimetres, as a
   slicer reads it), reads each one back and answers what it carries.

`/construct` lists the projects and what each checks to; `/construct <slug>`
shows one and where its page is.

A scene in full:

```json
{
  "format": "construct-scene",
  "version": 2,
  "name": "a shed",
  "category": "architecture",
  "sources": [{"id": "plan", "kind": "drawing", "title": "the plan"}],
  "facts": [{"text": "the shed is 3 m wide", "certainty": "verified", "source": "plan"}],
  "parts": [
    {"id": "floor", "shape": "box", "size": [3, 2.4, 0.1], "position": [0, 0, 0.05],
     "certainty": "inferred", "role": "mass", "color": "#8a7f6a"}
  ]
}
```

Size is the part's full extent in X, Y and Z, not half of it; position is
its centre, not its corner; rotation is XYZ degrees about that centre; Z is
up. A part with no certainty is `inferred`, and the page and the inspect
both say so.

## Test

```sh
.github/test-cell.sh tools/nodecode-construct
```

Everything in the slice is offline: the scenes are written by the test, and
the geometry, the checks, the three exports and the page are proved from the
files they leave on disk.

## What is not here

The original also does CAD (FreeCAD, STEP, 3MF), films, model kits, headless
review images, NetLogo runs drawn as constructs, and joints with animation,
and it ships sixteen constructs and sixteen tutorials. Each needs a program or
an engine this cell does not bundle. `NOTICE` says what was left out and why.

## License

Apache-2.0, the original's. See `LICENSE` and `NOTICE`.
