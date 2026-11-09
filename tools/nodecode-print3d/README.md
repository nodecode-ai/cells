# nodecode-print3d

Print3D: an idea becomes a checked, sliced Bambu Studio project for a Bambu Lab X2D. A Nodecode rewrite of [mfranzon/print3d](https://github.com/mfranzon/print3d) by [mfranzon](https://github.com/mfranzon).

You model in Blender, in a bpy script the cell runs for you; the cell checks the mesh it exports, gives every note its fix, and hands the file to Bambu Studio to slice. Nothing is sent anywhere, and nothing is sent to the printer.

## Get it

Tell the agent "add the print3d cell", or run `nodecode add nc://print3d`.

## Set it up

The cell drives the two programs a Bambu owner already has:

- **Blender 4.2 or later**, for the model. The cell refuses to start without it and says so; give its path in the `blender` setting if it is not `blender` on `PATH`.
- **Bambu Studio**, for the slice (`bambu-studio` on `PATH`, or the `studio` setting). A missing slicer is not a failed run: the job still models and checks, and the report says which setting to fill.

| Setting | Default | What it is |
| --- | --- | --- |
| `blender` | `blender` | Blender 4.2 or later, by name or path. |
| `studio` | `bambu-studio` | Bambu Studio's CLI, by name or path. |
| `jobs_dir` | empty | Where the jobs live; empty is `~/print3d/jobs`. |
| `bed` | `256,256,260` | The bed the checks measure against, in mm as X,Y,Z. |

## Use it

A job is a folder, `jobs/<slug>/`, holding `model.py`, `spec.sexp` and, once it runs, `part.stl` and `part.gcode.3mf`.

| Tool | What it answers |
| --- | --- |
| `print3d_spec` | Records a dimension and where it came from — the user, a shop listing, or assumed with the reason — or reads the job's whole spec back. |
| `print3d_check` | Printability of an STL: triangle count, size against the bed, volume, whether it is manifold, whether it sits on the bed, each error with the fix. |
| `print3d_make` | Runs the job's `model.py` through Blender, checks the mesh it exported, then slices it with Bambu Studio. With no `model.py` yet it writes a starter and names its path. |

```
/print3d             the jobs and how far each got
/print3d dolly-peg   one job's dimensions and their evidence
```

The loop the cell is built for: write `model.py` (the starter is a 20 mm calibration cube), call `print3d_make`, read the notes, fix the script until the report says `ok`, and hand the user the `.3mf` path. The model script gets the output path after `--` and exports the mesh there:

```python
import bpy, sys
out = sys.argv[sys.argv.index("--") + 1]
bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.mesh.primitive_cube_add(size=20, location=(0, 0, 10))
bpy.ops.wm.stl_export(filepath=out, export_selected_objects=True)
```

`(help :print3d)` has the same guidance the model is given.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The job folder and its spec, the STL reader and the printability checks, the Blender and Bambu Studio runs, the three tools and `/print3d`. |
| `test/cell-test.lisp` | Offline tests: a written cube checked clean, an oversized part, a lifted part, the make loop over a stubbed seam, a failed Blender, a missing slicer, the spec's evidence, and the contracts. |

What the original does beyond this folder — measuring a part off photographs, the likeness overlay, the HTML viewer, the handoff sheet — is listed in `NOTICE`.

## License

MIT. The original is Copyright (c) 2026 mfranzon: see `LICENSE` and `NOTICE`.
