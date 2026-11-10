# nodecode-godot-studio
Godot: headless check a project, export from a preset, and read the state of its builds. A Nodecode rewrite of [Foolafroos/godot-studio](https://github.com/Foolafroos/godot-studio) by [Foolafroos](https://github.com/Foolafroos).

The Godot executable you already have does the work; this cell drives it without
opening the editor. It gives the model four tools:

| Tool | What it does |
| --- | --- |
| `godot_headless_check` | Run a project headless and quit, surfacing script and resource errors. `--quit` validates what the project loads and parses; a frame count reaches runtime. |
| `godot_export` | Export a build from a named export preset, release or debug. The output is confined to the project, and a file the project did not build is never overwritten. |
| `godot_builds` | Every export preset in the project and the state of the artifact each would write: exists, size, modification time. |
| `godot_setup` | The engine this cell found, its version, and the projects the section lists. Nothing is installed. |

`/godot-studio [PROJECT]` says what the cell can see: the executable, its version, and
each project's presets and built artifacts. While the cell runs, `(help :godot-studio)`
carries the workflow — check after every batch of edits, export, then play — and
the pitfalls that cost the original's author real hours.

## Get it

Tell the agent "add the godot-studio cell", or run `nodecode add nc://godot-studio`.

## Set it up

Nothing is required when `godot` (or `godot4`) sits on `PATH`, or at a desktop
install's own path. The section `~/.nodecode/config.jsonc` may carry is:

```jsonc
{
  "godot-studio": {
    "binary": "/usr/local/bin/godot",      // the executable, not the .app
    "projects": ["~/games/hello"],         // what /godot-studio reports on
    "check_timeout": 300,                   // seconds one headless check may take
    "export_timeout": 900,                  // seconds one export may take
    "max_output": 20000                     // characters of engine output kept
  }
}
```

Every tool call names its own `project_path`; `projects` is only what `/godot-studio`
lists. Running `godot_setup` first says whether the engine was found.

## Use it

> check the hello project headless — any script errors?
> which export presets does it have, and is the Web build current?
> export the Web preset and tell me the size of the artifact

`godot_builds` is what to ask before re-exporting and after, to confirm the
artifact: it reports the file each preset would write and whether it is there.
`godot_export` writes only inside the project folder — an `output_path` outside
it is refused, and an existing file is only replaced when it is a known export
artifact of that project (the path a preset in `export_presets.cfg` names). The
output's parent directory is created, because Godot refuses to export into a
missing one.

**What it runs.** A check and an export run the target project's own GDScript —
autoloads, the main scene when frames are asked for, export plugins — with no
approval prompt, exactly as the original plugin did. Point them only at projects
you trust. Nothing here contacts a network, and nothing reads outside the project
a call names.

**What an export needs.** Godot's export templates, which are not bundled with
the editor: until they are installed for the engine's version, an export fails
with "export template not found", and the answer says so with the fix (Project →
Manage Export Templates) instead of raw engine output.

## What it does not do

- No live editor: scene and node editing belongs to an editor bridge. This is the
  other half of the loop — verify, export, and read the build.
- No build gallery pane and no version chip: the original shipped a desktop UI
  half; here `godot_builds` and `/godot-studio` answer the same data to the model and
  the operator.
- No MCP registration and no install report for other tools (`uv`, an editor
  addon): `godot_setup` reports this cell's own engine and projects only.
- No project creation, no template installation, and no playtest: run the build
  yourself.

## Test

From the cells repository's root:

```sh
.github/test-cell.sh tools/nodecode-godot-studio
```

No Godot runs and no editor is opened: the engine is a stubbed function that
records its argv, answers what a real one would, and writes the file an export
names. Fixtures hold a small project with `project.godot` and an
`export_presets.cfg` in the editor's own format.

MIT licensed; see [NOTICE](NOTICE).
