# nodecode-davinci-resolve

DaVinci Resolve: live project, media and render control on Studio, importable FCPXML and marker CSV without it. A Nodecode rewrite of [wassermanproductions/hermes-davinci-resolve-plugin](https://github.com/wassermanproductions/hermes-davinci-resolve-plugin) by [wassermanproductions](https://github.com/wassermanproductions).

Resolve publishes its scripting API as a Python module, `DaVinciResolveScript`,
that talks to the running app. Free Resolve cannot be driven that way, so the
cell has two halves, as the original plugin did: four verbs reach a running
Resolve Studio, and three write files Resolve imports through its own
**File > Import > Timeline** dialog, which is the workflow free Resolve has.

## Get it

Tell the agent "add the davinci-resolve cell", or run `nodecode add nc://davinci-resolve`.

## Set it up

Nothing is required for the interchange half: the cell starts on its defaults
and writes into `Documents/Nodecode Resolve Exports`. The live half needs an
interpreter that can `import DaVinciResolveScript`, which Resolve itself
installs under its own Scripting folder:

```jsonc
{
  "davinci-resolve": {
    // The interpreter that can import DaVinciResolveScript. Python 3 from
    // python.org or Homebrew; Resolve ships the module, not the interpreter.
    "python": "python3",
    // Resolve's Scripting folder, holding Modules/DaVinciResolveScript.py.
    // Empty reads RESOLVE_SCRIPT_API, else the standard macOS location.
    "script_api": "",
    // Where FCPXML timelines and marker CSV files are written.
    "export_dir": "",
    // The frame rate exports and marker seconds are counted at.
    "frame_rate": 24
  }
}
```

A frame rate outside 1-240 refuses at start, rather than in a file Resolve
will not import. Check the live half with `/resolve`: it says what Resolve
has open, or why nothing answered.

## Use it

Seven tools, and the same verbs in `resolve:` for eval:

| Tool | What it does |
| --- | --- |
| `resolve_status` | What Resolve has open: the app and version, the project, its timelines, the current timeline and frame rate, a running render. |
| `resolve_scan_media_folder` | Walk a folder for footage, music and stills, by kind, recursively. No Resolve needed. |
| `resolve_import_media` | Import files into the open project's media pool, optionally into a bin. |
| `resolve_add_timeline_marker` | Put a marker on the current timeline, in one of Resolve's marker colours. |
| `resolve_render_timeline` | Queue and start a render of the current timeline into a folder. |
| `resolve_generate_fcpxml_timeline` | Write an FCPXML 1.10 timeline: one clip a media path, with markers. Imports into free Resolve. |
| `resolve_generate_marker_csv` | Write a marker manifest as CSV: frame, seconds, name, colour, note, duration. |

Every mutation asks first. A verb called without `confirm` reports the plan and
leaves Resolve alone; only `confirm: true` imports, marks or renders. That is
the original plugin's rule, kept.

The interchange writers place a marker on the clip whose span holds its frame,
and a sequence as long as its clips; the CSV quotes a note that carries a
comma.

```lisp
(resolve:status)
(resolve:scan-media "~/Movies/Footage")
(resolve:write-fcpxml "Rough Cut 1" (list "/a.mov" "/b.mov")
                      :markers (list (list :frame 48 :name "Beat")))
(resolve:add-marker 48 :name "Beat" :color "Red" :confirm t)
(resolve:render "/exports" :custom-name "Cut 1" :confirm t)
```

`/resolve` prints the status; `/resolve scan FOLDER` lists a folder's media.
`(help :resolve)` carries the same manual.

Only the section's `script_api` and the operator's own interpreter are used:
the cell sends nothing anywhere, and Resolve answers on a local socket.

## Not ported

- **Launcher.** The original could open the Resolve app bundle for you;
  opening Resolve is one double-click, and a cell that spawns an app it does not
own earns nothing.
- **Scripted rough cut.** `create_scripted_timeline` imports clips, builds an
  empty timeline and appends them with per-clip in/out and track indices. The
  FCPXML writer carries the same shape to free Resolve, and the media-pool verbs
  cover the Studio case, so it is not a tool of its own here.
- **Append to the current timeline** and **create an empty or seeded
  timeline**: the original's two other pool verbs. `resolve_import_media`
  brings the media in; the timeline is one move in Resolve.
- **`resolve_capabilities`**: the same list of verbs now lives in
  `(help :resolve)`, where the model reads it.
- **ffprobe duration probing**: the original shelled out to `ffprobe` to size a
  clip. The FCPXML writer uses one clip length for the timeline instead.

## Test

From the cells repository:

```sh
.github/test-cell.sh tools/nodecode-davinci-resolve
```

No test needs Resolve, Python, a socket or the network: the live verbs answer
through a stubbed bridge, and the interchange writers write into a scratch
folder and are read back.

MIT licensed. See [NOTICE](NOTICE).
