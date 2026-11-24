# nodecode-orcaslicer

OrcaSlicer: search the installed presets, slice a model, read the job, and check or load a printer.
A Nodecode rewrite of [mazzanfar/hermes-orcaslicer](https://github.com/mazzanfar/hermes-orcaslicer)
by [mazzanfar](https://github.com/mazzanfar).

The stock OrcaSlicer on your machine does the slicing; this cell drives it and
keeps the job. It gives the model seven tools:

| Tool | What it does |
| --- | --- |
| `orca_diagnose` | The OrcaSlicer it found, whether that build's command line carries every flag slicing needs, the preset roots it reads, the printers configured. |
| `orca_presets` | Search the installed machine, process and filament presets, for any manufacturer, and answer exact paths. |
| `orca_slice` | Slice one model or one plate into a job: a copy of the input, the resolved presets, `slice.log`, the artifacts with their sha256, and what the G-code's own comments report. |
| `orca_job` | One job's record, as it stands. |
| `orca_export` | Copy an artifact to a new path for an SD card or a stick. It never overwrites a file. |
| `orca_printer_status` | One read-only look at a configured printer: state, readiness, progress, temperatures, the loaded file. |
| `orca_upload` | Upload a job's G-code to an idle printer. It never starts printing. |

`/orca` reports what the cell can see without slicing or printing.

## Get it

Tell the agent "add the orcaslicer cell", or run `nodecode add nc://orcaslicer`.

## Set it up

Nothing is required: when OrcaSlicer sits on `PATH` or at a desktop install's
own path, the cell finds it, and a preset search reads the install's presets.
The section `~/.nodecode/config.jsonc` may carry is:

```jsonc
{
  "orcaslicer": {
    "slicer": "/usr/bin/orca-slicer",          // the executable itself, not the .app
    "profiles": ["/usr/share/OrcaSlicer/resources/profiles"],
    "state_dir": "~/.nodecode/orcaslicer/",     // where jobs and artifacts live
    "printers": [
      {"name": "voron", "kind": "moonraker", "url": "http://printer.lan:7125",
       "api_key_env": "ORCA_VORON_KEY"}
    ],
    "slice_timeout": 1800, "printer_timeout": 20, "max_presets": 30
  }
}
```

A printer is `moonraker`, `octoprint` or `prusalink`, with its own base URL.
Its key is named, never written: `api_key_env` must be an `ORCA_` or
`HERMES_ORCA_` variable, so a printer URL can never be handed another
application's token, and the cell reads it at call time. A printer's slice is
reached over the address you gave; nothing else is contacted.

## Use it

Ask for the printer you have, not a preset name from memory:

> which PLA process presets are installed for a 0.4 nozzle?
> slice ~/Models/bracket.stl for the Voron at 0.2mm with three walls
> what did the last slice report, and is the printer free?
> upload that job — then I will start it at the printer

The first answer is a path to pass on. Slicing is one call that returns when
OrcaSlicer exits, so the job's state is always the truth: `sliced` or
`failed`, with the log beside it. A failure says what the slicer said.

**What a slice is not.** Slicing success is not a print: the cell cannot see
the bed, the filament or the toolpath. `orca_job` reports the slicer's own
estimates (time, filament, layer count) from the G-code's comments, and the
rest is for a person to check.

## What it does not do

- No Bambu LAN, Duet or Flashforge printers: Moonraker, OctoPrint and
  PrusaLink only. A Bambu printer is reached by exporting the file and using
  OrcaSlicer's own upload.
- No per-object process settings or per-instance move/rotate/scale inside a
  3MF: the cell changes global process settings (`layer_height`,
  `initial_layer_print_height`, `wall_loops`, `top_shell_layers`,
  `bottom_shell_layers`, `sparse_infill_density`, `enable_support`), each
  within its bound, and leaves the rest to the preset or the project.
- No camera snapshot, no SVG layer preview, and no "open it in OrcaSlicer for
  review" — open the artifact yourself.
- `changes` ride a partial settings file over the presets or the project, so
  inspect the sliced artifact before printing.

## Test

From the cells repository's root:

```sh
.github/test-cell.sh tools/nodecode-orcaslicer
```

No OrcaSlicer runs and no printer is dialled: the slicer is a stubbed
function that writes the G-code a real slice would, and every printer request
is a stubbed `nlk:http`.

MIT licensed; see [NOTICE](NOTICE).
