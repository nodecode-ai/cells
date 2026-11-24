# nodecode-hud-teach

HUD Teach: numbered marks and a step card painted on the live Mac window, so a lesson is shown on the app itself. A Nodecode rewrite of [iamlukethedev/hermes-hud-teach](https://github.com/iamlukethedev/hermes-hud-teach) by [iamlukethedev](https://github.com/iamlukethedev).

You ask "teach me how to export this as a PNG" while the app is open. The cell
photographs that window, the model looks at the picture, and it answers by
painting on it: a numbered ring on the button to press, an arrow from the menu
to the item, and a small step card that lists the lesson, ticks what is done
and highlights what to do now. You keep the mouse; a mark shows where a
control is, it does not press it.

## Get it

Tell the agent "add the hud-teach cell", or run

```sh
nodecode add nc://hud-teach
```

It runs on a Mac: capture is `screencapture`, the window list is `osascript`,
and the overlay is a Tcl/Tk window the cell starts.

## Set it up

Two permissions, once, for the app that runs Nodecode, in **System Settings ->
Privacy & Security**:

1. **Screen Recording**, so the cell can photograph the window. Restart that
   app after granting it.
2. **Accessibility**, so it can find windows by name and follow them when they
   move.

Then name one folder in the cell's section -- where screenshots are written,
for the model to look at -- and run `/teach check`, which reports every piece
with the fix for a miss:

```sh
export HUD_TEACH_SHOTS=~/Pictures/hud-teach    # any folder you can write
```

The section, as `~/.nodecode/config.jsonc` carries it:

| Setting | Default | What it does |
|---|---|---|
| `capture_dir` | (required) | the folder screenshots are written to; the model reads one with `(look PATH)` |
| `directory` | a folder of the home | where the cell keeps the overlay script, its heartbeat and the last capture's geometry |
| `tcl` | `wish` | the Tcl/Tk interpreter the overlay runs on; `wish8.6` and `wish8.5` are tried when `wish` is not there |
| `max_edge` | `1280` | the longest edge of a capture, in pixels: smaller is cheaper and still enough to find a control |
| `ttl_seconds` | `180` | how long marks stay up when a draw names no time |
| `idle_exit_seconds` | `1800` | how long the overlay waits with nothing on screen before it quits; the next draw starts it again |

A Mac without Tcl/Tk loads the cell and cannot paint: `/teach check` says so,
and `brew install tcl-tk` (or naming a `wish` in the `tcl` setting) fixes it.

## Use it

Ask in plain language while the app is visible:

- "I'm playing Chess. Show me a good move for white."
- "Where do I turn on Night Shift? Show me on screen."
- "Walk me through adding a reverb send, one step at a time."

The model has one tool, `teach_screen`:

| Action | What it does |
|---|---|
| `capture` | Photograph a window (`target` = app or title substring; the frontmost app's largest by default) and answer the picture's path, pixel size and the window's rect. |
| `draw` | Paint `shapes` in the capture's pixels, optionally with a lesson: `title`, `steps`, `current_step`, `legend_position`. |
| `clear` | Take the marks off. |
| `list` | The visible windows, with app, title and position. |
| `status` | The overlay, and the capture a draw would use. |
| `check` | Every permission and program a first run needs, each with its fix. |

Shapes: `circle` (x, y, radius), `rect` (x, y, width, height), `arrow` and
`line` (fromX, fromY, toX, toY), `polyline` (points), `label` (x, y, text).
Each takes `step` 1-12, a `color`, a short `label` caption and `dashed`; a
rect takes `fill`. Up to 24 marks; `ttl_seconds` defaults to 180 and is at
most 900. A mark far outside the picture it claims is refused, a capture id
older than the newest capture is refused, and a window that was resized since
the capture is refused -- a window that merely moved is followed.

From the operator's side, `/teach`:

| Command | What it does |
|---|---|
| `/teach check` | what is missing, and how to fix it |
| `/teach demo` | a sample lesson on the frontmost window, or `/teach demo Chess` |
| `/teach` or `/teach status` | the overlay and the last capture |
| `/teach clear` | marks off |
| `/teach stop` | the overlay process, too |

`(help :teach)` carries the whole lesson loop. The marks stay up while the user
follows them, a click on them takes them off, a new draw replaces them, and
they clear themselves at the TTL, on `/teach clear`, and when the cell stops.
