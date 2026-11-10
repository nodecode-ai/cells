# nodecode-hyprland

Hyprland Computer Use: drives a Hyprland desktop for the agent. A Nodecode rewrite of [mddragon18/hyprland-computer-use](https://github.com/mddragon18/hyprland-computer-use) by [mddragon18](https://github.com/mddragon18).

The agent takes a frame of the screen or of one window, reads the coordinates
in it, and clicks, drags, scrolls, types and presses keys on the live session;
it lists the windows and focuses one by name. Hyprland is Wayland-only, so
everything is Wayland-native: `grim` (wlr-screencopy) for a frame, `wtype`
for text and keys, `wl-copy` for the paste fallback, `hyprctl` for window
metadata and focus, and the `wlr-virtual-pointer` helper built from the
original repository's `pointer/` folder for clicks, drags and the wheel. No
XWayland, no accessibility tree, no root, no `/dev/uinput`.

The backend is pixel-based, as the plugin's was: a capture carries a
coordinate ruler, the frame is downscaled to a capture width, and the cell
maps a coordinate the model read back to native pixels itself. Naming a
window in a capture focuses it, crops to it and makes it the target, and a
click outside that target is refused with the window's own box instead of
landing in the tile next door.

## Get it

Tell the agent "add the hyprland cell", or run `nodecode add nc://hyprland`.

## Set it up

Four programs must be on `PATH`: `hyprctl` (ships with Hyprland), `grim`,
`wtype` and `wl-copy` (`wl-clipboard`); `magick` (ImageMagick) is optional
and only draws the ruler and the downscale. Then build the pointer helper,
which is C source in the original repository and is not vendored here:

```sh
make -C /path/to/hyprland-computer-use/plugin/hyprland/pointer
```

and name the binary it produces in the cell's `hyprland` section, under
`/settings` or the Settings tab. A `hypr-vinput` on `PATH` is used when the
setting is empty.

| Setting | Default | What it does |
| --- | --- | --- |
| `pointer_bin` | none | the `hypr-vinput` helper: `make -C pointer` in the original repository |
| `capture_width` | 1600 | how wide a capture is; click coordinates are read in this space |
| `grid` | on | draw the coordinate ruler on a capture |
| `ruler_step` | 200 | image pixels between ruler labels |
| `scope` | `screen` | what a capture without a named window covers: the screen, or the focused window |
| `enforce_target` | on | refuse a click outside the captured window; off for a drag between two windows |
| `capture_dir` | `hyprland-captures/` | where a capture is saved |

`/hyprland` prints a doctor: which of those programs this machine has, the
pointer helper in use, the settings, and how many windows are open.

## Use it

Ask in words -- "what is on my screen", "open the browser and click the
address bar", "scroll down in the terminal" -- and the model calls the
`hyprland` tool with an `action`. Or drive it from eval, where each verb
answers text and `(look PATH)` shows a frame:

```lisp
(hyprland:status)                          ; the doctor
(hyprland:windows)                         ; class, title, bounds, pid per window
(hyprland:capture :app "firefox")          ; a PNG of that window; it becomes the target
(hyprland:click 640 320)                   ; in the frame's pixels
(hyprland:drag 400 300 :to-x 900 :to-y 700)
(hyprland:scroll "down" :amount 3)
(hyprland:type-text "hello")
(hyprland:press-key "ctrl+Return")
(hyprland:focus "firefox")
```

The tool takes the same nine actions as one contract:

```jsonc
{"action": "capture", "app": "firefox"}
{"action": "click", "x": 640, "y": 320, "count": 2}
{"action": "key", "keys": "super+q"}
```

`/hyprland windows` lists the windows; `/hyprland apps` names the classes
and how many windows each holds.

What it does on your machine, plainly:

- **Real input.** It moves the pointer, clicks, drags, scrolls, types and
  presses keys on the live session. There is no sandbox and no synthetic
  mode.
- **Screen capture goes to the model.** Every capture reads the screen
  through `grim` and hands the file to the model the session is talking to.
- **The clipboard.** Text that starts with `-`, or that `wtype` refuses, is
  pasted with `wl-copy` + ctrl+v: that replaces your clipboard and leaves
  the text in it.
- **Fixed argv, no shell.** `hyprctl`, `grim`, `wtype`, `wl-copy`, `magick`
  and the pointer helper are each called with an argument list, so a window
  title with a quote in it is data.
- **No network, no telemetry, no credentials.** The cell reads its own
  `hyprland` section and nothing else.
- **Limits.** No element tree, so act on coordinates. Modifier+click and
  modifier+drag need `/dev/uinput` and are not supported: press the key, then
  click. A cross-window drag needs `enforce_target: false`. Multi-monitor
  uses the layout bounding box.
