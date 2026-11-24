# nodecode-godot

Godot: a live Godot 4 editor, driven as tools. A Nodecode rewrite of [Tormen28/argos](https://github.com/Tormen28/argos) by [Tormen28](https://github.com/Tormen28).

ARGOS keeps the editor itself as the workspace: the scene you have open, the
nodes in it, the properties you change, the animation you play, the frame you
capture. The original does that through a Godot addon that serves a loopback
WebSocket, and an MCP server in front of it. This cell speaks the addon's wire
directly: it reads the port and the token the addon publishes under the
project's `.argos/` folder, sends one JSON request, and reads one JSON answer.
No Python and no MCP server are needed — only the addon, inside the project
you have open in Godot.

## Get it

Tell the agent "add the godot cell", or run `nodecode add nc://godot`.

## Set it up

1. Copy the original's `argos/addons/argos` folder into your project's
   `addons/` and turn **Argos** on in *Project → Project Settings → Plugins*
   (the original's installer does the same). The addon writes `.argos/port`
   and `.argos/token` in the project root when the editor opens.
2. Point the `godot` section of `config.jsonc` (or `/settings`) at that
   project root — the folder holding `project.godot`:

```jsonc
{
  "godot": {
    "project": "~/games/arena",
    "timeout_seconds": 10
  }
}
```

The section may pin `port` and `token` instead of reading the files, for an
editor started elsewhere. Nothing here is a secret worth keeping: the token is
the addon's own, and it is only ever sent to `127.0.0.1`.

## Use it

With the editor open, ask for what you want in terms of the scene:

- "add a Sprite2D under Player and point it at the idle texture" —
  `godot_node_create`, `godot_node_set`.
- "what is in this scene?" — `godot_scene_tree`, trimmed by `max_nodes` and
  `max_depth`.
- "build this HUD as one undo step" — `godot_batch`, up to 64 node edits in
  one undo group.
- "play the run animation" — `godot_anim_list`, `godot_anim_get`,
  `godot_anim_play`.
- "take a screenshot of the editor" — `godot_capture`, a PNG under
  `res://....png` or `.argos/captures/`, never over an existing file.
- "run the game and stop it" — `godot_play`, `godot_stop`.

`/godot` shows the bridge and what the editor says of itself: Godot version,
the scene that is open, whether the game is playing.

Everything the editor does here is the editor's own undo history, so
`godot_undo` reverts a batch as one step. `godot_raw` is the escape hatch for
an action no tool covers.

Two things to know: playing a scene runs that project's own GDScript, so only
play projects you trust; and screenshots need the editor to have a window (a
headless editor renders none).

## What is not here

The original's Python server also launches and quits the editor, runs
`godot --check-only` over GDScript files, keeps an operation history under
`.argos/history.json`, and captures the running game. Those need a process
launcher and the Godot binary, not the addon's bridge, so this cell leaves them
out: start Godot yourself, and use `godot_play` and `godot_capture` for
playback and stills. See NOTICE.
