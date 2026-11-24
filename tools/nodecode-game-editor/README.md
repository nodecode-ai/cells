# nodecode-game-editor

Game Editor: read a Godot 4 project from the files in its folder - scene trees, scripts, project settings and the files there are. A Nodecode rewrite of [TakeshiTGAL/hermes-plugin-game-editor](https://github.com/TakeshiTGAL/hermes-plugin-game-editor) by [TakeshiTGAL](https://github.com/TakeshiTGAL).

## Get it

Tell the agent "add the game-editor cell", or run `nodecode add nc://game-editor`.

## Set it up

Name the project folder in the `game-editor` section of the config - the folder that holds `project.godot`:

```jsonc
"game-editor": {
  "project": "~/projects/lantern-run",
  "read_lines": 200,
  "read_chars": 6000
}
```

`read_lines` is how many rows or lines one answer carries when the call does not ask for fewer, and `read_chars` cuts a long answer with a line counting what it left. `/settings` writes both, and `(restart-cells "nodecode-game-editor")` picks up a hand-edited config.

## Use it

The model gets four tools, and you get one command. Nothing here opens Godot, writes a file or calls the network: every answer comes from `project.godot`, a `.tscn`, a `.gd` or a `.cs` under the folder you named, and unsaved edits in an open editor are not in it.

| Tool | What it answers |
| --- | --- |
| `scene_look` | a `.tscn` file: `scene_tree` (the node tree, with each node's type, groups, instanced scene and script), `find_nodes` (by name, type or group), `children`, `properties` (the values the file stores for a node), `groups`, `signals` (the connections). `path` is a node path from the root, `/Main/Player`; `scene` is a res:// path, the main scene when left out |
| `script_look` | a `.gd` or `.cs` file: `source` (numbered lines, `offset` and `limit`) or `outline` (every class, signal, exported value, constant and function declared at column zero, with its line number) |
| `file_look` | `text` (one res:// file, numbered), `files` (what is there, by name and type), `resources` (by the type each file declares) |
| `project_look` | `get` (one setting, `section/name`), `autoloads`, `input_map` (each action and the keys bound to it), `summary` |

`/game-editor` answers the project at a glance - its name, main scene, autoloads, input actions and how many scenes and scripts are on disk - and its panel: **Scenes**, **Scripts**, **Project settings**, and a file name to find. `/game-editor scenes`, `/game-editor scripts`, `/game-editor settings` and `/game-editor NAME` answer directly. Through eval the same four reads are functions in the `game-editor` package, each answering a string.

```
> what is in the player scene?
[scene_look {op: scene_tree, scene: res://scenes/main.tscn}]
scene res://scenes/main.tscn: 4 nodes, 2 connections, format 3 uid uid://c8lantern
/Main (Node2D)
  /Main/Player (CharacterBody2D) groups: player, damageable script res://player.gd
    /Main/Player/Sprite (Sprite2D)
  /Main/Coin instance of res://scenes/coin.tscn
```

A path that climbs out of the project (`res://../secrets`) or goes through a hidden folder (`.godot/`) is refused, `user://` - the game's player-data folder - is refused, and so is any file that is not a scene or a script where one is asked for. Every refusal says which rule and which path.

## What is not here

The original plugin publishes eight tools. The other four drive a running Godot editor through the Godot AI add-on: `editor_look` (live editor state), `asset_look` (materials, animations, particles), `plan_change` and `apply_plan` (a plan the user approves, which writes the project), plus playing, testing and exporting it. This cell reads the same project from disk and stops there, so it needs no Godot, no add-on, no Python and no port; see NOTICE for what that leaves out.
