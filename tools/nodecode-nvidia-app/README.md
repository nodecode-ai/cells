# nodecode-nvidia-app

NVIDIA App: driver status, game optimisation and In-Game Overlay recording through the app's own local MCP server. A Nodecode rewrite of [NousResearch/hermes-nvidia](https://github.com/NousResearch/hermes-nvidia) by [NousResearch](https://github.com/NousResearch).

NVIDIA App (Windows, 11.0.0 and later) runs an MCP server of its own on loopback, hosted by
NvContainer, and publishes where it listens in a file it rewrites whenever it changes. The original
plugin declared that server to Hermes; this cell is the client itself, so there is no connector to
declare: it reads the published address and token again on every connection, and its twelve tools
are model-facing tools.

## Get it

Tell the agent "add the nvidia-app cell", or run `nodecode add nc://nvidia-app`.

## Set it up

1. Install NVIDIA App, start it, and turn on MCP server access in its Settings.
2. Leave the section alone in the ordinary case. The app publishes its address and token in
   `%LOCALAPPDATA%\NVIDIA Corporation\NVIDIA App\McpServer\server.json`, and the cell reads them
   there. A restart of the app picks a new port and a new token, and the cell follows it with
   nothing to change.

Only when the app keeps its file elsewhere, or you want to name the address yourself:

```jsonc
"nvidia-app": {
  "enabled": true,
  "server_json": "D:/NVIDIA/server.json",   // where the app published its address and token
  "url": "http://127.0.0.1:47985/mcp",      // only when the app publishes nothing
  "timeout_seconds": 30                     // one call's ceiling
}
```

The app's published file wins over `url` and `token`, because a pinned port is a client dialling
nothing after the next restart. Then `(restart-cells "nodecode-nvidia-app")`.

No key of yours is needed, and nothing leaves the machine: every call goes to the app's own
loopback server.

## Use it

Ask for what the app does:

- "What driver do I have, and is there an update?"
- "Which of my games has NVIDIA App optimised, and what does Balanced do for this one?"
- "Start recording." / "Stop recording and save it." / "Take a screenshot."
- "Is Instant Replay on?" "Turn on the Statistics Overlay." "What am I playing?"
- "Set Whisper Mode to Quiet on battery."

The model gets thirteen tools:

| Tool | What it does |
| --- | --- |
| `nvapp_list_tools` | what this build exposes (it varies by build and access level) |
| `nvapp_client_get_applications` | games and applications NVIDIA App found, with their OPS details |
| `nvapp_client_launch_application` | launch one, by the exact game name |
| `nvapp_client_get_driver_status` | installed driver, recommendations, rollback driver |
| `nvapp_client_get_driver_release_notes` | one driver flavour's highlights |
| `nvapp_client_set_game_optimization` | apply a preset to a game, or revert it |
| `nvapp_client_get_laptop_features` | Battery Boost and Whisper Mode |
| `nvapp_client_set_laptop_feature` | set either, globally or for one game |
| `nvapp_overlay_get_status` | the In-Game Overlay's best-effort state |
| `nvapp_overlay_capture` | record, Instant Replay, Highlights, a screenshot |
| `nvapp_overlay_configure_stats` | show or hide the Statistics Overlay |
| `nvapp_overlay_configure_filters` | RTX Dynamic Vibrance on or off |
| `nvapp_overlay_get_current_game_info` | the game the Overlay identified |

From eval: `(nvapp:status)`, `(nvapp:tools)` and
`(nvapp:call-tool "nvapp_overlay_get_status")`. `/nvidia` answers the connection line,
`/nvidia status` reads the Overlay, the current game and the driver, `/nvidia tools` lists what
the build exposes, and `/nvidia <tool>` calls one with no arguments.

The Overlay workflow is restricted, as the original skill restricted it: five capture actions, the
`rtx_dvc` filter, and the statistics overlay's visibility. Desktop capture is never toggled, a
toggle must name its target state, and a save or a screenshot takes none.

## Left out

The plugin's Hermes pieces: the `plugin.json` manifest, the MCP server declaration and liveness
block (this cell is the client, and it reads the app's published file itself), and the skill's
deferral to Hermes's catalog, `tool_search` and `tool_describe` — this cell asks the server for its
tool list with `nvapp_list_tools`. The original's first-run PowerShell probe is guidance in the
manual rather than a tool.

## License

MIT. The original plugin is Copyright (c) Nous Research: see `LICENSE` and `NOTICE`.
