# nodecode-nvidia-broadcast

NVIDIA Broadcast: ten tools that turn the app's camera, microphone and speaker effects on and off, tune them, pick devices and camera resolutions, and process local media files. A Nodecode rewrite of [NousResearch/hermes-nvidia](https://github.com/NousResearch/hermes-nvidia) by [NousResearch](https://github.com/NousResearch).

NVIDIA Broadcast runs an MCP server on loopback, so this cell is an MCP client in Common Lisp: it speaks JSON-RPC to the app itself, with no key and nothing leaving the machine. The tools are the original plugin's, with the same names, arguments and answer shapes:

| Tool | What it does |
| --- | --- |
| `get_broadcast_state` | The effects that are on now, grouped by camera, microphone and speaker. |
| `set_effects` | Turns effects on and off and tunes them, all in one call; or restores the Live snapshot file processing suspended. |
| `describe_effects` | What each effect does, its badges, its availability and the parameters it takes. |
| `list_devices` | The cameras, microphones and speakers, the active one, and a camera's available resolutions. |
| `set_active_device` | Switches a device, and a camera's resolution with it, in one call. |
| `set_camera_resolution` | Changes the active camera's width and height. |
| `submit_file_processing` | Queues one local media file with the effects to apply. |
| `get_file_processing_jobs` | One job, or the recent ones, as a snapshot. |
| `wait_for_processing_update` | The one wait: for job-status and queue-pause changes. |
| `control_file_processing_jobs` | Pauses or resumes the queue, cancels or retries jobs. |

Ask "blur my background and clean up the mic", "what is my camera set to?", "which microphones can I use?", "process this recording with the same effects" — the model reads the state, changes what it needs in one call, and reports what applied and what did not.

## Get it

Tell the agent "add the nvidia-broadcast cell", or run `nodecode add nc://nvidia-broadcast`.

## Set it up

**NVIDIA Broadcast 2.3 or newer, running in your desktop session** (Windows). The gateway first ships in 2.3; on 2.2 and older there is no gateway at all, so nothing answers. The tools appear while the app runs.

There is no key: the gateway binds loopback and grants any local client the full surface. The cell's section is one endpoint and one timeout:

```jsonc
"nvidia-broadcast": {
  "url": "http://127.0.0.1:18100/gateway",
  "timeout": 60
}
```

`18100` is the default and is right on almost every install. When the app cannot take it, it binds the first free port from `18101` and records the port it took; set `url` to that port. If a call answers that the gateway did not answer at all, the app is not running, or not installed, or older than 2.3.

## Use it

You do not call the tools; the model does, when a request is about sound, camera or video. A person can call the same functions from eval, in the `nvidia-broadcast` package:

```lisp
(nvidia-broadcast:broadcast-state
  (nvidia-broadcast:decode-state-read (nlk:decode-json "{\"effect_type\": \"camera\"}")))
```

`(help :nvidia-broadcast)` is the model's manual for the ten verbs and how to read their answers, kept while the cell runs.

### What the rewrite leaves out

The original plugin is an MCP declaration plus a skill, and the skill also did system work around the app: it read the installed build's `ProductVersion`, launched the app hidden when it was not running, discovered a non-default port from the port record the app writes, and, with an explicit yes, downloaded NVIDIA's own installer from NVIDIA's update service, checked its SHA-512 and its NVIDIA signature, and opened it for the user. Those are the operator's side of the original, and this cell is the ten tools: the app has to be installed, at 2.3 or newer, and running. What remains of the port record is the `url` setting.

## License

MIT. The tools, their names and their answer shapes are NousResearch's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
