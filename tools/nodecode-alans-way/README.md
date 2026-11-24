# nodecode-alans-way

Alan's Way Computer: drive a Mac app's windows, controls and keyboard from a Nodecode session, through Alan's Workspace. A Nodecode rewrite of [capthvnsen/alans-way-agents](https://github.com/capthvnsen/alans-way-agents) by [capthvnsen](https://github.com/capthvnsen).

The original is a backend for Hermes's stock `computer_use` tool. Here the same Alan's Workspace router answers nine `computer_*` tools instead: it lists the apps, reads one app's window as a numbered list of its controls, and presses, types into, drags or scrolls those numbers. When the user's own computer is offline the router drives the VM's desktop instead, and every reply says which host answered.

## Get it

Tell the agent "add the alans-way cell", or run:

```
nodecode add nc://alans-way
```

## Set it up

Alan's Way gives you a workspace router: an MCP stdio server, a `.mjs` script that its own `setup-workspace.sh` writes into the profile. This cell starts that script with `node`, so the section needs two things and nothing else:

```jsonc
{
  "alans-way": {
    "router_args": ["<the workspace-router.mjs that Alan's Way's setup wrote>"],
    "router_command": "node"
  }
}
```

`call_timeout` (60 s) bounds one call, the router's start-up included, and `screenshot_dir` moves where a vision capture writes its PNG (under the home's `computer/` directory by default). A section with no `router_args` refuses at start, and `/cells` says so.

## Use it

The model drives it; you can watch:

- `computer_apps` lists the apps and the host that answered. Start there, and again after a reply says the host changed.
- `computer_capture` with `app` (a name from that list, or part of one) or `pid` reads one window and numbers its controls. Those numbers are what every later gesture takes, until the next capture.
- `computer_click` (element, or x and y), `computer_set_value` (element and value), `computer_type`, `computer_key`, `computer_scroll` and `computer_drag` act on them; `computer_focus` aims the cell at an app and launches it when it is not running.

`/computer` says what the cell is driving, and `/computer apps` lists the apps.

Approvals are the host's own: the router refuses input into an app the user is using right now, and a keychain, a password manager or a password field stays off limits. Pass `delivery_mode: "foreground"` when the user should see the window come forward first; the background leaves their typing where it is.

## What the rewrite does not do

- The element list is read whole at every capture, and an action does not fold the reply's changes back into it; the reply says what the router saw and asks for a fresh capture before acting by number again.
- No screenshot geometry conversion: a vision capture writes the PNG and lists screen-point bounds; coordinates are screen points.
- One router child per call rather than one live child per session; a gesture costs a `node` start-up.

MIT, as the original is; see LICENSE and NOTICE.
