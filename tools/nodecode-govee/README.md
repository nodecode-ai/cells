# nodecode-govee

Govee Lights: discover and control Govee lights, and check every change with a state read-back. A Nodecode rewrite of [promptclickrun/hermes-govee](https://github.com/promptclickrun/hermes-govee) by [promptclickrun](https://github.com/promptclickrun).

One tool, `govee_lights`, and one command, `/govee`. It talks to Govee's own developer API and nothing else, using the key you make yourself.

## Get it

Tell the agent "add the govee cell", or run `nodecode add nc://govee`.

## Set it up

Make a developer API key in the Govee Home app under Profile > Settings > Apply for API Key; Govee emails it to you. Then either export it:

```sh
export GOVEE_API_KEY=...
```

or set it in `/settings` (or the page's Settings tab) under `govee`: `api_key` for the key itself, or `api_key_env` for the name of the variable holding it. A key pasted into the panel is kept in a 0600 file under `~/.nodecode/secrets/`; the folder holds no secret.

Nothing else is required. A key that answers nothing is refused at the first call, with what to do about it. `(help :govee)` is the model's guide to the tool.

## Use it

Ask for what you want and the model picks the action:

| Action | What it does |
| --- | --- |
| `list` | every light: name, exact device ID, model, and the capabilities that model advertises |
| `state` | one light's power, brightness, colour, white temperature and whether it is online |
| `scenes` | the light's static and dynamic scenes, with the value to send back |
| `power` | on or off (`true` / `false`) |
| `brightness` | a percentage, inside the range that light advertises |
| `color` | `#RRGGBB` |
| `temperature` | white temperature in Kelvin, inside that light's range |
| `scene` | one scene by its exact name |

```json
{"action": "list"}
{"action": "brightness", "device": "Desk Lamp", "value": 40}
{"action": "color", "device": "Desk Lamp", "value": "#FF8800"}
{"action": "scene", "device": "Desk Lamp", "value": "Aurora"}
```

`device` takes an exact device ID from `list`, or a name that matches one light exactly (case-insensitively). A partial or shared name is refused, so the wrong light is never the one that changes.

From the keyboard, `/govee` lists your lights with their ids and `/govee <name>` reads one light's state.

### Two words the answer keeps apart

A command is checked against what the light itself advertises before it is sent, and its state is read back afterwards, twice, about two seconds apart. `accepted` means Govee took the command; `verified` means the state came back matching. `(help :govee)` tells the model to say a light changed only when `verified` is true, and to say so plainly when it is not.

Govee allows 30 state and scene reads a minute per device, so this is not a tool for animation or rapid polling, and a failed command is never resent on its own.

Lights only: Govee's heaters, plugs and other appliances are deliberately left out.
