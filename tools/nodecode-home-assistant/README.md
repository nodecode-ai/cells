# nodecode-home-assistant

Home Assistant: four tools that read and control the devices of your own Home Assistant instance. A Nodecode rewrite of [NousResearch/hermes-official-plugins](https://github.com/NousResearch/hermes-official-plugins) by [NousResearch](https://github.com/NousResearch).

The tools are the original plugin's, with the same names and arguments:

| Tool | What it does |
| --- | --- |
| `ha_list_entities` | The instance's entities, filtered by domain, area, name or an exact list, and capped. |
| `ha_get_state` | One entity's state and every attribute, with Home Assistant's own UTC timestamps. |
| `ha_list_services` | The actions the instance offers, per domain, with the fields each takes. |
| `ha_call_service` | Calls one service: turn a light on, set a thermostat, run a scene. |

Ask "which lights are on in the kitchen?", "is the front door locked?", "set the office thermostat to 21" — the model finds the entity with `ha_list_entities` and acts with `ha_call_service`.

## Get it

Tell the agent "add the home-assistant cell", or run `nodecode add nc://home-assistant`.

## Set it up

The cell talks to the instance you already run, over its REST API. It needs two things:

1. **A long-lived access token.** In Home Assistant: your **Profile** → **Security** → **Long-Lived Access Tokens** → **Create Token**, and copy it.
2. **The instance's base URL**, scheme included, e.g. `http://homeassistant.local:8123` or `https://ha.example.com`.

Then, in `/settings` (or in the `homeassistant` section of `config.jsonc`):

```jsonc
"homeassistant": {
  "url": "http://homeassistant.local:8123",
  "token_env": "HASS_TOKEN",   // or a file: "token_file": "~/.config/hass-token"
  "timeout": 15
}
```

A token is named, never written into the config: `token_env` holds the name of the environment variable that carries it (export `HASS_TOKEN` in the shell that starts Nodecode), and `token_file` a file only you can read. Set exactly one.

Optional, and off by default:

| Setting | What it does |
| --- | --- |
| `entity_allowlist` | Entity ids or patterns (`climate.*`, `sensor.office_*`) the tools are limited to. |
| `entity_denylist` | Patterns the tools always exclude; it wins over the allowlist, and a denied entity is refused before any request. |
| `max_entities` | A cap on `ha_list_entities` answers. |

An answer cut by a cap says so, and how to narrow the query.

## Use it

You do not call the tools; the model does, when a question is about the house. A person can call the same functions from eval, in the `home-assistant` package:

```lisp
(home-assistant:list-entities (nlk:make-json-object "domain" "light"))
```

`(help :home-assistant)` is the model's manual for the four verbs, kept while the cell runs.

### What the guards are

Home Assistant has no service-level access control, so the cell keeps the original plugin's guards:

- **Blocked service domains.** `shell_command`, `command_line`, `python_script`, `pyscript`, `hassio` and `rest_command` run code on the Home Assistant host or reach your local network, so `ha_call_service` refuses them before anything is sent.
- **Names are checked.** Domain and service names must match `^[a-z][a-z0-9_]*$` and entity ids `^[a-z_][a-z0-9_]*\.[a-z0-9_]+$`, which rules out path traversal into other REST endpoints (`shell_command/../light`).
- **The operator's lists are a final veto.** With `entity_allowlist` or `entity_denylist` set, only permitted entities are listed, read and controlled, and a call that targets by area, device, floor or label — or names no entity at all — is refused, because Home Assistant resolves those on the server where the lists cannot see them. `notify.*` and `persistent_notification.*` are exempt; `script.<name>` is checked as the entity `script.<name>`.
- **An empty service answer is explained.** Home Assistant answers only the states that changed, so "nothing changed" is read back and reported as a missing entity, an entity in another domain, or a state already reached — never as a silent success.

For defense in depth, give Nodecode a Home Assistant user whose token can only reach what it needs: these lists guard the tools, not the token.

### What the rewrite leaves out

The original plugin is also a gateway adapter: it subscribes to the instance's WebSocket event bus, forwards `state_changed` events into the conversation, delivers replies as persistent notifications, and serves cron's `deliver=homeassistant`. Those are the Hermes gateway's surfaces, and this cell has none of them — it is the four tools. Two smaller things the original does are not here either: the area/device registry lookup over the WebSocket (so `area` matches the room word in an entity's friendly name, its area attribute or its id, not the area registry), and the local-timezone twins of `ha_get_state`'s timestamps (the answer carries Home Assistant's own UTC values).

## License

MIT. The tools, their schemas and their guards are NousResearch's, rewritten in Common Lisp: see `NOTICE` and `LICENSE`.
