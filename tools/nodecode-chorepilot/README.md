# nodecode-chorepilot

ChorePilot: the household's devices and chore board as tools, through the user's own ChorePilot server. A Nodecode rewrite of [WestWaters/ChorePilot](https://github.com/WestWaters/ChorePilot) by [WestWaters](https://github.com/WestWaters).

ChorePilot keeps the house and its chore board; this cell is a thin client of
its MCP server on the same machine, the LAN or Tailscale. ChorePilot still
decides everything from the person the token belongs to: reads, lights, the
vacuum and the AC in the house's band run; a water or heat action (dishwasher,
washer, sprinkler, oven) waits for that person's own yes on their phone; a
kid's request goes to a parent's approval queue; unlocking, opening the garage
and disarming are never available to an agent.

## Get it

Tell the agent "add the chorepilot cell", or run `nodecode add nc://nodecode-chorepilot`.

## Set it up

On the ChorePilot machine, make a token (it is shown once; ChorePilot keeps it
hashed):

```sh
chorepilot mcp token add <person> --client nodecode
```

Keep ChorePilot's MCP server running there: `chorepilot run`, or
`chorepilot mcp --http` (add `--lan` for another machine on your network, or
put `tailscale serve --bg 8770` in front of it).

Then set the `chorepilot` section — in `/settings`, or in
`~/.nodecode/config.jsonc`:

| Member | What it is |
| --- | --- |
| `url` | the server's address: `http://127.0.0.1:8770/mcp` on the same machine, `http://<lan-host>:8770/mcp` on the LAN, `https://<host>.<tailnet>.ts.net/mcp` over Tailscale |
| `token_env` | the environment variable holding the token, e.g. `CHOREPILOT_TOKEN` |
| `token_file` | or the file holding it, 0600 |
| `timeout_seconds` | how long one call may wait, a confirmation included (default 150; ChorePilot itself gives up after 120) |
| `preload_tools` | list the house's tools at start, so `chorepilot_call`'s description names them (default true; a house that is down costs the start two seconds) |

Exactly one of `token_env` and `token_file`. Plain `http://` is accepted only
to this machine or a private network (LAN, Tailscale, `*.local`, `*.ts.net`);
anything else needs `https`, or the token would cross the internet in the
clear. A section with neither token member refuses at start, and the cells
report says what the section still needs.

## Use it

Ask "what's on the chore board?", "is the dishwasher done?", "turn the
living-room light off". The model calls:

| Tool | What it does |
| --- | --- |
| `chorepilot_status` | a device's state, or the whole house's. Read-only. |
| `chorepilot_chores` | the chore board, optionally for one person and one scope. Read-only. |
| `chorepilot_who_has` | who has a chore this week or next, and when it was last done. |
| `chorepilot_done` | mark a chore done, by the token's person unless another is named. |
| `chorepilot_whoami` | which household person this connection acts as, and what an agent may do. |
| `chorepilot_tools` | every tool this house offers, with its arguments. |
| `chorepilot_call` | any other house tool by name, e.g. `{"tool": "set_light", "arguments": {"device": "living_room_light", "action": "turn_on"}}` |

For water or heat, the answer comes back when the person has said yes on
their own phone, and the tool says so in a `note`; if they did not, the note
says nothing was done. `(help :chorepilot)` carries the same rules for the
model. The operator can also type:

```
/chorepilot              who this connection is
/chorepilot status [device]
/chorepilot chores
/chorepilot who <chore>
/chorepilot done <chore>
/chorepilot tools
```

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The seven tools, `/chorepilot`, the manual, and the `chorepilot` section they run on. |
| `mcp.lisp` | The MCP client: initialize, `tools/list`, `tools/call`, the session id, and the plain-http check. |
| `test/` | Offline tests over a stubbed `nlk:http` and a scripted house. |

## License

Apache-2.0, the original's licence: the LICENSE file is ChorePilot's,
unchanged. See `NOTICE` for what was rewritten and what was left out.
