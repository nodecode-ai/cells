# nodecode-qrblox

Qrblox: draft QR codes, audio codes and menus on a merchant account, and read their scan analytics. A Nodecode rewrite of [Terahertz-Inc/qrblox-mcp](https://github.com/Terahertz-Inc/qrblox-mcp) by [Terahertz-Inc](https://github.com/Terahertz-Inc).

## Get it

Tell the agent "add the qrblox cell", or run `nodecode add nc://qrblox`.

## Set it up

Qrblox keeps every code on the merchant's own workspace, and its MCP server asks for that merchant's key:

1. Sign in at `create.qrblox.com`, open **Settings → API keys** and mint one. Keys read `sk-qrblox-...` and are shown once.
2. Paste it into the `qrblox` section's `api_key` in `/settings`, or name the variable holding it in `api_key_env` (the plugin read `QRBLOX_API_KEY`), or write it into the file `api_key_file` names.

`server_url` defaults to `https://api.qrblox.com/mcp/v1`, the endpoint the plugin's `mcp.json` declares. The repository's README quotes an older path, `https://app.qrblox.com/api/mcp`; set `server_url` to it if that is the address you were given. `/qrblox key` says, in words, where the key in use came from — never the key itself.

A key that is missing, wrong or revoked is refused with one sentence naming `api_key`, not a stack trace. Without any key the cell refuses to start, as the original offered no merchant tool until `QRBLOX_API_KEY` was set.

## Use it

| Tool | Use |
| --- | --- |
| `qrblox_ping` | check the server is up and what the key reaches; call it first |
| `qrblox_codes` | the recent QR codes on the account, with the addresses they open |
| `qrblox_create` | draft a QR code from `destination_url` and `title` |
| `qrblox_audio` | draft an audio code (a near-ultrasonic payload) |
| `qrblox_menu` | `op list` the menus, `op create` a new one, `op add_item` extend `menu_id` |
| `qrblox_scans` | the scan totals and the by-day series |
| `qrblox_call` | any tool the server offers, by name; with no tool, its current tool list |

From the keyboard, `/qrblox` lists the codes on the account, `/qrblox ping` is the health call, and `/qrblox key` says where the key came from. The model reads the workflow with `(help :qrblox)`; the one-line summary rides every request.

Every tool takes an optional `arguments` object, passed to the server as-is for a member these tools do not name (a style, a campaign, a date range). The server's other tools — `qrblox.merchant.get_profile`, `qrblox.challenge.create` for scan streaks and loyalty campaigns, the voice-agent directory lookups — are one `qrblox_call` away, and `qrblox_call` with no tool answers the list.

Creations are drafts: the server's own security model leaves publishing and billing with the merchant in the dashboard. The cell says so in the manual topic, so the model reports a draft, not a live code.

### The seven tools are the plugin's MCP server

The original was a manifest and a skill: `plugin.json`, an `mcp.json` for `https://api.qrblox.com/mcp/v1` (streamable HTTP, `Authorization: Bearer ${QRBLOX_API_KEY}`) and `skills/qrblox/SKILL.md`. This cell speaks that server's JSON-RPC itself — `initialize`, the `Mcp-Session-Id`, `tools/list`, one `tools/call` per verb, and an answer that is either a JSON object or a one-event stream — through `nlk:http`, and keeps the server's own tool names under purpose-named tools.

## Not ported

- **The skill as a file.** README.md, SKILL.md and the guides are one manual topic, `(help :qrblox)`, because that is what the model reads while the cell runs.
- **`plugin.json` and `mcp.json`.** The cell contract replaces both with a section and one declaration; the endpoint they named is the section's default `server_url`.
- **The keyless REST endpoints** (`POST /api/generate-qr-code`, `POST /api/bulk-qr-codes`). They are not MCP tools, they are limited per address per day, and the bulk one needs the same key; this cell goes through the MCP server, which is what the plugin declared.
- **`qrblox.merchant.get_profile`, `qrblox.challenge.create` and the voice-agent directory lookups as named tools.** They are reached with `qrblox_call`, so a tool this rewrite does not know about is never a wall.
- **The `enabled` key of the Agent Plugins manifest.** Turning the folder off is `/cells`, as for every cell.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-qrblox
```

Every request is a stubbed `nlk:http` answering as a local JSON-RPC server, so nothing reaches Qrblox and no key is needed.

MIT licensed, as the original is. See `NOTICE`.
