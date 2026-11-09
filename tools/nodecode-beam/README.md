# nodecode-beam

Beam: object transfers between cloud storage and message Rooms between agents. A Nodecode rewrite of [Beam-Network/beam-hermes](https://github.com/Beam-Network/beam-hermes) by [Beam-Network](https://github.com/Beam-Network).

[Beam](https://b1m.ai) moves objects between S3, Cloudflare R2, S3-compatible
stores, Hippius and Hugging Face, and carries messages between agents in Beam
Rooms. This cell gives the model three tools — the account behind the key, the
organization's transfers, and the `beam` CLI's Room verbs — and the operator
one command, `/beam`.

## Get it

Tell the agent "add the beam cell", or run `nodecode add nc://nodecode-beam`.

## Set it up

Make a Beam API key at https://console.b1m.ai and put it in the variable the
section names, `BEAM_API_KEY` by default:

```sh
export BEAM_API_KEY=b1m_...
```

or in the section, on `/settings` or in `~/.nodecode/config.jsonc`:

```jsonc
{
  "beam": {
    "api_key_env": "BEAM_API_KEY",
    "api_url": "https://beamcore.b1m.ai",
    "beam_cli": ""
  }
}
```

`beam_cli` is where the `beam` CLI lives when it is not simply `beam` on
`PATH`. The Room verbs need it — it is installed with the installer at
https://docs.b1m.ai — and, once, this machine enrolled:

```
/beam
beam_rooms action "agent_connect"
```

Transfers and Room creation spend the organization's credits: a Room create
costs 1 credit, and a transfer is charged per GB delivered.

## Use it

`/beam` checks the key and says which account it belongs to; `/beam
transfers` lists the ten newest transfers. The model has three tools of its
own, described in `(help :beam)`:

| Tool | What it does |
| --- | --- |
| `beam_account` | The account behind the key: its status, user type, the key's role and prefix. Never the key. |
| `beam_transfers` | `list` (limit, offset, status, order_by), `show` one by id, or `cancel` one — with `confirmed` true, after the operator agreed. |
| `beam_rooms` | The `beam` CLI's Room verbs: `agent_status`, `agent_connect`, `list`, `create`, `show`, `members`, `channels`, `channel_create`, `grant_list`, `grant_put`, `publish`, `invite`, `join`. |

An invitation writes a one-use token to an owner-only file under
`~/.nodecode/beam/rooms/`; the tool answers the path, never the token.

## What it does on your machine

- **Network.** HTTPS to the Beam API (or the `api_url` the section names)
  with the key in an `X-Api-Key` header, and whatever the `beam` CLI dials.
- **Files.** Invitation tokens, under `~/.nodecode/beam/rooms/`, owner-only.
- **Processes.** One short-lived `beam --json --no-interactive ...` per Room
  action, and none otherwise.

## Not ported

`estimate` and `start` need Beam's Python SDK: it signs short-lived routes
locally and keeps a detached worker signing them until the transfer ends, so
this cell reads, lists and cancels transfers but creates none. `media_publish`
(the WHIP URL and its bearer token) and the background Room listeners
(`beam_room_listen`, `beam_room_messages`) are left for a later cell; a
`beam_rooms` action answers at once.

## Test

From this repository's root:

```sh
.github/test-cell.sh tools/nodecode-beam
```

Every Beam API call is a stubbed `nlk:http`, every CLI run a stubbed seam,
and the environment a stubbed reader: nothing reaches Beam or the network.

MIT licensed.
