# nodecode-gowa

GOWA device slots: list the WhatsApp devices a GOWA server holds, pair a new one from a QR or a phone code, and remove one. A Nodecode rewrite of [anpicasso/hermes-go-whatsapp](https://github.com/anpicasso/hermes-go-whatsapp) by [anpicasso](https://github.com/anpicasso).

[GOWA](https://github.com/aldinokemal/go-whatsapp-web-multidevice) is the go-whatsapp-web-multidevice server: several WhatsApp accounts, each a device linked the way WhatsApp Web is, served under one REST origin on your own machine or a remote one. It sends and receives messages, and its own MCP endpoint already gives an agent sending, chats, groups and schedules. What that endpoint lacks is the slots themselves -- which ones exist, whether they are logged in, and how a new one gets paired -- and that is what this cell does.

## Get it

Tell the agent "add the gowa cell", or run `nodecode add nc://gowa`.

## Set it up

The `gowa` section of `config.jsonc` names the server:

| Member | What it is |
| --- | --- |
| `base_url` | The origin the GOWA server answers on, such as `http://127.0.0.1:3000`. Plain HTTP is allowed on loopback only, unless the next member says otherwise. |
| `auth_env` | The environment variable holding the whole `Authorization` value the server prints (`Basic ...` or `Bearer ...`). `GOWA_AUTH_HEADER` by default. |
| `auth_file` | A file of `KEY=VALUE` lines holding that value instead, 0600 -- the shape GOWA's own setup writes. |
| `plain_http` | Whether an `http://` origin outside loopback is allowed. There the Authorization value travels in clear. |
| `state_dir` | Where a pairing QR is written. `<home>/gowa/` by default. |
| `timeout_seconds` | One request's ceiling, 15 by default. A pairing request gets 135 whatever this says: the account holder has to act. |

A GOWA without auth needs nothing: with no value in either place, calls go unauthenticated. The cell refuses at start when `base_url` is missing or is not an origin, so the `/cells` line and the setup panel say so before a tool is called.

It does not install GOWA. Run the server yourself -- `systemd --user`, a container, or the release binary from GOWA's own GitHub Releases -- and point `base_url` at it.

## Use it

| Tool | What it does |
| --- | --- |
| `gowa_devices` | `{action: "list"}` lists every slot with its state, its phone number and its JID; `{action: "get", device_id}` reads one slot. |
| `gowa_device_login` | `{action: "qr", device_id, create_if_missing}` writes a pairing QR under `state_dir` and answers its path -- `(look PATH)` shows it; `{action: "code", device_id, phone}` asks for a pairing code for an international number instead. |
| `gowa_device_remove` | `{device_id, confirm: true}` purges one exact slot and its stored session, and asks WhatsApp to unlink it. |

Type `/gowa` for the slots, `/gowa DEVICE` for one of them. `(help :gowa)` is the same in prose, for the model.

Pairing works the way WhatsApp Web does: the operator opens WhatsApp on the phone, chooses Linked devices, Link a device, and scans the QR (or types the code under Link with phone number). A QR and a code are temporary credentials for one account, so the tools say not to save or share them. `gowa_device_login` refuses a slot that is already linked or in the middle of pairing, and a number already linked to another slot, and `gowa_device_remove` will not run on a confirmation it was not given.

## Limits

- The QR image is one file per slot (`<device>-qr.png`), rewritten on each pairing; it is written 0600 under the home, as the original wrote it.
- A QR link the server answers must be on `base_url`'s own origin, or it is refused: the Authorization value goes with that download, and a server should not be able to walk it elsewhere.
- The cell does not supervise the GOWA process. It is a client: starting, stopping and upgrading the server is the operator's.
- Nothing here sends a message. Use GOWA's own MCP endpoint (`nodecode-mcp` lists it) for sending, chats, groups and schedules.
