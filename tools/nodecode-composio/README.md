# nodecode-composio

Composio Connections: see the accounts a Composio account has connected, connect a new service through its OAuth link, and disconnect one, with Composio's public toolkit catalog beside them. A Nodecode rewrite of [luisced/hermes-composio-connections](https://github.com/luisced/hermes-composio-connections) by [luisced](https://github.com/luisced).

## Get it

Tell the agent "add the composio cell", or run `nodecode add nc://composio`.

## Set it up

The `composio` CLI on the machine owns the sign-in and the connected accounts:

```sh
composio login
```

The toolkit catalog needs a Composio API key as well (composio.dev, Settings, API keys). Put it in `COMPOSIO_USER_API_KEY`, or write it in the cell's section:

```jsonc
"composio": { "api_key_env": "COMPOSIO_USER_API_KEY" }
```

Nothing is required to start: the account tools run on the CLI alone, and only `composio_services` asks for the key.

## Use it

Ask the agent, or call the tools yourself from `eval`:

| Tool | What it answers |
| --- | --- |
| `composio_accounts {toolkit?}` | every connected account: toolkit, alias, Composio status, and the selector that disconnects one |
| `composio_services {query?, linked?, limit?}` | the public toolkit catalog, filtered by words and by whether it is already connected |
| `composio_connect {toolkit, alias?}` | a `https://connect.composio.dev/link/...` URL to open in a browser |
| `composio_disconnect {selector, confirm}` | removes that one account, then reads the list again to prove it is gone |

`/composio` lists the accounts; `/composio TOOLKIT` answers that toolkit's link.

Rules the original puts on top of the CLI, kept here: a second account of a toolkit already connected needs a fresh alias; a selector two rows share is never removable; a link from anywhere but `connect.composio.dev` is refused; a removal is only claimed once the next `connections list` no longer shows the account.

Composio's status is Composio's word: `ACTIVE` does not prove every permission was used, and disconnecting from Composio does not revoke the grant at Google, Slack or GitHub.

## What the rewrite leaves out

The dashboard page itself (two tabs, paging through 1,600 logos, the disconnect dialog), and the packaged service logos. See [NOTICE](NOTICE).

## Licence

MIT, the original's. See [LICENSE](LICENSE).
