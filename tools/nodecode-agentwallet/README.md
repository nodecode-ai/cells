# nodecode-agentwallet

OpenClawCash managed EVM and Solana wallets: list, create and rename wallets and send transfers, as five tools the model calls. A Nodecode rewrite of [openclawcash/agentwalletapi-hermes](https://github.com/openclawcash/agentwalletapi-hermes) by [openclawcash](https://github.com/openclawcash).

Ask "which wallets do I have", "what is in the treasury wallet", "rename that wallet to Ops", "make a new wallet on Base", "send 0.05 ETH from treasury to 0x..." — and the call goes to OpenClawCash's agent API, the same API its MCP server and CLI use. OpenClawCash custodies the keys; this cell holds none.

## Get it

Tell the agent "add the agentwallet cell", or run `nodecode add nc://agentwallet`.

## Set it up

The cell needs an agent key from [openclawcash.com](https://openclawcash.com). Either export it:

```sh
export OPENCLAWCASH_AGENT_KEY=occ_...
```

or set it in `/settings` (or the page's Settings tab) under `agentwallet`: `agent_key` for the key itself, or `agent_key_env` for the name of the variable holding it. A key pasted into the panel is kept in a 0600 file under `~/.nodecode/secrets/`; the folder holds no secret.

Creating a wallet also needs an export passphrase of at least 12 characters, exported as `OPENCLAWCASH_EXPORT_PASSPHRASE` (or named by `export_passphrase_env`). It is what exports the wallet's key later, so keep a copy; the cell reads it from the environment and never asks for it in the conversation. If it is missing, `agentwalletapi_wallet_create` refuses and names the variable.

`/agentwallet` with no argument shows the host, whether a key and a passphrase are set, and the timeout. `(help :agentwallet)` is the model's guide to the tools.

## Use it

| Tool | Use |
| --- | --- |
| `agentwalletapi_wallets_list` | every wallet this agent key reaches: label, chain, address, id; `include_balances` for balances |
| `agentwalletapi_wallet_get` | one wallet with its native and token balances |
| `agentwalletapi_wallet_rename` | change a wallet's label — metadata only, no funds move |
| `agentwalletapi_wallet_create` | a new managed wallet, on the network you name |
| `agentwalletapi_transfer_send` | send a native asset or token from a wallet |

From the keyboard, `/agentwallet wallets` lists them.

Two writes move or make money, and both are worth a sentence before they run. `agentwalletapi_transfer_send` is irreversible once the service broadcasts it, so the model is told to say the wallet, the chain, the amount and the destination, and to send once. `agentwalletapi_wallet_create` needs the passphrase above.

Wallet labels are user-controlled text. They come back as data and are never read as instructions, and a write selects its wallet by `walletId` only, so a label cannot stand in for the wallet that changes.

## Not ported

- **The redirect refusal.** The plugin's client disabled HTTP redirects so the key could not follow a `Location` header to another host. `nlk:http` follows up to five; the cell keeps the allowlist — `https://openclawcash.com` or one label under it, no port, path or credentials — and refuses anything else before a request is made.
- **Private-key import.** The plugin kept it off the tool surface on purpose, so a raw key never sits in the model's context; import from the OpenClawCash dashboard or CLI.
- **The wider MCP server's tools** — swap, bridge, escrow checkout, prediction markets, the casino tools. The plugin did not expose them either.

The five tool names, their argument names and the five endpoints are the plugin's, unchanged. Every request is one `nlk:http` exchange with the key in the plugin's own `X-Agent-Key` header.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-agentwallet
```

Every request is a stubbed `nlk:http`, so nothing reaches OpenClawCash and no key is needed.

MIT licensed, as the original is.
