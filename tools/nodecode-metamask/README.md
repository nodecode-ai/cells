# nodecode-metamask

MetaMask Wallet: a self-custodial MetaMask agent wallet, driven from chat — status, balances, prices, history, swap quotes, transfers, swaps and signatures, and the wallet's own outflow policy, all over the MetaMask CLI and gated by MetaMask's policy and 2FA. A Nodecode rewrite of [akugone/hermes-metamask-wallet](https://github.com/akugone/hermes-metamask-wallet) by [akugone](https://github.com/akugone).

Every call is one run of the MetaMask agent wallet CLI, `mm --json`, on this machine. MetaMask holds the keys — a server wallet in its own TEE, or a BYOK mnemonic in the CLI's store — and this cell holds none of them: no key is in the section, none is asked for in the conversation.

## Get it

Tell the agent "add the metamask cell", or run `nodecode add nc://metamask`.

## Set it up

The cell needs the MetaMask agent wallet CLI and a wallet it is signed in to:

```sh
npm i -g @metamask/agent-wallet   # Node.js 22.18+
mm login                          # or `mm login browser` for a sign-in link
mm init --wallet server-wallet --mode guard
```

Nothing else is required: with `mm` on `PATH` the cell starts on its defaults — chain 8453 (Base), fiat `usd`, 90 seconds a call. In `/settings` (or the page's Settings tab) under `metamask`:

| Setting | Default | Meaning |
| --- | --- | --- |
| `program` | `mm` | the CLI's name on `PATH`, or a path to it |
| `default_chain_id` | `8453` | the chain a call that names none uses (1 is Ethereum) |
| `default_currency` | `usd` | the fiat balances and prices are read in |
| `timeout_seconds` | `90` | one `mm` call's ceiling |

A `program` that is not there refuses the start with the setting's name; a section with `"enabled": false` installs nothing. `/metamask` with no argument shows what the cell is pointed at, without running anything.

The login stays in the CLI's own store under the operator's home. A headless machine can sign in with `MM_CLI_TOKEN` in the environment, and a BYOK wallet reads its seed from `MM_MNEMONIC` — both are the operator's, exported in the shell that starts Nodecode, never pasted in chat.

## Use it

Ask "what's my address", "what do I hold on Base", "price of ETH", "how much USDC for 0.1 ETH", "send 20 USDC to 0x… on Base", "sign this message", "anything waiting for me?", "show my MetaMask policy".

| Tool | Use |
| --- | --- |
| `metamask_status` | signed in, wallet set up, address, trading mode; `include_policy` for the policy |
| `metamask_balance` | what the wallet holds, in fiat; one token, one chain, one address, or the testnets |
| `metamask_market` | `price`, `search`, `chains`, `trending`, `popular` |
| `metamask_history` | `list`, `tx` (a hash), `decode` (calldata) |
| `metamask_swap_quote` | compare-only routes with their `quoteId`; nothing executed |
| `metamask_swap_execute` | execute a swap, by `quote_id` or by the quote's arguments |
| `metamask_transfer` | send a token or coin to an address on a chain |
| `metamask_sign` | sign a message, or EIP-712 typed data such as a permit |
| `metamask_requests` | `list` what is pending, or `watch` a `pollingId` |
| `metamask_policy` | `get`, `template`, or `set` the 24h outflow limit and allowlist |

The operator's own: `/metamask` shows the connection, `/metamask status` the wallet as `mm` sees it, `/metamask balance` and `/metamask requests` ask the CLI directly, with no model turn. `(help :metamask)` is the model's guide, and `(metamask:balance)`, `(metamask:transfer …)` and the rest work from eval.

A write — a transfer, a swap execute, a signature, a policy set — is irreversible, and MetaMask's own gates still decide: Guard Mode's rolling outflow limit and allowlist, its Blockaid scan and simulation, and 2FA on the operator's phone or e-mail when policy requires it. A write that answers a `pollingId` is waiting on that 2FA; `metamask_requests action="watch"` follows it to its result. A fresh server wallet has a 0 limit and an empty allowlist, so the first transfers ask for 2FA until `metamask_policy set` broadens the policy.
