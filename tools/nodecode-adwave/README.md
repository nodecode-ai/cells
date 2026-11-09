# nodecode-adwave

Adwave: a business website into ads on streaming TV, Meta, Google Search, YouTube and Reddit, on one prepaid daily budget. A Nodecode rewrite of [AdwaveInc/adwave-agent-plugin](https://github.com/AdwaveInc/adwave-agent-plugin) by [AdwaveInc](https://github.com/AdwaveInc).

## Get it

Tell the agent "add the adwave cell", or run `nodecode add nc://adwave`.

## Set it up

Adwave is self-serve advertising that runs in the US and Canada. The cell talks to Adwave's own MCP server at `https://waverunner.adwave.com/api/mcp`, and needs one of these two credentials:

1. **An API key.** In Adwave, open the account menu, then **API & agents**, and create a key. Paste it into the `adwave` section's `token` member in `/settings`, name an environment variable in `token_env`, or write it into a file and name that file in `token_file`.
2. **A sign-in you already have.** Add the server in `/mcp` as `{"url": "https://waverunner.adwave.com/api/mcp"}` and finish its browser sign-in. The shipped mcp cell keeps that token, and this cell reuses it; nothing else is needed.

`server_url` defaults to `https://waverunner.adwave.com/api/mcp`. `/adwave login` says, in words, where the key in use came from, and never prints it.

## Use it

| Tool | Use |
| --- | --- |
| `adwave_tools` | every tool the Adwave server offers: its name, what it does, whether it is read-only, and the arguments it takes |
| `adwave_call` | one of them by name, with that tool's own arguments object |

The first call is `adwave_tools`: the server's tool list is the catalog (`add_business`, `list_businesses`, `get_campaign_report`, `launch_campaign`, `update_campaign_budget`, `get_tracking_setup` and the rest), and it changes without this cell moving. Then `adwave_call` with `{"name": "add_business"}` and the arguments the listing names for it, and so on for every step of a campaign. Nothing about the server's tools is restated here: the listing is read from the server.

A tool that is not marked read-only changes the account or spends from the prepaid wallet, so `adwave_call` refuses it until `confirmed` is `true`, and that flag is one person's yes to one call: say the amount in dollars first and get a clear yes in the conversation. Read-only tools need no confirmation. The rule is the skill's, and it is enforced here rather than asked for.

From the keyboard, `/adwave` lists the tools the server offers and says where the key came from, and `/adwave report` lists the ones whose name or description says report. The model reads the workflow with `(help :adwave)`.

The skill the original shipped is that manual topic, so its one line rides every request and the whole of it arrives when the model asks: a website becomes a business profile, buyer profiles, ads and media buying; get a clear yes before any call that spends and say the amount first; each morning a live campaign sets aside that day's budget and the next morning charges only what ran; amounts are integer cents, shown as dollars; `create_wallet_checkout` answers a Stripe link instead of taking card numbers; a first campaign runs `add_business`, `get_business`, the quote, `create_campaign`, `get_wallet`, `launch_campaign_dry_run`, `launch_campaign`, then `get_tracking_setup`; results come from `get_campaign_report`, `get_campaign_analytics` and `get_campaign_ads`; 401 means sign in again, 402 offers a top-up, 409 and 422 name the field to fix, and 429 waits a minute.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The skill as the manual topic, the section, the MCP wire over `nlk:http`, the two tools and the `/adwave` command. |
| `package.lisp` | The package, and the peripheral declaration. |
| `config.example.jsonc` | The section this cell reads, commented; documentation, not config. |

## Not ported

- **The MCP server declaration as a manifest.** `plugin.json`, `mcp.json` and the five other platform manifests said where the server is; a cell says it once, in its section, and reads its own tools from the server at run time, so a server-side tool that changes needs no new release here.
- **The OAuth browser flow.** The shipped mcp cell owns it; this cell reuses the token that sign-in keeps, or takes a pasted API key.
- **A tool of its own for each of the server's tools.** The repository carries no schema for them, only their names in the skill, so the two tools keep the server's own names and arguments rather than restating a list that would go stale.

## Test

From the cells repository root:

```sh
.github/test-cell.sh tools/nodecode-adwave
```

Every request is a stubbed `nlk:http`, so nothing reaches Adwave, no key is needed and no campaign is touched.

MIT licensed, as the original is. See `NOTICE`.
