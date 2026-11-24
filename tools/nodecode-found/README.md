# nodecode-found

FOUND: warm leads from buying signals, pipeline and campaign numbers, account research and drafted messages, from the AI SDR for LinkedIn and email. A Nodecode rewrite of [Utkarshkapil/found-hermes-plugin](https://github.com/Utkarshkapil/found-hermes-plugin) by [Utkarshkapil](https://github.com/Utkarshkapil).

The model gets six tools — `found_status`, `found_leads`, `found_numbers`,
`found_account`, `found_draft` and `found_find` — that reach the same hosted
server the original plugin configures, and `/found` to see where it points or
to list this week's warm leads from the shell. It works with an existing
FOUND account.

```
You:    Who are my warmest leads this week, and why?

Model:  found_leads {found_within_days: 7}  ->  6 people. The top 3:
        1. Head of Growth at a 40-person SaaS. Posted "our outbound reply
           rate fell off a cliff once we scaled sends" on Tuesday.
        2. Co-founder, seed-stage fintech. Raised $4M last week; commented on
           a post about hiring a first SDR.
        3. VP Sales, logistics software. Engaged with two posts about Apollo.
```

## Get it

Tell the agent "add the found cell", or run `nodecode add nc://found`.

## Set it up

The cell talks to FOUND's hosted MCP server. Its section:

```jsonc
{
  "found": {
    "base_url": "https://app.foundtofind.com/mcp",
    "api_key_env": "FOUND_API_KEY",   // or "api_key" with the key itself
    "timeout_seconds": 60
  }
}
```

`api_key` (or the variable `api_key_env` names, `FOUND_API_KEY` either way)
is a key from FOUND's **Settings → Connect → Advanced**. A key works like a
password for the account. A section with no key is refused at start, naming
the members that carry one.

The original signs in through a browser, by running `mcp-remote` through
`npx`; this cell needs no Node, so it takes the key path the original's
README documents for servers and headless machines.

## Use it

| Tool | What it answers |
| --- | --- |
| `found_status` | the plan, LinkedIn and mailbox health, campaigns and lists with their ids, credits |
| `found_leads` | the warm leads FOUND already found, warmest first, each with the signal behind it |
| `found_numbers` | `view`: `stats`, `breakdown`, `report`, `compare` or `accounts` |
| `found_account` | what FOUND knows about one company: its people, the signals, who to write to |
| `found_draft` | the message FOUND would send one lead, written from why that lead was found |
| `found_find` | `action`: `companies`, `contact`, `results` or `email` — the searches that spend credits |

Ask in words; the help topic `(help :found)` carries the same guidance the
original's skill did. Nothing sends from Nodecode: a campaign made here starts
paused, and it is switched on in FOUND. A search that spends credits answers
a price and a `quote_id` first, and spends only when called again with that
`quote_id` and `max_credits`, after the user has said yes.

## The wire

`POST https://app.foundtofind.com/mcp`, one JSON-RPC message each: `initialize`
(names the session `Mcp-Session-Id` answers, kept and sent back),
`notifications/initialized`, then `tools/call` with a tool's name and
arguments. A body is JSON, or an event stream whose `data:` lines carry the
answers; both are read. `Authorization: Bearer <key>` on every message.

## Not ported

- **The browser sign-in.** `mcp-remote` through `npx` opens a browser and a
  local callback port and keeps OAuth tokens under `~/.mcp-auth`; a Lisp cell
  bundles no Node, so the key path replaces it.
- **Five of the remote tools.** `get_campaign`, `create_campaign`,
  `update_sequence`, `add_to_list` and `add_to_campaign` write to the
  account and are not wrapped here; campaigns are made in FOUND's own app.
- **The tool set itself.** FOUND serves the tools at run time, so a tool it
  renames is reached here by the name it had at the pinned commit.
