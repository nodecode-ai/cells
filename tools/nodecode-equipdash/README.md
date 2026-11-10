# nodecode-equipdash

EquipDash: the day of a rental or activity shop, its bookings, availability and payments. A Nodecode rewrite of [equipdash/agent](https://github.com/equipdash/agent) by [equipdash](https://github.com/equipdash).

Ask "what is going out today", "who is late coming back", "find Dana's booking", "is the kayak free on Saturday and what does it cost", "make that booking", "send a payment link", "refund that card". EquipDash is booking and rental software for businesses that rent equipment and run activities, lessons and classes; this cell talks to the shop's own EquipDash MCP server and carries the guidance the original's skill gave, so a turn reads the day and changes it the way the shop's own screens would.

## Get it

Tell the agent "add the equipdash cell", or run `nodecode add nc://equipdash`.

## Set it up

You need an EquipDash business on any plan, and one of two credentials:

- **The sign-in the mcp cell keeps.** If this machine already signed in to `https://app.equipdash.com/mcp/v1` in `/mcp`, there is nothing to do: the cell reads that token, and every call acts as the person who signed in.
- **An API key.** Create one under **Settings > API & Integrations** with the **MCP Access** scope (`mcp:access`) plus the read scopes the agent needs — add write scopes only if it should make bookings or take payments (a key is a Pro-plan feature). Name it, never paste it into a committed file:

```sh
export EQUIPDASH_API_KEY=...
```

The section `equipdash` in `~/.nodecode/config.jsonc` reads it by that name by default; `token` takes a key inline and `token_file` a file holding one. Tell the agent "set up the equipdash cell" to write the section, or write it yourself:

```jsonc
"equipdash": {
  "token_env": "EQUIPDASH_API_KEY",   // where the key is read from
  "timeout_seconds": 30,              // one call's ceiling
  "max_chars": 12000                  // how much of one answer the model reads
}
```

Three more members are optional: `server_url` (default `https://app.equipdash.com/mcp/v1`), `token` and `token_file`.

## Use it

Three tools, and the model picks them:

| Tool | When | Arguments |
| --- | --- | --- |
| `equipdash_day` | "what is on today", "who is late", "what needs me" — Get today's screen and what needs the owner, in one call | `date`, `section` (`out_now` or `people`) |
| `equipdash_tools` | it does not yet know what this account offers, or a name was refused | `filter` |
| `equipdash_call` | everything else, by the server's own tool name | `name`, `arguments`, `preview`, `confirmed` |

The tool list is the account's own — bookings, availability, customers, items, payments, waivers, staff, reports and settings — and follows the shop's plan and the signed-in person's permissions, so `equipdash_tools` is the way to see it rather than a list written here.

**Writes keep the shop's two-step.** A tool that changes something answers a preview first: `equipdash_call` with `preview: true` asks what it would do, and after the person's clear yes the same call with `confirmed: true` sends the `user_confirmed: true` the server wants (or `confirmed: true` on `send_payment_link` and `process_refund`). A write called with neither flag is refused rather than sent. `record_payment` is the exception the skill names: it saves straight away, so check the amount first.

The operator can read the day from the shell:

```
/equipdash                      today's pickups, returns and what needs you
/equipdash tools payment        the tools whose name or description says payment
/equipdash call get_booking {"reference": "691"}
```

`/equipdash call` counts as the operator's own yes. `(help :equipdash)` carries the manual a model reads — the shop's timezone, money as the tools return it, ids from this session, preview then a yes, and a plan or permission refusal passed on with its reason — and `(equipdash:day nil)` answers the same object the tool returns.

## What it does not do

- It does not sign in to EquipDash itself: the token comes from `/mcp` or an API key. Without either, the cell says so and installs nothing.
- It sends no telemetry and writes nothing to disk. Every request goes to the one `server_url` in its section, and the only file it reads is the mcp cell's own sign-in store.
- Stock, pricing and the tool set are EquipDash's: whatever the shop's plan and the person's permissions answer is what the model sees.

## Test it

```sh
.github/test-cell.sh tools/nodecode-equipdash
```

The run stubs every `nlk:http` call, so it needs no network and no key: the section (a healthy start, and a missing key refused), the wire (one session, the bearer key, the session id echoed, a server error failed, an unknown name listed), the three tools, the preview-then-confirm gate, and the operator's command are all checked against answers shaped like the server's.
