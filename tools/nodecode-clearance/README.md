# nodecode-clearance

Clearance: per-sender tool permissions, checked in code on every tool call. A Nodecode rewrite of [yuzuru99/hermes-clearance](https://github.com/yuzuru99/hermes-clearance) by [yuzuru99](https://github.com/yuzuru99).

Put Nodecode behind a Telegram group, a Discord server or a webhook and everyone who gets in gets every tool: `eval`, the MCP servers, every cell you turned on. Telling the model "only Ana may use `eval`" does not hold — a prompt can ask for it. Clearance decides it in code, per sender, before the tool runs.

## Get it

Tell the agent "add the clearance cell", or run `nodecode add nc://clearance`.

## Set it up

No key, nothing to sign in to. The `clearance` section of `config.jsonc` (or `/settings`) is where people get their tiers:

```jsonc
"clearance": {
  "users": ["telegram:123456789=owner", "discord:987654321=admin", "telegram:*=guest"],
  "default_tier": "guest",   // whoever nobody named
  "local_tier": "owner",     // your own TUI, CLI and page sessions
  "audit": true
}
```

Or assign from a chat, as the owner: `/clearance set telegram:123456789 admin`. That writes the `users` list back to the config and takes effect on the next call.

A `tiers` object changes or adds a tier field by field — `{"user": {"allow": ["@web", "@read"], "rate_per_hour": 30}}`; its fields are `allow`, `deny`, `rate_per_hour`, `chat`, `manage` and `audit`, and the owner tier always keeps full access, so a typo cannot lock you out.

## Use it

Ask in a chat, or type the command yourself:

- `/clearance` — who you are, your tier, the tools it names and your rate.
- `/clearance set telegram:123456789 admin` — assign a sender (owner).
- `/clearance unset discord:987654321` — back to the default tier (owner).
- `/clearance tiers` — every tier, what it may call and its hourly limit.
- `/clearance users` / `/clearance audit 20` — who is where, and the last refusals (admin and owner).

The model finds the cell in its index and reads `(help 'clearance)`.

## Tiers

| Tier | Tools | Rate |
| --- | --- | --- |
| `owner` | everything, and the two assignments | — |
| `admin` | everything | — |
| `user` | `@web`, `@media`, `@memory`, `@skills` | 60/h |
| `guest` | `@chat` (ask, clarify, todo_*) — conversation and a question | 20/h |
| `blocked` | none: the turn is refused | — |

Groups: `@eval`, `@shell`, `@web`, `@media`, `@memory`, `@skills`, `@schedule`, `@mcp`, `@chat`. Anything else is a tool name or a glob (`mcp__github__*`). Decisions are deny-by-default, deny beats allow, and a tool no tier names is refused for everyone but the owner.

## Who is who

The sender of the turn is read off the session the call runs in: the channel kit's own lane table answers which lane and which platform user owns it, so a Telegram or Discord message is attributed the way the room attributes it. A session no channel claims is your own, at `local_tier`. A room session whose sender is not known yet falls to `default_tier`, never to the owner.

## The audit log

Refusals, refused assignments and tier changes are appended to `clearance/audit.jsonl` in Nodecode's home, rotated at 2 MB. Each record holds the time, the sender key, the tier, the tool name and the **names** of its arguments — never their values, so a refused `eval` does not copy the form it carried into the log.

## What it is not

- **Not a sandbox.** It decides which tools a sender may call; an allowed tool keeps its whole reach. `eval` is the whole machine, so keep restricted tiers small.
- **Identity is the platform's.** Clearance trusts the user id the channel adapter reports; display names are never used for a decision.
- **The rate limit is a turn limit.** Counted in memory per sender per hour, reset when Nodecode restarts, and checked when a turn opens by the command's own report.

## License

MIT, the original's. See `LICENSE` and `NOTICE`.
