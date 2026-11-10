# nodecode-household-tiers

Household Tiers: per-conversation tool tiers, quiet hours and an audit for one shared bot. A Nodecode rewrite of [chevlume/household-tiers](https://github.com/chevlume/household-tiers) by [chevlume](https://github.com/chevlume).

One bot, several people: the owner, other people, guests, and in a family a
child whose hours are 08:00 to 21:00. Each conversation is put in a tier, a
tier says which tools its turns may call, and a tier with hours may call them
only inside the window. It needs no key and talks to nothing outside the
machine.

## Get it

Tell the agent "add the household-tiers cell", or run `nodecode add nc://household-tiers`.

## Set it up

Nothing is required: with no policy document, every conversation is a `guest`
with no tools and the owner keeps everything (`"owner": ["*"]`).

The policy is one JSON document, `household-tiers.json` in the Nodecode home
unless the section's `policy_file` names another path:

```json
{"sessions": {"telegram-100001": "owner", "telegram-200002": "kid"},
 "default": "guest",
 "tiers": {"owner": ["*"], "kid": ["web_search", "web_extract"], "guest": []},
 "hours": {"kid": {"allow": "08:00-21:00"}},
 "audit": {"enabled": true, "retention_days": 30}}
```

A session is what a turn reports — a chat's room, a lane, a background job —
and an entry ending in `*` matches every session that starts with it. The
section itself is in `config.example.jsonc`.

## Use it

From the owner's conversation:

```
/household-tiers                    the last seven days: who was blocked on what
/household-tiers 2                  the last two
/household-tiers tier <session> kid give one conversation a tier; not your own
/household-tiers forget <session>   delete that person's audit lines
```

The model has the same summary as a tool, `household_tiers`, and reads it
with `(household-tiers:summary :days 2)`. A call the policy does not allow is
refused before it runs: the model sees the failed call, with the tier and the
tool named, and a line lands in `household-tiers-audit.jsonl`. The ledger
holds decision codes, session ids, tier names and tool names — never message
text, tool arguments or prompt contents.

An unreadable document fails closed: every tool call is refused, the owner's
included, until it is fixed.

## What it does not do

It does not rewrite the prompt. The original plugin took the owner's
`MEMORY.md` / `USER.md` out of other people's system prompts; Nodecode's
peripheral seam has no request-rewrite point, so the tiers bind what a turn
may call, not what it may read. A conversation whose tier keeps `memory` can
still reach the owner's memory through it — leave `memory` out of the
non-owner tiers. See `NOTICE`.

## License

MIT. The original plugin is Copyright (c) 2026 Chevlume: see `LICENSE` and `NOTICE`.
