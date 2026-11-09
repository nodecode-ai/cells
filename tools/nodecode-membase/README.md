# nodecode-membase

Membase long-term memory: search, save and profile a user's hosted Memories, and recall passages before each turn. A Nodecode rewrite of [unibaseio/membase](https://github.com/unibaseio/membase) by [unibaseio](https://github.com/unibaseio).

The cell speaks the same HTTP API as the original Hermes plugin, so it talks to
the same service and the same account: [Membase](https://membase.com), the
hosted memory API by Unibase. One developer key is one person's account, and
that account's Memories hold what its memory agent learned from what was handed
to it.

## Get it

Tell the agent "add the membase cell", or run `nodecode add nc://membase`.

## Set it up

1. Make a developer key in the [Membase app](https://app.membase.com) under
   **Connect > Developer keys**. Choose **Read & write** if the agent should be
   able to save, and tick **profile** if `membase_profile` should answer.
2. In `/settings`, put the key in `membase.api_key`, or export
   `MEMBASE_API_KEY`.
3. `base_url` defaults to `https://api.app.membase.com`; point it elsewhere
   only for a proxy or a staging endpoint.
4. `/membase` prints the endpoint, whether a key is there, and the switches.

Without a key the cell still installs, and every verb answers one sentence
saying what to set — it never dials.

No setting picks a Memory: a search covers every Memory the key reaches, and a
save lands in the key's **write target**, the one Memory it reaches. A key
that reaches several Memories has no write target and its saves are refused,
so give the key a single Memory if the agent should save.

## Use it

Talk, and the memory comes to you: at each model request the turn's own input
is searched and the passages ride that request under `## Membase`, each naming
the Memory it came from. The search is time-boxed and fail-open — a slow or
refused search leaves the turn without memory, never stops it — and the first
search after a quiet spell can be slow while the account's memory wakes.

| Tool | What it does |
| --- | --- |
| `membase_search` | the passages the Memories in reach hold on a question, most relevant first |
| `membase_save` | save one note into the key's write target; the Memory learns it on its next run |
| `membase_profile` | the user's standing facts, newest first |

and the same three by hand from `/membase`: `search QUERY`, `save TEXT`,
`profile`, `status`. From eval the verbs are `membase:recall`,
`membase:remember` and `membase:profile`, and `(help :membase)` is the manual.

Shapes worth knowing:

- **Saves are not instant.** A note is learned on the Memory's next run, so it
  is searchable after that rather than at once. There is no delete or forget
  verb: the owner removes a note in the Membase app.
- **Nothing is captured unless you say so.** `capture_turns` (off by default)
  saves the operator's own words from each completed turn as one note. The
  assistant's half is never sent, so an answer is never learned as a fact.
- **One key is one person.** On a profile several people talk to, every one of
  them reaches the owner's memory; give each person a profile and a key of
  their own.
- **Plan usage.** Searches and learning runs execute in the user's Membase
  account and count against its plan.

## What it talks to

The only network peer is the Membase API at `base_url`
(`https://api.app.membase.com` by default), with the key as a bearer token.
The cell sends each turn's own input as a recall query, the three tools'
calls, and — only with `capture_turns` on — the operator's words. It starts no
background process, runs no shell command and sends no telemetry.

## Licence

MIT. The original repository's licence covers this rewrite; see `LICENSE` and
`NOTICE`.
