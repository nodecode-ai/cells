# nodecode-provenance

Provenance gate: flags or refuses a memory write that states a user decision the user's own words do not back. A Nodecode rewrite of [simin-yuan/hermes-provenance-gate](https://github.com/simin-yuan/hermes-provenance-gate) by [simin-yuan](https://github.com/simin-yuan).

An agent that keeps facts about you drifts: "User decided to ship on Friday"
lands in memory, and nobody can say whether you said it. This cell reads the
text of every write that keeps a memory or a skill and looks for sentences
that attribute a decision to you — "User decided …", "The owner approved …",
"用户决定 …". Each one needs a quote in "…" that appears verbatim in
something you typed this session, shares key words with the decision, and does
not drop a negation.

- **Observe (the default) never refuses.** A write without such a quote goes
  through and is recorded as `would_block`; `/provenance` lists them.
- **Block** (`"mode": "block"`) refuses the write and tells the agent how to
  fix it: quote the user's exact words, or write the claim as its own
  suggestion or as an open question.

The detector is a keyword tripwire, not a proof: it misses wordings outside
its tables and flags some ordinary text. That is why recording is the default
and refusing is a choice you make after seeing what it flags on your own data.

## Get it

Tell the agent "add the provenance cell", or run `nodecode add nc://provenance`.

## Set it up

Nothing is required. Three knobs sit in the `provenance` section of
`~/.nodecode/config.jsonc` (or `/settings`):

| Member | Values | Default |
| --- | --- | --- |
| `mode` | `observe` records, `block` refuses | `observe` |
| `scope` | `decisions`, or `decisions+preferences` to also ask a quote for "prefers / likes / 喜欢 …" | `decisions` |
| `retention_days` | days of evidence rows kept | `30` |

```jsonc
"provenance": { "mode": "block", "scope": "decisions" }
```

## Use it

The cell works on its own: every write that keeps a memory or a skill
(`define-memory`, `define-skill`, through eval) passes the gate.

```lisp
(help :provenance)                       ; what the gate checks, in one paragraph
(nodecode-provenance:check-text "User decided to ship on Friday.")
;; => "provenance: would_block (no_quote) - ...", and the checks
```

- `/provenance` — what it has seen, and the newest claim it flagged.
- `/provenance check TEXT` — check a text now, without keeping it.
- `/provenance purge` — empty the evidence log.
- Tools `provenance_check` (`text`, optional `user_text`) and
  `provenance_report` (`limit`) do the same for the agent.

What it reads: the user messages of the request in flight, and its own
evidence log under the cell's home (`provenance/evidence.jsonl`). It writes
nothing else and reaches no network. What you typed is kept in memory for the
session and dropped when the cell stops; the evidence log holds the claim,
the quote, the verdict, the reason and the session id, and rows older than
`retention_days` are pruned at start and at every purge.

## Credit

A Nodecode rewrite of [hermes-provenance-gate](https://github.com/simin-yuan/hermes-provenance-gate)
by [simin-yuan](https://github.com/simin-yuan), Apache-2.0. See `NOTICE` for
what was rewritten and what was left out.
