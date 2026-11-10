# nodecode-sentinel

Sentinel: breaks identical tool-call loops, redacts credentials in tool results and keeps a local audit ledger. A Nodecode rewrite of [SaltProphet/hermes-sentinel](https://github.com/SaltProphet/hermes-sentinel) by [SaltProphet](https://github.com/SaltProphet).

## Get it

Tell the agent "add the sentinel cell", or run `nodecode add nc://sentinel`.

## Use it

Four guards, all on by default, and one command that reports them.

- **The loop trap** refuses a tool call that repeats an identical one for the third
  time in a row — same tool, same arguments — so a round cannot spin on a call
  that keeps answering the same thing. Two identical calls are allowed; a changed
  argument is a different call. Legitimate polling repeats, so the trap is one
  command away from over: `/sentinel reset` (as the refusal itself says).
- **Credential redaction** replaces recognized runs in every tool result — bearer
  tokens, `sk-` keys, `ghp_` tokens, AWS key ids, PEM private keys, and
  `password = …`-shaped assignments — with a marker such as
  `[REDACTED_OPENAI_KEY]`, before the model reads the text.
- **A bounded result** cuts every tool result to 4000 characters and 100 lines,
  middle-out with a marker saying what went, so one enormous file does not fill
  the window. The model reads a modified, sometimes incomplete result: that is
  the trade, and it is why both bounds and both switches are settings.
- **A message guard** refuses a message carrying a credential-shaped string
  before the turn it would start, so a secret pasted into the composer does not
  reach the provider. As in the original, it is pattern matching, not a
  guarantee: a code sample spelling `token = …` is refused too.
- **The ledger** files every dispatch, refusal, redaction and truncation, newest
  first, in memory for the life of the process; `/sentinel audit` reads the last
  twenty, `/sentinel clear` empties it.

`/sentinel` alone answers the state and the subcommands. `(help :sentinel)` holds
the same text for the model.

The settings, in the `sentinel` section of `~/.nodecode/config.jsonc` (see
`config.example.jsonc`): `loop_guard`, `loop_threshold` (3), `bound_results`,
`max_chars` (4000), `max_lines` (100), `redact`, `guard_messages`, `audit_limit`
(200). One switch each, so a guard that keeps getting in the way is turned off in
`/settings` without removing the cell.

## What it is not

Not a security boundary and not a secret detector: it recognizes shapes. A tool
that reads a large file and a model that needs the whole of it will want
`bound_results` off, and a poll that must repeat five times wants
`loop_threshold` raised or `/sentinel reset`.

## Tests

`sh .github/test-cell.sh tools/nodecode-sentinel` from the cells repository: the
lifecycle, the trap, the redaction, both bounds, the message guard and the
command, all offline against stubbed tool bodies.
