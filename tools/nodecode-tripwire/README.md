# nodecode-tripwire

Tripwire: one line for every tool call an agent was refused, and a digest of what is new, so the owner hears about it. A Nodecode rewrite of [percyhendrux123-prog/hermes-tripwire](https://github.com/percyhendrux123-prog/hermes-tripwire) by [percyhendrux123-prog](https://github.com/percyhendrux123-prog).

A refusal is rarely an accident: a guardrail stopped a call, an approval was
denied, a timeout ended one. Most of the time that is the guardrails working.
A cluster of them from one session can mean something tried to steer an agent
— a page, a form or a mail that carried an instruction the person never gave.
The agent that was steered writes a clean, friendly answer that mentions none
of it, so the record has to be made where the refusal happened, not by asking
the agent afterwards.

Tripwire is that record. It watches, it never decides, and if anything goes
wrong it gets out of the way: a log it cannot write is one warning, and the
call it was watching is never the loser.

## Get it

Tell the agent "add the tripwire cell", or run `nodecode add nc://tripwire`.

## Set it up

Nothing is required: with an empty section the log is `tripwire/events.jsonl`
under the Nodecode home and the cell only watches. Everything it takes is in
`/settings` (or the config's `tripwire` object; `config.example.jsonc` has it
commented):

| Setting | Default | What it is |
| --- | --- | --- |
| `events_file` | `tripwire/events.jsonl` under the home | where the rows are written; the digest's mark sits beside it |
| `drill_tool` | `""` | refuse this one tool, to prove the alarm end to end; empty means the cell only watches |
| `ignore` | none | tool names whose failures are housekeeping, never a trip |
| `record_errors` | `false` | record a call that failed on its own too, not only one a cell stopped |

To alert yourself, schedule the digest with the cron cell: a job whose prompt
is "call tripwire_digest and answer with exactly what it says" delivers a
message only on the runs where something was stopped — when nothing was, the
tool answers one line saying so, and the job can stay quiet on that.

## Use it

- One row a refused call: the time, the session, the tool, how it was
  stopped (`hook_refused` for a call a guard or another cell stopped,
  `approval_denied` and `approval_timeout` for the two answers an approval
  card can give, `error` for a plain failure with `record_errors` on, `drill`
  for the proving switch), and the first line of the reason, cut to 160
  characters.
- **What it never stores:** the call's arguments, its result, the message
  text. Only who, which tool, how, why in one line, and when. Some reasons
  name the rule that fired, so read your own log once before you share it.
- `/tripwire` answers the rows since the last check and marks them read, so
  a scheduled job delivers each one once. `/tripwire peek` answers the
  newest rows without marking them, `/tripwire path` says where the log is.
- The `tripwire_digest` tool is the same digest for a model to call, with
  one member: `digest` or `peek`.
- The model's manual is `(help :tripwire)`; `(nodecode-tripwire:refusal-how TEXT)`
  answers how a result says it was stopped.

**Prove it works:** set `drill_tool` to `eval` in the tripwire section, ask
an agent to evaluate anything, watch the row and the digest appear, then clear
the setting. While it is set the cell puts a standing notice up, so nobody
forgets it is on.

**Known gap, as in the original:** a call for a tool the model was not
offered at all is refused before any hook runs, so tripwire does not see it.
And `record_errors` off means an ordinary failure — a file that was not
there — is not a trip; that is the default, because a trip is what a
guardrail did.

## Licence

MIT. The rules for what tripwire may record are percyhendrux123-prog's: see
`LICENSE` and `NOTICE`.
