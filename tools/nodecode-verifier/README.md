# nodecode-verifier

Verifier: a second model reviews each turn's work and can hold a destructive call before it runs. A Nodecode rewrite of [am423/hermes-verifier](https://github.com/am423/hermes-verifier) by [am423](https://github.com/am423).

The primary agent does the work; the verifier checks it. After a completed
turn the new messages of the session go to a second model, which answers with
at most one finding:

| Finding | Meaning |
| --- | --- |
| `[NOTE]` | Non-urgent cleanup, a simplification, a low-risk edge case |
| `[CAUTION]` | A material risk: wrong direction, a missing constraint, an API that does not exist |
| `[BLOCKER]` | Continuing will clearly waste the work or ship it broken |

A finding stands under this cell's key as a notice, so the agent reads it in
its next request and the operator sees it in the shell. A review with nothing
to say answers `nothing to flag`, and the standing notice is cleared: a fixed
finding does not linger.

The verifier reviews; it never acts. It has no tools, edits no files and runs
nothing. It reads the turn's transcript (with the obvious credentials
redacted) and the project's review priorities, if there are any.

## Get it

Tell the agent "add the verifier cell", or run:

```sh
nodecode add nc://verifier
```

## Set it up

The section is off until `enabled` says otherwise; one review is one model
call, so the switch is the operator's.

```jsonc
{
  "verifier": {
    "enabled": true,          // review every completed turn
    "model": "",              // empty: the auxiliary model (/models picks it)
    "provider": "",           // with model, a verifier on a provider of its own
    "clip": 500,              // characters kept per tool call or result
    "max_finding_chars": 1200,
    "agreement": 1,           // reviews per turn; 1 is off
    "agreement_min": 1,       // how many must raise the same finding
    "phase_gate": false,      // review a destructive or publishing call before it runs
    "priorities": ""          // a file of review priorities; empty looks for VERIFIER.md
  }
}
```

No key of its own: the review is one side call on the model Nodecode already
has configured, so it is billed to that provider and nothing else leaves the
machine it would not otherwise. Set `model` and `provider` to send the review
to a second model — the point of the cell is a different angle, and a
different model is the cheapest way to get one. (In a session, `/models`
picks the auxiliary target for every side call; `verifier.model` overrides it
for this cell alone.)

`agreement` and `agreement_min` run each review more than once and deliver
only a finding that several of them raised: a reviewer whose false alarms
vary from run to run gets quieter, at that many times the price. A model whose
false alarms are consistent is unaffected, because agreement cannot filter a
consistent error.

## Use it

```
/verify                on or off, the model, the gate, the last finding
/verify on | off       review every completed turn, or stop
/verify gate on | off  let a review hold a destructive or publishing call
/verify review [focus] review the newest turn now
/verify agree N M      N reviews a turn, M of them to deliver a finding
/verify dump           the last finding, with its severity
/verify reset          drop the last finding and the standing notice
/verify test SEV NOTE  deliver a test finding: note, caution or blocker
```

The tool `verifier_review` does the same on demand: pass it a transcript and
a focus, and it answers one tagged finding, or `nothing to flag`.

### The phase gate

With `phase_gate` on, an impending tool call that reads destructive or
publishing — a recursive delete, a `drop table`, a force push, a restart, a
publish or a deploy — is shown to the model before it runs. It answers
`allow`, or `block: why`, and a block refuses the call with that reason, so
the agent can address it or explain why the call is safe and make it again.

It fails open. A gate that cannot reach its model, times out, or answers
something unparseable allows the call: the cost of a wrong block is the
operator's work stopped, and that is worse than the call it would have held.
A call that reads safe is never even shown to it.

### Review priorities

A project can keep its own review priorities in a `VERIFIER.md` file: one in
the working directory, or in any directory above it. The verifier reads it
into its own instruction, so the agent never sees it and never mistakes it for
its own standing guidance. `verifier.priorities` points at any other file.

Nothing else is read: the cell looks at the session's own transcript, the
priorities file it is given, and the model's answer.
