# nodecode-proofgate

ProofGate: checks a reply's completion claims against the tool evidence of the same session, before the user acts on them. A Nodecode rewrite of [Whydiriki700/proofgate](https://github.com/Whydiriki700/proofgate) by [Whydiriki700](https://github.com/Whydiriki700).

The failure it exists for is not a wrong answer. It is a false report that work
got done: a file "created" that does not exist, a message "sent" that failed, a
figure "checked" that was never printed. ProofGate holds the model to the
receipts its own tool calls left.

## Get it

Tell the agent "add the proofgate cell", or run

```sh
nodecode add nc://proofgate
```

and turn it on in `/cells`.

## Set it up

Nothing is required. The one choice is `mode`:

```jsonc
"proofgate": {
  "mode": "warn",          // enforce | warn | off
  "max_evidence": 200       // tool calls kept per session
}
```

* `warn` (default): checks are recorded and reported, and the reply is the model's own to correct.
* `enforce`: a report whose claims are proven by nothing is answered as the *tool call's* failure, so the model cannot call the work done past it.
* `off`: the hook records nothing and the tools say so.

## Use it

Every tool call of the session is recorded by the cell's `:TOOL` hook: the
name, what the call was asked with, what it answered, and whether the dispatch
recorded it failed. Nothing is redacted and nothing leaves the machine; the
evidence lives in memory for the life of the process.

The model calls **`proof_check`** with the reply it is about to send. Four
claim families are read out of it — a file written, a message sent, a command
or test that ran, a figure quoted — and each is matched against the recorded
calls:

* `PASS`: every claim is backed by a successful call of this session.
* `DOWNGRADE`: some are not; they are named back, with the family and the sentence.
* `BLOCK`: none is, or a claim is contradicted by a call that actually failed.

The answer is JSON: `verdict`, `claims`, `unproven`, `checked_tool_calls`. In
`enforce` mode a `BLOCK` comes back as `ERROR: ProofGate withheld this report`,
which every transcript draws as a failed call.

`proof_status` returns the machine's own record — how many replies were
checked, the last few verdicts. `/proof` prints the same ledger in the chat.
Each check appends one JSON line to `proofgate/ledger.jsonl` under the Nodecode
home; it is append-only and holds the verdict, the claim counts and the session
id, never the reply.

Honest limits, as the original's README had them: the claim check is
deterministic keyword and evidence matching, not a reading of meaning, so a
claim outside the four families passes untouched; evidence is the same
session's; and a call with no recorded status counts as no proof, by design.

## Credit

A rewrite of Whydiriki700's [Whydiriki700/proofgate](https://github.com/Whydiriki700/proofgate)
at commit b07f2fe5ee019a5d6e3fd24ad3b61dbce6302fec, MIT. See [NOTICE](NOTICE):
the original's `plugin.yaml` hooks and its Python checker, ledger and tool
surfaces, rewritten as one `define-cell` declaration. The claim families, the
verdict words and the fail-closed rule are the original's; the tests are new.

## Test

```sh
.github/test-cell.sh tools/nodecode-proofgate
```

Offline: a tool call is one op through the installed hook with a stubbed
`next`, the ledger is a file in a scratch home.
