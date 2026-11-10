# nodecode-proof-run

Proof Run: records a demonstrated run and gates its reuse on exact arguments and fresh reads. A Nodecode rewrite of [raffieeey/hermes-proof-run](https://github.com/raffieeey/hermes-proof-run) by [raffieeey](https://github.com/raffieeey).

The failure it exists for is the remembered command. A run worked once, so it
is repeated later with one argument changed, after the thing it depended on
moved. Proof Run records the run as a contract: the mutating calls, the exact
arguments each was made with, and the reads made before them. From then on a
covered call runs only when its arguments are one demonstrated call's and every
read that call depended on was made again since, with the same answer.

## Get it

Tell the agent "add the proof-run cell", or run

```sh
nodecode add nc://proof-run
```

and turn it on in `/cells`.

## Set it up

Nothing is required. The choices are in the `proof-run` section:

```jsonc
"proof-run": {
  "mode": "enforce",        // enforce | off
  "max_age": 60,            // seconds a read counts as fresh
  "reads": ["proof_status", "web_search", "web_extract", "look", "help"],
  "read_forms": ["help", "look", "config-get", "cells", "index", "search"],
  "check_evidence": true,   // compare the replay's answer with the demonstration
  "directory": "proof-run/" // under the Nodecode home
}
```

An eval call is a read when its first verb is in `read_forms`; a call of any
other tool is a read when the tool is in `reads`. Everything else is a step,
which is what a contract gates. In Nodecode nearly every act is the `eval`
tool, so `read_forms` is where a shell read (`sh` running `git status`, say)
is declared: add `"sh"` and a demonstration's `sh` calls become observations
rather than guarded steps.

## Use it

1. `/proof-run start deploy-api` in the session that will demonstrate the task.
2. Let the agent do the task, reads and mutations alike.
3. `/proof-run stop`. A successful capture becomes a live contract, in one
   atomic write, at version 1 (or the version after the one it rebinds).
4. On reuse, read again, then the covered call with the same arguments. The
   whole argument object must match a demonstrated call, with the same JSON
   types; a missing, changed, expired or spent observation refuses the call.

The refusal is the call's own failure — `ERROR: proof-run refused this call:
...` — so no transcript reads the call as done, and the reason names the
contract, the field, what was demonstrated and what is there now.

Other verbs:

* `/proof-run learn deploy-api 15` — a contract from the last 15 calls of the
  session (1 to 100; the whole buffer of 100 when omitted). A recent buffer can
  hold more than one task, so read the scope before you keep it.
* `/proof-run rebind deploy-api` — re-demonstrate a changed task. The saved
  contract keeps gating every other session until you stop; only this contract,
  in this session, is exempt while it is being recorded.
* `/proof-run status` — versions, bound state, evidence status.
* `/proof-run unbind deploy-api` — stop enforcing it, keeping the file.
* `/proof-run cancel` — drop this session's recording; saved contracts stay.

`proof_status` answers the same list to the model, as JSON.

Contracts are one JSON file each under `proof-run/` in the Nodecode home; a
rebind overwrites the file at its next version. Nothing here touches the
network, and nothing is trained or replayed automatically.

## What it is, and is not

* **Exact actions, not guessed ranges.** Nothing interpolates a number or
  recombines independently observed arguments: one example does not establish
  a threshold.
* **Bound observations.** A read fact is matched to the read's exact
  arguments and to the session. Only reads before an action are its
  preconditions.
* **Spent evidence.** A read is consumed by the next mutation, so two calls
  cannot share one observation. A read is not a transaction: the world can
  still move between the read and the call.
* **A different answer is a stop.** A replay whose answer differs from the
  demonstrated one, field for field, marks the contract unproven, and its
  covered calls are refused until you rebind (or set `check_evidence` false and
  gate on arguments and reads alone).
* It does not teach the model a plan, order a whole workflow, or prove the task
  right. The operator decides what is worth keeping.

## Credit

A rewrite of raffieeey's [raffieeey/hermes-proof-run](https://github.com/raffieeey/hermes-proof-run)
at commit 36f3f4111b6d8cd5cf67d53eaf54e62148fcf3ca, MIT. See [NOTICE](NOTICE):
the plugin's hooks, its contract model and its `/proof` verbs, rewritten as one
`define-cell` declaration. The semantics are the original's; the matcher and
the tests are new.

## Test

```sh
.github/test-cell.sh tools/nodecode-proof-run
```

Offline: a tool call is one op through the installed hook with a stubbed
`next`, and contracts are files in the run's scratch home.
