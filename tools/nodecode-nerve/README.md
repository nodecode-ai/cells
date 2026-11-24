# nodecode-nerve

Nerve: a supervisory decision layer that asks a decision model whether a risky
tool call should run, and answers verdicts, rankings and finish checks the model
asks for. A Nodecode rewrite of [keeltrace/hermes-nerve](https://github.com/keeltrace/hermes-nerve) by [keeltrace](https://github.com/keeltrace).

## Get it

Tell the agent "add the nerve cell", or run:

```sh
nodecode add nc://nodecode-nerve
```

The cell is a tool: it adds two tools the model can call, five verbs it can
reach through `eval`, a `/nerve` command, and one hook on the tool point.

## Set it up

Nerve asks a decision model, so it needs a provider and a key. The provider is
one of three that speak the same `POST {state, model, questions}` wire:

| provider | root | key in the environment |
| --- | --- | --- |
| `typesafe` | `https://api.typesafe.ai` | `TYPESAFE_API_KEY` |
| `openrouter` | `https://openrouter.ai` | `OPENROUTER_API_KEY` |
| `opencode` | `https://opencode.ai` | `OPENCODE_API_KEY` |

`"provider": "auto"`, the default, reads the environment: TypeSafe when only
its key is set, OpenRouter otherwise. Set `api_key` in the section to name the
key there instead. `base_url` points the cell at a System One server of your
own — on this machine or your network — which may need no key at all; `model`
overrides the model label.

The section, with its defaults:

```jsonc
{
  "nerve": {
    "provider": "auto",
    "api_key": "",
    "base_url": "",
    "model": "",
    "timeout_seconds": 10,
    "gate_mode": "off",
    "gate_tools": [],
    "gate_skip": [],
    "min_allow_percent": 90
  }
}
```

`enabled: false` vetoes the section and installs nothing.

## Use it

**The gate.** With `"gate_mode": "advisory"` or `"enforce"`, a tool call that
would change something is put to the model first, with its name and arguments.
Advisory says the verdict as a notice and lets the call run. Enforce refuses
it unless the verdict is ALLOW with p(ALLOW) at or above `min_allow_percent`.
A call whose words show nothing changed — a read, an arithmetic check — is
never sent anywhere, `gate_tools` names calls to always review, and `gate_skip`
names calls never to. If the provider cannot be reached the call runs and a
warning says so: the gate is a speed bump, not a security boundary, and `eval`
keeps the machine's full authority.

**The verbs**, through `eval`:

```lisp
(nerve:decide "the state: evidence, options, what is at stake"
              "which of these should the agent do next?"
              '("FIX" "REVERT")
              :criteria '(("FIX" . "the change is close and safe")
                          ("REVERT" . "the change is not understood")))
;; => (:VALUE "FIX" :CONFIDENCE 0.81d0 :PROBABILITIES (("FIX" . 0.81d0) ("REVERT" . 0.19d0))
;;     :MODEL "jev-1.14" :PROVIDER "typesafe" :USAGE #<HASH-TABLE> :REQUEST-ID "..." ...)

(nerve:rank "two ways to reach the same file" "which first?" '(("read" . "no risk") ("run" . "the suite")))
(nerve:verify "tests pass; the diff is two lines" "the suite is green")   ; PASS, RETRY, REPLAN or ESCALATE
(nerve:assess "a state" '(:risky (:type "noul" :instructions "risky?")
                          :kind (:type "choice" :instructions "what kind?"
                                 :criteria (:fix nil :feature nil))))
(nerve:stats)
```

**The tools.** `nerve_decide` and `nerve_verify` are the same two questions
as contracts, so a model that never reads the manual can call them directly.

**The command.** `/nerve` says where it asks, whether a key is there, the gate
in force, and what this run counted; `/nerve enforce`, `/nerve advisory` and
`/nerve off` switch the gate for the run; `/nerve stats` prints the counters.

**What leaves the machine.** The state you pass, and, at the gate, the tool's
name and arguments: every member named like a secret (`api_key`, `token`,
`password`, `..._secret`) is replaced whole, and a token inside any string —
a bearer header, a `sk-`/`ghp_`/`xox`/`AKIA` key, a JWT, a private key, a
`?token=` — is scrubbed before the request goes. Nothing else is sent, and
nothing is kept beyond this run's counters.

See [NOTICE](NOTICE) for what the rewrite carries and what it left out.
