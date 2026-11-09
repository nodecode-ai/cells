# nodecode-tracekit

Tracekit: a signer outside the agent decides every tool call, and signs the decision and its outcome into a run anyone can verify offline. A Nodecode rewrite of [Cygnux-Labs/Tracekit](https://github.com/Cygnux-Labs/Tracekit) by [Cygnux-Labs](https://github.com/Cygnux-Labs).

## Get it

Tell the agent "add the tracekit cell", or run `nodecode add nc://tracekit`.

## Set it up

Run a Tracekit signer, then give the cell its address. The cell's section is `tracekit` in `~/.nodecode/config.jsonc`, or set it from `/settings`:

```jsonc
"tracekit": {
  "signer": "http://127.0.0.1:7787",   // the signer's HTTP address (loopback, or https for a deployed one)
  "token_env": "TRACEKIT_SIGNER_TOKEN" // only for an https signer; a loopback one needs no token
}
```

- `signer` is required: without it the cell refuses to start, because there would be nothing to decide against.
- `token_env` names an environment variable holding the bearer token; `token_file` names a file holding it, re-read on every call so a rotated token is picked up. One or neither.
- `ask_seconds` (default 120) is how long a held call waits for a person's answer; `timeout_seconds` (default 15) bounds one call to the signer.
- `fail_open` (default off) lets a call run **unrecorded** while the signer is unreachable. A refusal still blocks.
- `agent` (default `nodecode`) is the name the run is registered under.

A signer you run yourself, on the same machine, needs no key: the cell reaches it over loopback, the same trust boundary the original used for its local dev signer.

## Use it

Nothing to type. Every tool call the model makes goes to the signer first:

- **Allowed** calls run, and their outcome is recorded against the decision.
- **A deny** blocks the call, and the model reads the rule that blocked it.
- **An ask** holds the call until a person approves or rejects it in Tracekit (`tracekit approvals`, or the signer's own page), or until `ask_seconds` passes: then the call runs, or is blocked and its approval withdrawn.

`/tracekit` says where things stand: the signer's address, this run, what it decided, and whether the signer answers now.

The run covers this Nodecode process: one run, registered at the first decided call and closed when the cell stops (and by the signer at its idle timeout). `tracekit last` exports and verifies it offline.

### What the signer sees

Each call's name and arguments, the decision, and the outcome. A result goes in full only for a tool the policy marks untrusted; otherwise only its SHA-256 digest. The cell stores nothing itself, and contacts nothing but the signer you name.

### Fails closed

If the signer cannot be reached, the call is blocked and the model is told so; so is a call the signer refused, a call the signer did not let through, and any error of the cell's own. The signer's policy can let an outage fail open (`fail_open`, or the run's own `fail_modes`), and only an outage: a refusal is never a reason to run.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The signer's wire (canonical JSON, its digests, the RPC calls), the `:TOOL` gate, and `/tracekit`. |
| `nodecode-tracekit.asd` | The system and its test system. |
| `test/` | A scripted signer on a loopback port, and the gate's verdicts. |

## What a rewrite leaves out

- **The Python package.** The cell speaks the signer's own RPC (`POST /v2/rpc`, one canonical-JSON frame each way) from Lisp, so nothing of `tracekit-ai` — no Python, no dependency to install — is carried.
- **Starting the dev signer for you.** The original autospawns a same-user dev signer on the first call; the cell expects one at the address you give it, so a signer is always something you can see and stop.
- **One run per session.** Hermes gives its hooks a session id; Nodecode's `:TOOL` point carries none, so a Nodecode process is one run.
- **The Unix-socket and `tcp://` transports.** HTTP is the one transport the signer serves for a client outside its host; the loopback dev signer and a deployed signer both speak it.
