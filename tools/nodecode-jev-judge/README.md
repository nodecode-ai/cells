# nodecode-jev-judge

Jev judge: one Jev judgment gates a call before it runs. A Nodecode rewrite of [DoGMaTiiC/hermes-jev](https://github.com/DoGMaTiiC/hermes-jev) by [DoGMaTiiC](https://github.com/DoGMaTiiC).

Before `eval` runs, one request to [TypeSafe](https://typesafe.ai)'s Jev asks three typed questions about the call — would it destroy or overwrite data, would it send local data out, how much damage if the operator did not want it — and the answers decide. Thresholds live in code (destructive 0.90, exfiltration 0.70, impact 2.5 of 3), so the model never sets the bar it is measured against. Two modes:

| Mode | On a flagged call |
| --- | --- |
| `shadow` (default) | nothing happens; one JSON line says what the judge thought |
| `enforce` | the call is put to the operator as **Allow once** / **Deny**, and runs only on Allow once; with nobody there to ask, it is refused |

The gate fails open on every path: no key, a timeout, an HTTP error, a body of the wrong shape, any question left unanswered, or a bug in the gate itself — the call runs exactly as it would have, and the log line says which it was. A gate that can stop a turn when it is broken is worse than no gate.

The model can also ask Jev itself, through `eval`, as the original's `jev_ask` tool did:

```lisp
(jev-judge:ask "The patch renames the public function parse-config."
         "{\"risky\":{\"type\":\"bool\",\"instructions\":\"Does this change risk breaking callers?\"}}")
;; => (:RISKY (:TYPE :BOOLEAN :PROBABILITY 0.82d0)), 540
```

## Get it

Tell the agent "add the jev-judge cell", or run `nodecode add nc://jev-judge`.

## Set it up

A key is the one thing it needs. Make a TypeSafe key at <https://console.typesafe.ai/>, then either export `TYPESAFE_API_KEY` in the shell that starts Nodecode, or save it where a provider's key lives (`~/.nodecode/auth.json`, `api_keys.typesafe`). An `OPENROUTER_API_KEY` works too: `backend: auto` takes a TypeSafe key when there is one, else OpenRouter's decisions route. With no key anywhere the cell refuses at start and says so, instead of sitting there judging nothing.

```jsonc
"jev-judge": {
  "mode": "shadow",          // or "enforce"
  "backend": "auto",         // "typesafe" | "openrouter"
  "timeout_seconds": 3,
  "cache_seconds": 300
}
```

In `/settings`, or the `jev-judge` section of `config.jsonc`. Everything else has a default worth keeping; the **Set it up** panel names each one.

## Use it

- **In shadow mode** — on, and read the log. Every decision is one JSON line under `logs/jev-judge.log` in the Nodecode home: `clear`, `flagged_shadow`, `escalated`, `allowed_after_ask`, `refused`, `fail_open` with its reason (`no_key`, `timeout`, `http_503`, `parse`, `payload_too_large`, `answer_missing`), and the three signals behind it.
- **In enforce mode** — a flagged call earns a card above the message box in the page or the desktop app. Allow once runs that one call; Deny refuses it and tells the model the operator chose Deny, so it asks instead of working around it; no answer in `ask_seconds` (default 120) refuses it, saying which it was. Nobody to ask (a chat lane, a terminal alone) is the refusal at once.
- **`/jev-judge`** says where the judge points — the mode, the backend and where its key comes from (never the key), what it gates, and the log's path — and offers **Check the judge**, which asks one judgment of a sample call. `/jev-judge check` does the same from the command line.

```
jev-judge: shadow mode, typesafe backend (key from the environment), judging eval, log ~/.nodecode/logs/jev-judge.log
```

## Settings

| Key | Default | What it does |
| --- | --- | --- |
| `mode` | `shadow` | `shadow` logs and never interferes; `enforce` asks, and refuses when nobody answers |
| `tools` | `["eval"]` | the tool names the gate reads; Nodecode has one tool, `eval` |
| `backend` | `auto` | `auto` uses the TypeSafe key when there is one, else OpenRouter; forcing one with no key is refused at start |
| `timeout_seconds` | `3` | one judgment's ceiling; clamped to 10 s in code |
| `cache_seconds` | `300` | an identical call is judged once per window |
| `typesafe_base_url` | empty | empty is `TYPESAFE_BASE_URL`, else `https://api.typesafe.ai` |
| `typesafe_model` | empty | empty is `TYPESAFE_DEFAULT_MODEL`, else `jev-latest` |
| `openrouter_base_url` | `https://openrouter.ai/api/alpha` | decisions post to `<base>/decisions` |
| `openrouter_model` | `~typesafe/jev-latest` | the slug the OpenRouter backend names in its body |
| `ask` | `true` | ask Allow once / Deny in enforce mode; nobody there is the refusal |
| `ask_seconds` | `120` | how long that question waits |
| `log_path` | empty | empty is `logs/jev-judge.log` under the Nodecode home |

## What leaves the machine

Per gated call: one JSON payload carrying the tool name and its arguments — the first 12, each redacted (key-shaped runs masked) and clipped to 600 characters — to the backend that judges it: TypeSafe's `/v1/systemone`, or OpenRouter's alpha `/decisions`, using your own key. No conversation history, no files, no memory, no tool output. `(jev-judge:ask ...)` sends exactly the state and questions it was given. A payload over 65536 characters is refused without sending, and a 3xx is never followed.

## Test

From this repository's root, offline against a pinned Nodecode tree:

```sh
.github/test-cell.sh tools/nodecode-jev-judge
```

## Not ported

The Vercel AI Gateway backend (`AI_GATEWAY_API_KEY`, `typesafe-ai/jev` on `ai-gateway.vercel.sh`): two backends cover the keys people have, and a third shape of request and answer is a third thing to keep honest. The rate-limit breaker and the pacing between outgoing calls: one retry after a `Retry-After` of at most 2 s remains, and the cache does the rest. The `gate_version: v2` six-signal ladder (reads_secrets × sends_outbound, blast_radius, self_advocating): the frozen triple is what most operators recognise, and the v2 signals would double the paid request for the same call. `jev_ask` is a function the model calls through `eval`, `(jev-judge:ask state questions)`, rather than a tool of its own — Nodecode's one tool is `eval`.

MIT licensed.
