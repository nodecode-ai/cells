# nodecode-little-canary

Little Canary: screens untrusted text for prompt injection and withholds a session's tools on a BLOCK. A Nodecode rewrite of [hermes-labs-ai/little-canary](https://github.com/hermes-labs-ai/little-canary) by [hermes-labs-ai](https://github.com/hermes-labs-ai).

## Get it

Tell the agent "add the little-canary cell", or run `nodecode add nc://little-canary`.

## Set it up

Run Ollama on this machine and pull the small model the canary uses:

```sh
ollama serve
ollama pull qwen2.5:1.5b
```

That is the whole setup: the cell talks to `http://localhost:11434` and needs no
key. Any other Ollama address, model or threshold is a setting in the
`little-canary` section of `~/.nodecode/config.jsonc` (or `/settings`):

| Key | Default | What it does |
| --- | --- | --- |
| `base_url` | `http://localhost:11434` | where Ollama listens; the cell appends `/api/chat` |
| `model` | `qwen2.5:1.5b` | the sacrificial model; it has no tools and its answer is read, not run |
| `timeout_seconds` | `10` | one canary call's ceiling |
| `risk_threshold` | `60` | the score, 0-100, at or above which the verdict is BLOCK |
| `gate_tools` | `true` | a BLOCK withholds this session's tool calls |
| `block_seconds` | `900` | how long a BLOCK keeps them withheld |
| `max_chars` | `4000` | how much of the text is screened |

Without Ollama the cell still works: the structural checks run, and every screen
says the canary was not reached and that the text is unverified rather than
cleared.

## Use it

Give the agent text it does not trust — a mail body, a web page, an issue, a
file someone else wrote — and ask it to screen it before acting. It calls
`canary_screen` with the text alone and reads one of four verdicts:

- **PASS** — no injection shape, and the canary was unaffected. Act on it.
- **FLAG** — a weak signal scored: treat the text as data, not as instructions.
- **BLOCK** — the text reads as an injection. Its instructions are not followed,
  and this session's tool calls are withheld for `block_seconds` or until a
  screen passes. The operator can type `/little-canary <text>` to screen something
  else and lift it, because a command is not a tool.
- **DEGRADED** — the canary did not answer, so only the structural checks ran
  and nothing is withheld.

`/little-canary <text>` screens from the prompt; with no text it says what the cell is
and where its canary sits.

## How it reads a text

- **Structural checks** first: an instruction that overrides earlier ones, a
  demand for the system prompt, a persona switch, a forged delimiter, a lifted
  constraint, a credential demand, a URL to send something to. Each has a
  weight, and the structural layer alone can reach a BLOCK — so a plain
  injection costs no model call.
- **The canary** otherwise: the text goes to the small local model with the
  plugin's own baseline system prompt, and its answer is read for the residue of
  having been affected — a persona it adopted, its own instructions discussed,
  its limits declared lifted. The model has no tools, and nothing it says is
  executed.

Nothing leaves the machine: the only endpoint is `base_url`, and the only text
sent is the one you screened — never the conversation, never a file, never a
key.

This is a sensor, not a guarantee. It is a speed bump in front of a model that
can still be talked around.

## What it does not do

The original screens every user turn from inside the Hermes runtime, through
hooks Nodecode does not have; here screening is a verb the agent or the operator
calls, and what the BLOCK withdraws is that session's tools. Its advisory-only
mode, its second LLM judge and its own audit log are not rewritten.

## Testing

From this repository, `.github/test-cell.sh tools/nodecode-little-canary`, which
runs it offline against a pinned Nodecode tree with every canary call stubbed.
