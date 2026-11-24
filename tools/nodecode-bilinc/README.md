# nodecode-bilinc

Bilinc: hosted, versioned long-term memory with recall, remember, revise and forget. A Nodecode rewrite of [ReARCLabs/bilinc-hermes-plugin](https://github.com/ReARCLabs/bilinc-hermes-plugin) by [ReARCLabs](https://github.com/ReARCLabs).

Bilinc Cloud keeps a workspace's memories server-side, one per key, and every change
keeps the previous value and the reason it was given. The model gets four tools
-- `bilinc_recall`, `bilinc_remember`, `bilinc_revise`, `bilinc_forget` -- and
`/bilinc` to show the cell's state or search the workspace from the shell. With
`auto_recall` on (the default) the turn's input is recalled before each model
request and the memories ride that request; with `capture_turns` on, each
finished turn is stored as a short-lived episodic record.

## Get it

Tell the agent "add the bilinc cell", or run `nodecode add nc://bilinc`.

## Set it up

A key comes from [bilinc.space](https://bilinc.space) (the free plan needs no card).
The `bilinc` section of `config.jsonc`, or `/settings`, names it:

```jsonc
{
  "bilinc": {
    "api_key_env": "BILINC_API_KEY",   // or "api_key" inline, or the variable alone
    "auto_recall": true,
    "recall_limit": 6
  }
}
```

A start with no key anywhere is refused, naming the member; so is a `base_url`
that is not https (http is accepted only for localhost).

| Member | Default | What it does |
| --- | --- | --- |
| `api_key` | unset | the Bilinc API key; `BILINC_API_KEY` and `api_key_env` are read too |
| `api_key_env` | `BILINC_API_KEY` | the environment variable holding the key |
| `base_url` | `https://bilinc.space` | the endpoint: https, or http to a localhost one |
| `auto_recall` | `true` | recall each turn's input and inject the memories into that request |
| `recall_limit` | `6` | how many memories one automatic recall injects (at most 20) |
| `capture_turns` | `false` | store each finished turn as a short-lived episodic record |
| `capture_ttl_days` | `30` | how many days a captured turn is kept |
| `timeout_seconds` | `10` | one request's ceiling |

## Use it

Ask the model to remember something, then ask about it in a later session:

> Remember that I drink green tea and live in Oslo.

The model calls `bilinc_remember` under a key such as `pref:city`. Later, "what
do I drink?" makes it call `bilinc_recall` first, and the answer lists each
memory with its key -- what `bilinc_revise` and `bilinc_forget` take. A revise
keeps the old value; a forget keeps the reason it was given. `/bilinc` prints
the endpoint, whether a key is set, and how many memories were recalled this
session; `/bilinc where do I live` searches the workspace from the shell.

Secrets are a safety net, not a guarantee: strings shaped like a key, a bearer
token or `password=...` are replaced with `[REDACTED]` before a request leaves,
so keep real secrets out of memory anyway.

## What is sent

Over HTTPS to `base_url` (Bilinc Cloud unless the section names another), under
the key, and nothing else:

- the turn's input, as a recall query, before each request (when `auto_recall` is on);
- the arguments of the Bilinc tools the model calls;
- each finished turn's two texts (only when `capture_turns` is on), with the session id;
- the session id on a write, and inside the key of a captured turn.

A failed recall pauses recall for a minute and never stops the turn; a rejected
key, or an exhausted plan, is what the tool answers to the model.

## Not ported

- **Mirroring the host's built-in memory tool.** Nodecode's own memory writes are
  not a seam this cell sees, so the model calls `bilinc_remember` for what should
  outlive the session.
- **`hermes memory setup` and `hermes memory status`.** The section, `/settings`
  and `/bilinc` are the same surface.
- **The Python threads and queue.** The capture runs on a worker fed by the frame
  point; recall before a turn runs on the round's own thread with a short ceiling.

MIT, ReARCLabs's.
