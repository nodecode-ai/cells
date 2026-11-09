# nodecode-tenuo

Tenuo: a warrant checked before every tool call runs, down to the arguments, with a local audit log of every decision. A Nodecode rewrite of [tenuo-ai/hermes-tenuo](https://github.com/tenuo-ai/hermes-tenuo) by [tenuo-ai](https://github.com/tenuo-ai).

## Get it

Tell the agent "add the tenuo cell", or run `nodecode add nc://tenuo`.

## Set it up

Write a warrant — a small JSON file naming the tools a call may use and, per tool, what its arguments may be:

```json
{
  "expires_at": "2030-01-01T00:00:00Z",
  "capabilities": {
    "eval": "*",
    "read_file": {"path": {"under": "/data/reports"}},
    "websearch": {"query": {"glob": "release-*"}},
    "cron": {"action": {"one_of": ["list", "show"]}}
  }
}
```

Then point the cell at it in `/settings` (the `tenuo` section), or ask in the conversation:

| Setting | What it is |
| --- | --- |
| `warrant` | the JSON warrant file. Unset means a no-op: every call passes. |
| `key_env` | optional: the environment variable holding the HMAC key the warrant is signed with. With a key set, a warrant with no matching `<warrant>.sig` refuses every call. `/tenuo sign` writes that file. |
| `audit_log` | where decisions are appended. Default `tenuo/audit.jsonl` under the Nodecode home. |
| `on_denial` | `block` (default) refuses the call; `log` records the denial and lets the call run. |

A call passes when its tool is in the warrant and every argument the warrant names holds its constraint. A literal is an exact value, `"*"` is anything, `{"under": DIR}` is DIR or a path beneath it (`..` resolved first, so `/data/reports/../../etc/passwd` is not under it, and `/data/reports-x` is not either), `{"glob": PATTERN}` matches `*` and `?`, and `{"one_of": [...]}` is membership. A named argument the call does not carry refuses; a tool the warrant does not name refuses; past `expires_at` everything refuses.

## Use it

A refused call is the call's failure and the model reads why:

```text
ERROR: refused by nodecode-tenuo: read_file is not in the warrant; read_file did not run ...
```

Ask, in the conversation or in eval:

- **`/tenuo`** — the warrant, its expiry, whether its signature verifies, and how many decisions the log holds. `/tenuo audit`, `/tenuo denied`, `/tenuo sign`, or `/tenuo read_file` to ask whether a bare call would pass.
- **`tenuo_check`** — the model asks whether a call it is about to make would pass, with the arguments it would carry, and changes its plan instead of being refused.
- **`tenuo_audit`** — the newest decisions, or only the refusals.

The audit log is one JSON object per line: `at`, `tool`, `arguments`, `decision`, `reason`, `call_id`.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The cell: the warrant reader, the constraint matching, the `:TOOL` hook, the two tools, the `/tenuo` command. |
| `test/` | Offline tests: the decision table, the audit log, both tools. |
| `LICENSE` | Apache-2.0, the original repository's. |
