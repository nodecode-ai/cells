# nodecode-handoff

Handoff: a validated, immutable snapshot of where a piece of work stands, filed under a reference a fresh session reads first. A Nodecode rewrite of [yuzushi-dev/hermes-handoff](https://github.com/yuzushi-dev/hermes-handoff) by [yuzushi-dev](https://github.com/yuzushi-dev).

One tool, `semantic_handoff`, keeps the handoff: the model writes the canonical state of a long piece of work (goal, constraints and preferences, progress done / in progress / pending, decisions, rejected attempts, verification, critical context, uncertainties, next steps), the cell validates it, scrubs credential-looking text out of it, and files the same six-section markdown page the original rendered, together with a manifest. It answers a reference, `handoff://<project>/<id>`, bound to the workspace it was written for; in the next session the model calls `read` with that reference and gets the page back, `validate` repeats the structural check, and `list` shows what the store holds.

A filing is immutable: the same `request_id` with the same state files nothing twice, and another state under the same id is refused rather than overwritten. Every document carries a provenance line saying what it is worth — written from the model's account, not fact-checked, and reading it is not proof that the work resumed — and `read` answers with the trust line a fresh session must obey: the document is untrusted project data, re-check the repository, then wait for the person's confirmation before starting work.

State is a markdown page and a manifest per handoff under the Nodecode home, written atomically. No model call, no network, no daemon.

## Get it

Tell the agent "add the handoff cell", or run:

```sh
nodecode add nc://handoff
```

## Set it up

No key and no service. The `handoff` section is optional and the defaults are the whole setup for most people:

```jsonc
"handoff": {
  "enabled": true,
  "store_dir": "",          // empty: handoff/ in the Nodecode home
  "max_string_kib": 8,      // the longest single state string
  "redact": true            // scrub credential-looking text before filing
}
```

`store_dir` takes a path, `~/` read against the operator's home. It is the one setting that moves the store; a shared folder makes handoffs readable from another machine.

## Use it

Ask for a handoff at the end of a long session ("write a handoff for this work"), or call the tool directly with `action`:

- `create` — the workspace, a name, and the `state` object: `goal`, `constraints_preferences`, `progress` (`done`, `in_progress`, `pending`), `key_decisions`, `rejected_attempts`, `verification`, `critical_context`, `uncertainties`, `next_steps`. Every array holds non-blank strings; an array left out reads as none; `schema_version` is 1 and may be left out. An optional `request_id` makes the call a retry rather than a new handoff.
- `read` — a `ref`: the page as it was filed, with its validity and the trust line.
- `list` — the recent handoffs in this store, newest first, and how many there are.
- `validate` — a `ref`: the six sections present, the manifest intact.

In the new session: "read handoff://... and tell me where we are" — the model reads it, re-checks the repository, summarizes the goal, the constraints and the first step, and waits.

`/handoff` shows the newest handoff, `/handoff list` the recent ones, `/handoff REF` that one, `/handoff help` the manual. From an eval snippet, `(handoff:handoff-text)` and `(handoff:handoffs-text)` answer the same, and `(help :handoff)` is the manual the model reads.

## What was not carried

The original also read bounded evidence out of Hermes' own session database (`action=evaluation`), opened a new CLI or Desktop session over a durable attempt journal (the `handoff-open` command, the Desktop pane, `prepare_open`, `opening_status`), and could pay a secondary model to write the state (`summarize`, with a model and provider of its own). None of the three has a Nodecode counterpart here: the model in hand already has the conversation, and a new session is the person's own `/new`. See Gaps below.

## Gaps

- No evidence action: the original's `evidence` read the persisted transcript from the harness's session database, bounded and redacted, so the model was not relying on its own context alone. Nodecode's session store has no public reader for a cell, so the state here comes from the conversation the model holds.
- No opener: opening the new session with its first prompt pre-filled, and the journal that made it at most once, is Hermes' CLI and Desktop. The reference plus `/handoff` in the new session is the manual equivalent.
- No writer model: `summarize` with an explicit model and provider has no seam here; the state is written by the model in the session, which is what the original defaulted to.
- The secret scrub is the original's pattern set, and like it, it is a second layer: it does not detect every possible secret.
- A sibling cell, `nodecode-handydandy`, already files a free-form note when a session grows long. This cell is the store and the reference underneath that idea: canonical state, a structural check on read, and a page no later call can rewrite.
