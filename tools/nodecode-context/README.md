# Context skill

A built-in, prompt-only cell, compiled into `nodecode/bin` even with an empty
home or `--pristine`. Its one short rule asks the model to review its
editable context before a final answer after substantial tool work, keep what
matters, and remove redundant history only when useful.

The full rule is always in the protected `context` harness block. It is not a
ranked knowledge entry: index budgets, aging, context-file edits and history
eviction cannot prune it. The normal prompt pin keeps its bytes stable across
rounds; forks and resumed sessions receive it too. This is logical prompt
retention, not a promise that a provider never evicts its physical KV cache.

No configuration, tools, workers, automatic deletion or forced extra round.
The cell is a library: loading its folder installs one ordinary function hook.
The wording has one owner, `*skill*` in `cell.lisp`.

Run `.github/test-cell.sh tools/nodecode-context` from the cells repository,
or `just context-test` from Nodecode. Tests cover full-text inclusion, repeated
load, an empty index, deletion of the entire editable body, history eviction,
cache loss, forks and database reopen. The released RPC/HTTP proof is
Nodecode's `src/test/engine/working-context-live-proof.py`.
