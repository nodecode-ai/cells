# Context skill

A built-in, prompt-only cell, compiled into `nodecode/bin` even with an empty
home or `--pristine`. Its one short rule asks the model to review its
editable context at useful checkpoints during substantial tool work (for example,
after settling a subproblem) and before a final answer, and says how: one call,
`(context :drop SPANS :keep IDS :note TEXT)`, naming blocks by the `[seq N]` line
every tool result ends with, with no export, read or print round. Keep what
matters and remove redundant history only when useful: not after every tool call,
and not while a subproblem is still open (an ARC-AGI-3 run, 2026-10-14: a wipe in
the middle of a level was followed by 15 calls and one game action).

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
