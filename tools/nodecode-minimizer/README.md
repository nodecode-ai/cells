# nodecode-minimizer

Minimizer: keeps a coding agent on the smallest complete change, said at the start of every turn. A Nodecode rewrite of [nosliwhtes/hermes-minimizer](https://github.com/nosliwhtes/hermes-minimizer) by [nosliwhtes](https://github.com/nosliwhtes).

## Get it

Tell the agent "add the minimizer cell", or run `nodecode add nc://minimizer`.

## Use it

Turn it on and nothing else happens: the first request of every turn carries the policy as that turn's reminder, and later rounds of the turn build on it. The words are the original's, whole:

- build the simplest complete system, and trace behavior and invariants before editing;
- take the first fitting rung — reuse what already behaves that way, else extend the layer that owns it, else the standard library or an installed dependency, else a small implementation where the invariant belongs, else the coherent refactor the task needs;
- no speculative features, imagined extension points, single-implementation interfaces, configuration for fixed values or premature services;
- keep every safeguard, approval boundary and explicitly requested behavior;
- prove the requested behavior before calling it done.

```lisp
(/minimizer)          ; what is being said, and the exact text of it
(/minimizer brief)    ; the rung ladder alone
(/minimizer full)     ; the whole policy
(help :minimizer)     ; the manual topic
```

Two settings, in `/settings`: `mode` is `full` (the whole policy) or `brief` (the opening and the five rungs alone), and `max_chars` caps how much one request carries, marked when it is cut.

It is guidance, not enforcement. Nothing here checks that a change was small, or that a check ran; the policy says so itself, and says that system, user and repository instructions outrank it.

A turn's reminder rides the `:ROUND` point and is recorded through `nlk:record-turn-context-prompt`, so the text is in the request being built rather than one round later. It repeats at each turn's start rather than in a system-prompt section, which Nodecode has no room for a folder to add to.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The policy text, the `:ROUND` hook, `(help :minimizer)` and `/minimizer`. |
| `test/` | The test slice: the hook, the two modes, the cap and the command, over a stubbed reminder seam. |

## License

MIT, the original's. See `LICENSE` and `NOTICE`.
