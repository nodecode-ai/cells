# nodecode-kata

Kata: the tool routines a session repeats, mined and compiled into one call. A Nodecode rewrite of [opeblow/kata](https://github.com/opeblow/kata) by [opeblow](https://github.com/opeblow).

## Get it

Tell the agent "add the kata cell", or run `nodecode add nc://kata`.

## Use it

Every tool call a turn runs passes through the cell's `:tool` hook. Nothing
needs to be switched on: the record fills itself, arguments masked (a value
under a credential-shaped name is kept as `[set]`).

- `/kata` or `/kata list` — the windows of tool names that repeat across
  separate sessions, best first: the id, the tools in order, the sessions
  and the fit.
- `/kata compile ID` — keep one as a routine named `kata_<tools>`, in
  `kata/routines.json` under the home, so it is there at the next start.
- `/kata show NAME` — its steps, each argument a constant or a parameter.
- `/kata run NAME path=/a/b` — the steps dispatched in order, one call.
- `/kata forget NAME`, `/kata clear`.

The model has the same table as one tool, `kata`, with `action` one of
`list`, `show`, `compile`, `run`, `forget`, `clear`, and `params` an object
for `run`. Through eval, `(help :kata)` is the primer and the verbs are
`kata:mined`, `kata:compile-routine`, `kata:show`, `kata:routines`,
`kata:run-routine`, `kata:forget-routine` and `kata:history`.

A step's argument is a constant when every occurrence carried the same text
and a parameter otherwise, so a run refuses until each parameter has a
value. A routine is a shortcut, not a guard: running one calls its tools
exactly as written, side effects included.

## Set it up

Nothing is required. The section is optional, and every member has a
default:

```jsonc
{ "kata": { "record": true, "history": 200, "max_steps": 6, "min_support": 2 } }
```

`min_support` is how many separate sessions a window must repeat in before
it is offered; `max_steps` bounds a routine's length; `history` bounds the
record kept in memory.

## Credits and what changed

Original: [opeblow/kata](https://github.com/opeblow/kata) at commit
`cc59b08f1bbeeb02524c060faac04edb34de0564`, MIT. The Hermes plugin watches
the harness's tool events, mines them into routine candidates, scores each by
leave-one-out fit, validates it against a battery of checks, classifies each
step's effect and asks for approval on the ones with side effects, keeps all
of it in SQLite, and shows it on a dashboard page. This rewrite keeps the
arc — record, mine, generalise, compile, replay — on the seams Nodecode has:
one `:tool` hook, one tool, one `/kata` command, one JSON file. See `NOTICE`.
