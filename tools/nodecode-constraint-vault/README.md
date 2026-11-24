# nodecode-constraint-vault

Constraint Vault: pins the operator's exact constraints and delivers them verbatim in front of the model on every request. A Nodecode rewrite of [jusst01/constraint-vault](https://github.com/jusst01/constraint-vault) by [jusst01](https://github.com/jusst01).

Memory summarises and compression paraphrases, so "never spend more than 0.05
ETH" comes back as "avoid large spends" and an address loses a character. The
vault keeps the few facts that must survive exactly as the operator typed
them: one JSON file under the home, a standing section built from it
byte-for-byte at every request, and a log line per delivery carrying the
SHA-256 of the block that went out. A constraint the model can read verbatim
is one it can follow, and one the operator can hold it to.

## Get it

Tell the agent "add the constraint-vault cell", or run `nodecode add nc://constraint-vault`.

## Set it up

One setting is needed: `constraint-vault.store_file`, the JSON file the
constraints are kept in — `constraint-vault/vault.json` under the home is the
usual answer. Set it in `/settings`, or in the `constraint-vault` section of
`~/.nodecode/config.jsonc`. Until it is set the three tools and
`/constraints` refuse and name the setting; the cell itself still starts.

Four optional knobs sit beside it: `max_constraints` (50), `max_chars`
(2000), `inject` (true) and `log_injections` (true). `inject` false keeps
the constraints and stops putting them in front of the model; `log_injections`
false stops only the log, which is written as `<store name>-injections.jsonl`
beside the store.

## Use it

Tell the agent a rule that must keep applying exactly — "never spend more than
0.05 ETH per swap, pin that" — and it calls `constraint_pin` with the words as
stated. From the next request's build, every turn opens with them:

```
[Constraint vault — exact values, verbatim]
The operator pinned these exactly. They are in force for this whole session: do not paraphrase, round, reinterpret or drop any of them.
1. never spend more than 0.05 ETH per swap
```

The model also has `constraint_list` (with an optional substring filter) and
`constraint_drop`. The operator types `/constraints` for the list,
`/constraints add <exact text>` to pin one and `/constraints drop <id>` to
remove one, and `(help :constraints)` has the same primer for the model.

A constraint belongs to the organism, not to a session: every session reads
the same set. Pinning the same text twice is no error and pins nothing; a
constraint longer than `max_chars`, or a vault already at `max_constraints`,
refuses and says which knob to change.

### What each delivery leaves behind

Every request that carries the block appends one line to the log:
when, the session, `"via": "harness"`, how many constraints, and the block's
SHA-256. A hash that matches a block rebuilt from the store is proof the exact
bytes went out, not a paraphrase of them.

## What is in it

| File | What it is |
| --- | --- |
| `cell.lisp` | The whole cell: the store, the three tools, `/constraints`, the standing-section hook, the section declaration. |
| `test/cell-test.lisp` | The tests: lifecycle and the refusals, pin/list/drop through the tools and the command, the verbatim section and its delivery into the prompt a request is built from, the logged hash, both switches, and a constraint outliving a restart. |
| `config.example.jsonc` | The section, commented. |
| `LICENSE` | The original's MIT licence. |

## Run the tests

```
.github/test-cell.sh tools/nodecode-constraint-vault
```

The store is a file in the run's scratch home, so nothing touches the network
and nothing outside it is read.

## Credits

jusst01's [constraint-vault](https://github.com/jusst01/constraint-vault) at
`e794972`, MIT, copyright (c) 2026 jusst. See [NOTICE](NOTICE) for what the
rewrite keeps and what it leaves out.
