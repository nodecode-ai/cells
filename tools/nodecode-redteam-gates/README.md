# nodecode-redteam-gates

RedTeam Gates: the stage gates of eight red-team work modes, checked by a tool the model must call instead of grading itself. A Nodecode rewrite of [SeaOf0/dsh-redteam-model](https://github.com/SeaOf0/dsh-redteam-model) by [SeaOf0](https://github.com/SeaOf0).

## Get it

Tell the agent "add the redteam-gates cell", or run `nodecode add nc://redteam-gates`.

## Set it up

Nothing is required: the cell is on from the first message and reads the directory it is run in. The `redteam-gates` section points it at an engagement's own folder and bounds what a verdict reads back - `workspace` (`.`; where the artifacts and `gate-log.md` live) and `log_rows` (20; how many of the log's newest rows a verdict shows). `config.example.jsonc` is the section written out.

## Use it

A red-team playbook breaks the work into stages and closes each one on a gate: the assets are mapped, the finding has its comparison set, the coverage matrix is filled, the sample is registered with its hash, the report carries every section. In the original that gate is a text contract, and a model can talk itself past it. Here the gate is a tool call:

- `stage_gate` takes `mode` and `stage`, reads the stage's artifacts under the workspace, and answers what is structurally there and what is not: a file missing or empty, a required marker absent, a table row that is not filled, a sample registration with no 64-digit hash. Every verdict appends one row to `<workspace>/gate-log.md`, the trail a report chapter reads.
- `gates_list` is the schema: every mode's stages, the files each one reads, whether it wants a `file` argument of its own, and the judgements that stay with a reviewer.
- `/redteam-gates` does the same for the operator: bare, the modes; with a mode, its stages; with a mode and a stage, the verdict.
- `(help :redteam-gates)` is the manual.

The eight gated modes, and what closes them:

| Mode | Gates | What the last one closes |
| --- | --- | --- |
| `pentest` | P1 assets and environment, P2 a finding's comparison set, P3 coverage and review ledger | the coverage matrix and the review log |
| `code-audit` | A1 surface map, A2 chain agreement, A3 coverage and scan reconciliation | hits reconciled against what reached a final state |
| `binary-analysis` | B0 sample registration, B1 restoration integrity, B2 coverage and hypotheses | the hypothesis ledger, decided or not |
| `attack-defense` | recon, breach, lateral, persistence, report | the assessment report and the operator trace ledger |
| `av-evasion` | V1 experiment plan, V3 pairing, V2 evidence, V4 extrapolation | that the conclusion is no wider than what was tested |
| `incident-response` | I1 preservation, I2 timeline, I3 classification, I4 remediation, I5 report | the incident report |
| `cloud-security` | C1 assets and exposure, C2 attack paths, C3 lateral and persistence, C4 permission chains, C5 detection gaps, C6 environment restore, C7 report | the cloud assessment report |
| `ctf-solver` | board, flag | the flag ledger |

A structural pass is not a full pass, and every verdict says so: whether a diff really flips, whether two chains agree, whether an N-A cell's reason stands is a reviewer's call, and each gate lists those lines by name. What the tool settles is the part that can be settled by reading files, so a stage cannot be declared done on artifacts that were never written.

What it does not do: it does not write the artifacts, does not run a scanner, does not reach the network, and does not read a credential. It also does not carry the original's playbook prose - each mode's persona, skills and reference library - which in the original are agent presets a session is started with; this cell carries the gates and the mode of work they belong to.

## What is in it

| File | What it is |
| --- | --- |
| `gates.lisp` | The schema: eight modes, 32 gates, their checks and their manual lines. Data, so a layer can extend it. |
| `cell.lisp` | The checks, the log, the two tools, `/redteam-gates` and the manual. |
| `nodecode-redteam-gates.asd` | The system and its test system. |
| `test/` | The offline tests: lifecycle, a gate that passes, one that does not, the file argument, the schema, the command, the log, the section. |
| `config.example.jsonc` | The section, written out as documentation. |

## License

MIT. The original is Copyright (c) 2026 SeaOf0: see `LICENSE` and `NOTICE`.
