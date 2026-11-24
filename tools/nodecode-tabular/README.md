# nodecode-tabular

Tabular: profile and split a delimited file, and the skill pack a tabular analysis starts from. A Nodecode rewrite of [frankiethull/hermes-tabular](https://github.com/frankiethull/hermes-tabular) by [frankiethull](https://github.com/frankiethull).

## Get it

Tell the agent "add the tabular cell", or run `nodecode add nc://tabular`.

## Set it up

Nothing is required to profile and split files. A saved profile needs one setting, because it writes a file:

```jsonc
{
  "tabular": {
    "workspace": "~/data",        // where a relative path is read from; empty means the process's own
    "delimiter": ",",             // "," ";" "|" or "tab"
    "max_rows_scanned": 20000,   // how many rows a profile reads; the row count is still the file's
    "artifacts_dir": "~/data/profiles"  // where tabular_save writes; only that verb needs it
  }
}
```

`(help :tabular)` has the workflow, and `/tabular` typed alone prints the settings in force.

## Use it

Ask about a table, or call a verb from eval:

```lisp
(tabular:profile-file "churn.csv" :target "churn")
(tabular:split-file "churn.csv" :target "churn" :seed 42)
(tabular:save-profile "churn.csv" :target "churn" :name "run-1")
```

The model reaches the same three as the tools `tabular_profile`, `tabular_split` and `tabular_save`.

`profile-file` answers one report: shape and band, the task the target implies, how the policy splits it, the model order to try, the metric to report, one line per column with its role, and the notes the pack would raise -- keys to drop, a column missing in a fifth of the rows, a name that looks recorded after the outcome. It fits nothing and changes nothing.

`split-file` writes `<name>.train.csv` and `<name>.test.csv` beside the file, or into `out_dir`. A classification target is stratified, so each class keeps its share exactly; rows are ranked by `(seed, row index)`, so the same seed on the same file reproduces the same halves on any machine, and another seed picks other rows. `keys` and a time column are named in the profile's split line: honouring a grouping or temporal constraint is still the caller's call.

`save-profile` writes the same profile as JSON under `artifacts_dir`, for a later turn to read instead of profiling the file again.

The pack's Python steps -- pandas for loading, CatBoost, TabICL, joblib checkpoints -- are not replaced. They run in the project's own environment, on the split this cell wrote; `skill/` carries the pack's own skills for the model to read.

## What is in it

| File | What it is |
| --- | --- |
| `table.lisp` | Delimited text read and written in Lisp: RFC 4180 quoting both ways. |
| `profile.lisp` | The profile, the policy numbers, the report and the split. |
| `cell.lisp` | The section, the three tools, the manual, `/tabular`. |
| `skill/` | The original pack, unchanged: its eleven `SKILL.md` files, `defaults.yaml`, `references/pitfalls.md` and the three worked examples. |

## License

MIT. The pack is Copyright (c) 2026 hermes-tabular contributors: see `LICENSE` and `NOTICE`.
