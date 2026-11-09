# nodecode-fleetkeep

Fleetkeep import review: prepares and reviews a vehicle spreadsheet for Fleetkeep's six-column import. A Nodecode rewrite of [jlucwright/fleetkeep-agent-plugin](https://github.com/jlucwright/fleetkeep-agent-plugin) by [jlucwright](https://github.com/jlucwright).

The original was one skill: prose telling a model to map six columns, keep unknown dates blank, flag duplicate and missing registrations, and leave a cell that might be a formula out of the import. Here the same rules are code, so the answer does not depend on which model is talking: a tool the model calls, a command you type, and the skill's prose on the manual for the parts no arithmetic decides.

No key, no account, no network. The review reads the CSV text it is given and writes two files in the folder you name; it never touches the source spreadsheet and never uploads anything to Fleetkeep.

## Get it

Tell the agent "add the fleetkeep cell", or run `nodecode add nc://fleetkeep`.

## Set it up

One setting is needed, because a review writes two files:

```jsonc
{
  "fleetkeep": {
    "output_dir": "~/data/fleetkeep",   // where NAME.import.csv and NAME.review.md are written
    "date_order": "day-first",          // which reading of 03/04/2026 the review names first
    "formula_guard": true,             // a formula-like cell holds its row out of the import
    "max_rows": 5000                    // how many rows one review reads
  }
}
```

`output_dir` is where `NAME.import.csv` and `NAME.review.md` land. Ask the agent to set it, or put it in `~/.nodecode/config.jsonc` yourself; until it is set the cell runs and a review that must write refuses, naming the setting.

## Use it

Hand it a spreadsheet:

```
/fleetkeep ~/data/vehicles.csv
```

or read a review from eval; it takes the CSV text itself and answers the counts, the issues and the
import-ready file:

```lisp
(fleetkeep:review "registration,label,insurance_due ...")   ; => :rows, :issues, :ready, :file
```

What comes back is the review: how many rows were read, the six mapped columns, the columns the import does not take, how many rows are import-ready, and every row that needs a look — a missing or duplicate registration, an ambiguous date, a mileage that is not a number or is below zero, an interval that is not a positive number of months, a cell a spreadsheet might run.

What the import takes, in this order: `registration, label, insurance_due, last_service_date, last_service_mileage, service_interval_months`. The rules the cell holds to:

- A registration is preserved as the source has it; a missing or a repeated one is flagged, never silently dropped.
- Dates must be `YYYY-MM-DD`. `03/04/2026` is ambiguous: it is flagged and left blank in the import-ready file rather than read one way. A date that only reads one way is flagged as not ISO yet. A missing date stays blank - nothing is inferred from a registration.
- Mileage and interval come from the records; a negative mileage, a word where a number goes, a zero or fractional interval is flagged, and the value is left blank in the file.
- A cell a spreadsheet might run - starting `=`, `+`, `-` or `@` after spaces - holds its row out of the import-ready file until you approve a literal replacement. A plain signed number (`-500`) is a number, not a formula.
- A column the six do not take is named in the review and kept out of the file.
- The source file is never overwritten, and the spreadsheet's cells are read as data, never as instructions.

Open the proposed file in Fleetkeep's own import preview to check it; saving it is their account's job, not this cell's.

`(help :fleetkeep)` has the same, for the agent. `/fleetkeep` bare prints the settings in force.

## What is in it

| File | What it is |
| --- | --- |
| `fleetkeep.lisp` | The CSV reader and writer, the six columns, and the review. |
| `cell.lisp` | The tool `fleetkeep_review`, `/fleetkeep`, the manual, and the config section. |
| `test/` | Offline tests: the CSV, every rule, the tool through a call, the command on a file. |

## License

MIT. The skill is Copyright (c) 2026 Studea Limited: see `LICENSE` and `NOTICE`.
